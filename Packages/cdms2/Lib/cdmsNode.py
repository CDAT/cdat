## Automatically adapted for numpy.oldnumeric Aug 01, 2007 by 
## Further modified to be pure new numpy June 24th 2008

"""
CDMS node classes
"""
import numpy
from numpy import get_printoptions, set_printoptions, inf
import CDML
import cdtime
import re
import string
import sys
from types import *
from error import CDMSError

# Regular expressions
_Name = re.compile('[a-zA-Z0-9_:][-a-zA-Z0-9._:]*$') # Note: allows digit as first character
_Integer = re.compile('[0-9]+$')
_ArraySep = re.compile('[\[\],\s]+')
_Illegal = re.compile('([<>&\"\'])|([^\t\r\n -\176\240-\377])')   #" illegal chars in content

# Data types

CdChar = CDML.CdChar
CdByte = CDML.CdByte
CdShort = CDML.CdShort
CdInt = CDML.CdInt
CdLong = CDML.CdLong
CdInt64 = CDML.CdInt64
CdFloat = CDML.CdFloat
CdDouble = CDML.CdDouble
CdString = CDML.CdString
CdFromObject = CDML.CdFromObject
CdAny = CDML.CdAny
CdDatatypes = [CdChar,CdByte,CdShort,CdInt,CdLong,CdInt64,CdFloat,CdDouble,CdString]

CdScalar = CDML.CdScalar
CdArray = CDML.CdArray

NumericToCdType = {numpy.sctype2char(numpy.float32):CdFloat,
                   numpy.sctype2char(numpy.float):CdDouble,
                   numpy.sctype2char(numpy.int16):CdShort,
                   numpy.sctype2char(numpy.int32):CdInt,
                   numpy.sctype2char(numpy.int):CdLong,
                   numpy.sctype2char(numpy.int64):CdInt64,
                   numpy.sctype2char(numpy.intc):CdLong,
                   numpy.sctype2char(numpy.int8):CdByte,
                   'c':CdChar,
                   'B':'B',
                   'H':'H',
                   'L':'L',
                   'q':CdInt64,
                   'Q':'Q',
                   'S':'S'
                   }

CdToNumericType = {CdChar:'c',
                   CdByte:numpy.int8,
                   CdShort:numpy.int16,
                   CdInt:numpy.int32,
                   CdLong:numpy.int,
                   CdInt64:numpy.int64,
                   CdFloat:numpy.float32,
                   CdDouble:numpy.float}

# Grid types
UnknownGridType = "unknown"
GaussianGridType = "gaussian"
UniformGridType = "uniform"
CdGridtypes = [UnknownGridType,GaussianGridType,UniformGridType]

DuplicateIdError = "Duplicate identifier: "
InvalidArgumentError = "Invalid argument: "
InvalidDatatype = "Invalid datatype: "
InvalidGridtype = "Invalid grid type: "
InvalidIdError = "Invalid identifier: "
NotMonotonic = "Result array is not monotonic "
class NotMonotonicError(CDMSError):
    pass

# Array representation
CdVector = 1
CdLinear = 2

# Monotonicity
CdNotMonotonic = 0
CdIncreasing = -1
CdDecreasing = 1
CdSingleton = 2

# Map illegal XML characters to entity references:
# '<' --> &lt;
# '>' --> &gt;
# '&' --> &amp;
# '"' --> &quot;
# "'" --> &apos;
# all other illegal characters are removed #"
def mapIllegalToEntity(matchobj):
    s = matchobj.group(0)
    if s == '<':
        return '&lt;'
    elif s == '>':
        return '&gt;'
    elif s == '&':
        return '&amp;'
    elif s == '"':   #"
        return '&quot;'
    elif s=="'":
        return '&apos;'
    else:
        return ""

# Named node
class CdmsNode:

    def __init__(self, tag, id=None, parent=None):
        if id and _Name.match(id) is None:
            raise CDMSError, InvalidIdError + id
        self.attribute = {}             # External attributes, attribute[name]=(value,cdDatatype)
        self.child = []                 # Children
        self.id = id                    # Identifier string
        self.parent = parent            # Parent node in a tree, None for root
        self.tag = tag                  # XML tag string
        self.content = None             # XML content string
        self.dtd = CDML.CDML().dtd.get(self.tag)  # CDML Document Type Definition for this tag
        self.extra = CDML.CDML().extra.get(self.tag) # Extra datatype constraints
        CdmsNode.mapToExternal(self)    # Don't call subclass mapToExternal!

    # Map to external attributes
    def mapToExternal(self):
        if self.id is not None and _Name.match(self.id) is None:
            raise CDMSError, InvalidIdError + self.id
        if self.id is not None: self.setExternalAttr('id',self.id)

    # Set content from a string. The interpretation
    # of content is class-dependent
    def setContentFromString(self,content):
        self.content=content

    # Get content
    def getContent(self):
        return self.content

    # Add a child node
    def add(self,child):
        if child is not None:
            self.child.append(child)
            child.parent=self
        return child

    # Return a list of child nodes
    def children(self):
        return self.child

    # Get the child node at index k
    def getChildAt(self,index):
        return self.child[index]

    # Remove and return the child at index k
    def removeChildAt(self,index):
        child = self.child[index]
        self.child = self.child[:index]+self.child[index+1:]
        return child

    # Get the number of children
    def getChildCount(self):
        return len(self.child)

    # Get the index of a node
    def getIndex(self,node):
        index = -1
        for i in range(len(self.child)):
            if node is self.child[i]:
                index = i
                break
        return index

    # Get the parent node
    def getParent(self):
        return self.parent

    # True iff node is a leaf node
    def isLeaf(self):
        return self.child==[]

    # Set an external attribute
    # 'attr' is an Attr object
    def setExternalAttrFromAttr(self, attr):
        if attr.value is None: return
        self.attribute[attr.name]=(attr.value,attr.getDatatype())

    # Get an external attribute, as an Attr instance
    def getExternalAttrAsAttr(self, name):
        attrPair = self.attribute.get(name)
        if attrPair:
            (value,datatype) = attrPair
            attr = AttrNode(name,value)
            attr.datatype = datatype
            return attr
        else:
            return None

    # Set an external attribute
    def setExternalAttr(self, name, value, datatype=None):
        attr = AttrNode(name,value)
        attr.datatype = datatype
        self.setExternalAttrFromAttr(attr)

    # Get an external attribute
    def getExternalAttr(self, name):
        attrPair = self.attribute.get(name)
        if attrPair:
            (value,datatype) = attrPair
            return value
        else:
            return None

    # Get a dictionary of external attributes, of form (value,datatype)
    def getExternalDict(self):
        return self.attribute

    # Set the external attribute dictionary. The input dictionary
    # is of the form {name:value,...} where value is a string.
    def setExternalDict(self,dict):
        for key in dict.keys():
            self.attribute[key]=(dict[key],CdString)

    # Write to a file, with formatting.
    # tablevel is the start number of tabs
    def write(self,fd=None,tablevel=0,format=1):
        if fd is None: fd = sys.stdout
        printLimit = get_printoptions()['threshold']
        set_printoptions(threshold=inf)            # Ensure that all Numeric array values will be printed
        if self.dtd:
            validAttrs = self.dtd.keys()
        else:
            validAttrs = None

        if format: fd.write(tablevel*'\t')
        fd.write('<'+self.tag)
        if format: fd.write('\n')

        # Write valid attributes
        for attname in self.attribute.keys():
            if (validAttrs and (attname in validAttrs)) or (not validAttrs):
                if format: fd.write((tablevel+1)*'\t')
                (attval,datatype)=self.attribute[attname]
                # attvalstr = string.replace(str(attval),'"',"'") # Map " to '
                attvalstr = _Illegal.sub(mapIllegalToEntity,str(attval))  # Map illegal chars to entities
                if format:
                    fd.write(attname+'\t="'+attvalstr+'"')
                else:
                    fd.write(' '+attname+'="'+attvalstr+'"')
                if format: fd.write('\n')
        if format: fd.write((tablevel+1)*'\t')
        fd.write('>')
        if format: fd.write('\n')

        # Write extra attributes
        for attname in self.attribute.keys():
            if validAttrs and (attname not in validAttrs):
                (attval,datatype)=self.attribute[attname]
                attr = AttrNode(attname,attval)
                attr.datatype=datatype
                attr.mapToExternal()
                attr.write(fd,tablevel+1,format)

        # Write content
        content = self.getContent()
        if content is not None:
            content = _Illegal.sub(mapIllegalToEntity,content)  # Map illegal chars to entities
            if format: fd.write((tablevel+1)*'\t')
            fd.write(content)
            if format: fd.write('\n')

        # Write children
        for node in self.child:
            node.write(fd,tablevel+1,format)

        if format: fd.write((tablevel+1)*'\t')
        fd.write('</'+self.tag+'>')
        if format: fd.write('\n')
        set_printoptions(threshold=printLimit)  # Restore original

    # Write to a file without formatting. 
    def write_raw(self,fd=None):
        if fd is None: fd = sys.stdout
        self.write(fd,0,0)

    # Write an LDIF (LDAP interchange format) entry
    # parentdn is the parent LDAP distinguished name
    # userAttrs is a string or list of strings of form "attr: value"
    # A trailing newline is added iff format==1
    # Note: unlike write, this does not write children as well
    def write_ldif(self, parentdn, userAttrs=[], fd=None, format=1):
        if fd is None: fd = sys.stdout
        if self.dtd:
            validAttrs = self.dtd.keys()
        else:
            validAttrs = None

        # Write distinguished name
        newdn = "%s=%s,%s"%(self.tag,self.id,parentdn)
        fd.write("dn: %s\n"%newdn)

        # Write valid attributes
        for attname in self.attribute.keys():
            if (validAttrs and (attname in validAttrs)) or (not validAttrs):
                (attval,datatype)=self.attribute[attname]
                # attvalstr = _Illegal.sub(mapIllegalToEntity,str(attval))  # Map illegal chars to entities
                if type(attval)!=StringType:
                    attval = `attval`
                attvalstr = string.strip(attval)
                attvalstr = re.sub('\n','\n ',attvalstr) # Make sure continuation lines are preceded with a space
                if attvalstr=='': attvalstr = "none"
                fd.write("%s: %s\n"%(attname,attvalstr))
        
        # Write extra attributes
        for attname in self.attribute.keys():
            if validAttrs and (attname not in validAttrs):
                (attval,datatype)=self.attribute[attname]
                if type(attval)!=StringType:
                    attval = `attval`
                attval = re.sub('\n','\n ',attval) # Make sure continuation lines are preceded with a space
                fd.write("attr: %s=%s\n"%(attname,attval))

        # Write content
        # content = self.getContent()
        # if content is not None:
        #     content = _Illegal.sub(mapIllegalToEntity,content)  # Map illegal chars to entities
        #     fd.write("value: %s"%(content,))

        # Write user attributes
        if type(userAttrs)==StringType:
            newAttrs = [userAttrs]
        else:
            newAttrs = userAttrs
        for entry in list(newAttrs):
            fd.write("%s\n"%entry)

        # Write classes
        fd.write("objectclass: top\n")
        fd.write("objectclass: %s\n"%(self.tag))

        if format==1:
            fd.write('\n')

        return newdn

    # Validate attributes
    def validate(self,idtable=None):

        # Check validity of enumerated values and references
        validKeys = self.dtd.keys()
        for attname in self.attribute.keys():
            if attname in validKeys:
                (atttype,default)=self.dtd[attname]
                if type(atttype) is TupleType:
                    attval=self.getExternalAttr(attname)
                    assert attval in atttype, 'Invalid attribute %s=%s must be in %s'%(attname,attval,`atttype`)
                elif atttype==CDML.Idref:
                    attval=self.getExternalAttr(attname)
                    if idtable:
                        if not idtable.has_key(attval):
                            print 'Warning: ID reference not found: %s=%s'%(attname,attval)
            
        # Validate children
        for node in self.children():
            node.validate(idtable)
    
# Container object for other CDMS objects
class DatasetNode(CdmsNode):

    def __init__(self, id):
        CdmsNode.__init__(self,"dataset",id )
        self.idtable = {}

    # Validate the dataset and all child nodes
    def validate(self,idtable=None):
        if not idtable:
            idtable=self.idtable
        CdmsNode.validate(self,idtable)

    # Add a child node with an ID
    def addId(self,id,child):
        if self.idtable.has_key(id): 
            raise CDMSError, DuplicateIdError +id
        CdmsNode.add(self,child)
        self.idtable[id]=child
        return child

    # Get a child node from its ID
    def getChildNamed(self,id):
        return self.idtable.get(id)

    # Get the ID table
    def getIdDict(self):
        return self.idtable

    # Dump to a CDML file.
    # path is the file to dump to, or None for standard output.
    # if format is true, write with tab, newline formatting
    def dump(self,path=None,format=1):
        if path:
            try:
                fd = open(path,'w')
            except IOError:
                raise IOError,'%s: %s'%(sys.exc_value,path)
        else:
            fd = sys.stdout
        fd.write('<?xml version="1.0"?>')
        if format: fd.write('\n')
        fd.write('<!DOCTYPE dataset SYSTEM "http://www-pcmdi.llnl.gov/software/cdms/cdml.dtd">')
        if format: fd.write('\n')
        self.write(fd,0,format)
        if fd!=sys.stdout: fd.close()

# Spatio-temporal variable
# Two ways to create a variable:
# (1) var = VariableNode(id,datatype,domain)
# (2) var = VariableNode(id,datatype)
#     var.setDomain(domain)
class VariableNode(CdmsNode):

    # Create a variable.
    # If validate is true, validate immediately
    def __init__(self, id, datatype, domain):
        assert type(datatype) is StringType, 'Invalid datatype: '+`datatype`
        assert datatype in CdDatatypes, 'Invalid datatype: '+`datatype`
        CdmsNode.__init__(self,"variable",id)
        self.datatype = datatype
        self.setDomain(domain)
        VariableNode.mapToExternal(self)

    # Set the domain
    def setDomain(self,domain):
        if not self.isLeaf():
            self.removeChildAt(0)
        self.add(domain)

    # Get the domain
    def getDomain(self):
        if self.getChildCount()>0:
            return self.getChildAt(0)
        else:
            return None

    # Map to external attributes
    def mapToExternal(self):
        self.setExternalAttr('datatype',self.datatype)
        
# Coordinate axis
class AxisNode(CdmsNode):

    # If datatype is None, assume values [0,1,..,length-1]
    # data is a numpy array, if specified
    def __init__(self, id, length, datatype=CdLong,data=None):
        assert isinstance(length, IntType), 'Invalid length: '+`length`
        assert type(datatype) is StringType, 'Invalid datatype: '+`datatype`
        assert datatype in CdDatatypes, 'Invalid datatype: '+`datatype`
        if data is not None: assert isinstance(data, numpy.ndarray), 'data must be a 1-D Numeric array'
        CdmsNode.__init__(self,"axis",id)
        self.datatype = datatype
        self.data = data
        # data representation is CdLinear or CdVector
        # If vector, self.data is a numpy array
        #   and the content is the array string representation
        # If linear, the linear node is a child node
        #   and the content is empty
        self.dataRepresent = None
        self.partition = None           # An array of integer indices, shape (2,self.length) if defined
        self.partition_length = 0       # Actual number of data values, for a linear, partitioned axis
        if data is not None:
            self.setData(data)
        else:
            self.length = length
        AxisNode.mapToExternal(self)

    # Map to external attributes
    def mapToExternal(self):
        self.setExternalAttr('datatype',self.datatype)
        self.setExternalAttr('length',self.length)

    # Set data from content string
    # The content of an axis is the data array.
    def setContentFromString(self,datastring):
        datatype = self.datatype
        numericType = CdToNumericType.get(datatype)
        if numericType is None: raise CDMSError, InvalidDatatype + datatype
        stringlist = _ArraySep.split(datastring)
        numlist = []
        for numstring in stringlist:
            if numstring=='': continue
            numlist.append(string.atof(numstring))
        if len(numlist)>0:
            # NB! len(zero-length array) causes IndexError on Linux!
            dataArray = numpy.array(numlist,numericType)
            self.data = dataArray
            self.length = len(self.data)

    # Set the partition from a string. This does not
    # set the external string representation
    def setPartitionFromString(self,partstring):
        stringlist = _ArraySep.split(partstring)
        numlist = []
        for numstring in stringlist:
            if numstring=='': continue
            numlist.append(string.atoi(numstring))
        dataArray = numpy.array(numlist,numpy.int)
        if len(dataArray)>0:
            self.partition = dataArray

    # Get the content string: the data values if the representation
    # is as a vector, or ane empty string otherwise
    def getContent(self):
        if self.data is None or self.dataRepresent==CdLinear:
            return ''
        else:
            return str(self.data)

    # Set the data as an array, check for monotonicity
    def setData(self,data):

        # If this axis is currently linear, remove the linear node
        if self.dataRepresent == CdLinear:
            index = self.getIndex(self.data)
            self.removeChildAt(index)
        self.data = data
        self.dataRepresent = CdVector
        self.length = len(data)
        self.setExternalAttr('length',self.length)
        if self.monotonicity()==CdNotMonotonic:
            raise NotMonotonicError, NotMonotonic

    # Get the data as an array
    def getData(self):
        if self.dataRepresent == CdLinear:
            return self.data.toVector(self.datatype)
        else:
            return self.data

    # Set the data as a linear vector
    # If the partition is set, derive the vector length from it
    def setLinearData(self,linearNode, partition=None):
        self.data = linearNode
        if self.getChildCount()>0:
            self.removeChildAt(0)           # Remove the previous linear node
        self.add(linearNode)
        self.dataRepresent = CdLinear
        # self.length = linearNode.getExternalAttr('length')
        if partition is None:
            self.length = linearNode.length
        else:
            self.partition = partition
            self.length = partition[-1]
            linearNode.length = self.length
            self.setExternalAttr('partition',str(self.partition))
        self.setExternalAttr('length',self.length)

    # Test if axis data vectors are equal
    def equal(self,axis):
        # Require that partitions (if any) are equal
        if self.partition is not None and axis.partition is not None:
            if len(self.partition)!=len(axis.partition):
                return 0
            if not numpy.alltrue(numpy.equal(self.partition,axis.partition)):
                return 0
        elif self.partition is not None or axis.partition is not None:
            return 0
        
        if self.dataRepresent == axis.dataRepresent == CdVector:
            try:
                return numpy.alltrue(numpy.equal(self.data,axis.data))
            except ValueError:
                return 0
        elif self.dataRepresent == axis.dataRepresent == CdLinear:
            return self.data.equal(axis.data)
        elif self.dataRepresent == CdVector:
            return axis.data.equalVector(self.data)
        else:
            return self.data.equalVector(axis.data)

    # Test if axis data vectors are element-wise close
    # True iff for each respective element a and b, abs((b-a)/b)<=eps
    def isClose(self,axis,eps):
        if eps==0:
            return self.equal(axis)
        if self.dataRepresent == axis.dataRepresent == CdVector:
            try:
                return numpy.alltrue(numpy.less_equal(numpy.absolute(self.data-axis.data),numpy.absolute(eps*self.data)))
            except ValueError:
                return 0
        elif self.dataRepresent == axis.dataRepresent == CdLinear:
            return self.data.isClose(axis.data,eps)
        elif self.dataRepresent == CdVector:
            return axis.data.isCloseVector(self.data,eps)
        else:
            return self.data.isCloseVector(axis.data,eps)

    # Test for strict monotonicity.
    # Returns CdNotMonotonic, CdIncreasing, CdDecreasing, or CdSingleton
    def monotonicity(self):
        if self.dataRepresent == CdLinear:
            return self.data.monotonicity()
        elif self.length==1:
            return CdSingleton
        else:
            first = self.data[:-1]
            second = self.data[1:]
            if numpy.alltrue(numpy.less(first,second)):
                return CdIncreasing
            elif numpy.alltrue(numpy.greater(first,second)):
                return CdDecreasing
            else:
                return CdNotMonotonic

    # Extend axes. 'isreltime' is true iff
    # the axes are relative time axes
    # If allowgaps is true, allow gaps when extending linear vectors
    def extend(self,axis,isreltime=0,allowgaps=0):
        # Set trylin true if should try to catenate linear vectors
        if self.dataRepresent==CdLinear:
            anode = self.data
            if axis.dataRepresent==CdLinear:
                bnode = axis.data
                trylin = 1
            elif axis.length==1:
                bnode = LinearDataNode(axis.data[0], 0.0, 1)
                trylin = 1
            else:
                trylin = 0
        elif self.length==1: 
            anode = LinearDataNode(self.data[0], 0.0, 1)
            if axis.dataRepresent==CdLinear:
                bnode = axis.data
                trylin = 1
            elif axis.length==1:
                bnode = LinearDataNode(axis.data[0], 0.0, 1)
                trylin = 1
            else:
                trylin = 0
        else:
            trylin = 0

        if isreltime==1:
            units1 = self.getExternalAttr('units')
            units2 = axis.getExternalAttr('units')
        else:
            units1 = units2 = None

        if trylin==1:
            try:
                aindex = 0
                alength = anode.length
                bindex = alength
                blength = bnode.length
                if isreltime==1 and units1 and units2 and units1!=units2:
                    rtime = cdtime.reltime(bnode.start,units2)
                    offset = rtime.torel(units1).value
                    bnode.start = bnode.start+offset
                else:
                    offset = None
                linNode = anode.concatenate(bnode,allowgaps)
            except NotMonotonicError:
                # The dimensions cannot be extended as linear arrays,
                # so try to extend them as vectors
                pass
            else:
                # Extend the partition attribute
                if offset is not None:
                    bindex = int(offset/linNode.delta+0.5)
                if self.partition  is None:
                    partition = numpy.array([aindex,aindex+alength,bindex,bindex+blength])
                    self.partition_length = alength+blength
                else:
                    partition = numpy.concatenate((self.partition,[bindex,bindex+blength]))
                    self.partition_length = self.partition_length+blength
                self.setLinearData(linNode,partition)
                self.setExternalAttr('partition_length',self.partition_length)
                return self

        # Else get both axis vectors, concatenate
        # and check that the result is monotonic
        ar1 = self.getData()
        ar2 = axis.getData()
        aindex = 0
        alength = len(ar1)
        bindex = alength
        blength = len(ar2)

        # Adjust array2 if relative time and units differ
        if isreltime==1:
            if units1 and units2 and units1 != units2:
                rtime = cdtime.reltime(0.0,units2)
                delta = rtime.torel(units1).value
                ar2 = ar2+delta

        ar = numpy.concatenate((ar1,ar2))
        try:
            self.setData(ar)
        except NotMonotonicError:
            # Restore original array and resignal
            self.setData(ar1)
            raise NotMonotonicError, NotMonotonic+`ar`

        # Extend the partition attribute
        if self.partition  is None:
            self.partition = numpy.array([aindex,aindex+alength,bindex,bindex+blength])
            self.partition_length = alength+blength
        else:
            self.partition = numpy.concatenate((self.partition,[bindex,bindex+blength]))
            self.partition_length = self.partition_length+blength
        self.setExternalAttr('partition',str(self.partition))
        self.setExternalAttr('partition_length',self.partition_length)

        return self

    def __len__(self):
        return len(self.data)

# Linear data element
class LinearDataNode(CdmsNode):

    validStartTypes = [IntType,FloatType,type(cdtime.comptime(0)),type(cdtime.reltime(0,"hours"))]
    validDeltaTypes = [IntType,FloatType,ListType]

    def __init__(self, start, delta, length):
        assert isinstance(start, numpy.floating) or isinstance(start, numpy.integer) or (type(start) in self.validStartTypes), 'Invalid start argument: '+`start`
        assert isinstance(start, numpy.floating) or isinstance(start, numpy.integer) or (type(delta) in self.validDeltaTypes), 'Invalid delta argument: '+`delta`
        assert isinstance(length, IntType), 'Invalid length argument: '+`length`
        CdmsNode.__init__(self,"linear")
        self.delta = delta
        self.length = length
        self.start = start
        LinearDataNode.mapToExternal(self)

    # Get an indexed value
    def __getitem__(self, index):
        return self.start + index*self.delta

    # Map to external attributes
    def mapToExternal(self):
        self.setExternalAttr("start",self.start)
        self.setExternalAttr("delta",self.delta)
        self.setExternalAttr("length",self.length)

    # Equality of linear vectors
    def equal(self,axis):
        return self.delta==axis.delta and self.length==axis.length and self.start==axis.start

    # Closeness of linear vectors
    def isClose(self,axis,eps):
        if eps==0:
            return self.equal(axis)
        else:
            return self.delta==axis.delta and self.length==axis.length and abs(self.start-axis.start)<=abs(eps*self.start)

    # Equality of linear vector and array
    def equalVector(self,ar):
        diff = ar[1:]-ar[:-1]
        try:
            comp = numpy.alltrue(numpy.equal((self.delta)*numpy.ones(self.length-1),diff))
        except ValueError:
            return 0
        return comp

    # Closeness of linear vector and array
    def isCloseVector(self,ar,eps):
        if eps==0:
            return self.equalVector(ar)
        diff = ar[1:]-ar[:-1]
        diff2 = self.delta*numpy.ones(self.length-1)
        try:
            comp = numpy.alltrue(numpy.less_equal(numpy.absolute(diff2-diff),numpy.absolute(eps*diff2)))
        except ValueError:
            return 0
        return comp

    # Return monotonicity: CdNotMonotonic, CdIncreasing, CdDecreasing, or CdSingleton
    def monotonicity(self):
        if self.length==1:
            return CdSingleton
        elif self.delta>0.0:
            return CdIncreasing
        elif self.delta<0.0:
            return CdDecreasing
        else:
            return CdNotMonotonic

    # Return a vector representation, given a CDMS datatype
    def toVector(self, datatype):
        numericType = CdToNumericType.get(datatype)
        if numericType is None: raise CDMSError, InvalidDatatype + datatype
        start = self.start
        delta = self.delta
        length = self.length
        if length>1:
            stop = start + (length-0.99)*delta
            if delta==0.0: delta=1.0
            ar = numpy.arange(start,stop,delta,numericType)
        else:
            ar = numpy.array([start],numericType)
        return ar

    # Concatenate linear arrays, preserving linearity
    # If allowgaps is set, don't require that the linear arrays be contiguous
    # Return a new linear node
    def concatenate(self,linearNode,allowgaps=0):
        if self.length>1 and linearNode.length>1 and self.delta != linearNode.delta:
            raise NotMonotonicError, NotMonotonic + 'linear vector deltas do not match: %s,%s'%(`self.delta`,`linearNode.delta`)

        if self.length>1:
            delta = self.delta
        elif linearNode.length>1:
            delta = linearNode.delta
        else:
            delta = linearNode.start - self.start
        if allowgaps==0:
            if linearNode.start-self.start != self.length*delta:
                raise NotMonotonicError, NotMonotonic + 'linear vectors are not contiguous'
        length = self.length + linearNode.length
        return LinearDataNode(self.start, delta, length)

    def __len__(self):
        return self.length

# Rectilinear lat-lon grid
class RectGridNode(CdmsNode):

    # Create a grid
    # All arguments are strings
    def __init__(self, id, latitude, longitude, gridtype=UnknownGridType, order="yx", mask=None):
        CdmsNode.__init__(self,"rectGrid",id)
        self.latitude = latitude
        self.longitude = longitude
        self.gridtype = gridtype
        self.mask = mask
        self.order = order
        RectGridNode.mapToExternal(self)

    # Map to external attributes
    def mapToExternal(self):
        self.setExternalAttr('type',self.gridtype)
        self.setExternalAttr('latitude', self.latitude)
        self.setExternalAttr('longitude',self.longitude)
        self.setExternalAttr('order',self.order)
        if self.mask is not None: self.setExternalAttr('mask',self.mask)

# Link to an external element
class XLinkNode(CdmsNode):

    def __init__(self, id, uri, contentRole, content=''):
        CdmsNode.__init__(self,"xlink",id)
        self.uri = uri
        self.contentRole = contentRole
        self.content = content
        XLinkNode.mapToExternal(self)

    # Map to external attributes
    def mapToExternal(self):
        self.setExternalAttr("href",self.uri,CdString)
        self.setExternalAttr("content-role",self.contentRole,CdString)

# Link to a document
class DocLinkNode(CdmsNode):

    def __init__(self, uri, content=''):
        CdmsNode.__init__(self,"doclink")
        self.uri = uri
        self.content = content
        DocLinkNode.mapToExternal(self)

    # Map to external attributes
    def mapToExternal(self):
        self.setExternalAttr("href",self.uri,CdString)

# Domain
class DomainNode(CdmsNode):

    def __init__(self):
        CdmsNode.__init__(self,"domain")

# Domain element
class DomElemNode(CdmsNode):

    def __init__(self, name, start=None, length=None):
        CdmsNode.__init__(self,"domElem")
        self.name = name
        self.start = start
        self.length = length
        DomElemNode.mapToExternal(self)

    # Map to external attributes
    def mapToExternal(self):
        self.setExternalAttr('name',self.name)
        if self.start is not None: self.setExternalAttr('start',self.start)
        if self.length is not None: self.setExternalAttr('length',self.length)

    # Set the name
    def setName(self,name):
        self.name = name
        self.setExternalAttr('name',self.name)

    # Get the name
    def getName(self):
        return self.name

    # Write to a file, with formatting.
    # tablevel is the start number of tabs
    def write(self,fd=None,tablevel=0,format=1):
        if fd is None: fd = sys.stdout
        if format: fd.write(tablevel*'\t')
        fd.write('<'+self.tag)
        for attname in self.attribute.keys():
            (attval,datatype)=self.attribute[attname]
            # attvalstr = string.replace(str(attval),'"',"'") # Map " to '
            attvalstr = _Illegal.sub(mapIllegalToEntity,str(attval))  # Map illegal chars to entities
            fd.write(' '+attname+'="'+attvalstr+'"')
        fd.write('/>')
        if format: fd.write('\n')

# Attribute node - only used as a placeholder during parse and write
#   Attr nodes are not placed on the tree
#
# Two ways to create an Attr object:
# (1) attr = AttrNode(name,value)
#     datatype = sometype # optionally, to override intrinsic type
# (2) attr = AttrNode(name,None)
#     attr.setValueFromString(somestring,sometype)
class AttrNode(CdmsNode):

    def __init__(self, name, value=None):
        CdmsNode.__init__(self,"attr")
        if not (isinstance(value,IntType)
                or isinstance(value,numpy.integer)
                or isinstance(value,FloatType)
                or isinstance(value,numpy.floating)
                or isinstance(value,StringType)
                or isinstance(value,NoneType)):
            raise CDMSError, 'Invalid attribute type: '+`value`
        self.name = name
        self.value = value
        self.datatype = None            # CDMS datatype, use getDatatype to retrieve
        self.content = ''             # string content

    # Note: mapToExternal is not called at init time, must be called explicitly
    #   if needed
    def mapToExternal(self):
        self.attribute['name']=(self.name,CdString)
        self.attribute['datatype']=(self.getDatatype(),CdString)
        self.content = self.getValueAsString()

    def getDatatype(self):
        if self.datatype:
            return self.datatype
        elif type(self.value) is StringType:
            return CdString
        elif isinstance(self.value, FloatType) or isinstance(self.value,numpy.floating):
            return CdDouble
        elif isinstance(self.value, IntType) or isinstance(self.value,numpy.integer):
            return CdLong
        else:
            raise CDMSError, 'Invalid attribute type: '+`self.value`

    def getLength(self):
        return 1

    # Map a string of a given datatype to a value
    #   Returns ValueError if the conversion fails
    def setValueFromString(self,valString,datatype):
        val = None
        if type(valString) is not StringType:
            raise CDMSError, 'input value is not a string'
        if datatype == CdString:
            val=valString
        elif datatype in (CdShort,CdInt,CdLong):
            try:
                val=string.atoi(valString)
            except ValueError:
                raise CDMSError, 'value is not an integer: '+valString
        elif datatype in (CdFloat,CdDouble):
            try:
                val=string.atof(valString)
            except ValueError:
                raise CDMSError, 'value is not floating-point: '+valString
        self.value=val
        self.datatype=datatype
        return val

    def getValueAsString(self):
        return str(self.value)

    # Set content
    # This may be called multiple times, so append
    def setContentFromString(self,content):
        self.content = self.content+content

    # Write to a file, with formatting.
    # tablevel is the start number of tabs
    def write(self,fd=None,tablevel=0,format=1):
        if fd is None: fd = sys.stdout
        if self.dtd:
            validAttrs = self.dtd.keys()
        else:
            validAttrs = None

        if format: fd.write(tablevel*'\t')
        fd.write('<'+self.tag)

        # Write valid attributes
        for attname in self.attribute.keys():
            if (validAttrs and (attname in validAttrs)) or (not validAttrs):
                (attval,datatype)=self.attribute[attname]
                # attvalstr = string.replace(str(attval),'"',"'") # Map " to '
                attvalstr = _Illegal.sub(mapIllegalToEntity,str(attval))  # Map illegal chars to entities
                fd.write(' '+attname+'="'+attvalstr+'"')
        fd.write('>')

        # Write content
        if self.content is not None:
            content = _Illegal.sub(mapIllegalToEntity,self.content)  # Map illegal chars to entities
            fd.write(content)

        fd.write('</'+self.tag+'>')
        if format: fd.write('\n')

if __name__ == '__main__':
    # a = numpy.array([0.,4.,8.,12.,16.,20.,24.,28.])
    # b = numpy.array([0.,4.,8.,12.,16.,20.,24.,28.])
    # c = numpy.array([0.,4.,8.,12.,16.,20.,24.,28.,32.])
    # aAxis = AxisNode('a',len(a),CdDouble,a)
    # bAxis = AxisNode('b',len(b),CdDouble,b)
    # cAxis = AxisNode('c',len(c),CdDouble,c)

    # d = LinearDataNode(0.,4.,8)
    # e = LinearDataNode(0.,4.,8)
    # f = LinearDataNode(0.,4.,9)
    # dAxis = AxisNode('d',8); dAxis.setLinearData(d)
    # eAxis = AxisNode('e',8); eAxis.setLinearData(e)
    # fAxis = AxisNode('f',9); fAxis.setLinearData(f)

    # print aAxis.equal(bAxis)
    # print aAxis.equal(cAxis)
    # print aAxis.equal(dAxis)
    # print aAxis.equal(fAxis)
    # print dAxis.equal(eAxis)
    # print dAxis.equal(fAxis)

    # g = numpy.array([0.,4.,8.,12.,16.,20.,24.,20.])
    # h = numpy.array([ 28.,  24.,  20.,  16.,  12.,   8.,   4.,   0.])
    # j = LinearDataNode(0.,-4.,8)
    # k = numpy.array([4.])
    # gAxis = AxisNode('g',len(g),CdDouble,g)
    # hAxis = AxisNode('h',len(h),CdDouble,h)
    # jAxis = AxisNode('j',len(j),CdDouble); jAxis.setLinearData(j)
    # kAxis = AxisNode('k',len(k),CdDouble,k)
    
    # print aAxis.monotonicity()
    # print hAxis.monotonicity()
    # print kAxis.monotonicity()
    # print gAxis.monotonicity()
    # print dAxis.monotonicity()
    # print jAxis.monotonicity()

    m = LinearDataNode(1,2,3)
    n = LinearDataNode(11,2,3)
    p = LinearDataNode(15,-4,3)
    q = numpy.array([4.,2.,1.])
    r = numpy.array([11.,9.,8.])
    s = numpy.array([7.])
    t = numpy.array([9.])
    v = numpy.array([5.])

    mAxis = AxisNode('m',len(m),CdLong); mAxis.setLinearData(m)
    nAxis = AxisNode('n',len(n),CdLong); nAxis.setLinearData(n)
    pAxis = AxisNode('p',len(p),CdLong); pAxis.setLinearData(p)
    qAxis = AxisNode('q',len(q),CdDouble,q)
    rAxis = AxisNode('r',len(r),CdDouble,r)
    sAxis = AxisNode('s',len(s),CdDouble,s)
    tAxis = AxisNode('t',len(t),CdDouble,t)
    vAxis = AxisNode('v',len(v),CdDouble,v)

    def printType(axis):
        if axis.dataRepresent==CdLinear: print 'linear'
        else: print 'vector'
        
    def testit(a,b):
        import copy
        x=copy.copy(a)
        print x.extend(b).getData()
        printType(x)

    # testit(mAxis,nAxis)
    # testit(mAxis,pAxis)
    # testit(qAxis,nAxis)
    # testit(qAxis,rAxis)
    # testit(mAxis,sAxis)
    # testit(mAxis,tAxis)
    # testit(sAxis,tAxis)
    # testit(mAxis,rAxis)
    # testit(vAxis,nAxis)
    # testit(sAxis,rAxis)
    
    # Errors:
    # testit(mAxis,nAxis)
