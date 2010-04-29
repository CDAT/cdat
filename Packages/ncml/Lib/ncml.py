#!/usr/bin/env python
# Adapted for numpy/ma/cdms2 by convertcdms.py
"""
NcML support. See http://www.vets.ucar.edu/luca/netcdf/
"""

import numpy.oldnumeric as Numeric, string, os.path, re
from cdms2.cdmsNode import _Illegal, mapIllegalToEntity, DatasetNode, AxisNode, DomainNode, DomElemNode
from cdms2 import CdByte, CdDouble, CdFloat, CdInt, CdLong, CdShort, CdString, cdmsNode

class NCMLError (Exception):
    def __init__ (self, args="Unspecified error from package ncml"):
        self.args = args
    def __str__(self):
        return str(self.args)

nsprefix = "nc"
namespace = "http://www.ucar.edu/schemas/netcdf"
schemaLocation = "http://www.ucar.edu/schemas/netcdf.xsd"

NumericToNCType = {Numeric.Float32:"float",
                   Numeric.Float:"double",
                   Numeric.Int16:"short",
                   Numeric.Int32:"int",
                   Numeric.Int:"long",
                   Numeric.Int8:"byte",
                   'c':"string"
                   }

NCToCdType = {
    "byte":CdByte,
    "short":CdShort,
    "int":CdInt,
    "float":CdFloat,
    "double":CdDouble,
    "string":CdString,
    "boolean":CdByte,
    "long":CdLong,
    }

NCToNumericType = {
    "byte":Numeric.Int8,
    "short":Numeric.Int16,
    "int":Numeric.Int32,
    "float":Numeric.Float32,
    "double":Numeric.Float,
    "string":'c',
    "boolean":Numeric.Int8,
    "long":Numeric.Int
    }

def _strValue(value):
    if type(value) in [type(' '), type(u' ')]:
        result = value
    elif isinstance(value, Numeric.ArrayType):
        result = string.replace(str(value)[1:-1],","," ")
    return result

def _copyObjectAttributes(fromobj, tonode):
    for name, value in fromobj.__dict__.items():
        if type(value) is type(' '):
            datatype = "string"
        else:
            datatype = NumericToNCType[value.dtype.char]
        a = AttributeNode(name, datatype, value)
        tonode.setAttribute(a)

def scan(path):
    """Scan a Cdunif file, return a tree of NCML objects, in the
    form of a NetcdfNode."""

    from cdms2 import Cdunif

    f = Cdunif.CdunifFile(path)
    nc = NetcdfNode(uri=path)
    for name, length in f.dimensions.items():
        if length is None:
            isUnlimited = "true"
            length = len(f.readDimension(name))
        else:
            isUnlimited = "false"
        nc.setDimension(DimensionNode(name, length, isUnlimited=isUnlimited))
    for name, var in f.variables.items():
        v = VariableNode(name, NumericToNCType[var.typecode()], var.dimensions)
        if len(var.dimensions)>0 and name==var.dimensions[0]:              # Write coordinate variable values only
            v.setValues(ValueNode(var.getValue()))
        nc.setVariable(v)
        _copyObjectAttributes(var, v)
    _copyObjectAttributes(f, nc)
    
    f.close()
    return nc

class NCMLNode:
    """Abstract NCML node."""

    def __init__(self):
        self.tag = None                 # NCML element tag

    def startwrite(self, fd):
        fd.write('<%s:%s'%(nsprefix, self.tag))

    def endwrite(self, fd, content=0):
        if content:
            fd.write('</%s:%s>'%(nsprefix, self.tag))
        else:
            fd.write('/>')
        fd.write('\n')

    def write(self, fd):
        """Output to a file. fd is an open file descriptor."""
        raise NCMLError, "Method not implemented"

class NetcdfNode(NCMLNode):

    def __init__(self, id=None, uri=None):
        NCMLNode.__init__(self)
        self.tag = "netcdf"
        self.id = id
        self.uri = os.path.abspath(uri)
        self.dimensions = {}
        self.variables = {}
        self.attributes = {}

    def write(self, fd):
        self.startwrite(fd)
        fd.write(' xmlns:%s="%s" uri="%s"'%(nsprefix,namespace,self.uri))
        fd.write(' xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"')
        fd.write(' xsi:schemaLocation="%s %s">'%(namespace,schemaLocation))

        dimensionkeys = self.dimensions.keys()
        dimensionkeys.sort()
        for dimkey in dimensionkeys:
            self.dimensions[dimkey].write(fd)
        
        variablekeys = self.variables.keys()
        variablekeys.sort()
        for dimkey in variablekeys:
            self.variables[dimkey].write(fd)

        attributekeys = self.attributes.keys()
        attributekeys.sort()
        for dimkey in attributekeys:
            self.attributes[dimkey].write(fd)

        self.endwrite(fd,1)

    def setDimension(self, d):
        """Add a dimension node d."""
        self.dimensions[d.name] = d

    def setVariable(self, v):
        """Add a variable node v."""
        self.variables[v.name] = v

    def setAttribute(self, a):
        """Add an attribute node a."""
        self.attributes[a.name] = a

    def toCDML(self, coorddict=None, forcedata=0):
        """Translate to a CDML tree. Returns a cdms.cdmsNode.DatasetNode.
        If coorddict is specified, don't translate dimensions with names in coorddict.
"""
        if self.id is None:
            dsetid = "None"
        else:
            dsetid = self.id
        datasetnode = DatasetNode(id=dsetid)
        datasetnode.setExternalAttr("uri", self.uri)

        coordkeys = []
        if coorddict is not None:
            coordkeys = coorddict.keys()
        for dimid, dim in self.dimensions.items():
            if dimid not in self.variables.keys() and dimid not in coordkeys:
                datasetnode.addId(dimid, dim.toCDML(forcedata))

        varlist = ""
        for varid, var in self.variables.items():
            datasetnode.addId(varid, var.toCDML(self))
            varlist += varid+','

        for attid, att in self.attributes.items():
            value = att.value
            if isinstance(value, Numeric.ArrayType):
                value = value[0]
            datasetnode.setExternalAttr(attid, value, NCToCdType[att.datatype])

        # Add directory and cdms_filelist attributes.
        if self.uri is not None:
            direc, base = os.path.split(self.uri)
        datasetnode.setExternalAttr('cdms_filemap','[[[%s],[[-,-,-,-,%s]]]]'%(varlist[:-1],base))
        datasetnode.setExternalAttr('directory',direc)

        return datasetnode

class DimensionNode(NCMLNode):

    def __init__(self, name, length, isUnlimited="false"):
        NCMLNode.__init__(self)
        self.tag = "dimension"
        self.name = name
        if length is None:
            self.length = 0
            self.isUnlimited = "true"
        else:
            self.length = length
            self.isUnlimited = isUnlimited

    def write(self, fd):
        self.startwrite(fd)

        fd.write(' name="%s" length="%d"'%(self.name, self.length))
        if self.isUnlimited=="true":
            fd.write(' isUnlimited="%s"'%self.isUnlimited)
        self.endwrite(fd)

    def toCDML(self, forcedata=0):
        "Translate to a CDML tree. Returns a cdms.cdmsNode.AxisNode."
        axisnode = AxisNode(self.name, self.length, CdFloat) # Only for virtual axes
        axisnode.setExternalAttr("isvar", "false", CdString)
        if forcedata:
            axisnode.setData(Numeric.arrayrange(float(self.length)))
        return axisnode

class VariableNode(NCMLNode):

    def __init__(self, name, datatype, shape=None):
        NCMLNode.__init__(self)
        self.tag = "variable"
        self.name = name
        self.datatype = datatype        # NCML datatype
        if shape=="":
            self.shape = None
        else:
            self.shape = shape          # List of string dimensions names
        self.attributes = {}
        self.values = None              # Value node

    def setAttribute(self, att):
        """Add an attribute. att is an AttributeNode"""
        self.attributes[att.name] = att

    def setValues(self, values):
        """Set values, a ValueNode."""
        self.values = values

    def write(self, fd):
        self.startwrite(fd)
        fd.write(' name="%s" type="%s"'%(self.name, self.datatype))
        if self.shape is not None:
            fd.write(' shape="%s"'%string.join(self.shape))
        fd.write('>\n')
        attkeys = self.attributes.keys()
        attkeys.sort()
        for attkey in attkeys:
            att = self.attributes[attkey]
            att.write(fd)
        if self.values is not None:
            self.values.write(fd)
        self.endwrite(fd, 1)

    def toCDML(self, ncnode, forcedata=0):
        "Translate to a CDML tree. Returns a cdms.cdmsNode.VariableNode or cdms.cdmsNode.AxisNode."
        dimdict = ncnode.dimensions
        if self.name in dimdict.keys() and self.shape is not None and len(self.shape)==1:
            dimnode = dimdict[self.name]
            varnode = AxisNode(self.name, dimnode.length, NCToCdType[self.datatype])
            if self.values is not None:
                varnode.setData(self.values.values)
            elif forcedata:
                varnode.setData(Numeric.arrayrange(float(dimnode.length)))
        else:
            domnode = DomainNode()
            if self.shape is not None:
                for dimid in self.shape:
                    dimncnode = dimdict[dimid]
                    domelemnode = DomElemNode(dimid, 0, dimncnode.length)
                    domnode.add(domelemnode)
            varnode = cdmsNode.VariableNode(self.name, NCToCdType[self.datatype], domnode)

        attkeys = self.attributes.keys()
        attkeys.sort()
        for attkey in attkeys:
            attncnode = self.attributes[attkey]
            value = attncnode.value
            if isinstance(value, Numeric.ArrayType):
                value = value[0]
            varnode.setExternalAttr(attncnode.name, value, NCToCdType[attncnode.datatype])
        return varnode

class ValueNode(NCMLNode):

    def __init__(self, values=None, separator=" "):
        self.tag = "values"
        self.values = values            # NumPy array
        self.separator = separator

    def setValues(self, values):
        """Set values, a Numeric array."""
        self.values = values

    def write(self, fd):
        fd.write('  ')
        self.startwrite(fd)
        # Note: _strValue changes the separator to blank
        fd.write('>')
        fd.write(_strValue(self.values))
        self.endwrite(fd, 1)

class AttributeNode(NCMLNode):

    def __init__(self, name, datatype, value=None):
        NCMLNode.__init__(self)
        self.tag = "attribute"
        self.name = name
        self.datatype = datatype
        self.value = value              # string or Numpy array
        if type(self.value) is type(' '):
            self.value = _Illegal.sub(mapIllegalToEntity, self.value)

    def write(self, fd):
        fd.write('  ')
        self.startwrite(fd)
        fd.write(' name="%s" type="%s"'%(self.name, self.datatype))
        if self.value is not None:
            fd.write(' value="%s"'%_strValue(self.value))
        self.endwrite(fd)

if __name__=='__main__':

    import sys

    nc = scan(sys.argv[1])
##     print '<?xml version="1.0" encoding="UTF-8"?>'
##     nc.write(sys.stdout)

    datasetnode = nc.toCDML()
    datasetnode.dump()


