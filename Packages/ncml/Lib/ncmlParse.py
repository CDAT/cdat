#!/usr/bin/env python
# Adapted for numpy/ma/cdms2 by convertcdms.py
"""
NcML parsing. See http://www.vets.ucar.edu/luca/netcdf/
"""

import ncml, string, re, numpy.oldnumeric as Numeric
from xml.sax import make_parser, saxutils, ContentHandler, ErrorHandler, SAXParseException
from xml.sax.handler import feature_namespaces

class NCMLParseException(SAXParseException):

    def __str__(self):
        "Create a string representation of the exception."
        sysid = self.getSystemId()
        if sysid is None:
            sysid = "<unknown>"
        return "%s, in file %s, line %d" % (self._msg, sysid, self.getLineNumber())

def load(path):
    """Create a tree of NCML nodes from a file path.
    Returns the parse tree root node.
    """
    parser = make_parser()
    parser.setFeature(feature_namespaces,1)
    ch = ncmlContentHandler()
    eh = ncmlErrorHandler()
    parser.setContentHandler(ch)
    parser.setErrorHandler(eh)
    parser.parse(path)
    return ch.root

def _getatt(attrs, key, ns=None, default=None):
    """Get the value of attribute 'key' from attrs object,
    an AttributesNS object. Return default value if not found."""
    keytuple = (ns, unicode(key))
    if attrs.has_key(keytuple):
        result = attrs[keytuple]
    else:
        result = default
    if type(result) is type(u' '):      # Translate unicode to a regular string
        result = result.encode()
    return result

def _stringToNumPy(s, sep, datatype):
    """Translate a string to a NumPy array.
    'sep' is the separator character.
    'datatype' is the Numeric datatypr"""
    _ArraySep = re.compile('[\s,%s]+'%sep)
    stringlist = _ArraySep.split(s)
    numlist = []
    for numstring in stringlist:
        if numstring=='': continue
        numlist.append(string.atof(numstring))
        
    if len(numlist)>0:
        dataArray = Numeric.array(numlist,datatype) 
    else:
        dataArray = None
    return dataArray

class ncmlContentHandler(ContentHandler):
    """Handle NCML content. An instance of this class is associated
    with the parser using setContentHandler()."""

    def __init__(self):
        self.stack = []                 # Context stack
        self.root = None                # Result node
        self.content = ""               # content of current node

    def setDocumentLocator(self, locator):
        self._locator = locator

    def push(self, item):
        self.stack = [item] + self.stack

    def pop(self):
        top, self.stack = self.stack[0], self.stack[1:]
        return top

    def peek(self):
        return self.stack[0]

    def startElement(self, name, attrs):
        raise NCMLParseException('No namespace found.'%elocal, None, self._locator)

    def endElement(self, name):
        raise NCMLParseException('No namespace found.'%elocal, None, self._locator)

    def startElementNS(self, name, qname, attrs):
        uri, localname = name
        elocal = localname.encode()
        if elocal=="netcdf":
            self.startNetcdf(attrs)
        elif elocal=="dimension":
            self.startDimension(attrs)
        elif elocal=="variable":
            self.startVariable(attrs)
        elif elocal=="values":
            self.startValues(attrs)
        elif elocal=="attribute":
            self.startAttribute(attrs)
        else:
            raise NCMLParseException('Invalid element tag: %s'%elocal, None, self._locator)

    def endElementNS(self, name, qname):
        uri, localname = name
        elocal = localname.encode()
        if elocal=="netcdf":
            self.endNetcdf()
        elif elocal=="variable":
            self.endVariable()
        elif elocal=="values":
            self.endValues()

    def characters(self, ch):
        self.content += ch

    def startNetcdf(self, attrs):
        uri = _getatt(attrs, 'uri')
        nc = ncml.NetcdfNode(uri=uri)
        self.push(nc)

    def endNetcdf(self):
        self.root = self.pop()

    def startDimension(self, attrs):
        ncnode = self.peek()
        name = _getatt(attrs, 'name')
        length = string.atoi(_getatt(attrs, 'length'))
        isUnlimited = _getatt(attrs, 'isUnlimited', default="false")
        dimnode = ncml.DimensionNode(name, length, isUnlimited)
        ncnode.setDimension(dimnode)

    def startVariable(self, attrs):
        ncnode = self.peek()
        name = _getatt(attrs, 'name')
        shape = _getatt(attrs, 'shape', default=None)
        datatype = _getatt(attrs, 'type')
        if shape is None:
            shapelist = None
        else:
            shapelist = string.split(shape)
        varnode = ncml.VariableNode(name, datatype, shapelist)
        ncnode.setVariable(varnode)
        self.push(varnode)

    def endVariable(self):
        self.pop()

    def startValues(self, attrs):
        varnode = self.peek()
        separator = _getatt(attrs, 'separator', default=" ")
        self.content = ""
        valuenode = ncml.ValueNode(separator=separator)
        varnode.setValues(valuenode)
        self.push(valuenode)

    def endValues(self):
        valuenode = self.pop()
        varnode = self.peek()
        datatype = ncml.NCToNumericType[varnode.datatype]

        try:
            dataArray = _stringToNumPy(self.content, valuenode.separator, datatype)
        except:
            t,v,tr = sys.exc_info()
            raise NCMLParseException(v, None, self._locator)

        if dataArray is not None:
            valuenode.setValues(dataArray)

    def startAttribute(self, attrs):
        parent = self.peek()            # netcdf or variable node
        name = _getatt(attrs, 'name')
        datatype = _getatt(attrs, 'type', default="string")
        value = _getatt(attrs, 'value', default=None)
        separator = _getatt(attrs, 'separator', default=" ")
        if datatype!="string":
            try:
                value = _stringToNumPy(value, separator, ncml.NCToNumericType[datatype])
            except:
                t,v,tr = sys.exc_info()
                raise NCMLParseException(v, None, self._locator)
        attnode = ncml.AttributeNode(name, datatype, value)
        parent.setAttribute(attnode)

class ncmlErrorHandler(ErrorHandler):

    """Handle an NCML parsing error. An object of this type is associated
    with the parser using setErrorHandler()."""

    def __init__(self):
        pass

    def printErrorLine(self, path, linenum):
        f = open(path)
        lines = f.readlines()
        line = lines[linenum-1]
        print line,

    def fatalError(self, exception):
        print 'Got a fatal error'
        print 'Message:',exception.getMessage()
        print 'Column:',exception.getColumnNumber()
        linenum = exception.getLineNumber()
        print 'Line:', linenum
        print 'Public ID:',exception.getPublicId()
        systemid = exception.getSystemId()
        print 'System ID:', systemid
        print 'Error in line:', self.printErrorLine(systemid, linenum)
        raise exception

if __name__=='__main__':
    import sys

    ncnode = load(sys.argv[1])
    print '<?xml version="1.0" encoding="UTF-8"?>'
    ncnode.write(sys.stdout)
