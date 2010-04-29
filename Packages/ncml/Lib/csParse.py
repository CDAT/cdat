#!/usr/bin/env python
"""
NcML + coordinate extensions parsing. See http://www.unidata.ucar.edu/schemas/netcdf-cs.xsd
"""

import cs, ncmlParse, string
from ncmlParse import NCMLParseException, _getatt
from xml.sax import make_parser
from xml.sax.handler import feature_namespaces

def load(path):
    """Create a tree of NCML nodes from a file path.
    Returns the parse tree root node.
    """
    parser = make_parser()
    parser.setFeature(feature_namespaces,1)
    ch = CSContentHandler()
    eh = ncmlParse.ncmlErrorHandler()
    parser.setContentHandler(ch)
    parser.setErrorHandler(eh)
    parser.parse(path)
    return ch.root

class CSContentHandler(ncmlParse.ncmlContentHandler):

    def __init__(self):
        ncmlParse.ncmlContentHandler.__init__(self)

    def startElementNS(self, name, qname, attrs):
        uri, localname = name
        elocal = localname.encode()
        if elocal in ["dimension","values","attribute"]:
            ncmlParse.ncmlContentHandler.startElementNS(self, name, qname, attrs)
        elif elocal=="netcdf":
            self.startNetcdf(attrs)
        elif elocal=="variable":
            self.startVariable(attrs)
        elif elocal=="coordinateAxis":
            self.startCoordinateAxis(attrs)
        elif elocal=="coordinateAxisBoundary":
            self.startCoordinateAxisBoundary(attrs)
        elif elocal=="coordinateAxisRef":
            self.startCoordinateAxisRef(attrs)
        elif elocal=="coordinateTransformRef":
            self.startCoordinateTransformRef(attrs)
        elif elocal=="coordinateSystem":
            self.startCoordinateSystem(attrs)
        elif elocal=="parameter":
            self.startParameter(attrs)
        elif elocal=="coordinateTransform":
            self.startCoordinateTransform(attrs)
        else:
            raise NCMLParseException('Invalid element tag: %s'%elocal, None, self._locator)

    def endElementNS(self, name, qname):
        uri, localname = name
        elocal = localname.encode()
        if elocal in ["dimension","values","attribute"]:
            ncmlParse.ncmlContentHandler.endElementNS(self, name, qname)
        elif elocal=="netcdf":
            self.endNetcdf()
        elif elocal=="variable":
            self.endVariable()
        elif elocal=="coordinateAxis":
            self.endCoordinateAxis()
        elif elocal=="coordinateAxisBoundary":
            self.endCoordinateAxisBoundary()
        elif elocal=="coordinateSystem":
            self.endCoordinateSystem()
        elif elocal=="coordinateTransform":
            self.endCoordinateTransform()

    def startNetcdf(self, attrs):
        uri = _getatt(attrs, 'uri')
        nc = cs.NetcdfNode(uri=uri)
        self.push(nc)

    def endNetcdf(self):
        self.root = self.pop()

    def startVariable(self, attrs):
        ncnode = self.peek()
        name = _getatt(attrs, 'name')
        shape = _getatt(attrs, 'shape', default=None)
        datatype = _getatt(attrs, 'type')
        coordinateSystems = _getatt(attrs, 'coordinateSystems')
        if shape is None:
            shapelist = None
        else:
            shapelist = string.split(shape)
        varnode = cs.VariableNode(name, datatype, shapelist, coordinateSystems)
        ncnode.setVariable(varnode)
        self.push(varnode)

    def endVariable(self):
        self.pop()

    def startCoordinateAxis(self, attrs):
        ncnode = self.peek()
        name = _getatt(attrs, 'name')
        shape = _getatt(attrs, 'shape', default=None)
        if shape is None:
            shapelist = None
        else:
            shapelist = string.split(shape)
        datatype = _getatt(attrs, 'type')
        units = _getatt(attrs, 'units')
        axis = _getatt(attrs, 'axis')
        axisType = None
        if axis is not None:
            try:
                axisType = cdToCsAxisType[string.lower(var.axis)]
            except:
                pass
        positive = _getatt(attrs, 'positive')
        boundaryRef = _getatt(attrs, 'bounds')
        varnode = cs.CoordinateAxisNode(name, datatype, units, shapelist, axisType, positive, boundaryRef)
        ncnode.setCoordinateAxis(varnode)
        self.push(varnode)

    def endCoordinateAxis(self):
        self.pop()

    def startCoordinateAxisBoundary(self, attrs):
        ncnode = self.peek()
        name = _getatt(attrs, 'name')
        shape = _getatt(attrs, 'shape', default=None)
        datatype = _getatt(attrs, 'type')
        if shape is None:
            shapelist = None
        else:
            shapelist = string.split(shape)
        varnode = cs.CoordinateAxisBoundaryNode(name, datatype, shapelist)
        ncnode.setVariable(varnode)
        self.push(varnode)

    def endCoordinateAxisBoundary(self):
        self.pop()

    def startCoordinateAxisRef(self, attrs):
        ref = _getatt(attrs, 'ref')
        csnode = self.peek()
        csnode.addCoordinateAxis(ref)

    def startCoordinateTransformRef(self, attrs):
        ref = _getatt(attrs, 'ref')
        csnode = self.peek()
        csnode.addCoordinateTransform(ref)

    def startCoordinateSystem(self, attrs):
        name = _getatt(attrs, 'name')
        ncnode = self.peek()
        csnode = cs.CoordinateSystemNode(name)
        ncnode.setCoordinateSystem(csnode)
        self.push(csnode)

    def endCoordinateSystem(self):
        self.pop()

    def startParameter(self, attrs):
        self.startAttribute(attrs)      # Parameters are attributes
        name = _getatt(attrs, 'name')
        self.peek().parameters[name].tag='parameter'

    def startCoordinateTransform(self, attrs):
        name = _getatt(attrs, 'name')
        authority = _getatt(attrs, 'authority')
        transformType = _getatt(attrs, 'transformType')
        ctnode = cs.CoordinateTransformNode(name, authority, transformType)
        nsnode = self.peek()
        nsnode.setCoordinateTransform(ctnode)
        self.push(ctnode)

    def endCoordinateTransform(self):
        self.pop()

if __name__=='__main__':
    import sys

    csnode = load(sys.argv[1])
    print '<?xml version="1.0" encoding="UTF-8"?>'
    csnode.write(sys.stdout)
