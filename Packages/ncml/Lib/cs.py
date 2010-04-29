#!/usr/bin/env python
# Adapted for numpy/ma/cdms2 by convertcdms.py
"""
NcML + coordinate extensions support. See http://www.unidata.ucar.edu/schemas/netcdf-cs.xsd
"""

import ncml, string
from cdms2.cdmsNode import AxisNode, CdString

schemaLocation = "http://www.ucar.edu/schemas/netcdf-cs.xsd"

cdToCsAxisType = {
    'x':'Lon',
    'y':'Lat',
    'z':'Height',
    't':'Time'
    }

def scan(path):
    """Scan a Cdunif file, return a tree of NCML objects, in the
    form of a NetcdfNode."""

    from cdms2 import Cdunif
    from ncml import _copyObjectAttributes, NumericToNCType

    f = Cdunif.CdunifFile(path)
    nc = NetcdfNode(uri=path)
    for name, length in f.dimensions.items():
        if length is None:
            isUnlimited = "true"
            length = len(f.readDimension(name))
        else:
            isUnlimited = "false"
        nc.setDimension(ncml.DimensionNode(name, length, isUnlimited=isUnlimited))
    for name, var in f.variables.items():
        datatype = NumericToNCType[var.typecode()]
        if len(var.dimensions)==1 and name==var.dimensions[0]:
            axisType = positive = boundaryRef = None
            if hasattr(var, 'axis'):
                try:
                    axisType = cdToCsAxisType[string.lower(var.axis)]
                except:
                    pass
            if hasattr(var, 'positive'):
                positive = var.positive
            if hasattr(var, 'bounds'):
                boundaryRef = var.bounds
            units = None
            if hasattr(var, 'units'):
                units = var.units
            v = CoordinateAxisNode(name, datatype, units, var.dimensions, axisType=axisType, positive=positive, boundaryRef=boundaryRef)
            v.setValues(ncml.ValueNode(var.getValue()))
            nc.setCoordinateAxis(v)
        else:
            v = VariableNode(name, datatype, var.dimensions)
            nc.setVariable(v)
        _copyObjectAttributes(var, v)

    _copyObjectAttributes(f, nc)
    
    f.close()
    return nc

class NetcdfNode(ncml.NetcdfNode):

    def __init__(self, id=None, uri=None):
        ncml.NetcdfNode.__init__(self, id, uri)
        self.coordinateAxes = {}
        self.coordinateSystems = {}
        self.coordinateTransforms = {}

    def setCoordinateAxis(self, c):
        """Add a coordinate axis node."""
        self.coordinateAxes[c.name] = c

    def setCoordinateSystem(self, c):
        """Add a coordinate system node."""
        self.coordinateSystems[c.name] = c

    def setCoordinateTransform(self, c):
        """Add a coordinate transform."""
        self.coordinateTransforms[c.name] = c

    def write(self, fd):
        self.startwrite(fd)
        fd.write(' xmlns:%s="%s" uri="%s"'%(ncml.nsprefix,ncml.namespace,self.uri))
        fd.write(' xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"')
        fd.write(' xsi:schemaLocation="%s %s">\n'%(ncml.namespace,schemaLocation))

        dimensionkeys = self.dimensions.keys()
        dimensionkeys.sort()
        for dimkey in dimensionkeys:
            self.dimensions[dimkey].write(fd)
        
        axiskeys = self.coordinateAxes.keys()
        axiskeys.sort()
        for axiskey in axiskeys:
            self.coordinateAxes[axiskey].write(fd)

        variablekeys = self.variables.keys()
        variablekeys.sort()
        for dimkey in variablekeys:
            self.variables[dimkey].write(fd)

        attributekeys = self.attributes.keys()
        attributekeys.sort()
        for dimkey in attributekeys:
            self.attributes[dimkey].write(fd)

        cskeys = self.coordinateSystems.keys()
        cskeys.sort()
        for cskey in cskeys:
            self.coordinateSystems[cskey].write(fd)

        ctkeys = self.coordinateTransforms.keys()
        ctkeys.sort()
        for ctkey in ctkeys:
            self.coordinateTransforms[ctkey].write(fd)

        self.endwrite(fd,1)

    def toCDML(self, forcedata=0):
        "Translate to a CDML tree. Returns a cdms.cdmsNode.DatasetNode."

        # Translate dimensions, variables, attributes
        datasetnode = ncml.NetcdfNode.toCDML(self, self.coordinateAxes, forcedata=forcedata)

        # Translate axes
        for axisid, axis in self.coordinateAxes.items():
            datasetnode.addId(axisid, axis.toCDML(self, forcedata=forcedata))

        return datasetnode

class VariableNode(ncml.VariableNode):

    def __init__(self, name, datatype, shape=None, coordinateSystems=None):
        ncml.VariableNode.__init__(self, name, datatype, shape)
        self.coordinateSystems = coordinateSystems

    def write(self, fd):
        self.startwrite(fd)
        fd.write(' name="%s" type="%s"'%(self.name, self.datatype))
        if self.shape is not None:
            fd.write(' shape="%s"'%string.join(self.shape))
        if self.coordinateSystems is not None:
            fd.write(' coordinateSystems="%s"'%self.coordinateSystems)
        fd.write('>\n')
        attkeys = self.attributes.keys()
        attkeys.sort()
        for attkey in attkeys:
            att = self.attributes[attkey]
            att.write(fd)
        if self.values is not None:
            self.values.write(fd)
        self.endwrite(fd, 1)

    def toCDML(self, ncnode):
        "Translate to a CDML tree. Returns a cdms.cdmsNode.VariableNode or cdms.cdmsNode.AxisNode."
        varnode = ncml.VariableNode.toCDML(self, ncnode)
        if self.coordinateSystems is not None:
            cskeys = string.split(self.coordinateSystems)
            domainok = 1
            for cskey in cskeys:
                if cskey not in ncnode.variables.keys():
                    domainok=0
                    break
            if domainok:
                coordsys = ncnode.coordinateSystems[cskeys[0]]
                coordinates = string.join(coordsys.coordinateAxes, " ")
                varnode.setExternalAttr("coordinates", coordinates)
        return varnode

class CoordinateAxisNode(ncml.VariableNode):

    def __init__(self, name, datatype, units, shape=None, axisType=None, positive=None, boundaryRef=None):
        ncml.VariableNode.__init__(self, name, datatype, shape)
        self.tag = "coordinateAxis"
        self.units = units
        self.axisType = axisType
        self.positive = positive
        self.boundaryRef = boundaryRef

    def write(self, fd):
        self.startwrite(fd)
        fd.write(' name="%s" type="%s"'%(self.name, self.datatype))
        if self.shape is not None:
            fd.write(' shape="%s"'%string.join(self.shape))
        fd.write(' units="%s"'%self.units)
        if self.axisType is not None:
            fd.write(' axisType="%s"'%self.axisType)
        if self.positive is not None:
            fd.write(' positive="%s"'%self.positive)
        if self.boundaryRef is not None:
            fd.write(' boundaryRef="%s"'%self.boundaryRef)
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
        """Translate to a CDML tree. Returns a cdms.cdmsNode.AxisNode.
        If forcedata=1, values are set to [0., 1., ...] even if not explicitly defined.
        """
        axisnode = ncml.VariableNode.toCDML(self, ncnode, forcedata=forcedata)
        axisnode.setExternalAttr("units", self.units, CdString)
        if self.axisType is not None:
            axisnode.setExternalAttr("axisType", self.axisType, CdString)
        if self.positive is not None:
            axisnode.setExternalAttr("positive", self.positive, CdString)
        if self.boundaryRef is not None:
            axisnode.setExternalAttr("bounds", self.boundaryRef, CdString)
        return axisnode

class CoordinateAxisBoundaryNode(ncml.VariableNode):

    def __init__(self, name, datatype, shape=None):
        ncml.VariableNode.__init__(self, name, datatype, shape)
        self.tag = "coordinateAxisBoundary"

class CoordinateSystemNode(ncml.NCMLNode):

    def __init__(self, name):
        self.tag = "coordinateSystem"
        self.name = name
        self.coordinateAxes = []        # References, order is important!
        self.coordinateTransforms = []  # References, no ordering

    def addCoordinateAxis(self, a):
        self.coordinateAxes += [a]

    def addCoordinateTransform(self, t):
        self.coordinateTransforms += [t]

    def write(self, fd):
        self.startwrite(fd)
        fd.write(' name="%s">\n'%self.name)

        cartag = '%s:coordinateAxisRef'%ncml.nsprefix
        for car in self.coordinateAxes:
            fd.write('  <%s ref="%s"/>\n'%(cartag, car))

        ctrtag = '%s:coordinateTransformRef'%ncml.nsprefix
        for ctr in self.coordinateTransforms:
            fd.write('  <%s ref="%s"/>\n'%(ctrtag, ctr))

        self.endwrite(fd, 1)

class CoordinateTransformNode(ncml.NCMLNode):

    def __init__(self, name, authority, transformType=None):
        self.tag = "coordinateTransform"
        self.name = name
        self.authority = authority
        self.transformType = transformType
        self.parameters = {}            # Attributes by a different name

    def setAttribute(self, p):
        self.parameters[p.name] = p

    def write(self, fd):
        self.startwrite(fd)
        fd.write(' name="%s" authority="%s"'%(self.name, self.authority))
        if self.transformType is not None:
            fd.write(' transformType="%s"'%self.transformType)
        fd.write('>\n')

        paramkeys = self.parameters.keys()
        for paramkey in paramkeys:
            param = self.parameters[paramkey]
            param.write(fd)

        self.endwrite(fd, 1)

if __name__=='__main__':

    import sys

    nc = scan(sys.argv[1])
    print '<?xml version="1.0" encoding="UTF-8"?>'
    nc.write(sys.stdout)

##     datasetnode = nc.toCDML()
##     datasetnode.dump()


