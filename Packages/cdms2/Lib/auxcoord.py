"""
CDMS 1-D auxiliary coordinates.

Note: In contrast to Axis objects (concrete classes subclassed from AbstractAxis), auxiliary coordinate variables are not monotonic in value, and do not share a name with the dimension.
"""

## import internattr
from error import CDMSError
from coord import AbstractCoordinateAxis
from fvariable import FileVariable
from variable import DatasetVariable
from tvariable import TransientVariable
from avariable import AbstractVariable

class AbstractAuxAxis1D(AbstractCoordinateAxis):

    def __init__ (self, parent=None, variableNode=None, bounds=None):
        AbstractCoordinateAxis.__init__(self, parent, variableNode, bounds=bounds)

    def clone (self, copyData=1):
        """clone (self, copyData=1)
        Return a copy of self as a transient axis.
        If copyData is 1, make a separate copy of the data."""
        result = TransientAuxAxis1D(self[:], copy=copyData, axes=self.getAxisList(), attributes=self.attributes, bounds=self.getBounds())
        return result

    def setBounds(self, bounds):
        if bounds is not None:
            if len(bounds.shape)!=2:
                raise CDMSError, 'Bounds must have rank=2'
            if bounds.shape[0:1]!=self.shape:
                raise CDMSError, 'Bounds shape %s is inconsistent with axis shape %s'%(`bounds.shape`,`self.shape`)
        AbstractCoordinateAxis.setBounds(self, bounds)

    def subSlice (self, *specs, **keys):
        # Take a subslice, returning a TransientAuxAxis1D
        avar = AbstractVariable.subSlice(self, *specs, **keys)
        bounds = self.getBounds()
        if bounds is None:
            newbounds = None
        else:
            newbounds = bounds[specs]   # bounds can be a numarray or DatasetVariable

        # Note: disable axis copy to preserve identity of grid and variable domains
        result = TransientAuxAxis1D(avar, bounds=newbounds, copyaxes=0)    
        return result

class DatasetAuxAxis1D(AbstractAuxAxis1D, DatasetVariable):

    # Note: node is a VariableNode
    def __init__(self, parent, id=None, variableNode=None, bounds=None):
        AbstractAuxAxis1D.__init__(self, parent, variableNode, bounds=bounds)
        DatasetVariable.__init__(self, parent, id, variableNode)
        self._data_ = None              # Cached values

    def __repr__(self):
        if self.parent is not None:
            return "<DatasetAuxAxis1D: %s, file: %s, shape: %s>"%(self.id, self.parent.id, `self.shape`)
        else:
            return "<DatasetAuxAxis1D: %s, file: **CLOSED**>"%self.id

## internattr.initialize_internal_attributes(DatasetAuxAxis1D) # Copy internal attrs from parents

class FileAuxAxis1D(AbstractAuxAxis1D, FileVariable):

    def __init__(self, parent, id, obj=None, bounds=None):
        AbstractAuxAxis1D.__init__(self, parent, None, bounds=bounds)
        FileVariable.__init__(self, parent, id, obj)
        self._data_ = None              # Cached values

    def __repr__(self):
        if self.parent is not None:
            return "<FileAuxAxis1D: %s, file: %s, shape: %s>"%(self.id, self.parent.id, `self.shape`)
        else:
            return "<FileAuxAxis1D: %s, file: **CLOSED**>"%self.id

## internattr.initialize_internal_attributes(FileAuxAxis1D) # Copy internal attrs from parents

class TransientAuxAxis1D(AbstractAuxAxis1D, TransientVariable):

    def __init__(self, data, typecode=None, copy=0, savespace=0, mask=None, fill_value=None,
                 axes=None, attributes=None, id=None, copyaxes=1, bounds=None):
        """Create a transient, auxiliary 1-D axis.
        All arguments are as for TransientVariable.
        'bounds' is the bounds array, having shape (m,nvert) where data.shape is (m,) and
          nvert is the max number of vertices per cell.
        """
        AbstractAuxAxis1D.__init__(self, None, None, bounds=bounds)
        TransientVariable.__init__(self, data, typecode=typecode, copy=copy, savespace=savespace,
                                   mask=mask, fill_value=fill_value, axes=axes, attributes=attributes,
                                   id=id, copyaxes=copyaxes)
        if axes is not None:
            self.setBounds(bounds)

## internattr.initialize_internal_attributes(TransientAuxAxis1D) # Copy internal attrs from parents

