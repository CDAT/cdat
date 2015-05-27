## Automatically adapted for numpy.oldnumeric Aug 01, 2007 by 
## Further modified to be pure new numpy June 24th 2008

"CDMS File-based variables."
import numpy
import typeconv
import types
import re

from cdmsobj import Max32int
from variable import DatasetVariable
from error import CDMSError
from sliceut import reverseSlice
from avariable import AbstractVariable
from cdms2 import Cdunif
from Cdunif import CdunifError

FileClosed = "Cannot read from closed file, variable: "
FileClosedWrite = "Cannot write to a closed file, variable: "

class FileVariable(DatasetVariable):
    "A variable in a single file."
    def __init__(self,parent,varname,cdunifobj=None):
        DatasetVariable.__init__(self, parent, varname)
        self._obj_ = cdunifobj
        if cdunifobj is not None:
            for attname, attval in cdunifobj.__dict__.items():
                self.__dict__[attname] = attval
                self.attributes[attname]=attval
        val = self.__cdms_internals__+['name_in_file',]
        self.___cdms_internals__ = val

    # Initialize the domain
    def initDomain(self, axisdict):
        "Called by whoever made me."
        self.domain = []
        for dimname in self._obj_.dimensions:
            axis = axisdict.get(dimname)
            start = 0
            length = len(axis)
            truelen = length
            self.domain.append((axis,start,length,truelen))

    def typecode(self):
        # Compatibility: convert to new typecode
        tc = self._obj_.typecode()
        tc = typeconv.convtypecode2(tc).char
        return tc

    def assignValue(self,data):
        if self.parent is None:
            raise CDMSError, FileClosedWrite+self.id
        if numpy.ma.isMaskedArray(data):
            saveFill = data.fill_value
            if self.getMissing() is None:
                self.setMissing(saveFill)
            else:
                data.set_fill_value(self.getMissing())
        self._obj_.assignValue(numpy.ma.filled(data))
        if numpy.ma.isMaskedArray(data):
            data.set_fill_value(saveFill)

    def expertSlice (self, initslicelist):
        # Handle negative slices
        revlist = []                    # Slices to apply to result if reversals needed
        slist = []                      # Slices with positive strides
        haveReversals = 0               # True iff result array needs reversing
        i=0
        for s in initslicelist:
            if s.step<0:
                axislen = self.shape[i]
                slist.append(reverseSlice(s,axislen))
                revlist.append(slice(None,None,-1))
                haveReversals = 1
            else:
                slist.append(s)
                revlist.append(slice(None,None,1))
            i += 1

        if self.parent is None:
            raise CDMSError, FileClosed+self.id
        if self.rank() == 0:
            return self._obj_.getValue()
        result = apply(self._obj_.getitem,slist)

        # If slices with negative strides were input, apply the appropriate
        # reversals.
        if haveReversals:
            result = result[revlist]

        return result

    def __setitem__(self, index, value):
        if self.parent is None:
            raise CDMSError, FileClosedWrite+self.id
        if numpy.ma.isMaskedArray(value):
            saveFill = value.fill_value
            if self.getMissing() is None:
                self.setMissing(saveFill)
            else:
                value.set_fill_value(self.getMissing())
        apply(self._obj_.setitem,(index,numpy.ma.filled(value)))
        if numpy.ma.isMaskedArray(value):
            value.set_fill_value(saveFill)

    def __setslice__(self, low, high, value):
        if self.parent is None:
            raise CDMSError, FileClosedWrite+self.id

        # Hack to prevent netCDF overflow error on 64-bit architectures
        high = min(Max32int, high)
        
        if numpy.ma.isMaskedArray(value):
            saveFill = value.fill_value
            if self.getMissing() is None:
                self.setMissing(saveFill)
            else:
                value.set_fill_value(self.getMissing())
        apply(self._obj_.setslice,(low,high,numpy.ma.filled(value)))
        if numpy.ma.isMaskedArray(value):
            value.set_fill_value(saveFill)

    def _getShape (self):
        if self.parent is None:
            raise CDMSError, FileClosed+self.id
        return self._obj_.shape

    # Write external attributes to the file.
    # Note: __setattr__ is defined at the PropertiedClasses level.
    # This function intercepts the basic set operation, and ensures
    # that the value is propagated to the external file.
    def __setattr__(self, name, value):
        if hasattr(self, "parent") and self.parent is None:
            raise CDMSError, FileClosedWrite+self.id
        if (not name in self.__cdms_internals__) and (value is not None) and (name[0]!='_'):
            try:
                setattr(self._obj_, name, value)
            except CdunifError:
                raise CDMSError, "Setting %s.%s=%s"%(self.id,name,`value`)
            self.attributes[name]=value
        self.__dict__[name] = value

    # Delete external file attributes.
    # Note: __delattr__ is defined at the PropertiedClasses level.
    # This function intercepts the basic del operation, and ensures
    # that the delete is propagated to the external file.
    def __delattr__(self, name):
        if (not name in self.__cdms_internals__):
            try:
                delattr(self._obj_, name)
            except CdunifError:
                raise CDMSError, "Deleting %s.%s"%(self.id,name)
            del(self.attributes[name])
        del self.__dict__[name]

    def getValue(self, squeeze=1):
        """Return the entire set of values."""
        if self.parent is None:
            raise CDMSError, FileClosed+self.id
        if self.rank()>0:
            return self.getSlice(Ellipsis, squeeze=squeeze)
        else:
            return self._obj_.getValue()
    
    def __len__(self):
        " Length of first dimension. "
        if self.parent is None:
            raise CDMSError, FileClosed+self.id
        return len(self._obj_)

#    def __repr__(self):
#        if self.parent is not None:
#            return "<Variable: %s, file: %s, shape: %s>"%(self.id, self.parent.id, `self.shape`)
#        else:
#            return "<Variable: %s, file: **CLOSED**>"%self.id


    shape = property(_getShape,None)
