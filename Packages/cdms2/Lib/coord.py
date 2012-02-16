## Automatically adapted for numpy.oldnumeric Aug 01, 2007 by 
## Further modified to be pure new numpy June 24th 2008

"""
CDMS CoordinateAxis objects
"""
import cdmsNode
import cdtime
import copy
import numpy
#import internattr
import types
import string
from cdmsobj import CdmsObj
from axis import createAxis, TransientVirtualAxis
from error import CDMSError
from convention import AliasList, level_aliases, longitude_aliases, latitude_aliases, time_aliases,\
     forecast_aliases
from fvariable import FileVariable
from variable import DatasetVariable
from tvariable import TransientVariable
from avariable import AbstractVariable

MethodNotImplemented = "Method not yet implemented"
NoSuchAxisOrGrid = "No such axis or grid: "
InvalidGridElement = "Grid domain elements are not yet implemented: "

std_axis_attributes = ['name', 'units', 'length', 'values', 'bounds']

# Map between cdtime calendar and CF tags
calendarToTag = {
    cdtime.MixedCalendar : 'gregorian',
    cdtime.NoLeapCalendar : 'noleap',
    cdtime.GregorianCalendar : 'proleptic_gregorian',
    cdtime.JulianCalendar : 'julian',
    cdtime.Calendar360 : '360_day'
    }

tagToCalendar = {
    'gregorian' : cdtime.MixedCalendar,
    'standard' : cdtime.GregorianCalendar,
    'noleap' : cdtime.NoLeapCalendar,
    'julian' : cdtime.JulianCalendar,
    'proleptic_gregorian' : cdtime.GregorianCalendar,
    '360_day' : cdtime.Calendar360,
    '360' : cdtime.Calendar360,
    '365_day' : cdtime.NoLeapCalendar,
    }

# This is not an error message, it is used to detect which things have
# been left as default indices or coordinates.
unspecified = "No value specified."

# Create a transient axis
def createCoordinateAxis(data, bounds=None, id=None, copy=0):
    return TransientAxis(data, bounds, id, copy=copy)

# AbstractCoordinateAxis defines the common interface
# for coordinate variables/axes.
class AbstractCoordinateAxis(CdmsObj):

    axis_count = 0                      # Transient axis count

    def __init__(self, parent=None, variableNode=None, bounds=None):
        CdmsObj.__init__(self, variableNode)
        self._bounds_ = bounds
        
    def isAbstractCoordinate(self):
        return 1

    def clone (self, copyData=1):
        """clone (self, copyData=1)
        Return a copy of self as a transient axis.
        If copyData is 1, make a separate copy of the data."""
        raise CDMSError, MethodNotImplemented

    # Designate axis as a latitude axis.
    # If persistent is true, write metadata to the container.
    def designateLatitude(self, persistent=0):
        if persistent:
            self.axis = "Y"
        else:
            self.__dict__['axis'] = "Y"

    # Designate axis as a vertical level axis
    # If persistent is true, write metadata to the container.
    def designateLevel(self, persistent=0):
        if persistent:
            self.axis = "Z"
        else:
            self.__dict__['axis'] = "Z"

    # Designate axis as a longitude axis
    # If persistent is true, write metadata to the container.
    def designateLongitude(self, persistent=0):
        if persistent:
            self.axis = "X"
        else:
            self.__dict__['axis'] = "X"

    # Designate axis as a time axis, and optionally set the calendar
    # If persistent is true, write metadata to the container.
    def designateTime(self, persistent=0, calendar=None):
        if calendar is None:
            calendar = cdtime.DefaultCalendar
        if persistent:
            self.axis = "T"
            if calendar is not None:
                self.setCalendar(calendar, persistent)
        else:
            self.__dict__['axis'] = "T"
            if calendar is not None:
                self.setCalendar(calendar, persistent)

    # Return the cdtime calendar: GregorianCalendar, NoLeapCalendar, JulianCalendar, Calendar360
    # or None. If the axis does not have a calendar attribute, return the global
    # calendar.
    def getCalendar(self):
        if hasattr(self,'calendar'):
            calendar = string.lower(self.calendar)
        else:
            calendar = None

        cdcal = tagToCalendar.get(calendar,None)
        return cdcal

    def getData(self):
        raise CDMSError, MethodNotImplemented
 
    # Return None if not explicitly defined
    def getExplicitBounds(self):
        if self._bounds_ is not None:
            return copy.copy(self._bounds_)
        else:
            return None

    def info(self, flag=None, device=None):
        "Write info about axis; include dimension values and weights if flag"
        if device is None: device = sys.stdout
        device.write(str(self))

    # Return true iff the axis is a latitude axis
    def isLatitude(self):
        convention = self.getConvention()
        return convention.axisIsLatitude(self)

    # Return true iff the axis is a level axis
    def isLevel(self):
        id = string.lower(self.id)
        if (hasattr(self,'axis') and self.axis=='Z'): return 1
        return ((id[0:3] == 'lev') or (id[0:5] == 'depth') or (id in level_aliases))

    # Return true iff the axis is a longitude axis
    def isLongitude(self):
        convention = self.getConvention()
        return convention.axisIsLongitude(self)

    # Return true iff the axis is a time axis
    def isTime(self):
        id = string.lower(self.id)
        if (hasattr(self,'axis') and self.axis=='T'): return 1
        return (id[0:4] == 'time') or (id in time_aliases)

    # Return true iff the axis is a forecast axis
    def isForecast(self):
        id = string.lower(self.id)
        if (hasattr(self,'axis') and self.axis=='F'): return 1
        return (id[0:6] == 'fctau0') or (id in forecast_aliases)

    def listall (self, all=None):
        "Get list of info about this axis."
        aname = self.id
        result = []
        result.append('   id: ' + aname)
        if self.isLatitude(): result.append('   Designated a latitude axis.')
        if self.isLongitude(): result.append('   Designated a longitude axis.')
        if self.isTime(): result.append('   Designated a time axis.')
        if self.isLevel(): result.append('   Designated a level axis.')
        try:
            units = self.units
            result.append('   units:  ' + units)
        except:
            pass
        d = self.getValue()
        result.append('   Shape: ' + str(d.shape))
        flag = 1
        for k in self.attributes.keys():
            if k in std_axis_attributes: continue
            if flag:
                result.append('   Other axis attributes:')
                flag = 0
            result.append('      '+k+': '+str(self.attributes[k]))
        result.append('   Python id:  %s' % hex(id(self)))

        if all:
            result.append("   Values:")
            result.append(str(d))
            b = self.getBounds()
            result.append("   Bounds:")
            result.append(str(b))
        return result

    def getBounds(self):
        return self._bounds_

    def setBounds(self, bounds):
        if bounds is not None:
            if isinstance(bounds, numpy.ma.MaskedArray):
                bounds = numpy.ma.filled(bounds)
        self._bounds_ = bounds

    # Set the calendar
    def setCalendar(self, calendar, persistent=1):
        if persistent:
            self.calendar = calendarToTag.get(calendar, None)
            if self.calendar is None:
                raise CDMSError, InvalidCalendar + calendar
        else:
            self.__dict__['calendar'] = calendarToTag.get(calendar, None)
            if self.__dict__['calendar'] is None:
                raise CDMSError, InvalidCalendar + calendar

    def size (self, axis = None):
        "Number of elements in array, or in a particular axis."
        s = self.shape
        if axis is None:
            if len(s) == 0:
                return 1
            else:
                return reduce(lambda x,y: x*y, s)
        else:
            return s[axis]
        
    def writeToFile(self, file):

        if self._bounds_ is not None:
            if hasattr(self,"bounds"):
                boundsid = self.bounds
            else:
                boundsid = "bounds_"+self.id
            self.bounds = boundsid

        fvar = file.write(self)

        # Create the bounds variable 
        if (self._bounds_ is not None) and not file.variables.has_key(boundsid):
            boundslen = self._bounds_.shape[-1]
            try:
                boundid = self._bounds_.getAxis(-1).id
                boundsaxis = file.getBoundsAxis(boundslen,boundid=boundid)
            except:
                boundsaxis = file.getBoundsAxis(boundslen)
            
            axislist = fvar.getAxisList()
            axislist.append(boundsaxis)
            boundsvar = file.createVariable(boundsid, cdmsNode.NumericToCdType.get(self.dtype.char), axislist)
            boundsvar[:] = self._bounds_.astype(boundsvar.dtype)
        return fvar

class AbstractAxis2D(AbstractCoordinateAxis):

    def __init__ (self, parent=None, variableNode=None, bounds=None):
        AbstractCoordinateAxis.__init__(self, parent, variableNode, bounds=bounds)

    def clone (self, copyData=1):
        """clone (self, copyData=1)
        Return a copy of self as a transient axis.
        If copyData is 1, make a separate copy of the data."""
        result = TransientAxis2D(self[:], copy=copyData, axes=self.getAxisList(), attributes=self.attributes, bounds=self.getBounds())
        return result

    def setBounds(self, bounds):
        if bounds is not None:
            if len(bounds.shape)!=3:
                raise CDMSError, 'Bounds must have rank=3'
            if bounds.shape[0:2]!=self.shape:
                raise CDMSError, 'Bounds shape %s is inconsistent with axis shape %s'%(`bounds.shape`,`self.shape`)
        AbstractCoordinateAxis.setBounds(self, bounds)

    def subSlice (self, *specs, **keys):
        # Take a subslice, returning a TransientAxis2D
        avar = AbstractVariable.subSlice(self, *specs, **keys)
        bounds = self.getBounds()
        if bounds is None:
            newbounds = None
        else:
            newbounds = bounds[specs]   # bounds can be a numarray or DatasetVariable

        # Note: disable axis copy to preserve identity of grid and variable domains
        result = TransientAxis2D(avar, bounds=newbounds, copyaxes=0)    
        return result

# Two-dimensional coordinate axis in a dataset.
class DatasetAxis2D(AbstractAxis2D, DatasetVariable):

    # Note: node is a VariableNode
    def __init__(self, parent, id=None, variableNode=None, bounds=None):
        AbstractAxis2D.__init__(self, parent, variableNode, bounds=bounds)
        DatasetVariable.__init__(self, parent, id, variableNode)
        self._data_ = None              # Cached values

    def __repr__(self):
        if self.parent is not None:
            return "<DatasetAxis2D: %s, file: %s, shape: %s>"%(self.id, self.parent.id, `self.shape`)
        else:
            return "<DatasetAxis2D: %s, file: **CLOSED**>"%self.id

## internattr.initialize_internal_attributes(DatasetAxis2D) Copy internal attrs from parents

# Two-dimensional coordinate axis in a file.
class FileAxis2D(AbstractAxis2D, FileVariable):

    def __init__(self, parent, id, obj=None, bounds=None):
        AbstractAxis2D.__init__(self, parent, None, bounds=bounds)
        FileVariable.__init__(self, parent, id, obj)
        self._data_ = None              # Cached values

    def __repr__(self):
        if self.parent is not None:
            return "<FileAxis2D: %s, file: %s, shape: %s>"%(self.id, self.parent.id, `self.shape`)
        else:
            return "<FileAxis2D: %s, file: **CLOSED**>"%self.id

## internattr.initialize_internal_attributes(FileAxis2D) # Copy internal attrs from parents

class TransientAxis2D(AbstractAxis2D, TransientVariable):

    def __init__(self, data, typecode=None, copy=0, savespace=0, mask=None, fill_value=None,
                 axes=None, attributes=None, id=None, copyaxes=1, bounds=None):
        """Create a transient 2D axis.
        All arguments are as for TransientVariable.
        'bounds' is the bounds array, having shape (m,n,nvert) where data.shape is (m,n) and
          nvert is the max number of vertices per cell.
        """
        AbstractAxis2D.__init__(self, None, None, bounds=bounds)
        TransientVariable.__init__(self, data, typecode=typecode, copy=copy, savespace=savespace,
                                   mask=mask, fill_value=fill_value, axes=axes, attributes=attributes,
                                   id=id, copyaxes=copyaxes)
        if axes is not None:
            self.setBounds(bounds)

## internattr.initialize_internal_attributes(TransientAxis2D) # Copy internal attrs from parents

