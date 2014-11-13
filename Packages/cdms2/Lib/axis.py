## Automatically adapted for numpy.oldnumeric Aug 01, 2007 by 
## Further modified to be pure new numpy June 24th 2008

"""
CDMS Axis objects
"""
_debug = 0
std_axis_attributes = ['name', 'units', 'length', 'values', 'bounds']
import string, sys, types, copy
import numpy
# import regrid2._regrid
import cdmsNode
import cdtime
import cdmsobj
from cdmsobj import CdmsObj, Max32int
from sliceut import reverseSlice, splitSlice, splitSliceExt
from error import CDMSError
import forecast
#import internattr
from UserList import UserList
class AliasList (UserList):
    def __init__(self, alist):
        UserList.__init__(self,alist)
    def __setitem__ (self, i, value):
        self.data[i] = string.lower(value)
    def __setslice(self, i, j, values):
        self.data[i:j] = map(lambda x: string.lower(x), values)
    def append(self, value):
        self.data.append(string.lower(value))
    def extend(self, values):
        self.data.extend(map(string.lower, values))

level_aliases = AliasList(['plev'])
longitude_aliases = AliasList([])
latitude_aliases = AliasList([])
time_aliases = AliasList([])
forecast_aliases = AliasList([])

FileWasClosed = "File was closed for object: "
InvalidBoundsArray = "Invalid boundary array: "
InvalidCalendar = "Invalid calendar: %i"
MethodNotImplemented = "Method not yet implemented"
ReadOnlyAxis = "Axis is read-only: "

# mf 20010402 -- prevent user from going beyond N cycles
InvalidNCycles = "Invalid number of cycles requested for wrapped dimension: "

ComptimeType = type(cdtime.comptime(0))
ReltimeType = type(cdtime.reltime(0,"days"))
CdtimeTypes = (ComptimeType, ReltimeType)

# Map between cdtime calendar and CF tags
calendarToTag = {
    cdtime.MixedCalendar : 'gregorian',
    cdtime.NoLeapCalendar : 'noleap',
    cdtime.GregorianCalendar : 'proleptic_gregorian',
    cdtime.JulianCalendar : 'julian',
    cdtime.Calendar360 : '360_day',
    cdtime.ClimCalendar : 'clim_noncf',
    cdtime.ClimLeapCalendar : 'climleap_noncf',
    cdtime.DefaultCalendar : 'gregorian',
    cdtime.StandardCalendar : 'proleptic_gregorian',
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
    'clim' : cdtime.ClimCalendar,
    'clim_noncf' : cdtime.ClimCalendar,
    'climleap_noncf' : cdtime.ClimLeapCalendar,
    'climleap' : cdtime.ClimLeapCalendar,
    }

# This is not an error message, it is used to detect which things have
# been left as default indices or coordinates.
unspecified = "No value specified."

_autobounds = 2                         # Automatically generate axis and grid boundaries in getBounds
                                        # (for 1D lat/lon axes only.)
                                        # Modes:
                                        # 0 : off (not bounds generation)
                                        # 1 : on  (generate bounds)
                                        # 2 : grid (generate bounds for lat/lon grids only)

# Set autobounds mode to 'on' or 'off'. If on, getBounds will automatically
# generate boundary information for an axis or grid, if not explicitly defined.
# If 'off', and no boundary data is explicitly defined, the bounds will NOT
# be generated; getBounds will return None for the boundaries.
def setAutoBounds(mode):
    global _autobounds
    if mode=='on' or mode==1:
        _autobounds=1
    elif mode=='off' or mode==0:
        _autobounds=0
    elif mode=='grid' or mode==2:
        _autobounds=2

def getAutoBounds():
    return _autobounds

# Create a transient axis
def createAxis(data, bounds=None, id=None, copy=0):
    return TransientAxis(data, bounds, id, copy=copy)

# Generate a Gaussian latitude axis, north-to-south
def createGaussianAxis(nlat):
    import regrid2._regrid

    lats,wts,bnds = regrid2._regrid.gridattr(nlat,'gaussian')

    # For odd number of latitudes, gridattr returns 0 in the second half of lats
    if nlat%2:
        mid = nlat/2
        lats[mid+1:] = -lats[:mid][::-1]
        
    latBounds = numpy.zeros((nlat,2),numpy.float)
    latBounds[:,0] = bnds[:-1]
    latBounds[:,1] = bnds[1:]
    lat = createAxis(lats,latBounds,id="latitude")
    lat.designateLatitude()
    lat.units = "degrees_north"
    return lat

# Generate an equal-area latitude axis, north-to-south
def createEqualAreaAxis(nlat):
    import regrid2._regrid

    lats,wts,bnds = regrid2._regrid.gridattr(nlat,'equalarea')
    latBounds = numpy.zeros((nlat,2),numpy.float)
    latBounds[:,0] = bnds[:-1]
    latBounds[:,1] = bnds[1:]
    lat = createAxis(lats,latBounds,id="latitude")
    lat.designateLatitude()
    lat.units = "degrees_north"
    return lat

# Generate a uniform latitude axis
def createUniformLatitudeAxis(startLat, nlat, deltaLat):
    latArray = startLat + deltaLat*numpy.arange(nlat)
    lat = createAxis(latArray,id="latitude")
    lat.designateLatitude()
    lat.units = "degrees_north"
    latBounds = lat.genGenericBounds(width=deltaLat)
    lat.setBounds(latBounds)
    return lat

# Generate a uniform longitude axis
def createUniformLongitudeAxis(startLon, nlon, deltaLon):
    lonArray = startLon + deltaLon*numpy.arange(nlon)
    lon = createAxis(lonArray,id="longitude")
    lon.designateLongitude()
    lon.units = "degrees_east"
    lonBounds = lon.genGenericBounds(width=deltaLon)
    lon.setBounds(lonBounds)
    return lon

def mapLinearIntersection(xind,yind,iind,
                          aMinusEps,aPlusEps,bPlusEps,bMinusEps,
                          boundLeft,nodeSubI,boundRight):
    """

    Return true iff the coordinate interval (a,b) intersects the node
    nodeSubI or cell bounds [boundLeft,boundRight], where the interval
    (a,b) is defined by:

    xind = 'c' if (a,b) is closed on the left, 'o' if open,
    yind same for right endpoint
    aMinusEps,aPlusEps = a +/- epsilon
    bPlusEps,bMinusEps = b +/- epsilon

    and the intersection option iind = 'n','b','e','s' specifies
    whether the intersection is with respect to the node value
    nodeSubI ('n' or 'e') or the cell bounds [boundLeft,boundRight].
    See mapLinearExt.

    """

    if(iind == 'n' or iind == 'e'):
        testC_ = ( aMinusEps  <= nodeSubI   )
        test_C = (  nodeSubI  <= bPlusEps   )
        testO_ = (  aPlusEps  <  nodeSubI   )
        test_O = (  nodeSubI  <  bMinusEps  )
    elif(iind == 'b'):
        testC_ = ( aMinusEps  <= boundRight )
        test_C = ( boundLeft  <= bPlusEps   )
        testO_ = ( aPlusEps   <  boundRight )
        test_O = ( boundLeft  <  bMinusEps  )
    elif(iind == 's'):
        testC_ = ( aMinusEps  <= boundLeft  )
        test_C = ( boundRight <= bPlusEps   )
        testO_ = ( aPlusEps   <  boundLeft  )
        test_O = ( boundRight <  bMinusEps  )

    if(xind == 'c' and yind == 'c'):
        test=(testC_ and test_C)
    elif(xind == 'c' and yind == 'o'):
        test=(testC_ and test_O)
    elif(xind == 'o' and yind == 'c'):
        test=(testO_ and test_C)
    elif(xind == 'o' and yind == 'o'):
        test=(testO_ and test_O)

    return(test)

def mapLinearExt(axis, bounds, interval, indicator ='ccn', epsilon=None, stride=1, wrapped=0):

    """Map coordinate interval to index interval, without
    wraparound. interval has the form (x,y) where x and y are the
    endpoints in coordinate space. indicator is a three-character
    string, where the first character is 'c' if the interval is closed
    on the left, 'o' if open, and the second character has the same
    meaning for the right-hand point. The third character indicates
    how the intersection of the interval and axis is treated:

    'n' - the node is in the interval
    'b' - the interval intersects the cell bounds
    's' - the cell bounds are a subset of the interval
    'e' - same as 'n', plus an extra node on either side.
    
    Returns the corresponding index interval (i,j), where i<j,
    indicating the half-open index interval [i,j), or None if the
    intersection is empty.
    """
    
    indicator = string.lower(indicator)
    length = len(axis)

    # Make the interval and search array non-decreasing
    x,y = interval

    iind  = indicator[2]
    
    if x>y:
        x,y = y,x
        xind  = indicator[1]
        yind  = indicator[0]
        
    else:
        xind = indicator[0]
        yind = indicator[1]

    if axis[0]>axis[-1]:
        ar = axis[::-1]
        if bounds[0,0]<bounds[0,1]:
            bd = bounds[::-1]
        else:
            bd = bounds[::-1,::-1]
        direc = 'dec'
    else:
        ar = axis
        if bounds[0,0]<bounds[0,1]:
            bd = bounds
        else:
            bd = bounds[:,::-1]
        direc = 'inc'

    if(epsilon is None):
        eps=1.0e-5
        if len(ar)>1:
            epsilon = eps * min(abs(ar[1]-ar[0]), abs(ar[-1]-ar[-2]))
        else:
            epsilon=eps

    #
    #  interval bound +/- epsilon
    #

    aMinusEps=(x-epsilon)
    aPlusEps=(x+epsilon)
    bMinusEps=(y-epsilon)
    bPlusEps=(y+epsilon)


    #oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
    #
    # out-of-bounds requests
    #
    #oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

    if iind in ['n','e']:
        mina = ar[0]
        maxa = ar[-1]
    else:
        mina = bd[0,0]
        maxa = bd[-1,1]
        
    if(bPlusEps < mina or aMinusEps > maxa):
        return None

    #nnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnn
    #
    # empty node check
    #
    #nnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnn

    # Handle empty intersections
    if (
        ( ((aPlusEps)  >  ar[-1]) and (iind == 'n') and (xind == 'o') ) or
        ( ((aMinusEps) >= ar[-1]) and (iind == 'n') and (xind == 'c') ) or
        ( ((bMinusEps) <  ar[0] ) and (iind == 'n') and (yind == 'o') ) or
        ( ((bPlusEps)  <= ar[0] ) and (iind == 'n') and (yind == 'c') ) 
        ):
        return None


    bdMaxRight=max(bd[-1][0],bd[-1][1])
    bdMinLeft=min(bd[0][0],bd[0][1])
    if (
        ( ( (aMinusEps) >  bdMaxRight ) and (iind != 'n') and (xind == 'o') ) or
        ( ( (aMinusEps) >= bdMaxRight ) and (iind != 'n') and (xind == 'c') ) or
        ( ( (bPlusEps)  <  bdMinLeft  ) and (iind != 'n') and (yind == 'o') ) or
        ( ( (bPlusEps)  <= bdMinLeft  ) and (iind != 'n') and (yind == 'c') ) 
        ): 
        return None
    
    # The intersection is nonempty; use searchsorted to get left/right limits for testing

    ii,jj = numpy.searchsorted(ar,(x,y))

    #
    #  find index range for left (iStart,iEnd) and right (jStart,jEnd)
    #
    
    # iEnd + 2 because last point in loop not done
    iStart=ii-1
    iEnd=ii+2
    if(iStart < 0): iStart=0
    if( iEnd >= length ): iEnd = length - 1

    jStart=jj-1
    jEnd=jj+2
    if( jStart < 0 ): jStart=0
    if( jEnd >= length ): jEnd = length - 1

    #
    #  initialise the index to -1 (does not exist)
    #

    iInterval=-1
    jInterval=-1
    iIntervalB=-1
    jIntervalB=-1

    #pppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppp
    #
    #  preliminary checks
    #
    #pppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppp


    if(iStart == jStart == iEnd == jEnd):
        iInterval = jInterval = iStart

    elif(jEnd < iEnd):
        pass

    else:

        #llllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllll
        #
        #  left interval check
        #
        #llllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllll

        # - user check
        
        for i in range(iStart,iEnd+1):

            nodeSubI=ar[i]
            boundLeft=bd[i][0]
            boundRight=bd[i][1]

            test=mapLinearIntersection(xind,yind,iind,
                                       aMinusEps,aPlusEps,bPlusEps,bMinusEps,
                                       boundLeft,nodeSubI,boundRight)

            if( iInterval == -1 and test ):
                iInterval = i
                break

        # - "B" check for extension
        
        for i in range(iStart,iEnd+1):

            nodeSubI=ar[i]
            boundLeft=bd[i][0]
            boundRight=bd[i][1]

            testB=mapLinearIntersection(xind,yind,'b',
                                       aMinusEps,aPlusEps,bPlusEps,bMinusEps,
                                       boundLeft,nodeSubI,boundRight)

            if( iIntervalB == -1 and testB ):
                iIntervalB = i
                break

        #rrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrr
        #
        #  right interval check
        #
        #rrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrr

        for j in range(jStart,jEnd+1):

            nodeSubI=ar[j]
            boundLeft=bd[j][0]
            boundRight=bd[j][1]

            #
            #  user test
            #

            test=mapLinearIntersection(xind,yind,iind,
                                       aMinusEps,aPlusEps,bPlusEps,bMinusEps,
                                       boundLeft,nodeSubI,boundRight)

            if( ( jInterval == -1 and iInterval != -1 and test == 0  and j <= jEnd ) ):
                jInterval = j-1

            if( (j == length-1 and test == 1) ):
                jInterval = j
                
                # no break here...

        #
        #  B test on right 
        #

        for j in range(jStart,jEnd+1):

            nodeSubI=ar[j]
            boundLeft=bd[j][0]
            boundRight=bd[j][1]

            testB=mapLinearIntersection(xind,yind,'b',
                                       aMinusEps,aPlusEps,bPlusEps,bMinusEps,
                                       boundLeft,nodeSubI,boundRight)

            if( ( jIntervalB == -1 and iIntervalB != -1 and testB == 0  and j <= jEnd ) ):
                jIntervalB = j-1

            if( ( j == length-1 and testB == 1) ):
                jIntervalB = j


    #eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
    #
    #  extension check
    #
    #eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee

    if(iind == 'e'):

        # if B index does not exist return
        if(iIntervalB < 0 or jIntervalB <0):
            return None

        # if user index exists:
        elif ( ( iInterval > -1 and jInterval > -1 ) ):

            if(jInterval < iInterval):
                
                npoints=iInterval-jInterval
                if(npoints > 0):
                    (iInterval,jInterval)=(jInterval+1,iInterval+1)
                    
                else:
                    jInterval=iInterval
                    iInterval=jInterval+1
                    
            else:

                iInterval = iInterval-1
                jInterval = jInterval+1
                
        # else set index interval to B index interval
        else:
            
            iInterval=iIntervalB
            jInterval=jIntervalB

        if(iInterval == jInterval):
            if( x < ar[iInterval] and iInterval > 0 ):
                iInterval=jInterval-1
            elif( jIntervalB < length-1 ): 
                jInterval=iInterval+1

        if(jInterval < iInterval):
            npoints=jInterval-iInterval
            if(npoints > 2):
                jInterval=iIntervalB
                iInterval=jIntervalB
            else:
                jInterval=iIntervalB
                iInterval=jIntervalB+1

        # Since the lookup is linear, ensure that the result is in range [0..length)
        iInterval = max(iInterval,0)
        jInterval = min(jInterval,length-1)
            
    #ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
    #
    # final checks
    #
    #ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff

    # if jInteval < iInterval have a single point; set to iInterval

    if(jInterval < iInterval):
        jInterval=iInterval
        
    elif(jInterval < 0 and iInterval < 0):
        return None
    
    # Reverse back if necessary
    if direc=='dec':
        iInterval,jInterval = length-jInterval-1,length-iInterval-1
    
    iReturn=iInterval
    jReturn=jInterval+1

    return (iReturn,jReturn)

def lookupArray(ar,value):
    """Lookup value in array ar. Return index such that:
    (a) ar is monotonically increasing:
    value <= ar[index], index==0..len(ar)-1
    value > ar[index], index==len(ar)
    (b) ar is monotonically decreasing:
    value >= ar[index], index==0..len(ar)-1
    value < ar[index], index==len(ar)
    """
    ar = numpy.ma.filled(ar)
    ascending = (ar[0]<ar[-1]) or len(ar)==1
    if ascending:
        index = numpy.searchsorted(ar,value)
    else:
        revar = ar[::-1]
        index = numpy.searchsorted(revar,value)
        if index<len(revar) and value==revar[index]:
            index = len(ar)-index-1
        else:
            index = len(ar)-index
    return index

## # Lookup a value in a monotonic 1-D array. value is a scalar
## # Always returns a valid index for ar
## def lookupArray(ar,value):
##     ascending = (ar[0]<ar[-1])
##     if ascending:
##         index = numpy.searchsorted(ar,value)
##     else:
##         index = numpy.searchsorted(ar[::-1],value)
##         index = len(ar)-index-1
##     index = max(index,0)
##     index = min(index,len(ar))
##     return index

# Return true if vector vec1 is a subset of vec2, within tolerance tol.
# Return second arg of index, if it is a subset
def isSubsetVector(vec1, vec2, tol):
    index = lookupArray(vec2,vec1[0])
    if index>(len(vec2)-len(vec1)):
        return (0,-1)                   # vec1 is too large, cannot be a subset
    issubset = numpy.alltrue(numpy.less(numpy.absolute(vec1-vec2[index:index+len(vec1)]),tol))
    if issubset:
        return (issubset,index)
    else:
        return (0,-1)

def isOverlapVector(vec1, vec2, atol=1.e-8):
    """Returns (isoverlap, index) where:
    isoverlap is true iff a leading portion of vec1 is a subset of vec2;
    index is the index such that vec1[0]<=vec2[index]. If index==len(vec2),
    then vec1[0]>vec2[len(vec2)-1]
    """
    index = lookupArray(vec2,vec1[0])
    if index==0 and abs(vec1[0]-vec2[0]):
        return (0,index)
    elif index==len(vec2):
        return (1,index)
    else:
        ar2 = vec2[index:index+len(vec1)]
        ar1 = vec1[:len(ar2)]
        isoverlap = numpy.ma.allclose(ar1, ar2, atol=atol)
    if isoverlap:
        return (isoverlap,index)
    else:
        return (0,index)

def allclose(ax1, ax2, rtol=1.e-5, atol=1.e-8):
    """True if all elements of axes ax1 and ax2 are close,
    in the sense of numpy.ma.allclose."""
    return ((ax1 is ax2) or numpy.ma.allclose(ax1[:],ax2[:],rtol=rtol,atol=atol))

# AbstractAxis defines the common axis interface. 
# Concrete axis classes are derived from this class.

class AbstractAxis(CdmsObj):
    def __init__ (self, parent, node):
        CdmsObj.__init__ (self, node)
        val = self.__cdms_internals__ + ['id',]
        self.___cdms_internals__ = val
        self.parent = parent
        self.id = id
        # Cached data values
        self._data_ = None
        # Cached wraparound values for circular axes
        self._doubledata_ = None
        
    def __str__ (self):
        return string.join(self.listall(), "\n") + "\n"

    __repr__ = __str__

    def __len__(self):
        raise CDMSError, MethodNotImplemented

    def _getshape(self):
        return (len(self),)

    def _getdtype(self, name):
        tc = self.typecode()
        return numpy.dtype(tc)

    def __getitem__(self, key):
        raise CDMSError, MethodNotImplemented

    def __setitem__(self, index, value):
        raise CDMSError, MethodNotImplemented

    def __getslice__(self, low, high):
        raise CDMSError, MethodNotImplemented

    def __setslice__(self, low, high, value):
        raise CDMSError, MethodNotImplemented

    def rank(self):
        return len(self.shape)

    # Designate axis as a latitude axis.
    # If persistent is true, write metadata to the container.
    def designateLatitude(self, persistent=0):
        if persistent:
            self.axis = "Y"
        else:
            self.__dict__['axis'] = "Y"
            self.attributes['axis']="Y"

    # Return true iff the axis is a latitude axis
    def isLatitude(self):
        id = self.id.strip().lower()
        if (hasattr(self,'axis') and self.axis=='Y'): return 1
        units = getattr(self,"units","").strip().lower()
        if units in ["degrees_north","degree_north","degree_n","degrees_n","degreen","degreesn"]:
          return 1
        return (id[0:3] == 'lat') or (id in latitude_aliases)

    # Designate axis as a vertical level axis
    # If persistent is true, write metadata to the container.
    def designateLevel(self, persistent=0):
        if persistent:
            self.axis = "Z"
        else:
            self.__dict__['axis'] = "Z"
            self.attributes['axis']="Z"

    # Return true iff the axis is a level axis
    def isLevel(self):
        id = self.id.strip().lower()
        if (hasattr(self,'axis') and self.axis=='Z'): return 1
        if getattr(self,"positive","").strip().lower() in ["up","down"]:
          return 1
        try:
          #Ok let's see if this thing as pressure units
          import genutil
          p=genutil.udunits(1,"Pa")
          units=getattr(self,'units',"").strip()
          p2=p.to(units)
          return 1
        except Exception,err:
          pass
        return ((id[0:3] == 'lev') or (id[0:5] == 'depth') or (id in level_aliases))

    # Designate axis as a longitude axis
    # If persistent is true, write metadata to the container.
    # If modulo is defined, set as circular
    def designateLongitude(self, persistent=0, modulo=360.0):
        if persistent:
            self.axis = "X"
            if modulo is None:
                self.topology = 'linear'
            else:
                self.modulo = modulo
                self.topology = 'circular'
        else:
            self.__dict__['axis'] = "X"
            self.attributes['axis']="X"
            if modulo is None:
                self.__dict__['topology'] = 'linear'
                self.attributes['topology'] = 'linear'
            else:
                self.__dict__['modulo'] = modulo
                self.__dict__['topology'] = 'circular'
                self.attributes['modulo'] = modulo
                self.attributes['topology'] = 'circular'

    # Return true iff the axis is a longitude axis
    def isLongitude(self):
        id = self.id.strip().lower()
        if (hasattr(self,'axis') and self.axis=='X'): return 1
        units = getattr(self,"units","").strip().lower()
        if units in ["degrees_east","degree_east","degree_e","degrees_e","degreee","degreese"]:
          return 1
        return (id[0:3] == 'lon') or (id in longitude_aliases)

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
            self.attributes['axis'] = "T"
            if calendar is not None:
                self.setCalendar(calendar, persistent)

    # Return true iff the axis is a time axis
    def isTime(self):
        id = self.id.strip().lower()
        if (hasattr(self,'axis') and self.axis=='T'): return 1
        ## Try to figure it out from units
        try:
          import genutil
          units=getattr(self,"units","").lower()
          sp = units.split("since")
          if len(sp)>1:
            t=genutil.udunits(1,"day")
            s = sp[0].strip()
            if s in t.available_units() and t.know_units()[s]=="TIME":
              return 1
            #try the plural version since udunits only as singular (day noy days)
            s=s+"s"
            if s in t.available_units() and t.know_units()[s]=="TIME":
              return 1
        except:
          pass
        return (id[0:4] == 'time') or (id in time_aliases)

    # Return true iff the axis is a forecast axis
    def isForecast(self):
        id = self.id.strip().lower()
        if (hasattr(self,'axis') and self.axis=='F'): return 1
        return (id[0:6] == 'fctau0') or (id in forecast_aliases)
    def isForecastTime(self):
        return self.isForecast()

    def asComponentTime(self, calendar=None):
        "Array version of cdtime tocomp. Returns a list of component times."
        if not hasattr(self, 'units'):
            raise CDMSError, "No time units defined"
        if calendar is None:
            calendar = self.getCalendar()
        if self.isForecast():
            result = [ forecast.comptime(t) for t in self[:] ]
        else:
            result = []
            for val in self[:]:
                result.append(cdtime.reltime(val, self.units).tocomp(calendar))
        return result
    
    #
    #  mf 20010418 -- output DTGs (YYYYMMDDHH)
    #
    def asDTGTime(self, calendar=None):
        "Array version of cdtime tocomp. Returns a list of component times in DTG format."
        if not hasattr(self, 'units'):
            raise CDMSError, "No time units defined"
        result = []
        if calendar is None:
            calendar = self.getCalendar()
        for val in self[:]:
            comptime=cdtime.reltime(val, self.units).tocomp(calendar)
            s=repr(comptime)
            tt=string.split(s,' ')
        
            ttt=string.split(tt[0],'-')
            yr=int(ttt[0])
            mo=int(ttt[1])
            da=int(ttt[2])
        
            ttt=string.split(tt[1],':')
            hr=int(ttt[0])
            dtg="%04d%02d%02d%02d"%(yr,mo,da,hr)
            result.append(dtg)

        return result

    def asdatetime(self, calendar=None):
        "Array version of cdtime tocomp. Returns a list of datetime objects."
        import datetime
        if not hasattr(self, 'units'):
            raise CDMSError, "No time units defined"
        result = []
        if calendar is None:
            calendar = self.getCalendar()
        for val in self[:]:
            c=cdtime.reltime(val, self.units).tocomp(calendar)
            dtg = datetime.datetime(c.year,c.month,c.day,c.hour,c.minute,int(c.second),int((c.second-int(c.second))*1000))
            result.append(dtg)
        return result

    def asRelativeTime( self, units=None ):
        "Array version of cdtime torel. Returns a list of relative times."
        sunits = getattr(self,'units',None)
        if sunits==None or sunits=='None':
            raise CDMSError, "No time units defined"
        if units==None or units=='None':
            units=sunits
        if self.isForecast():
            result = [ forecast.comptime(t).torel(units) for t in self[:] ]
        else:
            cal = self.getCalendar()
            result = [ cdtime.reltime(t,sunits).torel(units,cal) for t in self[:] ]
        return result

    def toRelativeTime(self, units, calendar=None):
        "Convert time axis values to another unit possibly in another calendar"
        if not hasattr(self, 'units'):
            raise CDMSError, "No time units defined"
        n=len(self[:])
        b=self.getBounds()
        scal = self.getCalendar()
        if calendar is None:
            calendar = scal
        else:
            self.setCalendar(calendar)
        for i in range(n):
            tmp=cdtime.reltime(self[i], self.units).tocomp(scal)
            tmp2 = numpy.array(float(tmp.torel(units, calendar).value)).astype(self[:].dtype.char)
            ## if i==1 : print self[:].dtype.char,'tmp2:',tmp2,tmp2.astype('f'),self[i],self[i].astype('f')
            self[i]=tmp2
            if b is not None:
                tmp=cdtime.reltime(b[i,0], self.units).tocomp(scal)
                b[i,0]=numpy.array(float(tmp.torel(units, calendar).value)).astype(b.dtype.char)
                tmp=cdtime.reltime(b[i,1], self.units).tocomp(scal)
                b[i,1]=numpy.array(float(tmp.torel(units, calendar).value)).astype(b.dtype.char)
        if b is not None:
            self.setBounds(b)
        self.units=units
        return

#mfmfmfmfmfmfmfmfmfmfmfmfmfmfmfmfmfmfmfmfmfmfmfmfmfmfmfmfmfmfmfmfmfmfmfmfmfmfmfmfmfmfmfmf
#
# mf 20010412 -- test if an Axis is intrinsically circular
#
#mfmfmfmfmfmfmfmfmfmfmfmfmfmfmfmfmfmfmfmfmfmfmfmfmfmfmfmfmfmfmfmfmfmfmfmfmfmfmfmfmfmfmfmf


    # Return true iff the axis wraps around
    # An axis is defined as circular if:
    # (1) self.topology=='circular', or
    # (2) self.topology is undefined, and the axis is a longitude
    def isCircularAxis(self):
        
        if hasattr(self,'topology'):
            iscircle = (self.topology=='circular')
        elif self.isLongitude():
            iscircle = 1
        else:
            iscircle = 0

        return iscircle


#mfmfmfmfmfmfmfmfmfmfmfmfmfmfmfmfmfmfmfmfmfmfmfmfmfmfmfmfmfmfmfmfmfmfmfmfmfmfmfmfmfmfmfmf
#
# mf 20010405 -- test if an transient Axis is REALLY circular
#
#mfmfmfmfmfmfmfmfmfmfmfmfmfmfmfmfmfmfmfmfmfmfmfmfmfmfmfmfmfmfmfmfmfmfmfmfmfmfmfmfmfmfmfmf


    # Return true iff the axis wraps around
    # An axis is defined as circular if:
    # (1) self.topology=='circular', or
    # (2) self.topology is undefined, and the axis is a longitude
    def isCircular(self):

        if(len(self) < 2):
            return 0
        
        baxis = self[0]
        eaxis = self[-1]
        deltaend = self[-1] - self[-2]
        eaxistest = eaxis + deltaend - baxis

        cycle=self.getModuloCycle()

        tol=0.01*deltaend

        test=0
        if(abs(eaxistest - cycle) < tol): test=1
        
        if hasattr(self,'topology') and test == 1:
            iscircle = (self.topology=='circular')
        elif (self.isLongitude() and test == 1):
            iscircle = 1
        else:
            iscircle = 0

        return iscircle

    def designateCircular(self, modulo, persistent=0):
        if persistent:
            self.topology = 'circular'
            self.modulo = modulo
        else:
            self.__dict__['topology'] = 'circular'
            self.__dict__['modulo'] = modulo
            self.attributes['modulo'] = modulo
            self.attributes['topology'] = 'linear'

    def isLinear(self):
        raise CDMSError, MethodNotImplemented

    def getBounds(self):
        raise CDMSError, MethodNotImplemented

    # Return None if not explicitly defined
    def getExplicitBounds(self):
        raise CDMSError, MethodNotImplemented

    def setBounds(self, bounds):
        raise CDMSError, MethodNotImplemented

    # Return the cdtime calendar: GregorianCalendar, NoLeapCalendar, JulianCalendar, Calendar360
    # or None. If the axis does not have a calendar attribute, return the global
    # calendar.
    def getCalendar(self):
        if hasattr(self,'calendar'):
            calendar = string.lower(self.calendar)
        else:
            calendar = None

        cdcal = tagToCalendar.get(calendar, cdtime.DefaultCalendar)
        return cdcal

    # Set the calendar
    def setCalendar(self, calendar, persistent=1):
        if persistent:
            self.calendar = calendarToTag.get(calendar, None)
            self.attributes['calendar']=self.calendar
            if self.calendar is None:
                raise CDMSError, InvalidCalendar % calendar
        else:
            self.__dict__['calendar'] = calendarToTag.get(calendar, None)
            self.attributes['calendar']=self.calendar
            if self.__dict__['calendar'] is None:
                raise CDMSError, InvalidCalendar % calendar

    def getData(self):
        raise CDMSError, MethodNotImplemented
 
    # Return the entire array
    def getValue(self):
        return self.__getitem__(slice(None))

    def assignValue(self,data):
        self.__setitem__(slice(None),data)

    def _time2value(self, value):
        """ Map value of type comptime, reltime, or string of form "yyyy-mm-dd hh:mi:ss" to value"""
        if self.isTime():
            if type(value) in CdtimeTypes:
                value = value.torel(self.units, self.getCalendar()).value
            elif type(value) is types.StringType and value not in [':',unspecified]:
                cal = self.getCalendar()
                value = cdtime.s2c(value, cal).torel(self.units, cal).value
        return value


    def getModuloCycle(self):

        if hasattr(self,'modulo'):
            cycle = self.modulo
            #
            # mf 20010419 test if attribute is a string (non CF), set to 360.0
            #
            if(type(cycle) == types.StringType):
                cycle = 360.0
        else:
            cycle = 360.0

        if isinstance(cycle, numpy.ndarray):
            cycle = cycle[0]

        return(cycle)


    def getModulo(self):

        if not self.isCircular():
            return(None)

        return(self.getModuloCycle())

    def mapInterval(self,interval,indicator='ccn',cycle=None):
        """
        Map coordinate interval to index interval. interval has one of the forms:

          (x,y)
          (x,y,indicator): indicator overrides keywork argument
          (x,y,indicator,cycle): indicator, cycle override keyword arguments
          None: indicates the full interval

        where x and y are the endpoints in coordinate space. indicator is a
        two-character string, where the first character is 'c' if the interval
        is closed on the left, 'o' if open, and the second character has the
        same meaning for the right-hand point. Set cycle to a nonzero value
        to force wraparound.

        Returns the corresponding index interval (i,j), where i<j, indicating
        the half-open index interval [i,j), or None if the intersection is empty.

        For an axis which is circular (self.topology == 'circular'), [i,j)
        is interpreted as follows (where N = len(self)):

        (1) if j<=N, the interval does not wrap around the axis endpoint
        (2) if j>N, the interval wraps around, and is equivalent to the
            two consecutive intervals [i,N), [0,j-N)
        For example, if the vector is [0,2,4,...,358] of length 180,
        and the coordinate interval is [-5,5), the return index interval is
        [178,183). This is equivalent to the two intervals [178,180) and [0,3).

        Note: if the interval is interior to the axis, but does not span any
        axis element, a singleton (i,i+1) indicating an adjacent index is returned.
        """
        i,j,k = self.mapIntervalExt(interval,indicator,cycle)
        j = min(j, i+len(self))
        #i=i-1
        return (i,j)


#mfmfmfmfmfmfmfmfmfmfmfmfmfmfmfmfmfmfmfmfmfmfmfmfmfmfmfmfmfmfmfmfmfmfmfmfmfmfmfmfmfmfmfmf
#
# mf 20010308 - 20010412 -- general handing of wrapping
#
#mfmfmfmfmfmfmfmfmfmfmfmfmfmfmfmfmfmfmfmfmfmfmfmfmfmfmfmfmfmfmfmfmfmfmfmfmfmfmfmfmfmfmfmf

    def mapIntervalExt(self,interval,indicator='ccn',cycle=None,epsilon=None):
        """Like mapInterval, but returns (i,j,k) where k is stride,
        and (i,j) is not restricted to one cycle."""

        # nCycleMax : max number of cycles a user a specify in wrapping

        nCycleMax=6
        
        # interval is None returns the full interval
        if interval is None or interval==':':
            return (0, len(self), 1)

        # Allow intervals of the same form as getRegion.
        if len(interval)==3:
            x,y,indicator = interval
            interval = (x,y)
        elif len(interval)==4:
            x,y,indicator,cycle = interval
            interval = (x,y)

        # check length of indicator if overridden by user
        #

        indicator = string.lower(indicator)
        if len(indicator)==2: indicator += 'n'

        if( ( len(indicator) != 3 ) or
               ( (indicator[0] != 'c' and indicator[0] != 'o') or
                 (indicator[1] != 'c' and indicator[1] != 'o') or
                 (indicator[2] != 'n' and indicator[2] != 'b' and indicator[2] != 's' and
                  indicator[2] != 'e')
                 )
            ):
            raise CDMSError, "EEE: 3-character interval/intersection indicator incomplete or incorrect = "\
                 +indicator
       
        if self._data_ is None:
            self._data_ = self.getData()

        #ttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttt
        # Handle time types
        interval = (self._time2value(interval[0]), self._time2value(interval[1]))

        # If the interval is reversed wrt self, reverse the interval and
        # set the stride to -1
        if (interval[0]<=interval[1])==(self[0]<=self[-1]):
            stride=1
        else:
            stride=-1
            interval = (interval[1],interval[0])
            indicator = indicator[1]+indicator[0]+indicator[2]

        #mmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmm
        #
        # basic test for wrapping - is axis REALLY circular?
        #
        #ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff

        xind = indicator[0]
        yind = indicator[1]
        iind = indicator[2]

        xi,yi = interval

        length = len(self)
        ar = self[:]
        ar0 = ar[0]
        arn = ar[-1]
        armin = min(ar0,arn)
        armax = max(ar0,arn)

        # Wrapped if circular and at least one value is outside the axis range.
        wraptest1 = self.isCircular()
        wraptest2 = not ((armin <= xi <= armax) and (armin <= yi <= armax))
        
        if (wraptest1 and wraptest2):

            #
            #  find cycle and calc # of cycles in the interval
            #
            
            cycle=self.getModulo()
            
            intervalLength=yi-xi
            intervalCycles=intervalLength/cycle
            
            bd = self.getBounds()
            
            nPointsCycle = len(ar)

            ar0 = ar[0]
            ar1 = ar[-1]

            #
            # test for reversed coordinates
            #
            
            if ar0>ar1:
                cycle = -1 * abs(cycle)

            #eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
            #
            #  make sure xi<yi and shift to positive axis indices
            #
            #eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
            
            # Ensure that xi<yi
            
            if cycle>0 and yi<xi: xi,yi = yi,xi
            if cycle<0 and yi>xi: xi,yi = yi,xi

            # calculate the number of cycles to shift to positive side
            
            nCycleShift=numpy.floor((xi-ar0)/cycle)
            xp = xi - cycle * nCycleShift
            yp = xp + intervalLength

            # Extend the data vector with wraparound number of cycles in interval and shifts

            nCycle = int(intervalCycles + 1.0 + 0.5) + abs(nCycleShift)
            
                
            #
            # check if nCycle is > nCycleMax
            #
            if(nCycle >= nCycleMax):
                raise CDMSError, InvalidNCycles + repr(nCycle)

            self._doubledata_ = numpy.concatenate(( ar, ar + cycle ))
            k=2
            while(k<nCycle):
                self._doubledata_ = numpy.concatenate(( self._doubledata_, ar + k*cycle ) )
                k=k+1

            # Map the canonical coordinate interval (xp,yp) in the 'extended' data array
            # create axis to get the bounds array

            bigar = self._doubledata_
            bigarAxis=createAxis(bigar)
            bd = bigarAxis.getBounds()
            if bd is None:              # In case autobounds is off
                bd = bigarAxis.genGenericBounds()

            # run the more general mapLinearExt to get the indices
            
            indexInterval= mapLinearExt(bigar,bd,(xp,yp),indicator,wrapped=1)

            #
            # check to make sure we got an interval
            #
            
            if(indexInterval is None):
                return None

            i,j=indexInterval

            #
            #  now shift i back
            #

            i = i + int(nCycleShift*float(nPointsCycle))

            #   
            #  adjust the length of the output interval by the indicator
            #  mapLinear does the calc correctly, we have to modify because we
            #  are overriding with the (float)(number of cycles) in the interval
            #
            
            j = j + int(nCycleShift*float(nPointsCycle))
            retval = (i,j)
            
        else:
            bd = self.getBounds()
            if bd is None:              # In case autobounds is off
                bd = self.genGenericBounds()
            retval = mapLinearExt(ar, bd, interval, indicator)

        if retval is not None:
            i,j = retval
            if stride==-1:
                if(j == length):
                    i,j=j-1,i-1
                else:
                    i,j=j-1,i-1
                if j==-1:
                    j=None

            retval = (i,j,stride)

        return retval

    def subaxis(self,i,j,k=1, wrap=True):
        """Create a transient axis for the index slice [i:j:k]
        The stride k can be positive or negative. Wraparound is
        supported for longitude dimensions or those with a modulus attribute.
        """
        fullBounds = self.getBounds()
        _debug=0
        _debugprefix="SS__XX subaxis "
        

        # Handle wraparound
        modulo = None
        size = len(self)
        
        #----------------------------------------------------------------------
        # mf 20010328 negative stride i >= vice i > 
        #----------------------------------------------------------------------
        
        if wrap and ((k>0 and j>size) or (k<0 and i >= size)) and self.isCircular():
            modulo = self.getModuloCycle()

        if modulo is not None:
            # If self is decreasing and stride is positive,
            # or self is increasing and stride is negative, subtract the modulus,
            # otherwise add it.
            if (self[0]>self[-1])==(k>0):
                modulo = -modulo

            #----------------------------------------------------------------------
            #
            #  mf 20010329 -- N vice two slice scheme (more general)
            #
            #----------------------------------------------------------------------

            donew=1

            if(donew):

                sn = splitSliceExt(slice(i,j,k),size)
                if(_debug): print "SSSS1-------------------- ",sn,len(sn)

                for kk in range(0,len(sn)):
                    sl=sn[kk]
                    if(_debug): print "SSSSSSSS kk = ",kk,sl
                    part = self[sl] + kk*modulo
                    if(_debug): print "SSSSSSSSSSSSSSS modulo",part[0],part[-1],modulo
                    if(kk == 0):
                        data = part
                    else:
                        data = numpy.concatenate((data,part))

                    if fullBounds is not None:
                        bound = fullBounds[sl] + kk*modulo
                        if (kk == 0):
                            bounds = bound
                        else:
                            bounds = numpy.concatenate((bounds,bound))
                    else:
                        bounds = None
                        
            
            else:
                
                s1, s2 = splitSlice(slice(i,j,k),size)
                if(_debug): print "SSSS0: original ",s1,s2
                
                part1 = self[s1]
                part2 = self[s2]+modulo
                if(_debug): print "SSSSSSSSSSSSSSS modulo",self[0],self[-1],modulo
                data = numpy.concatenate((part1,part2))
                if fullBounds is not None:
                    bounds1 = fullBounds[s1]
                    bounds2 = fullBounds[s2]+modulo
                    bounds = numpy.concatenate((bounds1,bounds2))
                else:
                    bounds = None
            

        else:                           # no wraparound
            data = self[i:j:k]
            if fullBounds is not None:
                bounds = fullBounds[i:j:k]
            else:
                bounds = None
        
        newaxis = TransientAxis(data, bounds, id=self.id, copy=1)

        if self.isLatitude(): newaxis.designateLatitude()
        if self.isLongitude(): newaxis.designateLongitude()
        if self.isLevel(): newaxis.designateLevel()
        if self.isTime(): newaxis.designateTime()

        for attname in self.attributes.keys():
            if attname not in ["datatype", "length","isvar","name_in_file","partition","partition_length"]:
                setattr(newaxis, attname, getattr(self, attname))
                newaxis.attributes[attname]=getattr(self,attname)

        # Change circular topology to linear if a strict subset was copied
        if hasattr(self,"topology") and self.topology=="circular" and len(newaxis)<len(self):
            newaxis.topology="linear"

        return newaxis
    
#----------------------------------------------------------------------
# mf 2001 set calls to subAxis as subaxis
#----------------------------------------------------------------------

    subAxis = subaxis

    def typecode(self):
        raise CDMSError, MethodNotImplemented

    # Check that a boundary array is valid, raise exception if not. bounds is an array of shape (n,2)
    def validateBounds(self,bounds):
        requiredShape = (len(self),2)
        requiredShape2 = (len(self)+1,)
        if bounds.shape!=requiredShape and bounds.shape!=requiredShape2:
            raise CDMSError, InvalidBoundsArray + \
                 'shape is %s, should be %s or %s'%(`bounds.shape`,`requiredShape`,`requiredShape2`)
        if bounds.shape==requiredShape2: # case of "n+1" bounds
            bounds2=numpy.zeros(requiredShape)
            bounds2[:,0]=bounds[:-1]
            bounds2[:,1]=bounds[1::]
            bounds=bounds2
        mono = (bounds[0,0]<=bounds[0,1])
        if mono:
            for i in range(bounds.shape[0]):
                if not bounds[i,0]<=self[i]<=bounds[i,1]:
                    raise CDMSError, InvalidBoundsArray + \
'bounds[%i]=%f is not in the range [%f,%f]'%(i,self[i],bounds[i,0],bounds[i,1])
        else:
            for i in range(bounds.shape[0]):
                if not bounds[i,0]>=self[i]>=bounds[i,1]:
                    raise CDMSError, InvalidBoundsArray + \
'bounds[%i]=%f is not in the range [%f,%f]'%(i,self[i],bounds[i,1],bounds[i,0])
        return bounds

    # Generate bounds from midpoints. width is the width of the zone if the axis has one value.
    def genGenericBounds(self, width=1.0):
        if self._data_ is None:
            self._data_ = self.getData()
        ar = self._data_
        if len(self)>1:
            leftPoint = numpy.array([1.5*ar[0]-0.5*ar[1]])
            midArray = (ar[0:-1]+ar[1:])/2.0
            rightPoint = numpy.array([1.5*ar[-1]-0.5*ar[-2]])
            bnds = numpy.concatenate((leftPoint,midArray,rightPoint))
        else:
            delta = width/2.0
            bnds = numpy.array([self[0]-delta,self[0]+delta])

        # Transform to (n,2) array
        retbnds = numpy.zeros((len(ar),2),numpy.float)
        retbnds[:,0] = bnds[:-1]
        retbnds[:,1] = bnds[1:]

        if self.isLatitude():
            retbnds[0,:] = numpy.maximum(-90.0, numpy.minimum(90.0,retbnds[0,:]))
            retbnds[-1,:] = numpy.maximum(-90.0, numpy.minimum(90.0,retbnds[-1,:]))

        return retbnds

    def clone (self, copyData=1):
        """clone (self, copyData=1)
        Return a copy of self as a transient axis.
        If copyData is 1, make a separate copy of the data."""
        b = self.getBounds()
        if copyData==1:
            mycopy = createAxis(copy.copy(self[:]))
        else:
            mycopy = createAxis(self[:])
        mycopy.id = self.id
        try:
            mycopy.setBounds(b)
        except CDMSError:
            b = mycopy.genGenericBounds()
            mycopy.setBounds(b)
        for k, v in self.attributes.items():
           setattr(mycopy, k, v)
        return mycopy

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
        result.append('   Length: ' + str(len(d)))
        result.append('   First:  ' + str(d[0]))
        result.append('   Last:   ' + str(d[-1]))
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

    def info(self, flag=None, device=None):
        "Write info about axis; include dimension values and weights if flag"
        if device is None: device = sys.stdout
        device.write(str(self))

    def isVirtual(self):
        "Return true iff coordinate values are implicitly defined."
        return 0

    shape = property(_getshape,None)
    dtype = _getdtype

## PropertiedClasses.set_property(AbstractAxis, 'shape', 
##                         AbstractAxis._getshape, nowrite=1, nodelete=1)
## PropertiedClasses.set_property(AbstractAxis, 'dtype', 
##                         AbstractAxis._getdtype, nowrite=1, nodelete=1)
## internattr.add_internal_attribute (AbstractAxis, 'id', 'parent')

# One-dimensional coordinate axis in a dataset
class Axis(AbstractAxis):
    def __init__(self,parent,axisNode=None):
        if axisNode is not None and axisNode.tag != 'axis':
               raise CDMSError, 'Creating axis, node is not an axis node.'
        AbstractAxis.__init__(self, parent, axisNode)
        if axisNode is not None:
            if axisNode.partition is not None:
                flatpart = axisNode.partition
                self.__dict__['partition']=numpy.reshape(flatpart,(len(flatpart)/2,2))
                self.attributes['partition']=self.partition
        self.id = axisNode.id
    
    def typecode(self):
        return cdmsNode.CdToNumericType.get(self._node_.datatype)

    # Handle slices of the form x[i], x[i:j:k], x[(slice(i,j,k),)], and x[...]
    def __getitem__(self, key):
        node = self._node_
        length = len(node)

        # Allow key of form (slice(i,j),) etc.
        if type(key) is types.TupleType and len(key)==1:
            key = key[0]

        if isinstance(key, (types.IntType, numpy.int,numpy.int32)):  # x[i]
            if key>=length:
                raise IndexError, 'index out of bounds'
            else:
                # Don't generate the entire array (if linear) just for one value
                return node.data[key%length]
        elif type(key) is types.SliceType: # x[i:j:k]
            if self._data_ is None:
                self._data_ = node.getData()
            return self._data_[key.start:key.stop:key.step]
        elif type(key) is types.EllipsisType: # x[...]
            if self._data_ is None:
                self._data_ = node.getData()
            return self._data_
        elif type(key) is types.TupleType:
            raise IndexError,'axis is one-dimensional'
        else:
            raise IndexError,'index must be an integer: %s'%`key`

    # Get axis data
    def getData(self):
        return self._node_.getData()      # Axis data is retrieved from the metafile

    # Handle slices of the form x[i:j]
    def __getslice__(self, low, high):
        if self._data_ is None:
            self._data_ = self.getData()
        return self._data_[low:high]

    def __len__(self):
        return len(self._node_)

    # Return true iff the axis representation is linear
    def isLinear(self):
        return self._node_.dataRepresent==cdmsNode.CdLinear

    # Return the bounds array, or generate a default if autoBounds mode is on
    def getBounds(self):
        boundsArray = self.getExplicitBounds()
        try:
            self.validateBounds(boundsArray)
        except:
            boundsArray = None
        abopt = getAutoBounds()
        if boundsArray is None and (abopt==1 or (abopt==2 and (self.isLatitude() or self.isLongitude()))) :
            boundsArray = self.genGenericBounds()
            
        return boundsArray

    # Return the bounds array, or None
    def getExplicitBounds(self):
        boundsArray = None
        if hasattr(self,'bounds'):
            boundsName = self.bounds
            try:
                boundsVar = self.parent.variables[boundsName]
                boundsArray = numpy.ma.filled(boundsVar.getSlice())
            except KeyError:
                boundsArray = None

        return boundsArray

    def getCalendar(self):
        if hasattr(self,'calendar'):
            calendar = string.lower(self.calendar)
        elif self.parent is not None and hasattr(self.parent, 'calendar'):
            calendar = string.lower(self.parent.calendar)
        else:
            calendar = None

        cdcal = tagToCalendar.get(calendar, cdtime.DefaultCalendar)
        return cdcal

# In-memory coordinate axis
class TransientAxis(AbstractAxis):
    axis_count = 0
    def __init__(self, data, bounds=None, id=None, attributes=None, copy=0):
        AbstractAxis.__init__(self, None, None)
        if id is None:
            TransientAxis.axis_count = TransientAxis.axis_count + 1
            id = 'axis_' + str(TransientAxis.axis_count)
        if attributes is None:
            if hasattr(data, 'attributes'): attributes = data.attributes
        if attributes is not None:
            for name, value in attributes.items():
                if name not in ['missing_value', 'name']:
                    setattr(self, name, value)
        self.id = id
        if isinstance(data, AbstractAxis):
            if copy == 0:
                self._data_ = data[:]
            else:
                self._data_ = numpy.array(data[:])
        elif isinstance(data, numpy.ndarray):
            if copy == 0:
                self._data_ = data
            else:
                self._data_ = numpy.array(data)
        elif isinstance(data, numpy.ma.MaskedArray):
            if numpy.ma.getmask(data) is not numpy.ma.nomask:
                raise CDMSError, \
                      'Cannot construct an axis with a missing value.'
            data = data.data
            if copy == 0:
                self._data_ = data
            else:
                self._data_ = numpy.array(data)
        elif data is None:
            self._data_ = None
        else:
            self._data_ = numpy.array(data)

        self._doubledata_ = None
        self.setBounds(bounds)

    def __getitem__(self, key):
        return self._data_[key]

    def __getslice__(self, low, high):
        return self._data_[low:high]

    def __setitem__(self, index, value):
        self._data_[index] = numpy.ma.filled(value)

    def __setslice__(self, low, high, value):
        self._data_[low:high] = numpy.ma.filled(value)

    def __len__(self):
        return len(self._data_)

    def getBounds(self):
        if self._bounds_ is not None:
            return copy.copy(self._bounds_)
        elif (getAutoBounds()==1 or (getAutoBounds()==2 and (self.isLatitude() or self.isLongitude()))):
            return self.genGenericBounds()
        else:
            return None

    def getData(self):
        return self._data_

    def getExplicitBounds(self):
        return copy.copy(self._bounds_)

    # Set bounds. The persistent argument is for compatibility with
    # persistent versions, is ignored. Same for boundsid and index.
    #
    # mf 20010308 - add validate key word, by default do not validate
    #
    def setBounds(self, bounds, persistent=0, validate=0, index=None, boundsid=None):
        if bounds is not None:
            if isinstance(bounds, numpy.ma.MaskedArray):
                bounds = numpy.ma.filled(bounds)
            if validate:
                bounds = self.validateBounds(bounds)
            else:                       # Just do the absolute minimum validation
                requiredShape = (len(self),2)
                requiredShape2 = (len(self)+1,)
                if bounds.shape!=requiredShape and bounds.shape!=requiredShape2:
                    raise CDMSError, InvalidBoundsArray + \
                          'shape is %s, should be %s or %s'%(`bounds.shape`,`requiredShape`,`requiredShape2`)
                if bounds.shape==requiredShape2: # case of "n+1" bounds
                    bounds2=numpy.zeros(requiredShape)
                    bounds2[:,0]=bounds[:-1]
                    bounds2[:,1]=bounds[1::]
                    bounds=bounds2
            self._bounds_ = copy.copy(bounds)
        else:
            if (getAutoBounds()==1 or (getAutoBounds()==2 and (self.isLatitude() or self.isLongitude()))):
                self._bounds_ = self.genGenericBounds()
            else:
                self._bounds_ = None

    def isLinear(self):
        return 0

    def typecode(self):
        return self._data_.dtype.char

class TransientVirtualAxis(TransientAxis):
    """An axis with no explicit representation of data values.
    It appears to be a float vector with values [0.0, 1.0, ..., float(axislen-1)]
    """

    def __init__(self, axisname, axislen):
        TransientAxis.__init__(self, None, id=axisname)
        self._virtualLength = axislen # length of the axis

    def __len__(self):
        return self._virtualLength

    def __str__ (self):
        return "<TransientVirtualAxis %s(%d)>"%(self.id, self._virtualLength)

    __repr__ = __str__

    def clone (self, copyData=1):
        """clone (self, copyData=1)
        Return a copy of self as a transient virtual axis.
        If copyData is 1, make a separate copy of the data."""
        return TransientVirtualAxis(self.id, len(self))

    def getData(self):
        return numpy.arange(float(self._virtualLength))

    def isCircular(self):
        return 0                        # Circularity doesn't apply to index space.

    def isVirtual(self):
        "Return true iff coordinate values are implicitly defined."
        return 1

    def setBounds(self, bounds):
        "No boundaries on virtual axes"
        self._bounds_ = None

    def __getitem__(self, key):
        return self.getData()[key]

    def __getslice__(self, low, high):
        return self.getData()[low:high]

## PropertiedClasses.initialize_property_class (TransientVirtualAxis)

# One-dimensional coordinate axis in a CdmsFile.
class FileAxis(AbstractAxis):
    
    def __init__(self, parent, axisname, obj=None):
        AbstractAxis.__init__ (self, parent, None)
        val = self.__cdms_internals__ +['name_in_file',]
        self.___cdms_internals__ = val
        self.id = axisname
        self._obj_ = obj
        # Overshadows file boundary data, if not None
        self._boundsArray_ = None
        (units,typecode,name_in_file,parent_varname,dimtype,ncid) = \
                   parent._file_.dimensioninfo[axisname]
        self.__dict__['_units'] = units
        att = self.attributes
        att['units']=units
        self.attributes = att
        self.name_in_file = self.id
        if name_in_file:
            self.name_in_file = name_in_file
        # Combine the attributes of the variable object, if any
        if obj is not None:
            for attname in self._obj_.__dict__.keys():
                attval = getattr(self._obj_,attname)
                if type(attval)!=types.BuiltinFunctionType:
                    self.__dict__[attname]  = attval
                    att = self.attributes
                    att[attname]=attval
                    self.attributes= att
        
    def getData(self):
        if cdmsobj._debug==1:
            print 'Getting array for axis',self.id
        if self.parent is None:
            raise CDMSError, FileWasClosed + self.id
        try:
            result = self.parent._file_.readDimension(self.id)
        except:
            try:
                result = apply(self._obj_.getitem, (slice(None,None),))
            except:
                raise CDMSError,'Data for dimension %s not found'%self.id
        return result

    def typecode(self):
        if self.parent is None:
            raise CDMSError, FileWasClosed + self.id
        (units,typecode,name_in_file,parent_varname,dimtype,ncid) = \
                             self.parent._file_.dimensioninfo[self.id]
        return typecode
    
    def _setunits(self, value):
        self._units = value
        self.attributes['units']=value
        if self.parent is None:
            raise CDMSError, FileWasClosed + self.id
        setattr(self._obj_, 'units', value)
        (units,typecode,name_in_file,parent_varname,dimtype,ncid) = \
            self.parent._file_.dimensioninfo[self.id]
        self.parent._file_.dimensioninfo[self.id] = \
                  (value,typecode,name_in_file,parent_varname,dimtype,ncid)
    def _getunits(self):
        return self._units

    def _delunits(self):
        del(self._units)
        del(self.attributes['units'])
        delattr(self._obj_,'units')


    def __getattr__(self,name):
        if name == 'units':
            return self._units
        try:
            return self.__dict__[name]
        except:
            raise AttributeError
    # setattr writes external attributes to the file
    def __setattr__ (self, name, value):
        if name == 'units':
            self._setunits(value)
            return
        if hasattr(self, 'parent') and self.parent is None:
            raise CDMSError, FileWasClosed + self.id
##         s = self.get_property_s (name)
##         if s is not None:
##             s(self, name, value)
##             return
        if not name in self.__cdms_internals__ and name[0]!='_':
            setattr(self._obj_, name, value)
            self.attributes[name]=value
        self.__dict__[name]  = value

    # delattr deletes external global attributes in the file
    def __delattr__(self, name):
##         d = self.get_property_d(name)
##         if d is not None:
##             d(self, name)
##             return
        if name == "units":
            self._delunits()
            return
        try:
            del self.__dict__[name]
        except KeyError:
            raise AttributeError, "%s instance has no attribute %s." % \
                  (self.__class__.__name__, name)
        if not name in self.__cdms_internals__(name):
            delattr(self._obj_, name)
            del(self.attributes[name])

    # Read data
    # If the axis has a related Cdunif variable object, just read that variable
    # otherwise, cache the Cdunif (read-only) data values in self._data_. in this case, 
    # the axis is not extensible, so it is not necessary to reread it each time.
    def __getitem__(self, key):
        if self.parent is None:
            raise CDMSError, FileWasClosed + self.id
        # See __getslice__ comment below.
        if (self._obj_ is not None) and (self.parent._mode_!='r') and not (hasattr(self.parent,'format') and self.parent.format=="DRS"):
            # For negative strides, get the equivalent slice with positive stride,
            # then reverse the result.
            if (type(key) is types.SliceType) and (key.step is not None) and key.step<0:
                posslice = reverseSlice(key,len(self))
                result = apply(self._obj_.getitem, (posslice,))
                return result[::-1]
            else:
                if isinstance(key, types.IntType) and key>=len(self):
                    raise IndexError, 'Index out of bounds: %d'%key
                if type(key) is not types.TupleType:
                    key = (key,)
                return apply(self._obj_.getitem, key)
        if self._data_ is None:
            self._data_ = self.getData()
        length = len(self._data_)
        if isinstance(key, types.IntType):  # x[i]
            if key>=length:
                raise IndexError, 'index out of bounds'
            else:
                return self._data_[key%length]
        elif type(key) is types.SliceType: # x[i:j:k]
            return self._data_[key.start:key.stop:key.step]
        elif type(key) is types.EllipsisType: # x[...]
            return self._data_
        elif type(key) is types.TupleType:
            raise IndexError,'axis is one-dimensional'
        else:
            raise IndexError,'index must be an integer or slice: %s'%`key`

    def __getslice__(self, low, high):
        # Hack to prevent netCDF overflow error on 64-bit architectures
        high = min(Max32int, high)
        
        # Hack to fix a DRS bug: force use of readDimension for DRS axes.
        # Since DRS is read-only here, it is OK just to cache all dimensions.
        if self.parent is None:
            raise CDMSError, FileWasClosed + self.id
        if (self._obj_ is not None) and (self.parent._mode_!='r') and not (hasattr(self.parent,'format') and self.parent.format=="DRS"):
            return apply(self._obj_.getslice,(low,high))
        else:
            if self._data_ is None:
                self._data_ = self.getData()
            return self._data_[low:high]

    def __setitem__(self, index, value):
        if self._obj_ is None:
            raise CDMSError, ReadOnlyAxis + self.id
        if self.parent is None:
            raise CDMSError, FileWasClosed+self.id
        return apply(self._obj_.setitem,(index,numpy.ma.filled(value)))

    def __setslice__(self, low, high, value):
        # Hack to prevent netCDF overflow error on 64-bit architectures
        high = min(Max32int, high)
        if self._obj_ is None:
            raise CDMSError, ReadOnlyAxis + self.id
        if self.parent is None:
            raise CDMSError, FileWasClosed+self.id
        return apply(self._obj_.setslice,(low,high,numpy.ma.filled(value)))

    def __len__(self):
        if self.parent is None:
            raise CDMSError, FileWasClosed + self.id
        if self._obj_ is not None:
            length = len(self._obj_)
        elif self._data_ is None:
            self._data_ = self.getData()
            length = len(self._data_)
        else:
            length = len(self._data_)
        return length

    def isLinear(self):
        return 0                        # All file axes are vector representation

    # Return the bounds array, or generate a default if autobounds mode is set
    def getBounds(self):
        boundsArray = self.getExplicitBounds()
        try:
            boundsArray = self.validateBounds(boundsArray)
        except Exception,err:
            boundsArray = None
        if boundsArray is None and (getAutoBounds()==1 or (getAutoBounds()==2 and (self.isLatitude() or self.isLongitude()))):
            boundsArray = self.genGenericBounds()
            
        return boundsArray

    # Return the bounds array, or None
    def getExplicitBounds(self):
        if self._boundsArray_ is None:
            boundsArray = None
            if hasattr(self,'bounds'):
                boundsName = self.bounds
                try:
                    boundsVar = self.parent[boundsName]
                    boundsArray = numpy.ma.filled(boundsVar)
                except KeyError,err:
                    print err
                    boundsArray = None
        else:
            boundsArray = self._boundsArray_

        return boundsArray

    # Create and write boundary data variable. An exception is raised
    # if the bounds are already set. bounds is a numpy array.
    # If persistent==1, write to file, else save in self._boundsArray_
    # For a persistent axis, index=n writes the bounds starting at that
    # index in the extended dimension (default is index=0).
    # If the bounds variable is new, use the name boundsid, or 'bounds_<varid>'
    # if unspecified.
    def setBounds(self, bounds, persistent=0, validate=0, index=None, boundsid=None):
        if persistent:
            if index is None:
                if validate:
                    bounds = self.validateBounds(bounds)
                index = 0

            # Create the bound axis, if necessary
            file = self.parent
            if file._boundAxis_ is None:

                # First look for 'bound' of length two
                if file.axes.has_key("bound") and len(file.axes["bound"])==2:
                    file._boundAxis_ = file.axes["bound"]
                else:
                    file._boundAxis_ = file.createVirtualAxis("bound",2)

            # Create the boundary variable if necessary
            if hasattr(self,'bounds'):
                boundName = self.bounds
                boundVar = file.variables[boundName]
            else:
                if boundsid is None:
                    boundName = "bounds_"+self.id
                else:
                    boundName = boundsid
                boundVar = file.createVariable(boundName, cdmsNode.NumericToCdType.get(bounds.dtype.char), (self,file._boundAxis_))
                # And link to self
                self.bounds = boundName
                self._boundsArray_ = None

            boundVar[index:index+len(bounds)] = bounds

        else:
            self._boundsArray_ = copy.copy(bounds)

    def getCalendar(self):
        if hasattr(self,'calendar'):
            calendar = string.lower(self.calendar)
        elif self.parent is not None and hasattr(self.parent, 'calendar'):
            calendar = string.lower(self.parent.calendar)
        else:
            calendar = None

        cdcal = tagToCalendar.get(calendar, cdtime.DefaultCalendar)
        return cdcal

    def isVirtual(self):
        "Return true iff coordinate values are implicitly defined."

        # No virtual axes in GrADS files
        if self.parent is not None and hasattr(self.parent, 'format') and self.parent.format=='GRADS':
            return 0
        return (self._obj_ is None)

    def isUnlimited(self):
        "Return true iff the axis is 'Unlimited' (extensible)"
        if self.parent is not None and self.parent._file_.dimensions.has_key(self.id):
            return (self.parent._file_.dimensions[self.id] is None)
        else:
            return False
## PropertiedClasses.set_property (FileAxis, 'units', 
##                                 acts=FileAxis._setunits,
##                                 nodelete=1
##                                )
## internattr.add_internal_attribute(FileAxis, 'name_in_file')

class FileVirtualAxis(FileAxis):
    """An axis with no explicit representation of data values in the file.
    It appears to be a float vector with values [0.0, 1.0, ..., float(axislen-1)]
    This is especially useful for the bound axis used with boundary variables.
    For a netCDF file the representation is a dimension with no associated
    coordinate variable.
    """

    def __init__(self, parent, axisname, axislen):
        FileAxis.__init__(self, parent, axisname)
        self._virtualLength = axislen # length of the axis
        
    def __len__(self):
        return self._virtualLength

    def getData(self):
        return numpy.arange(float(self._virtualLength))

    def isVirtual(self):
        "Return true iff coordinate values are implicitly defined."
        return 1

## PropertiedClasses.initialize_property_class (FileVirtualAxis)

######## Functions for selecting axes
def axisMatchAxis (axes, specifications=None, omit=None, order=None):
    """Given a list of axes and a specification or list of 
     specificatons, and a specification or list of specifications
     of those axes to omit, return a list of 
     those axes in the list that match the specification but 
     do not include in the list any axes that matches an omit 
     specification.

     If specifications is None, include all axes less the omitted ones.

     Individual specifications must be integer indices into axes or 
     matching criteria as detailed in axisMatches.

     Axes are returned in the order they occur in the axes argument unless
     order is given. 

     order can be a string containing the symbols t,x,y,z, or -. 
     If a - is given, any elements of the result not chosen otherwise are 
     filled in from left to right with remaining candidates.
    """
    return [axes[i] for i in \
            axisMatchIndex(axes, specifications, omit, order)]

def axisMatchIndex (axes, specifications=None, omit=None, order=None):
    """Given a list of axes and a specification or list of 
     specificatons, and a specification or list of specifications
     of those axes to omit, return a list of the indices of 
     those axes in the list that match the specification but 
     do not include in the list any axes that matches an omit 
     specification.

     If specifications is None, include all axes less the omitted ones.

     Individual specifications must be integer indices into axes or 
     matching criteria as detailed in axisMatches.

     The indices of axes are returned in the order the axes
     occur in the axes argument, unless order is given.

     order can be a string containing the symbols t,x,y,z, or -. 
     If a - is given, any elements of the result not chosen otherwise are 
     filled in from left to right with remaining candidates.
    """
    if specifications is None:
        speclist = axes
    elif isinstance(specifications, types.StringType):
        speclist = [specifications]
    elif isinstance(specifications, types.ListType):
        speclist = specifications
    elif isinstance(specifications, types.TupleType):
        speclist=list(specifications)
    elif isinstance(specifications, types.IntType):
        speclist = [specifications]
    elif isinstance(specifications, types.FunctionType):
        speclist = [specifications]
    else: # to allow arange, etc.
        speclist = list(numpy.ma.filled(specifications))

    candidates = []
    for i in range(len(axes)):
        for s in speclist:
            if isinstance(s, types.IntType):
                r = (s == i)
            else:
                r = axisMatches(axes[i], s)
            if r:
                candidates.append(i)
                break

    if not candidates:
        return candidates   #list empty

    if omit is None:
        omitlist = []
    elif isinstance(omit, types.StringType):
        omitlist = [omit]
    elif isinstance(omit, types.ListType):
        omitlist = omit
    elif isinstance(omit, types.TupleType):
        omitlist=list(omit)
    elif isinstance(omit, types.IntType):
        omitlist = [omit]
    elif isinstance(omit, types.FunctionType):
        omitlist = [omit]
    elif isinstance(omit, AbstractAxis):
        omitlist = [omit]
    else:
        raise CDMSError, 'Unknown type of omit specifier.'

    for s in omitlist:
        if isinstance(s, types.IntType):
            for i in range(len(candidates)):
                if axes[candidates[i]] is axes[s]:
                    del candidates[i]
                    break
        elif isinstance(s, AbstractAxis):
            for i in range(len(candidates)):
                if s is axes[candidates[i]]:
                    del candidates[i] 
                    break
        else:
            for i in range(len(candidates)):
                r = axisMatches(axes[candidates[i]], s)
                if r:
                    del candidates[i]
                    break

    if order is None:
        return candidates

    n = len(candidates)
    m = len(order)
    result = [None]*n
# this loop is done this way for future escapes where elements of order
# are not single chars.
    j = 0
    io = 0
    while j < n:
        if j >= m or order[io] == '-':
            result[j] = candidates[0]
            del candidates[0]
            j += 1
            io += 1
            continue
        elif order[j] == 't':
            oj = 'time'
            io += 1
        elif order[j] == 'x':
            oj = 'longitude'
            io += 1
        elif order[j] == 'y':
            oj = 'latitude'
            io += 1
        elif order[j] == 'z':
            oj = 'level'
            io += 1
        else:
            # later could put in escaped ids or indices
            raise CDMSError, 'Unknown character in order string'

        for i in range(n):
            if axisMatches(axes[candidates[i]], oj):
                result[j] = candidates[i]
                del candidates[i]
                break
        else:
            raise CDMSError, "Axis requested in order specification not there"
        j += 1    
    return result
            

def axisMatches(axis, specification):
    """Return 1 or 0 depending on whether axis matches the specification.
       Specification must be one of:
       1. a string representing an axis id or one of 
          the keywords time, fctau0, latitude or lat, longitude or lon, or 
          lev or level. 

          axis may be surrounded with parentheses or spaces.

          We first attempt to match the axis id and the specification.
          Keywords try to match using isTime, isLatitude, etc.
          Comparisons to keywords and axis ids is case-insensitive.

       2. a function that takes an axis as an argument and returns a value.
          if the value returned is true, the axis matches.

       3. an axis object; will match if it is the same object as axis.
    """   
    if isinstance(specification, types.StringType):
        s = string.lower(specification)
        s = s.strip()
        while s[0] == '(':
            if s[-1] != ')':
                raise CDMSError, 'Malformed axis spec, ' + specification
            s = s[1:-1].strip()
        if string.lower(axis.id) == s:
            return 1
        elif (s == 'time') or (s in time_aliases):
            return axis.isTime() 
        elif (s == 'fctau0') or (s in forecast_aliases):
            return axis.isForecast() 
        elif (s[0:3] == 'lat') or (s in latitude_aliases):
            return axis.isLatitude()
        elif (s[0:3] == 'lon') or (s in longitude_aliases):
            return axis.isLongitude()
        elif (s[0:3] == 'lev') or (s in level_aliases):
            return axis.isLevel()
        else:
            return 0

    elif isinstance(specification, types.FunctionType):
        r = specification(axis)
        if r: 
            return 1
        else: 
            return 0

    elif isinstance(specification, AbstractAxis):
        return (specification is axis)

    raise CDMSError, "Specification not acceptable: "\
          + str(type(specification)) + ', ' + str(specification)
    
def concatenate(axes, id=None, attributes=None):
    """Concatenate the axes, return a transient axis."""
    
    data = numpy.ma.concatenate([ax[:] for ax in axes])
    boundsArray = [ax.getBounds() for ax in axes]
    if None in boundsArray:
        bounds = None
    else:
        bounds = numpy.ma.concatenate(boundsArray)
    return TransientAxis(data, bounds=bounds, id=id, attributes=attributes)

def take(ax, indices):
    """Take values indicated by indices list, return a transient axis."""

    # Bug in ma compatibility module
    data = numpy.ma.take(ax[:], indices)
    abounds = ax.getBounds()
    if abounds is not None:
        bounds = numpy.ma.take(abounds, indices, axis=0)
    else:
        bounds = None
    return TransientAxis(data, bounds=bounds, id=ax.id, attributes=ax.attributes)
