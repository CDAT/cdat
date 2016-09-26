# Module:      Python LATS module
#
# Copyright:	1997, Regents of the University of California
#		This software may not be distributed to others without
#		permission of the author.
#
# Author:      Bob Drach, Lawrence Livermore National Laboratory
#              drach@llnl.gov
#
# Version:     $Id$
#
# Revision History:
#
# $Log: lats.py,v $
# Revision 1.4  1999/01/28 20:48:28  drach
# - Closing a LatsGDTFile now removes all references to grids and
#   vertical dimensions, so that they can be deleted.
#
# Revision 1.3  1998/10/28 21:27:20  drach
# - Added weights and bounds, hybrid dimensions
# - Fixed bug reading QC section of parameter table
#
# Revision 1.2  1998/09/30 22:56:55  drach
# - Added LatsGDTFile, LatsGDTFile.createTimeBounds, LatsGDTFile.setCurrentTimeBounds, LatsGrid.setWeights, LatsVertDim.setHybrid
#
# Revision 1.1.1.1  1998/09/01 22:17:59  drach
# Imported from ~williams/devel/cdat/python
#
# Revision 1.6  1998/07/14 00:14:43  drach
# - Added the GDTRel convention
#
# Revision 1.5  1998/07/02 18:38:51  drach
# - Added support for GDT, LATS V1.6, dimension boundary values
#
# Revision 1.4  1998/05/01 20:04:12  drach
# - Grid deletion works even if variables are not deleted
#
# Revision 1.3  1998/04/07 16:52:25  drach
# - Added synonyms
#
# Revision 1.2  1998/03/10 16:56:35  drach
# - Added missing error definitions
#
# Revision 1.1.1.1  1998/01/21 17:54:57  drach
# Initial repository version
#
#
#

import os, cdtime, Numeric, latsParmTab, string
import slats

# Default parameter table (a {ParmTab instace) is set:
#  explicitly by setParmDefault
#  *or*
#  by createVar (path = environ variable LATS_PARMS)
#  *or*
#  by LatsVertDim (path = environ variable LATS_PARMS)

DefaultParmTable = None

# LATS enumerated types

# Conventions
GDT = Pcmdi = slats.GDT
GDTRel = slats.GDTRel
GradsGrib = slats.GradsGrib
GribOnly = slats.GribOnly
Coards = slats.Coards

# Calendars
Standard = slats.Standard
Gregorian = slats.Gregorian
Julian = slats.Julian
Noleap = slats.Noleap
Calendar360 = slats.Calendar360
Clim = slats.Clim
ClimLeap = slats.ClimLeap
Clim360 = slats.Clim360

# Frequencies
Yearly = slats.Yearly
Monthly = slats.Monthly
MonthlyTableComp = slats.MonthlyTableComp
Weekly = slats.Weekly
Daily = slats.Daily
Hourly = slats.Hourly
ForecastHourly = slats.ForecastHourly
Fixed = slats.Fixed

# Datatypes
LatsFloat = slats.LatsFloat
LatsInt = slats.LatsInt

# Statistics
Average = slats.Average
Instant = slats.Instant
Accum = slats.Accum
OtherTimeStat = slats.OtherTimeStat

# Grid types
Gaussian = slats.Gaussian
Linear = slats.Linear
Generic = slats.Generic

# Verticality
SingleLevel = 1
MultiLevel = 2

# Direction ('Positive')
UpDirection = 1
DownDirection = 2

# LATS errors
LatsAlreadyClosedError = "File already closed"
LatsCloseError = "Error closing LATS file"
LatsDataShapeError = "Field must have shape"
LatsDefaultTableError = "Environment variable LATS_PARMS must be set to the parameter file path"
LatsGridDeleteError = "Error deleting grid"
LatsGridError = "Error creating grid"
LatsLevelNeededError = "Level value must be specified for variable"
LatsMissingError = "Error setting missing data value for variable"
LatsNotGDTConvention = "Not a GDT convention"
LatsNoTimeBounds = "Must create time bounds before setting"
LatsParmTabSetError = "Error setting parameter table to path"
LatsFileCreateError = "Error creating LATS file"
LatsTImeBoundsError = "Error in time bounds"
LatsVarError = "Error creating variable"
LatsVarMatchError = "Variable not found in parameter table"
LatsVertDeleteError = "Error deleting vertical dimension"
LatsVertDimError = "Error creating vertical dimension"
LatsVertMatchError = "Vertical dimension type not found in parameter table"
LatsWriteError = "Error writing variable"
LatsWriteTimeFirst = "Must write timepoint before setting time bounds"
LatsWrongDatatype = "Wrong data type for data array"
LatsWrongMissing = "Wrong datatype for missing data value"

# LATS variable
#
# Note: LATS variables are normally created via createVar
#
#   id       = LATS variable ID
#   name     = variable name
#   file     = LATS file object
#   stat     = statistic
#   datatype = variable datatype, LatsFloat or LatsInt
#   grid     = LatsGrid object
#   vertdim  = LatsVertDim object
#   missing  = missing data value (if present)
#   delta    = missing value fudge factor (if present, floats only)
#   comments = comments string
#   tabentry = LATS table entry

class LatsVar:

    def __init__(self, file, name, stat, datatype, grid, vertdim, missing, delta, comments, tabentry=None):
	if vertdim:
	    vertid = vertdim.id
	else:
	    vertid = 0
	id = slats.var(file.id, name, datatype, stat, grid.id, vertid, comments)
	if id==0:
	    raise LatsVarError, name
	self.id = id
	self.name = name
	self.file = file
	self.stat = stat
	self.datatype = datatype
	self.grid = grid
	self.vertdim = vertdim
	self.comments = comments
	self.tabentry = tabentry

	if missing:
	    self.set_missing(missing, delta)

    # Set the missing data value, either a float or an integer. 'delta' is the floating-point
    # fudge factor

    def set_missing(self, value, delta=None):
	if self.datatype==LatsFloat:
	    if delta:
		deltaVal = delta
	    else:
		deltaVal = 0.0
	    err = slats.missingFloat(self.file.id, self.id, value, deltaVal)
	    if err==0:
		raise LatsMissingError, self.name
	    self.missing = value
	    self.delta = deltaVal
	elif self.datatype==LatsInt:
	    if type(value)==type(0.0):
		raise LatsWrongMissing, value
	    err = slats.missingInt(self.file.id, self.id, value);
	    if err==0:
		raise LatsMissingError, self.name
	    self.missing = value

    def write(self, t, data, lev=None):

	# If the variable is multilevel, require the level value
	# Else if it is an explicit dimension, just use the single value
	# Else use level=0.0

	if self.vertdim and lev==None:
	    if len(self.vertdim.levs)>1:
		raise LatsLevelNeededError, self.name
	    else:
		lev = self.vertdim.levs[0]
	elif lev==None:
	    lev=0.0

	# Require floats to be Float32 or Float64, and integers to be Int or Int32
	if self.datatype==LatsFloat and data.typecode() not in Numeric.typecodes['Float']:
	    raise LatsWrongDatatype, "variable %s, at time %s, level %d"%(self.name,`t`,lev)
	
	if self.datatype==LatsInt and data.typecode() not in Numeric.typecodes['Integer']:
	    raise LatsWrongDatatype, "variable %s, at time %s, level %d"%(self.name,`t`,lev)

	if type(t)==type(cdtime.reltime(0,"years")):
	    c = t.tocomp()
	else:
	    c = t
	if data.shape!=self.grid.shape:
	    raise LatsDataShapeError, "%s, variable %s, at time %s, level %d\nInput field shape: %s"%(`self.grid.shape`,self.name,`t`,lev,`data.shape`)
	err = slats.write(self.file.id, self.id, lev, c.year, c.month, c.day, c.hour, data)
	if err==0:
	    raise LatsWriteError, "%s, at time %s, level %d"%(self.name,`t`,lev)

 
# LATS GDT variable
#
# Note: LATS variables are normally created via createVar
#
#   id       = LATS variable ID
#   name     = variable name
#   file     = LATS file object
#   stat     = statistic
#   datatype = variable datatype, LatsFloat or LatsInt
#   grid     = LatsGrid object
#   vertdim  = LatsVertDim object
#   missing  = missing data value (if present)
#   delta    = missing value fudge factor (if present, floats only)
#   comments = comments string
#   tabentry = LATS table entry

class LatsGDTVar(LatsVar):

    def __init__(self, file, name, stat, datatype, grid, vertdim, missing, delta, comments, tabentry=None):
        LatsVar.__init__(self, file, name, stat, datatype, grid, vertdim, missing, delta, comments, tabentry=None)

    def write(self, t, data, lev=None):

        LatsVar.write(self, t, data, lev)

        # Write grid weights, hybrid vertical dimension components
        # Get the time units
        if self.file.define==1:
            for (storegrid, ncid, weightid) in self.file.gridlist:
                err = slats.writeGridWeights(ncid,weightid,storegrid.latweights)
            for(storedVert,ncid,compid1,compid2,boundid1,boundid2,bounddim) in self.file.vertlist:
                comp1 = storedVert.component[1]
                comp2 = storedVert.component[3]
                bounds1 = storedVert.component[6]
                bounds2 = storedVert.component[8]
                if(bounds1 and bounds2):
                    err = slats.writeVertDimComp(ncid,compid1,compid2,comp1,comp2,boundid1,boundid2,bounds1,bounds2)
                else:
                    err = slats.writeVertDimComp(ncid,compid1,compid2,comp1,comp2)
            if self.file.timeBounds:
                ncid, boundid, timeid = self.file.timeBounds
                self.file.timeUnits = slats.getTimeUnits(ncid,timeid)
            self.file.define = 0

        # Set time index, current time for time boundary routines
        # Write alternate time value
        newtime = 0
        if not self.file.currentTime:
            self.file.currentTime = t
            self.file.timeIndex = 0
            newtime = 1
        elif self.file.currentTime.cmp(t) != 0:
            self.file.currentTime = t
            self.file.timeIndex = self.file.timeIndex+1
            newtime = 1
        if newtime and self.file.convention==GDTRel:
            talt = t.tocomp().absvalue
            ncid, alttimeid, timeid = self.file.altTime
            err = slats.writeAltTime(ncid,alttimeid,self.file.timeIndex,talt)
    
# LATS file
#
#   id       = LATS file ID, or -1 if the file has been closed
#   path     = file path
#   basetime = file base time
#   convention=file convention (e.g., Coards)
#   calendar = LATS calendar
#   frequency= time frequence
#   delta    = time delta
#   center   = modeling center
#   model    = name of the model which created the data
#   comments = string comments

class LatsFile:
    
    def __init__(self, path, convention, calendar, frequency, delta, basetime, center=Pcmdi, model="unknown", comments=" "):
	self.id = slats.create(path, convention, calendar, frequency, delta, center, model, comments)
	if self.id==0:
	    raise LatsFileCreateError, path
	self.path = path
	if type(basetime)==type(cdtime.reltime(0,"years")):
	    c = basetime.tocomp()
	else:
	    c = basetime
	slats.basetime(self.id, c.year, c.month, c.day, c.hour)
	self.basetime = c
	self.convention = convention
	self.calendar = calendar
	self.frequency = frequency
	self.delta = delta
	self.center = center
	self.model = model
	self.comments = comments
        self.varlist = []               # Only used to remove grid, vertdim references on close

    def __del__(self):
	if self.id!=-1:
	    self.close()

    # Create a variable. 'stat' is one of Average, Instant, Accum, or OtherTimeStat.
    # 'grid' is a LatsGrid, 'vertdim' is a LatsVertDim, 'missing' is the missing data
    # value, 'comments' is a string.

    def createVar(self, name, stat, grid, vertdim=None, missing=None, delta=None, comments=" "):
	global DefaultParmTable
	if DefaultParmTable==None:
	    setParmDefault()
	try:
	    varentry = DefaultParmTable.vars[string.lower(string.strip(name))]
	except KeyError:
	    raise LatsVarMatchError, name
	datatype = varentry[latsParmTab.vartabDatatype]
	var = LatsVar(self, name, stat, datatype, grid, vertdim, missing, delta, comments, varentry)
        self.varlist.append(var)

        return var

    def close(self):
        # Remove references to grids, vertical dimensions,
        # so that they can be deleted if desired.
        for var in self.varlist:
            var.grid = None
            var.vertdim = None
	if self.id==-1:
	    raise LatsAlreadyClosedError, self.path
	err = slats.close(self.id)
	if err==0:
	    raise LatsCloseError, self.path
	self.id = -1

    # Synonyms
    createvariable = createVar

# LATS GDT file
#
#   id       = LATS file ID, or -1 if the file has been closed
#   path     = file path
#   basetime = file base time
#   convention=file convention (e.g. GDT or GDTRel)
#   calendar = LATS calendar
#   frequency= time frequence
#   delta    = time delta
#   center   = modeling center
#   model    = name of the model which created the data
#   comments = string comments

class LatsGDTFile(LatsFile):

    def __init__(self, path, convention, calendar, frequency, delta, basetime, center=Pcmdi, model="unknown", comments=" "):
        if convention not in [GDT, GDTRel]:
            raise LatsNotGDTConvention
        LatsFile.__init__(self, path, convention, calendar, frequency, delta, basetime, center, model, comments)
        self.gridlist = []              # To determine (grid,ncid,weightid) for weights
        self.vertlist = []              # To determind (vert,ncid,compid1,compid2,boundid1,boundid2,bounddim) for component vertdims
        self.define = 1                 # 1 if in define mode, 0 if in data mode
        self.timeBounds = None          # (ncid,boundid) set by createTimeBounds
        self.timeIndex = 0              # Current time index
        self.currentTime = None         # Most recent time written
        self.timeUnits = None           # Time units
        self.altTime = None             # (ncid, alttimeid, timeid) for alternate time dimension
        if convention==GDTRel:
            self.altTime = slats.initGDT(self.id)

    def createVar(self, name, stat, grid, vertdim=None, missing=None, delta=None, comments=" "):
	global DefaultParmTable
	if DefaultParmTable==None:
	    setParmDefault()
	try:
	    varentry = DefaultParmTable.vars[string.lower(string.strip(name))]
	except KeyError:
	    raise LatsVarMatchError, name
	datatype = varentry[latsParmTab.vartabDatatype]
	var = LatsGDTVar(self, name, stat, datatype, grid, vertdim, missing, delta, comments, varentry)
        self.varlist.append(var)

        # If the grid was just defined in the file, define the grid weights
        # Also see lats_grid_nc in latsnc.c.
        if grid.latweights:
            foundGrid = 0
            for (storedGrid,ncid,weightid) in self.gridlist:
                if storedGrid.id == grid.id:
                    foundGrid = 1
            if not foundGrid:
                if len(self.gridlist)==0:
                    latname = "latitude"
                else:
                    latname = "latitude_"+grid.name
                weightsName = "weights_"+latname
                ncid,weightid = slats.defineGridWeights(self.id, latname, weightsName)
                self.gridlist.append((grid,ncid,weightid))
                
        # If the vertical dimension was just defined in the file, define the
        # hybrid components, if any
        if vertdim and vertdim.component:
            foundVert = 0
            for (storedVert,ncid,compid1,compid2,boundid1,boundid2,bounddim) in self.vertlist:
                if storedVert.id == vertdim.id:
                    foundVert = 1
            if not foundVert:
                compname1 = vertdim.component[0]
                compname2 = vertdim.component[2]
                p0 = vertdim.component[4]
                boundname1= vertdim.component[5]
                bounds1 = vertdim.component[6]
                boundname2= vertdim.component[7]
                bounds2 = vertdim.component[8]
                comment = vertdim.component[9]
                if not comment: comment=""
                if (bounds1 and bounds2):
                    ncid,compid1,compid2,boundid1,boundid2, boundvid = slats.defineVertDimComp(self.id, vertdim.name, compname1, compname2, compname1+" "+compname2,p0,comment, boundname1,boundname2)
                else:
                    ncid,compid1,compid2,boundid1,boundid2, boundvid = slats.defineVertDimComp(self.id, vertdim.name, compname1, compname2, compname1+" "+compname2,p0,comment)
                self.vertlist.append((vertdim,ncid,compid1,compid2,boundid1,boundid2,boundvid))

        return var

    def createTimeBounds(self):
        self.timeBounds = slats.createTimeBounds(self.id)

    def setTimeBounds(self, lower, upper):
        if not self.timeUnits:
            raise LatsWriteTimeFirst
        if not self.timeBounds:
            raise LatsNoTimeBounds
        if lower.cmp(self.currentTime)==1 or self.currentTime.cmp(upper)==1:
            raise LatsTImeBoundsError, "Current time %s is not in the range [%s,%s]"%(`self.currentTime`,`lower`,`upper`)
        if self.convention==GDTRel:
            lowval = lower.torel(self.timeUnits).value
            upval = upper.torel(self.timeUnits).value
        elif self.convention==GDT:
            lowval = lower.absvalue
            upval = upper.absvalue
        ncid, boundid, timeid = self.timeBounds
        err = slats.setTimeBounds(ncid, boundid, self.timeIndex, lowval, upval)

    def close(self):
        # Get rid of additional references to grids, vertical dimensions
        self.gridlist = []
        self.vertlist = []
        LatsFile.close(self)

    # Synonyms
    createvariable = createVar

# LATS grid
#
#   id       = LATS grid ID
#   name     = grid name
#   gridtype = grid type (e.g., Gaussian)
#   lons     = longitude array
#   lonbounds= longitude boundary array
#   lats     = latitude array
#   latbounds= latitude boundary array

class LatsGrid:

    def __init__(self, name, gridtype, lons, lats, lonbounds=None, latbounds=None):
        if (lonbounds is None) or (latbounds is None):
            id = slats.grid(name, gridtype, lons, lats)
        else:
            id = slats.grid(name, gridtype, lons, lats, lonbounds, latbounds)
	if id==0:
	    raise LatsGridError, name

	self.id = id
	self.name = name
	self.gridtype = gridtype
	self.lons = lons
	self.lats = lats
        self.latbounds = latbounds
        self.lonbounds = lonbounds
        self.latweights = None
 
    # Set the grid weights

    def setWeights(self, weights):
        if len(weights) != len(self.lats):
            raise LatsDataShapeError, "%d, weights for grid %s"%(len(self.lats),self.name)
        self.latweights = weights

    # Length of the grid

    def __len__(self):
	return len(self.lons)*len(self.lats)

    # Shape

    def __getattr__(self, name):
	if name == 'shape':
	    return (len(self.lats),len(self.lons))

    # Delete

    def __del__(self):
	if self.id==0: return
	err = slats.gridDelete(self.id)
	if err==0:
	    raise LatsGridDeleteError, self.name
	self.id=0

    # Print grid
    
    def __str__(self):
        return '<LatsGrid %s>'%self.name

    # def __repr__(self):
    #     return '<LatsGrid %s>'%self.name

# LATS vertical dimension
#
#   id       = LATS vertical dimension ID
#   name     = dimension name
#   verttype = dimension type (e.g., "sfc")
#   levs     = level values
#   bounds   = level boundaries, shape=(2,nlev)
#   vertentry= parameter table entry for vertical type

class LatsVertDim:

    def __init__(self, name, verttype, levs, bounds=None):
	global DefaultParmTable
	if DefaultParmTable==None:
	    setParmDefault()
	try:
	    vertentry = DefaultParmTable.verts[string.lower(string.strip(verttype))]
	except KeyError:
	    raise LatsVertMatchError, verttype
        if bounds is None :
            id = slats.vertdim(name, verttype, levs)
        else:
            id = slats.vertdim(name, verttype, levs, bounds)
        if id==0:
	    raise LatsVertDimError, name

	self.id = id
	self.name = name
	self.verttype = verttype
	self.levs = levs
        self.bounds = bounds
	self.vertentry = vertentry
        self.component = None           # Hybrid components

    # Define hybrid vertical dimension
    def setHybrid(self, compname1, component1, compname2, component2, p0, comment=None, bound1=None, bound2=None):
        if (len(component1) != len(self.levs)) or (len(component2) != len(self.levs)):
            raise LatsDataShapeError, "%d, component for vertdim %s"%(len(self.levs),self.name)
        self.component = (compname1, component1, compname2, component2, p0, "bounds_"+compname1, bound1, "bounds_"+compname2, bound2, comment)

    def __len__(self):
	return len(self.levs)

    def __del__(self):
	if self.id==0: return
	err = slats.vertdimDelete(self.id)
	if err==0:
	    raise LatsVertDeleteError, self.name
	self.id=0

# Read the parameter file 'path', and return a ParmTab class instance
# If path is unspecified, read the file specified by LATS_PARMS

def readParm(path=None):
    if path==None:
	if os.environ.has_key('LATS_PARMS'):
	    path = os.environ['LATS_PARMS']
	else:
	    raise LatsDefaultTableError
    parm = latsParmTab.ParmTab()
    parm.read(path)
    return parm

# Set the default parameter file to 'path', and return a ParmTab class instance.
# If path is unspecified, read and set the file specified by LATS_PARMS

def setParmDefault(path=None):
    global DefaultParmTable
    DefaultParmTable = readParm(path)
    path = DefaultParmTable.path
    err = slats.parmtab(path)
    if err==0:
	raise LatsParmTabSetError, path
    return DefaultParmTable

# Synonyms

LatsVerticalDimension = LatsVertDim
readparameter = readParm
setparameterdefault = setParmDefault
