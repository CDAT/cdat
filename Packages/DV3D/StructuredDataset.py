'''
Created on Dec 11, 2010

@author: tpmaxwel
'''   
import vtk, sys, os, copy, time, traceback, collections
from collections import OrderedDict 
import numpy.ma as ma
import numpy as np
EnableMemoryLogging = False
from DV3DPlot import  PlotType
# from vtk.util.misc import vtkGetDataRoot
# packagePath = os.path.dirname( __file__ ) 
import cdms2, cdtime, cdutil, MV2 
DataSetVersion = 0
DefaultDecimation = [ 0, 7 ]
cdms2.axis.level_aliases.append('isobaric')

def getItem( output, index = 0 ): 
    if not ( isinstance(output,list) or isinstance(output,tuple) ): return  output
    return output[ index ] 

def getVarNDim( vardata ):
    dims = [ 0, 0, 0 ]
    for dval in vardata.domain:
        axis = dval[0] 
        if axis.isLongitude(): 
            dims[0] = 1
        elif axis.isLatitude(): 
            dims[1] = 1
        elif axis.isLevel() or PlotType.isLevelAxis( axis.id ): 
            dims[2] = 1
    return dims[0] + dims[1] + dims[2]

class MemoryLogger:
    def __init__( self, enabled = True ):
        self.logfile = None
        self.enabled = enabled
        
    def close(self):
        if self.logfile <> None: 
            self.logfile.close( )
            self.logfile = None
        
    def log( self, label ):
        import shlex, subprocess, gc
        if self.enabled:
            gc.collect()
            args = ['ps', 'u', '-p', str(os.getpid())]
            psout = subprocess.check_output( args ).split('\n')
            ps_vals = psout[1].split()
            try:
                mem_usage_MB = float( ps_vals[5] ) / 1024.0
                mem_usage_GB = mem_usage_MB / 1024.0
            except ValueError, err:
                print>>sys.stderr, "Error parsing psout: ", str(err)
                print>>sys.stderr, str(psout)
                return
                    
            if self.logfile == None:
                self.logfile = open( "/tmp/dv3d-memory_usage.log", 'w' )
            self.logfile.write(" %10.2f (%6.3f): %s\n" % ( mem_usage_MB, mem_usage_GB, label ) )
            self.logfile.flush()
        
memoryLogger = MemoryLogger( EnableMemoryLogging )        

def getHomeRelativePath( fullpath ):
    if not fullpath or not os.path.isabs( fullpath ): return fullpath
    homepath = os.path.expanduser('~')
    commonpath = os.path.commonprefix( [ homepath, fullpath ] )
    if (len(commonpath) > 1) and os.path.exists( commonpath ): 
        relpath = os.path.relpath( fullpath, homepath )
        return '/'.join( [ '~', relpath ] )
    return fullpath

def getFullPath( relPath ):
    return os.path.expanduser( relPath )

def setDecimation( decimation_factor, isServer=False ):
    index = 1 if isServer else 0
    DefaultDecimation[ index ] = decimation_factor

def getDecimation( isServer=False ):
    index = 1 if isServer else 0
    return DefaultDecimation[ index ] 

def normalize_lon( lon ):
    return lon if lon <= 180.0 else (lon - 360.0)

def splitGridSpecs( gridSpecs ):
    inParen = False
    sliceStart = 0
    slices = []
    for i in range( len( gridSpecs ) ):
        sVal = gridSpecs[i]
        if sVal == '(': inParen = True
        elif sVal == ')': inParen = False
        elif sVal == ',' and not inParen: 
            slices.append( gridSpecs[sliceStart:i] )
            sliceStart = i+1
    slices.append( gridSpecs[sliceStart:] )
    return slices

def getCompTime( timeString ):
    print " >> GetCompTime: ", timeString
    timeStringFields = timeString.strip("'").split(' ')
    date = timeStringFields[0].split('-')
    if len( timeStringFields ) == 1:
        return cdtime.comptime( int(date[0]), int(date[1]), float(date[2]) )
    else:
        time = timeStringFields[1].split(':')
        for iT in range(3): time.append(0)
        return cdtime.comptime( int(date[0]), int(date[1]), int(date[2]), int(time[0]), int(time[1]), float(time[2]) )
                   
def deserializeFileMap( serialized_strMap ): 
    stringMap = {}
    if serialized_strMap:
        for dsrec in serialized_strMap.split(';'):
            dsitems = dsrec.split('#')
            if len( dsitems ) == 2: stringMap[ dsitems[0] ] = dsitems[1]
            elif len( dsitems ) == 1:
                fileId = os.path.splitext( os.path.basename( dsitems[0] ) )[0]
                stringMap[ fileId ] = dsitems[0]
    return stringMap

def getRelativeTimeValues( dataset ):
    rv = []
    dt = 0.0
    time_units = None
    if dataset <> None:
        dims = dataset.axes.keys()
        for dim in dims:
            axis = dataset.getAxis( dim )
            if axis.isTime():
                time_units = axis.units
                try:
                    if axis.calendar.lower() == 'gregorian': 
                        cdtime.DefaultCalendar = cdtime.GregorianCalendar 
                except: pass
                if hasattr( axis, 'partition' ):
                    for part in axis.partition:
                        for iTime in range( part[0], part[1] ):
                            rval = cdtime.reltime( axis[iTime], time_units )
                            rv.append( rval.torel(time_units) )
                    break
                else:
                    for tval in axis:
                        rval = cdtime.reltime( tval, time_units )
                        rv.append( rval.torel(time_units) )
        if (len(rv) > 1):
            dt = rv[1].value - rv[0].value
    return rv, dt, time_units

class CDMSDatasetRecord(): 
   
    def __init__( self, id, dataset=None, dataFile = None ):
        self.id = id
        self.lev = None
        self.dataset = dataset
        self.cdmsFile = dataFile
#        self.cachedFileVariables = {} 

    def getTimeValues( self, dsid ):
        return self.dataset['time'].getValue() 
    
    def getVariable(self, varName ):
        return self.dataset[ varName ] 
    
#    def clearDataCache( self ):
#         self.cachedFileVariables = {} 
    
    def getLevAxis(self ):
        for axis in self.dataset.axes.values():
            if axis.isLevel() or PlotType.isLevelAxis( axis ): return axis
        return None

    def getLevBounds( self, levaxis ):
        levbounds = None
        if levaxis:
            values = levaxis.getValue()
            ascending_values = ( values[-1] > values[0] )
            if levaxis:
                if   levaxis.attributes.get( 'positive', '' ) == 'down' and ascending_values:   levbounds = slice( None, None, -1 )
                elif levaxis.attributes.get( 'positive', '' ) == 'up' and not ascending_values: levbounds = slice( None, None, -1 )
        return levbounds
    
    def getVarDataTimeSlice( self, varName, timeValue, gridBounds, decimation, referenceVar=None, referenceLev=None ):
        """
        This method extracts a CDMS variable object (varName) and then cuts out a data slice with the correct axis ordering (returning a NumPy masked array).
        """        
#        cachedFileVariableRec = self.cachedFileVariables.get( varName )
#        if cachedFileVariableRec:
#            cachedTimeVal = cachedFileVariableRec[ 0 ]
#            if cachedTimeVal.value == timeValue.value:
#                return cachedFileVariableRec[ 1 ]
        
        rv = CDMSDataset.NullVariable
        varData = self.dataset[ varName ] 
#        print "Reading Variable %s, attributes: %s" % ( varName, str(varData.attributes) )

        refFile = self.cdmsFile
        refVar = varName
        refGrid = None
        if referenceVar:
            referenceData = referenceVar.split('*')
            refDsid = referenceData[0]
            refFileRelPath = referenceData[1]
            refVar  = referenceData[2]
            try:
                refFile = getFullPath( refFileRelPath )
                f=cdms2.open( refFile )
                refGrid=f[refVar].getGrid()
            except cdms2.error.CDMSError, err:
                print>>sys.stderr, " --- Error[1] opening dataset file %s: %s " % ( refFile, str( err ) )
        if not refGrid: refGrid = varData.getGrid()
        if not refGrid: 
            mb = QtGui.QMessageBox.warning( None, "DV3D Error", "CDAT is unable to create a grid for this dataset."  )
            return None
        refLat=refGrid.getLatitude()
        refLon=refGrid.getLongitude()
        nRefLat, nRefLon = len(refLat) - 1, len(refLon) - 1
        LatMin, LatMax =  float(refLat[0]), float(refLat[-1]) 
        LonMin, LonMax =  float(refLon[0]), float(refLon[-1]) 
        if LatMin > LatMax:
            tmpLatMin = LatMin
            LatMin = LatMax
            LatMax = tmpLatMin
        
        args1 = {} 
        gridMaker = None
        decimationFactor = 1
        if decimation: decimationFactor =  decimation[0]+1
#        try:
        args1['time'] = timeValue
        if gridBounds[0] < LonMin and gridBounds[0]+360.0<LonMax: gridBounds[0] = gridBounds[0] + 360.0
        if gridBounds[2] < LonMin and gridBounds[2]+360.0<LonMax: gridBounds[2] = gridBounds[2] + 360.0
        if gridBounds[0] > LonMax and gridBounds[0]-360.0>LonMin: gridBounds[0] = gridBounds[0] - 360.0
        if gridBounds[2] > LonMax and gridBounds[2]-360.0>LonMin: gridBounds[2] = gridBounds[2] - 360.0
        if decimationFactor == 1:
            args1['lon'] = ( gridBounds[0], gridBounds[2] )
            args1['lat'] = ( gridBounds[1], gridBounds[3] )
        else:
            varGrid = varData.getGrid() 
            if varGrid: 
                varLonInt = varGrid.getLongitude().mapIntervalExt( [ gridBounds[0], gridBounds[2] ] )
                latAxis = varGrid.getLatitude()
                latVals = latAxis.getValue()
                latBounds =  [ gridBounds[3], gridBounds[1] ] if latVals[0] > latVals[1] else  [ gridBounds[1], gridBounds[3] ]            
                varLatInt = latAxis.mapIntervalExt( latBounds )
                args1['lon'] = slice( varLonInt[0], varLonInt[1], decimationFactor )
                args1['lat'] = slice( varLatInt[0], varLatInt[1], decimationFactor )
                print " ---- Decimate(%d) grid %s: varLonInt=%s, varLatInt=%s, lonSlice=%s, latSlice=%s" % ( decimationFactor, str(gridBounds), str(varLonInt), str(varLatInt), str(args1['lon']), str(args1['lat']) )
#        args1['squeeze'] = 1
        start_t = time.time() 
            
#        if (gridMaker == None) or ( gridMaker.grid == varData.getGrid() ):
                   
        if ( (referenceVar==None) or ( ( referenceVar[0] == self.cdmsFile ) and ( referenceVar[1] == varName ) ) ) and ( decimationFactor == 1):
            levbounds = self.getLevBounds( referenceLev )
            if levbounds: args1['lev'] = levbounds
            args1['order'] = 'xyz'
            rv = varData( **args1 )
        else:
            refDelLat = ( LatMax - LatMin ) / nRefLat
            refDelLon = ( LonMax - LonMin ) / nRefLon
#            nodataMask = cdutil.WeightsMaker( source=self.cdmsFile, var=varName,  actions=[ MV2.not_equal ], values=[ nodata_value ] ) if nodata_value else None
            gridMaker = cdutil.WeightedGridMaker( flat=LatMin, flon=LonMin, nlat=int(nRefLat/decimationFactor), nlon=int(nRefLon/decimationFactor), dellat=(refDelLat*decimationFactor), dellon=(refDelLon*decimationFactor) ) # weightsMaker=nodataMask  )                    
            
            vc = cdutil.VariableConditioner( source=self.cdmsFile, var=varName,  cdmsKeywords=args1, weightedGridMaker=gridMaker ) 
            regridded_var_slice = vc.get( returnTuple=0 )
            if referenceLev: regridded_var_slice = regridded_var_slice.pressureRegrid( referenceLev )
            args2 = { 'order':'xyz', 'squeeze':1 }
            levbounds = self.getLevBounds( referenceLev )
            if levbounds: args2['lev'] = levbounds
            rv = regridded_var_slice( **args2 ) 
            try: rv = MV2.masked_equal( rv, rv.fill_value ) 
            except: pass
#            max_values = [ regridded_var_slice.max(), rv.max()  ]
#            print " Regrid variable %s: max values = %s " % ( varName, str(max_values) )
            
            end_t = time.time() 
#            self.cachedFileVariables[ varName ] = ( timeValue, rv )
#            print  "Reading variable %s, shape = %s, base shape = %s, time = %s (%s), args = %s, slice duration = %.4f sec." % ( varName, str(rv.shape), str(varData.shape), str(timeValue), str(timeValue.tocomp()), str(args1), end_t-start_t  ) 
#        except Exception, err:
#            print>>sys.stderr, ' Exception getting var slice: %s ' % str( err )
        return rv

    def getFileVarDataCube( self, varName, decimation, **args ):
        """
        This method extracts a CDMS variable object (varName) and then cuts out a data slice with the correct axis ordering (returning a NumPy masked array).
        """ 
        lonBounds = args.get( 'lon', None )
        latBounds = args.get( 'lat', None )
        timeBounds = args.get( 'time', None )
        [ timeValue, timeIndex, useTimeIndex ] = timeBounds if timeBounds else [ None, None, None ]
        referenceVar = args.get( 'refVar', None )
        referenceLev = args.get( 'refLev', None )
        
        rv = CDMSDataset.NullVariable
        varData = self.dataset[ varName ] 
        currentLevel = varData.getLevel()

        refFile = self.cdmsFile
        refVar = varName
        refGrid = None
        if referenceVar:
            referenceData = referenceVar.split('*')
            refDsid = referenceData[0]
            relFilePath = referenceData[1]
            refVar  = referenceData[2]
            try:
                cdmsFile = getFullPath( relFilePath )
                f=cdms2.open( cdmsFile )
                refGrid=f[refVar].getGrid()
            except cdms2.error.CDMSError, err:
                print>>sys.stderr, " --- Error[2] opening dataset file %s: %s " % ( cdmsFile, str( err ) )
        if not refGrid: refGrid = varData.getGrid()
        if not refGrid: 
            mb = QtGui.QMessageBox.warning( None, "DV3D Error", "CDAT is unable to create a grid for this dataset."  )
            return None
        refLat=refGrid.getLatitude()
        refLon=refGrid.getLongitude()
        nRefLat, nRefLon = len(refLat) - 1, len(refLon) - 1
        LatMin, LatMax =  float(refLat[0]), float(refLat[-1]) 
        LonMin, LonMax =  float(refLon[0]), float(refLon[-1]) 
        if LatMin > LatMax:
            tmpLatMin = LatMin
            LatMin = LatMax
            LatMax = tmpLatMin
        
        args1 = {} 
        gridMaker = None
        decimationFactor = 1
        order = 'xyt' if ( timeBounds == None) else 'xyz'
        if decimation: decimationFactor =  decimation[0]+1
        try:
            nts = self.dataset['time'].shape[0]
            if ( timeIndex <> None ) and  useTimeIndex: 
                args1['time'] = slice( timeIndex, timeIndex+1, 1 )
            elif timeValue and (nts>1): 
                args1['time'] = timeValue
        except: pass
        
        if lonBounds <> None:
            if (lonBounds[1] - lonBounds[0]) < 355.0:
                if lonBounds[0] < LonMin: lonBounds[0] = LonMin
                if lonBounds[1] > LonMax: lonBounds[1] = LonMax
#            if lonBounds[0] < LonMin and lonBounds[0]+360.0 < LonMax: lonBounds[0] = lonBounds[0] + 360.0
#            if lonBounds[0] > LonMax and lonBounds[0]-360.0 > LonMin: lonBounds[0] = lonBounds[0] - 360.0
#            if len( lonBounds ) > 1:
#                if lonBounds[1] < LonMin and lonBounds[1]+360.0<LonMax: lonBounds[1] = lonBounds[1] + 360.0
#                if lonBounds[1] > LonMax and lonBounds[1]-360.0>LonMin: lonBounds[1] = lonBounds[1] - 360.0                       
            if (decimationFactor == 1) or len( lonBounds ) == 1:
                args1['lon'] = lonBounds[0] if ( len( lonBounds ) == 1 ) else lonBounds
            else:
                varGrid = varData.getGrid() 
                varLonInt = varGrid.getLongitude().mapIntervalExt( [ lonBounds[0], lonBounds[1] ] )
                args1['lon'] = slice( varLonInt[0], varLonInt[1], decimationFactor )
               
        if latBounds <> None:
            if decimationFactor == 1:
                args1['lat'] = latBounds[0] if ( len( latBounds ) == 1 ) else latBounds
            else:
                latAxis = varGrid.getLatitude()
                latVals = latAxis.getValue()
                latBounds =  [ latBounds[1], latBounds[0] ] if latVals[0] > latVals[1] else  [ latBounds[0], latBounds[1] ]            
                varLatInt = latAxis.mapIntervalExt( latBounds )
                args1['lat'] = slice( varLatInt[0], varLatInt[1], decimationFactor )
                
        start_t = time.time() 
        
        levBounds = args.get( 'lev', None )
        if ( (referenceVar==None) or ( ( referenceVar[0] == self.cdmsFile ) and ( referenceVar[1] == varName ) ) ) and ( decimationFactor == 1):
            if levBounds <> None:
                args1['lev'] =  levBounds[0] if ( len( levBounds ) == 1 ) else levBounds                        
            else:
                levBounds = self.getLevBounds( referenceLev )
                if levBounds: args1['lev'] = levBounds
            args1['order'] = order
            rv = varData( **args1 )
        else:
            refDelLat = ( LatMax - LatMin ) / nRefLat
            refDelLon = ( LonMax - LonMin ) / nRefLon
#            nodataMask = cdutil.WeightsMaker( source=self.cdmsFile, var=varName,  actions=[ MV2.not_equal ], values=[ nodata_value ] ) if nodata_value else None
            gridMaker = cdutil.WeightedGridMaker( flat=LatMin, flon=LonMin, nlat=int(nRefLat/decimationFactor), nlon=int(nRefLon/decimationFactor), dellat=(refDelLat*decimationFactor), dellon=(refDelLon*decimationFactor) ) # weightsMaker=nodataMask  )                    
                
#            from packages.vtDV3D.CDMS_DatasetReaders import getRelativeTimeValues 
#            time_values, dt, time_units = getRelativeTimeValues ( cdms2.open( self.cdmsFile ) ) 
            
            vc = cdutil.VariableConditioner( source=self.cdmsFile, var=varName,  cdmsKeywords=args1, weightedGridMaker=gridMaker ) 
            print " regridded_var_slice(%s:%s): %s " % ( self.dataset.id, varName, str( args1 ) )
            regridded_var_slice = vc.get( returnTuple=0 )
#            if (referenceLev <> None) and ( referenceLev.shape[0] <> currentLevel.shape[0] ): 
#                regridded_var_slice = regridded_var_slice.pressureRegrid( referenceLev ) 
            
            args2 = { 'order' : order, 'squeeze' : 1 }
            if levBounds <> None:
                args2['lev'] = levBounds[0] if ( len( levBounds ) == 1 ) else levBounds                            
            else:
                levBounds = self.getLevBounds( currentLevel )
                if levBounds: args2['lev'] = levBounds
            rv = regridded_var_slice( **args2 ) 
            try: rv = MV2.masked_equal( rv, rv.fill_value )
            except: pass
#            max_values = [ regridded_var_slice.max(), rv.max()  ]
#            print " Regrid variable %s: max values = %s " % ( varName, str(max_values) )
            
            end_t = time.time() 
#            self.cachedFileVariables[ varName ] = ( timeValue, rv )
            print  "Reading variable %s, shape = %s, base shape = %s, args = %s" % ( varName, str(rv.shape), str(varData.shape), str(args1) ) 
#        except Exception, err:
#            print>>sys.stderr, ' Exception getting var slice: %s ' % str( err )
        return rv


#    def init( self, timeRange, roi, zscale ):
#        self.timeRange = timeRange
#        self.roi = roi
#        self.zscale = zscale

#    def getGridSpecs( self, var, roi, zscale, outputType ):   
#        dims = self.dataset.axes.keys()
#        gridOrigin = newList( 3, 0.0 )
#        outputOrigin = newList( 3, 0.0 )
#        gridBounds = newList( 6, 0.0 )
#        gridSpacing = newList( 3, 1.0 )
#        gridExtent = newList( 6, 0 )
#        outputExtent = newList( 6, 0 )
#        gridShape = newList( 3, 0 )
#        gridSize = 1
#        domain = var.getDomain()
#        if not self.lev: self.lev = var.getLevel()
#        axis_list = var.getAxisList()
#        for axis in axis_list:
#            size = len( axis )
#            iCoord = self.getCoordType( axis, outputType )
#            roiBounds, values = self.getAxisValues( axis, roi )
#            if iCoord >= 0:
#                iCoord2 = 2*iCoord
#                gridShape[ iCoord ] = size
#                gridSize = gridSize * size
#                outputExtent[ iCoord2+1 ] = gridExtent[ iCoord2+1 ] = size-1                    
#                if iCoord < 2:
#                    lonOffset = 0.0 #360.0 if ( ( iCoord == 0 ) and ( roiBounds[0] < -180.0 ) ) else 0.0
#                    outputOrigin[ iCoord ] = gridOrigin[ iCoord ] = values[0] + lonOffset
#                    spacing = (values[size-1] - values[0])/(size-1)
#                    if roiBounds:
#                        if ( roiBounds[1] < 0.0 ) and  ( roiBounds[0] >= 0.0 ): roiBounds[1] = roiBounds[1] + 360.0
#                        gridExtent[ iCoord2 ] = int( round( ( roiBounds[0] - values[0] )  / spacing ) )                
#                        gridExtent[ iCoord2+1 ] = int( round( ( roiBounds[1] - values[0] )  / spacing ) )
#                        outputExtent[ iCoord2+1 ] = gridExtent[ iCoord2+1 ] - gridExtent[ iCoord2 ]
#                        outputOrigin[ iCoord ] = lonOffset + roiBounds[0]
#                    roisize = gridExtent[ iCoord2+1 ] - gridExtent[ iCoord2 ] + 1                  
#                    gridSpacing[ iCoord ] = spacing
#                    gridBounds[ iCoord2 ] = roiBounds[0] if roiBounds else values[0] 
#                    gridBounds[ iCoord2+1 ] = (roiBounds[0] + roisize*spacing) if roiBounds else values[ size-1 ]
#                else:                                             
#                    gridSpacing[ iCoord ] = zscale
#                    gridBounds[ iCoord2 ] = values[0]  # 0.0
#                    gridBounds[ iCoord2+1 ] = values[ size-1 ] # float( size-1 )
#        if gridBounds[ 2 ] > gridBounds[ 3 ]:
#            tmp = gridBounds[ 2 ]
#            gridBounds[ 2 ] = gridBounds[ 3 ]
#            gridBounds[ 3 ] = tmp
#        gridSpecs = {}
#        md = { 'datasetId' : self.id,  'bounds':gridBounds, 'lat':self.lat, 'lon':self.lon, 'lev':self.lev, 'attributes':self.dataset.attributes }
#        gridSpecs['gridOrigin'] = gridOrigin
#        gridSpecs['outputOrigin'] = outputOrigin
#        gridSpecs['gridBounds'] = gridBounds
#        gridSpecs['gridSpacing'] = gridSpacing
#        gridSpecs['gridExtent'] = gridExtent
#        gridSpecs['outputExtent'] = outputExtent
#        gridSpecs['gridShape'] = gridShape
#        gridSpecs['gridSize'] = gridSize
#        gridSpecs['md'] = md
#        return gridSpecs

class CDMSDataset: 
    
    NullVariable = cdms2.createVariable( np.array([]), id='NULL' )

    def __init__( self ):
        self.datasetRecs = collections.OrderedDict()
        self.variableRecs = collections.OrderedDict()
        self.transientVariables = collections.OrderedDict()
#        self.cachedTransVariables = {}
        self.outputVariables = collections.OrderedDict()
        self.referenceVariable = None
        self.timeRange = None
        self.referenceTimeUnits = None
        self.gridBounds = None
        self.decimation = DefaultDecimation
        self.zscale = 1.0
        self.cells = []
        self.latLonGrid = True
        
    def setCells( self, cells ):
        self.cells[:] = cells[:]
      
    def setVariableRecord( self, id, varName ):
        self.variableRecs[id] = varName

    def getVariableRecord( self, id ):
        return self.variableRecs[id] 

    def getVarRecValues( self ):
        return self.variableRecs.values() 

    def getVarRecKeys( self ):
        return self.variableRecs.keys() 

    def setRoi( self, roi ): 
        if roi <> None: 
            self.gridBounds = list(roi)

    def setBounds( self, timeRange, time_units, roi, zscale, decimation ): 
        self.timeRange = timeRange
        self.referenceTimeUnits = time_units
        self.setRoi( roi )
        self.zscale = zscale
        self.decimation = decimation
        
    def getTimeValues( self, asComp = True ):
        if self.timeRange == None: return None
        time_values = []
        try:
            start_rel_time = cdtime.reltime( float( self.timeRange[2] ), self.referenceTimeUnits )
            for iTime in range( self.timeRange[0], self.timeRange[1]+1 ):
                rval = start_rel_time.value + iTime * self.timeRange[3]
                tval = cdtime.reltime( float( rval ), self.referenceTimeUnits )
                if asComp:   time_values.append( tval.tocomp() )
                else:        time_values.append( tval )
        except: pass
        return time_values
    
    def getGrid( self, gridData ):
        dsetRec = self.datasetRecs.get( gridData[0], None )
        if dsetRec:
            grids = dsetRec.dataset.grids
            return grids.get( gridData[1], None  )
        return None

    def setReferenceVariable( self, selected_grid_id ):
        try:
            if (selected_grid_id == None) or (selected_grid_id == 'None'): return
            grid_id = getItem( selected_grid_id )
            refVarData = grid_id.split('*')
            if len( refVarData ) > 1:
                dsid = refVarData[0]
                varName = refVarData[1].split('(')[0].strip()
                dsetRec = self.datasetRecs.get( dsid, None )
                if dsetRec:
                    variable = dsetRec.dataset.variables.get( varName, None )
                    if variable: 
                        self.referenceVariable = "*".join( [ dsid, dsetRec.cdmsFile, varName ] )
                        self.referenceLev = variable.getLevel()
        except Exception, err:
            print>>sys.stderr, " Error in setReferenceVariable: ", str(err)
            
    def getReferenceDsetId(self):
        if self.referenceVariable == None: return self.datasetRecs.keys()[0]
        return self.referenceVariable.split("*")[0]
                                                             
    def getStartTime(self):
        return cdtime.reltime( float( self.timeRange[2] ), self.referenceTimeUnits )

    def close( self ):
        for dsetRec in self.datasetRecs.values(): dsetRec.dataset.close()
         
    def addTransientVariable( self, varName, variable, ndim = None ):
        if varName in self.transientVariables:
            var = self.transientVariables[ varName ]
            if id(var) <> id(variable): print>>sys.stderr, "Warning, transient variable %s already exists in dataset, overwriting!" % ( varName )
            else: return
        self.transientVariables[ varName ] = variable

    def getTransientVariable( self, varName ):
        return self.transientVariables.get( varName, None )

    def getTransientVariableNames( self ):
        return self.transientVariables.keys()

    def addOutputVariable( self, varName, variable, ndim = None ):
        self.outputVariables[ varName ] = variable

    def getOutputVariable( self, varName ):
        return self.outputVariables.get( varName, None )

    def getOutputVariableNames( self ):
        return self.outputVariables.keys()

    def __getitem__(self, dsid ):
        return self.datasetRecs.get( dsid, None )

    def __delitem__(self, dsid ):
        dsetRec = self.datasetRecs[ dsid ]
        dsetRec.dataset.close()
        del self.datasetRecs[ dsid ]
    
#    def getVarData( self, dsid, varName ):
#        dsetRec = self.datasetRecs[ dsid ]
#        if varName in dsetRec.dataset.variables:
#            return dsetRec.getVarData( self, varName )
#        elif varName in self.transientVariables:
#            return self.transientVariables[ varName ]
#        else: 
#            print>>sys.stderr, "Error: can't find variable %s in dataset" % varName
#            return self.NullVariable

    def clearDataCache( self ):
        for dsetRec in self.datasetRecs.values(): dsetRec.clearDataCache()
        
#    def clearVariableCache( self, varName ):
#        cachedData = self.cachedTransVariables.get( varName, None )
#        if cachedData:
#            ( timeValue, tvar ) = cachedData
#            del self.cachedTransVariables[ varName]
#            del tvar

    def clearTransientVariable( self, varName ):
        try:
            tvar = self.transientVariables[ varName ]
            del self.transientVariables[ varName]
            del tvar
        except Exception, err:
            print>>sys.stderr, "Error releasing tvar: ", str(err)

    def getVarDataTimeSlice( self, dsid, varName, timeValue ):
        """
        This method extracts a CDMS variable object (varName) and then cuts out a data slice with the correct axis ordering (returning a NumPy masked array).
        """
        rv = CDMSDataset.NullVariable
        if dsid:
            dsetRec = self.datasetRecs[ dsid ]
            if varName in dsetRec.dataset.variables:
                rv = dsetRec.getVarDataTimeSlice( varName, timeValue, self.gridBounds, self.decimation, self.referenceVariable, self.referenceLev )   
        if (rv.id == "NULL") and (varName in self.transientVariables):
            rv = self.transientVariables[ varName ]
        if rv.id <> "NULL": 
            return rv 
#            current_grid = rv.getGrid()
#            if ( gridMaker == None ) or SameGrid( current_grid, gridMaker.grid ): return rv
#            else:       
#                vc = cdutil.VariableConditioner( source=rv, weightedGridMaker=gridMaker )
#                return vc.get( returnTuple=0 )
        print>>sys.stderr, "Error: can't find time slice variable %s in dataset" % varName
        return rv

    def getVarDataCube( self, dsid, varName, timeValues, levelValues = None, **kwargs ):
        """
        This method extracts a CDMS variable object (varName) and then cuts out a data slice with the correct axis ordering (returning a NumPy masked array).
        """
        memoryLogger.log("Begin getVarDataCube")
        rv = CDMSDataset.NullVariable
        if dsid:
            dsetRec = self.datasetRecs.get( dsid, None )
            if dsetRec:
                if varName in dsetRec.dataset.variables:
                    args = { 'time':timeValues, 'lev':levelValues, 'refVar':self.referenceVariable, 'refLev':self.referenceLev }
                    for item in kwargs.iteritems(): args[ item[0] ] = item[1]
                    if self.gridBounds:
                        args['lon'] = [self.gridBounds[0],self.gridBounds[2]] 
                        args['lat'] = [self.gridBounds[1],self.gridBounds[3]] 
                    rv = dsetRec.getFileVarDataCube( varName, self.decimation, **args )  
            elif varName in self.getTransientVariableNames():
                tvar = self.getTransientVariable( varName ) 
                args = { 'time':timeValues, 'lev':levelValues }
                for item in kwargs.iteritems(): args[ item[0] ] = item[1]
                if self.gridBounds:
                    args['lon'] = [self.gridBounds[0],self.gridBounds[2]] 
                    args['lat'] = [self.gridBounds[1],self.gridBounds[3]] 
                rv = self.getTransVarDataCube( varName, tvar, self.decimation, **args )  
        if (rv.id == "NULL") and (varName in self.outputVariables):
            rv = self.outputVariables[ varName ]
        if rv.id == "NULL": 
            print>>sys.stderr, "Error: can't find time slice data cube for variable %s in dataset" % varName
        memoryLogger.log("End getVarDataCube")
        return rv


    def getTransVarDataCube( self, varName, transVar, decimation, **args ):
        """
        This method returns a data slice with the correct axis ordering (returning a NumPy masked array).
        """ 
        memoryLogger.log("Begin getTransVarDataCube")
        invert_z = False
        invert_y = False
        levaxis = transVar.getLevel() 
        timeaxis = transVar.getTime() 
        level = args.get( 'lev', None )
        lonBounds = args.get( 'lon', None )
        latBounds = args.get( 'lat', None )
        cell_coords = args.get( 'cell', None )

        if levaxis:
            values = levaxis.getValue()
            ascending_values = ( values[-1] > values[0] )
            invert_z = ( (levaxis.attributes.get( 'positive', '' ) == 'down') and ascending_values ) or ( (levaxis.attributes.get( 'positive', '' ) == 'up') and not ascending_values )
               
        timeBounds = args.get( 'time', None )
        [ timeValue, timeIndex, useTimeIndex ] = timeBounds if timeBounds else [ None, None, None ]

#        cachedTransVariableRec = self.cachedTransVariables.get( varName )
#        if cachedTransVariableRec:
#            cachedTimeVal = cachedTransVariableRec[ 0 ]
#            if cachedTimeVal.value == timeValue.value:
#                print>>sys.stderr, "Returning cached trans var %s" % varName
#                return cachedTransVariableRec[ 1 ]
        
        rv = CDMSDataset.NullVariable 
        currentLevel = transVar.getLevel()
#        print "Reading Variable %s, attributes: %s" % ( varName, str(transVar.attributes) )

        decimationFactor = 1
        if decimation: decimationFactor = decimation[0]+1
        
        args1 = {} 
        order = 'xyt' if ( timeBounds == None) else 'xyz' if levaxis else 'xy'
        try:
            nts = self.timeRange[1]
            if ( timeIndex <> None ) and  useTimeIndex: 
                args1['time'] = slice( timeIndex, timeIndex+1 )
            elif timeValue and (nts>1): 
                args1['time'] = timeValue
        except: pass

        if (decimationFactor > 1) or lonBounds or latBounds:
            lonAxis = transVar.getLongitude() 
            lonVals = lonBounds if lonBounds else lonAxis.getValue()
            varLonInt = lonAxis.mapIntervalExt( [ lonVals[0], lonVals[-1] ], 'ccn' )
            if varLonInt:
                if (decimationFactor > 1):  args1['lon'] = slice( varLonInt[0], varLonInt[1], decimationFactor )
                else:                       args1['lon'] = slice( varLonInt[0], varLonInt[1] )
           
            latAxis = transVar.getLatitude() 
            latVals = latAxis.getValue()
            latRange = [ latVals[0], latVals[-1] ]
            if latBounds:
                if ( latVals[-1] > latVals[0] ):     
                    latRange = [ latBounds[0], latBounds[-1] ] if (latBounds[-1] > latBounds[0]) else [ latBounds[-1], latBounds[0] ]
                else:                                
                    latRange = [ latBounds[0], latBounds[-1] ] if (latBounds[-1] < latBounds[0]) else [ latBounds[-1], latBounds[0] ]
                    invert_y = True
            varLatInt = latAxis.mapIntervalExt( latRange, 'ccn' )
            if varLatInt:
                if invert_y:    args1['lat'] = slice( varLatInt[1], varLatInt[0], -decimationFactor )
                else:           args1['lat'] = slice( varLatInt[0], varLatInt[1], decimationFactor )   
                     
        args1['order'] = order
        if levaxis:
            if level: args1['lev'] = float( level )
            elif invert_z:  args1['lev'] = slice( None, None, -1 )
            
        memoryLogger.log("Begin subsetting")
        
        try:
            rv = transVar( **args1 )
        except Exception, err: 
            print>>sys.stderr, "Error Reading Variable", str(err) 
            return CDMSDataset.NullVariable
  
        memoryLogger.log("Create Mask")
      
        try: 
            rv = MV2.masked_equal( rv, rv.fill_value )
        except: 
            pass         
 #       self.cachedTransVariables[ varName ] = ( timeValue, rvm )
        print  "Reading variable %s, shape = %s, base shape = %s, args = %s" % ( varName, str(rv.shape), str(transVar.shape), str(args1) ) 
        return rv
    
    def ensure3D( self, cdms_variable ):
        lev = cdms_variable.getLevel()
        if lev == None:
            axis_list = cdms_variable.getAxisList()
            axis = cdms2.createAxis( [0.0] )
            axis.designateLevel()
            axis_list.append( axis )
            new_shape = list( cdms_variable.data.shape )
            new_shape.append(1)
            cdms_variable.data.reshape( new_shape )
            cdms_variable.setAxisList( axis_list )

    def getVarDataTimeSlices( self, varList, timeValue ):
        """
        This method extracts a CDMS variable object (varName) and then cuts out a data slice with the correct axis ordering (returning a NumPy masked array).
        """
        timeSlices, condTimeSlices = [], []
        vc0 = None
        for ( dsid, varName ) in varList:
            varTimeSlice = self.getVarDataTimeSlice( dsid, varName, timeValue )
            if not vc0: vc0 = cdutil.VariableConditioner( varTimeSlice )    
            else:       timeSlices.append( varTimeSlice )                
        for varTimeSlice in timeSlices:
            vc1 = cdutil.VariableConditioner( varTimeSlice ) 
            VM = cdutil.VariablesMatcher( vc0, vc1 )
            condTimeSlice0, condTimeSlice1 = VM.get( returnTuple=0 )
            if not condTimeSlices: condTimeSlices.append( condTimeSlice0 )
            condTimeSlices.append( condTimeSlice1 )
        return condTimeSlices
    
    def addDatasetRecord( self, dsetId, relFilePath ):
        cdmsDSet = self.datasetRecs.get( dsetId, None )
        if (cdmsDSet <> None) and (cdmsDSet.cdmsFile == relFilePath):
            return cdmsDSet
        try:
            relFilePath = relFilePath.strip()
            if relFilePath:
                cdmsFile = getFullPath( relFilePath )
                dataset = cdms2.open( cdmsFile ) 
                cdmsDSet = CDMSDatasetRecord( dsetId, dataset, cdmsFile )
                self.datasetRecs[ dsetId ] = cdmsDSet
        except Exception, err:
            print>>sys.stderr, " --- Error[3] opening dataset file %s: %s " % ( cdmsFile, str( err ) )
        return cdmsDSet             

    def getVariableList( self, ndims ):
        vars = []     
        for dsetRec in self.datasetRecs.values(): 
            for var in dsetRec.dataset.variables:               
                vardata = dsetRec.dataset[var]
                var_ndim = getVarNDim( vardata )
                if var_ndim == ndims: vars.append( '%s*%s' % ( dsetRec.id, var ) )
        return vars
    
    def getDsetId(self): 
        rv = '-'.join( self.datasetRecs.keys() )
        return rv

class SerializedInterfaceSpecs:
        
    def __init__( self, serializedConfiguration = None ):
        self.inputs = {}
        self.cells = []
        self.configParms = None
        if serializedConfiguration and (serializedConfiguration <> 'None'):
            self.parseInputSpecs( serializedConfiguration )
            
    def parseInputSpecs( self, serializedConfiguration ):
        inputSpecElements = serializedConfiguration.split(';')
        fileInputSpecs = inputSpecElements[0].split('|')
        varInputSpecs = inputSpecElements[1].split('|')
        gridInputSpecs = inputSpecElements[2].split('|')
        cellInputSpecs = inputSpecElements[3].split('|')
        if fileInputSpecs[0]:
            if len( fileInputSpecs ) == 1:
                fileMetadata = fileInputSpecs[0].split('!')
                fileId = fileMetadata[1]
                fileName = fileMetadata[2] if fileMetadata[2] else fileId
                for iVar in range( len(varInputSpecs) ):
                    varSpecs = varInputSpecs[iVar].split('!')
                    if len( varSpecs ) > 2:
                        varName = varSpecs[2] if varSpecs[2] else varSpecs[1]
                        axes = gridInputSpecs[iVar].split('!')[1]
                        self.addInput( ("Input%d" % iVar), fileId, fileName, varName, axes )
            elif len( fileInputSpecs ) == len( varInputSpecs ):
                for iVar in range( len(varInputSpecs) ):
                    fileMetadata = fileInputSpecs[iVar].split('!')
                    fileId = fileMetadata[1]
                    fileName = fileMetadata[2] if fileMetadata[2] else fileId
                    varSpecs = varInputSpecs[iVar].split('!')
                    if len( varSpecs ) > 2:
                        varName = varSpecs[2] if varSpecs[2] else varSpecs[1]
                        axes = gridInputSpecs[iVar].split('!')[1]
                        self.addInput( ("Input%d" % iVar), fileId, fileName, varName, axes )
            else:
                print>>sys.stderr, " ERROR: Number of Files and number of Variables do not match."
        for iCell in range( len(cellInputSpecs) ):
            cellMetadata = cellInputSpecs[iCell].split('!')
            if len( cellMetadata ) > 1: self.cells.append( ( cellMetadata[0], cellMetadata[1] ) )
                
        
    def addInput(self, inputName, fileId, fileName, variableName, axes ):
        print " --- AddInput: ", inputName, fileId, fileName, variableName, axes
        relFilePath = getHomeRelativePath( fileName )
        self.inputs[ inputName ] = ( fileId, relFilePath, variableName, axes )
        
    def getNInputs(self):
        return len(self.inputs)
            
    def getInput(self, **args ):
        inputName = args.get( 'name', None )
        if not inputName: 
            inputIndex = args.get( 'index', 0 )
            keys = self.inputs.keys()
            if len(keys) > inputIndex:
                keys.sort()
                inputName = keys[inputIndex]        
        return self.inputs.get( inputName )
                            
class StructuredFileReader:

    def __init__(self, mid, **args):
        self.datasetModule = CDMSDataset()
        
    def clearDataCache(self):
        pass
        
    def computeGridFromSpecs(self):
        start_time, end_time, min_dt  = -float('inf'), float('inf'), float('inf')
        self.roi = [ 0.0, -90.0, 360.0, 90.0 ]
        for gridSpec in self.gridSpecs:
            print " -- GridSpec: ", gridSpec
            gridFields = gridSpec.split('=')
            if len( gridFields ) == 2:
                type = gridFields[0].strip()
                values = gridFields[1].strip('() ').split(',')
                if len(values) == 1: values = values[0].strip(' ').split(' ')
                if type == 'time':
                    cval = getCompTime( values[0].strip(" ") )
                    start_time = cval.torel(self.referenceTimeUnits).value
                    cval = getCompTime( values[1].strip(" ") )
                    end_time = cval.torel(self.referenceTimeUnits).value
#                    print " TimeRange Specs: ", str( values ), str( start_time ), str( end_time ), str( self.referenceTimeUnits )
                elif type.startswith('lat' ):
                    lat_bounds = [ float( values[0] ), float( values[1] ) ]
                    self.roi[1] = lat_bounds[0] if lat_bounds[0] < lat_bounds[1] else lat_bounds[1]
                    self.roi[3] = lat_bounds[1] if lat_bounds[0] < lat_bounds[1] else lat_bounds[0] 
                elif type.startswith('lon' ):
                    self.roi[0] = float( values[0] ) 
                    self.roi[2] = float( values[1] )                   
        dataset_list = []
        for relFilePath in self.fileSpecs:
            try:
                cdmsFile = getFullPath( relFilePath )
                dataset = cdms2.open( cdmsFile ) 
                dataset_list.append( dataset )
            except:
                print "Error opening dataset: %s" % str(cdmsFile)
        for dataset in dataset_list:
            time_values, dt, time_units = getRelativeTimeValues ( dataset )
            if time_values:
                nTS = len( time_values )
                tbounds = [ time_values[0].value, time_values[nTS-1].value ]
                if  tbounds[0] > start_time: start_time  = tbounds[0]
                if dt == 0.0: end_time = start_time
                elif tbounds[1] < end_time:   end_time = tbounds[1]
                if dt < min_dt: min_dt = dt               
        for dataset in dataset_list: dataset.close()
        if min_dt == float('inf'): 
            nTS = 1
            start_time = 0 
            end_time = 0
            min_dt = 0.0
        elif min_dt == 0.0:
           nTS = 1 
        else: nTS = int( ( ( end_time - start_time ) / min_dt ) + 0.0001 )  
        self.timeRange = [ 0, nTS, start_time, min_dt ]
        print "Compute TimeRange From Specs: ", str( [ start_time, end_time, min_dt ] ), str( self.timeRange )

            
    def execute(self, **args ):
        """ compute() -> None
        Dispatch the vtkRenderer to the actual rendering widget
        """  
        self.fileSpecs, self.varSpecs, self.gridSpecs, self.datasetMap, self.ref_var = None, None, None, None, None         
        decimation = self.getInputValue( "decimation" )
        zscale = getItem( self.getInputValue( "zscale",   1.0  )  )
        
        serializedInputSpecs = getItem( self.getInputValue( "executionSpecs" ) )
        if serializedInputSpecs:
            inputSpecs = SerializedInterfaceSpecs( serializedInputSpecs ) if serializedInputSpecs else None
#            print " ** serializedInputSpecs: ", str( serializedInputSpecs ) 
#            print " ** InputSpecs: ", str( inputSpecs ) 
            self.idSpecs, self.fileSpecs, self.varSpecs, self.gridSpecs = [], [], [], []
            nInputs = inputSpecs.getNInputs() if inputSpecs else 0
            if nInputs: 
#                print " _____________________ File Reader _____________________ "    
                for iInput in range( nInputs  ):
                    inputSpec = inputSpecs.getInput(  index=iInput )
#                    print " ** InputSpec: ", str( inputSpec ) 
                    self.idSpecs.append( inputSpec[0] ) 
                    self.fileSpecs.append( inputSpec[1] )
                    self.varSpecs.append( inputSpec[2] )
                    if( not len(self.gridSpecs) and len(inputSpec[3]) ): 
                        self.gridSpecs = splitGridSpecs( inputSpec[3] )                   
#                        print " ** Grid Specs: ", str( self.gridSpecs )  
                dsMapData = ';'.join( self.fileSpecs )   
                self.computeGridFromSpecs()
#                print " ** ID Specs: ", str( self.idSpecs )
#                print " ** File Specs: ", str( self.fileSpecs )
#                print " ** Var Specs: ", str( self.varSpecs )            
#                print " ** dsMapData: ", str( dsMapData )
#                print " ** ROI: ", str( self.roi )
#                print " ** zscale: ", str( zscale )
#                print " ** decimation: ", str( decimation )
#                print " ________________________________________________________ "   
                self.datasetMap = deserializeFileMap( getItem( dsMapData ) )
                dsKeys = self.datasetMap.keys()
                for iVar in range( len(self.varSpecs) ):
                    iDset = 0 if ( len( dsKeys ) == 1 ) else iVar
                    varSpec = "%s*%s" % ( dsKeys[ iDset ], self.varSpecs[iVar] )
                    if iVar == 0: self.ref_var = varSpec
                    self.datasetModule.setVariableRecord( "VariableName%d" % iVar, varSpec )
            else:    
                time_range = self.getInputValue( "timeRange"  )
                self.timeRange =[ int(time_range[0]), int(time_range[1]), float(time_range[2]), float(time_range[3])  ] if time_range else None
                roi_data = self.getInputValue( "roi" )
                self.roi = [ float(sroi) for sroi in roi_data ] if roi_data else None
                dsMapData = self.getInputValue( "datasets" ) 
                self.datasetMap = deserializeFileMap( getItem( dsMapData ) )
                self.ref_var = self.getInputValue( "grid"  )
            
            self.datasetModule.setBounds( self.timeRange, self.referenceTimeUnits, self.roi, zscale, decimation ) 
            self.datasetModule.setCells( inputSpecs.cells )
      
            if self.datasetMap:             
                for datasetId in self.datasetMap:
                    relFilePath = self.datasetMap[ datasetId ]
                    if relFilePath.strip():
                        self.datasetModule.addDatasetRecord( datasetId, relFilePath )
    #                print " - addDatasetRecord: ", str( datasetId ), str( cdmsFile )
            self.setParameter( "timeRange" , self.timeRange )
            self.setParameter( "roi", self.roi )
            self.datasetModule.timeRange = self.timeRange
            self.datasetModule.setReferenceVariable( self.ref_var )
            if inputSpecs: self.persistDatasetParameters() 
            self.setResult( 'dataset', self.datasetModule )
#            print " ......  Start Workflow, dsid=%s, zscale = %.2f ......  " % ( self.datasetModule.getDsetId(), zscale )


    def dvUpdate( self, **args ):
        pass     
        
