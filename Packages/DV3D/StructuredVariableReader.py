'''
Created on Nov 21, 2011

@author: tpmaxwel
'''
   
import vtk, sys, os, copy, time, traceback
import cdms2, cdtime, cdutil, MV2, cPickle 
PortDataVersion = 0
from ConfigurationFunctions import *
from StructuredDataset import *
 
def getVarName( var ):
    if hasattr( var,'outvar'): return var.outvar.name 
    if hasattr( var,'name'): return var.name 
    if hasattr( var,'name_in_file'): return var.name_in_file 
    if hasattr( var,'id'): return var.id 

def getRoiSize( roi ):
    if roi == None: return 0
    return abs((roi[2]-roi[0])*(roi[3]-roi[1]))

class OutputRecManager: 
    
    sep = ';#:|!'   
            
    def __init__( self, serializedData = None ): 
        self.outputRecs = {}
        if serializedData <> None:
            self.deserialize( serializedData )
            
#     def deleteOutput( self, dsid, outputName ):
#         orecMap =  self.outputRecs.get( dsid, None )
#         if orecMap: del orecMap[outputName] 

    def addOutputRec( self, dsid, orec ): 
        orecMap =  self.outputRecs.setdefault( dsid, {} )
        orecMap[ orec.getKey() ] = orec

    def getOutputRec( self, dsid, outputName ):
        orecMap =  self.outputRecs.get( dsid, None )
        return orecMap[ outputName ] if orecMap else None

    def getOutputRecNames( self, dsid  ): 
        orecMap =  self.outputRecs.get( dsid, None )
        return orecMap.keys() if orecMap else []

    def getOutputRecs( self, dsid ):
        orecMap =  self.outputRecs.get( dsid, None )
        return orecMap.values() if orecMap else []
    
class OutputRec:
    
    def __init__(self, name, **args ): 
        self.name = name
        self.varComboList = args.get( "varComboList", [] )
        self.levelsCombo = args.get( "levelsCombo", None )
        self.level = args.get( "level", None )
        self.varTable = args.get( "varTable", None )
        self.varList = args.get( "varList", None )
        self.varSelections = args.get( "varSelections", [] )
        self.type = args.get( "type", None )
        self.ndim = args.get( "ndim", 3 )
        self.updateSelections() 
        
    def getKey(self):
        return '-'.join( [self.name] + self.varList )

    def getVarList(self):
        vlist = []
        for vrec in self.varList:
            vlist.append( str( getItem( vrec ) ) )
        return vlist
    
    def getSelectedVariableList(self):
        return [ str( varCombo.currentText() ) for varCombo in self.varComboList ]

    def getSelectedLevel(self):
        return str( self.levelsCombo.currentText() ) if self.levelsCombo else None
    
    def updateSelections(self):
        self.varSelections = []
        for varCombo in self.varComboList:
            varSelection = str( varCombo.currentText() ) 
            self.varSelections.append( [ varSelection, "" ] )
                  
class StructuredDataReader:

    dataCache = {}
    imageDataCache = {}
    
    def __init__(self, **args):
        init_specs = args.get( 'init_specs', None )
        if init_specs <> None:
            self.datasetId = init_specs[1]
            self.fileSpecs = init_specs[1]
            self.varSpecs = init_specs[3]
            self.gridSpecs = init_specs[4]
            self.subSpace = init_specs[7]
            self.df = cdms2.open( self.fileSpecs ) 
            self.vars =  [ self.df[ varSpec ] for varSpec in self.varSpecs ]
        else:
            self.vars =  args.get( 'vars', None )
            if self.vars <> None:
                dfile = self.vars[0].parent
                self.datasetId = dfile.Title if hasattr( dfile, 'Title' ) else dfile.id
                self.fileSpecs = dfile.id
                self.varSpecs = [ var.name_in_file for var in self.vars ]
                self.subSpace = args.get( 'axes', 'xyz' )
                self.df = cdms2.open( self.fileSpecs ) 
        self.referenceTimeUnits = None
        self.parameters = {}
        self.currentTime = 0
        self.currentLevel = None
        self.timeIndex = 0
        self.timeValue = None
        self.useTimeIndex = False
        self.timeAxis = None
        self.fieldData = None
        self.outputType = CDMSDataType.Hoffmuller if ( self.subSpace == 'xyt' ) else CDMSDataType.Volume 
# #        memoryLogger.log("Init CDMSDataReader")
#         if self.outputType == CDMSDataType.Hoffmuller:
#             self.addUVCDATConfigGuiFunction( 'chooseLevel', LevelConfigurationDialog, 'L', label='Choose Level' ) 
            
    def getTimeAxis(self):
        return self.timeAxis
       
    def getCachedImageData( self, data_id ):
        return self.imageDataCache.get( data_id, None )

    def setCachedImageData( self, data_id, image_data ):
        self.imageDataCache[data_id] = image_data

    @classmethod
    def clearCache( cls, cell_coords ):
#         from packages.vtDV3D.vtUtilities import memoryLogger
#         memoryLogger.log("start VolumeRader.clearCache")
        for dataCacheItems in cls.dataCache.items():
            dataCacheKey = dataCacheItems[0]
            dataCacheObj = dataCacheItems[1]
            if cell_coords in dataCacheObj.cells:
                dataCacheObj.cells.remove( cell_coords )
                if len( dataCacheObj.cells ) == 0:
                    varDataMap = dataCacheObj.data.get('varData', None )
                    if varDataMap:
                        newDataArray = varDataMap.get( 'newDataArray', None  )
                        try:
                            varDataMap['newDataArray' ] = None
                            del newDataArray
                        except Exception, err:
                            print>>sys.stderr, "Error releasing variable data: ", str(err)
                    dataCacheObj.data['varData'] = None
                    del cls.dataCache[ dataCacheKey ]
                    print "Removing Cached data: ", str( dataCacheKey )
#        memoryLogger.log(" finished clearing data cache ")
        for imageDataItem in cls.imageDataCache.items():
            freeImageData( imageDataItem[1] )
#        memoryLogger.log("finished clearing image cache")
        
    def getCachedData( self, varDataId, cell_coords ):
        dataCacheObj = self.dataCache.setdefault( varDataId, DataCache() )
        data = dataCacheObj.data.get( 'varData', None )
        if data: dataCacheObj.cells.add( cell_coords )
        return data

    def setCachedData(self, varDataId, cell_coords, varDataMap ):
        dataCacheObj = self.dataCache.setdefault( varDataId, DataCache() )
        dataCacheObj.data[ 'varData' ] = varDataMap
        dataCacheObj.cells.add( cell_coords )
                
    def getParameterDisplay( self, parmName, parmValue ):
        if parmName == 'timestep':
#            timestep = self.getTimeIndex( int( parmValue[0] ) )
            timestep = int( parmValue[0] )
            try:    return str( self.timeLabels[ timestep ] ), 10
            except: pass
        return None, 1

    def addCDMSVariable( self, cdms_var, index ):
        varname = cdms_var.id
        self.cdmsDataset.addTransientVariable( varname, cdms_var )
        self.cdmsDataset.setVariableRecord( "VariableName%d" % index, '*'.join( [ self.datasetId, varname ] ) )
        return cdms_var
    
    def designateAxes(self,var):
        lev_aliases = [ 'bottom', 'top', 'zdim', 'level' ]
        lev_axis_attr = [ 'z' ]
        lat_aliases = [ 'north', 'south', 'ydim' ]
        lat_axis_attr = [ 'y' ]
        lon_aliases = [ 'east', 'west', 'xdim' ]
        lon_axis_attr = [ 'x' ]
        latLonGrid = True
        for axis in var.getAxisList():
            if not isDesignated( axis ):
                if matchesAxisType( axis, lev_axis_attr, lev_aliases ):
                    axis.designateLevel()
                    print " --> Designating axis %s as a Level axis " % axis.id            
                elif matchesAxisType( axis, lat_axis_attr, lat_aliases ):
                    axis.designateLatitude()
                    print " --> Designating axis %s as a Latitude axis " % axis.id 
                    latLonGrid = False                     
                elif matchesAxisType( axis, lon_axis_attr, lon_aliases ):
                    axis.designateLongitude()
                    print " --> Designating axis %s as a Longitude axis " % axis.id 
                    latLonGrid = False 
            elif ( axis.isLatitude() or axis.isLongitude() ):
                if ( axis.id.lower()[0] == 'x' ) or ( axis.id.lower()[0] == 'y' ):
                    latLonGrid = False 
        return latLonGrid
    
    def setParameter( self, key , value ):
        self.parameters[key] = value

    def getParameter( self, key ):
        return self.parameters.get(key, None )

    def setupTimeAxis( self, var, **args ):
        self.nTimesteps = 1
        self.timeRange = [ 0, self.nTimesteps, 0.0, 0.0 ]
        self.timeAxis = var.getTime()
        if self.timeAxis:
            self.nTimesteps = len( self.timeAxis ) if self.timeAxis else 1
            try:
                if self.referenceTimeUnits == None: self.referenceTimeUnits = self.timeAxis.units
                comp_time_values = self.timeAxis.asComponentTime()
                t0 = comp_time_values[0].torel(self.referenceTimeUnits).value
                if (t0 < 0):
                    self.referenceTimeUnits = self.timeAxis.units
                    t0 = comp_time_values[0].torel(self.referenceTimeUnits).value
                dt = 0.0
                if self.nTimesteps > 1:
                    t1 = comp_time_values[-1].torel(self.referenceTimeUnits).value
                    dt = (t1-t0)/(self.nTimesteps-1)
                    self.timeRange = [ 0, self.nTimesteps, t0, dt ]
            except:
                values = self.timeAxis.getValue()
                t0 = values[0] if len(values) > 0 else 0
                t1 = values[-1] if len(values) > 1 else t0
                dt = ( values[1] - values[0] )/( len(values) - 1 ) if len(values) > 1 else 0
                self.timeRange = [ 0, self.nTimesteps, t0, dt ]
        self.setParameter( "timeRange" , self.timeRange )
        self.cdmsDataset.timeRange = self.timeRange
        self.cdmsDataset.referenceTimeUnits = self.referenceTimeUnits
        self.timeLabels = self.cdmsDataset.getTimeValues()
        timeData = args.get( 'timeData', [ self.cdmsDataset.timeRange[2], 0, False ] )
        if timeData:
            self.timeValue = cdtime.reltime( float(timeData[0]), self.referenceTimeUnits )
            self.timeIndex = timeData[1]
            self.useTimeIndex = timeData[2]
        else:
            self.timeValue = cdtime.reltime( t0, self.referenceTimeUnits )
            self.timeIndex = 0
            self.useTimeIndex = False
#            print "Set Time [mid = %d]: %s, NTS: %d, Range: %s, Index: %d (use: %s)" % ( self.moduleID, str(self.timeValue), self.nTimesteps, str(self.timeRange), self.timeIndex, str(self.useTimeIndex) )
#            print "Time Step Labels: %s" % str( self.timeLabels )
           
    def execute(self, **args ):
        iVar = 1
        cdms_var = self.vars[0]
        self.cdmsDataset = CDMSDataset()
        var = self.addCDMSVariable( cdms_var, iVar )
        self.newDataset = False
#            if self.newDataset: ModuleStore.archiveCdmsDataset( dsetId, self.cdmsDataset )
        self.newLayerConfiguration = self.newDataset
        self.cdmsDataset.latLonGrid = self.designateAxes(var)
        self.setupTimeAxis( var, **args )
        intersectedRoi = self.cdmsDataset.gridBounds
        intersectedRoi = self.getIntersectedRoi( cdms_var, intersectedRoi )
        for cdms_var in self.vars[1:]:
            if id(cdms_var) <> id(None): 
                iVar = iVar+1
                self.addCDMSVariable( cdms_var, iVar )
                intersectedRoi = self.getIntersectedRoi( cdms_var, intersectedRoi )
                              
        if hasattr(cdms_var,'url'): self.generateOutput( roi=intersectedRoi, url=cdms_var.url )
        else:                       self.generateOutput( roi=intersectedRoi )
 
            
    def getParameterId(self):
        return self.datasetId
            
    def getPortData( self, **args ):
        return self.getInputValue( "portData", **args )  

    def generateVariableOutput( self, cdms_var ): 
        print str(cdms_var.var)
        self.set3DOutput( name=cdms_var.name,  output=cdms_var.var )

    def refreshVersion(self):
        portData = self.getPortData()
        if portData:
            portDataVersion = portData[1] + 1
            serializedPortData = portData[0]
            self.persistParameter( 'portData', [ serializedPortData, portDataVersion ] )
        
    def getOutputRecord( self, ndim = -1 ):
        portData = self.getPortData()
        if portData:
            oRecMgr = OutputRecManager( portData[0]  )
            orecs = oRecMgr.getOutputRecs( self.datasetId ) if oRecMgr else None
            if not orecs: raise Exception( 'No Variable selected for dataset %s.' % self.datasetId )             
            for orec in orecs:
                if (ndim < 0 ) or (orec.ndim == ndim): return orec
        return None
             
    def generateOutput( self, **args ): 
        oRecMgr = None 
        varRecs = self.cdmsDataset.getVarRecValues()
        cell_coords = [ 0, 0 ]
        if len( varRecs ):
#            print " VolumeReader->generateOutput, varSpecs: ", str(varRecs)
            oRecMgr = OutputRecManager() 
#            varCombo = QComboBox()
            for var in varRecs: 
                otype = 'volume'
                orec = OutputRec( otype, ndim=3, varList=[var] )   
                oRecMgr.addOutputRec( self.datasetId, orec ) 
        else:
            portData = self.getPortData()
            if portData:
#                print " VolumeReader->generateOutput, portData: ", portData
                oRecMgr = OutputRecManager( portData[0]  )
        orecs = oRecMgr.getOutputRecs( self.datasetId ) if oRecMgr else None
        if not orecs: raise Exception( 'No Variable selected for dataset %s.' % self.datasetId ) 
        self.output_names = [] 
        self.outputSpecs = []           
        for orec in orecs:
            cachedImageDataName = self.getImageData( orec, **args ) 
            self.output_names.append( cachedImageDataName )
            ispec = InputSpecs()
            ispec.initializeInput( self.getCachedImageData( cachedImageDataName ), self.getFieldData() )
            self.outputSpecs.append( ispec )
#             image_data_specs = cachedImageDataName.split('-')
#             for image_data_spec in image_data_specs:
#                 self.output_names.append( image_data_spec )
#                 ispec = InputSpecs()
#                 ispec.initializeInput( self.getCachedImageData( image_data_spec ), self.getFieldData() )
#                 self.outputSpecs.append( ispec )
        self.currentTime = self.getTimestep()

    def output( self, iIndex=0 ):
        cachedImageDataName = self.output_names[ iIndex ]
        cachedImageData = self.getCachedImageData( cachedImageDataName ) 
        return cachedImageData

    def outputSpec( self, iIndex=0 ):
        return self.outputSpecs[ iIndex ]
    
    def nOutputs(self):
        return len( self.outputSpecs )
     
    def getTimestep( self ):
        dt = self.timeRange[3]
        return 0 if dt <= 0.0 else int( round( ( self.timeValue.value - self.timeRange[2] ) / dt ) )

    def setCurrentLevel(self, level ): 
        self.currentLevel = level

    def getFileMetadata( self, orec, **args ):
        varList = orec.varList
        if len( varList ) == 0: return False
        varDataIds = []
        intersectedRoi = args.get('roi', None )
        url = args.get('url', None )
        if intersectedRoi: self.cdmsDataset.setRoi( intersectedRoi )
        dsid = None
        fieldData = self.getFieldData()
        if fieldData:
            na = fieldData.GetNumberOfArrays()
            for ia in range(na):
                aname = fieldData.GetArrayName(ia)
                if (aname <> None) and aname.startswith('metadata'):
                    fieldData.RemoveArray(aname)
        vars = []
        for varRec in varList:
            range_min, range_max, scale, shift  = 0.0, 0.0, 1.0, 0.0   
            imageDataName = getItem( varRec )
            varNameComponents = imageDataName.split('*')
            if len( varNameComponents ) == 1:
                dsid = self.cdmsDataset.getReferenceDsetId() 
                varName = varNameComponents[0]
            else:
                dsid = varNameComponents[0]
                varName = varNameComponents[1]
            ds = self.cdmsDataset[ dsid ]
            if ds:
                var = ds.getVariable( varName )
                self.setupTimeAxis( var, **args )
            portName = orec.name
            selectedLevel = orec.getSelectedLevel() if ( self.currentLevel == None ) else self.currentLevel
            ndim = 3 if ( orec.ndim == 4 ) else orec.ndim
            default_dtype = np.float
            scalar_dtype = args.get( "dtype", default_dtype )
            self._max_scalar_value = getMaxScalarValue( scalar_dtype )
            self._range = [ 0.0, self._max_scalar_value ]  
            datatype = getDatatypeString( scalar_dtype )
            if (self.outputType == CDMSDataType.Hoffmuller):
                if ( selectedLevel == None ):
                    varDataIdIndex = 0
                else:
                    varDataIdIndex = selectedLevel  
                                      
            iTimestep = self.timeIndex if ( varName <> '__zeros__' ) else 0
            varDataIdIndex = iTimestep  
            roiStr = ":".join( [ ( "%.1f" % self.cdmsDataset.gridBounds[i] ) for i in range(4) ] ) if self.cdmsDataset.gridBounds else ""
            varDataId = '%s;%s;%d;%s;%s' % ( dsid, varName, self.outputType, str(varDataIdIndex), roiStr )
            vmd = {}         
            vmd[ 'dsid' ] = dsid 
            vmd[ 'file' ] = url if url else dsid              
            vmd[ 'varName' ] = varName                 
            vmd[ 'outputType' ] = self.outputType                 
            vmd[ 'varDataIdIndex' ] = varDataIdIndex
            vmd['datatype'] = datatype
            vmd['timeIndex']= iTimestep
            vmd['timeValue']= self.timeValue.value
            vmd['latLonGrid']= self.cdmsDataset.latLonGrid
            vmd['timeUnits' ] = self.referenceTimeUnits 
            vmd[ 'bounds' ] = self.cdmsDataset.gridBounds          
            enc_mdata = encodeToString( vmd ) 
            if enc_mdata and fieldData: 
                fieldData.AddArray( getStringDataArray( 'metadata:%s' % varName,   [ enc_mdata ]  ) ) 
                vars.append( varName )                   
        fieldData.AddArray( getStringDataArray( 'varlist',  vars  ) )                       

    def getFieldData( self ):
        if self.fieldData == None:
            self.initializeMetadata()
        return self.fieldData  

    def initializeMetadata( self ):
        try:
            self.fieldData = vtk.vtkDataSetAttributes()
            mdarray = getStringDataArray( 'metadata' )
            self.fieldData.AddArray( mdarray )
        except Exception, err:
            print>>sys.stderr, "Error initializing metadata"

    def addMetadata( self, metadata ):
        dataVector = self.fieldData.GetAbstractArray( 'metadata' ) 
        enc_mdata = encodeToString( metadata )
        dataVector.InsertNextValue( enc_mdata  )
               
    def getImageData( self, orec, **args ):
        """
        This method converts cdat data into vtkImageData objects. The ds object is a CDMSDataset instance which wraps a CDAT CDMS Dataset object. 
        The ds.getVarDataCube method execution extracts a CDMS variable object (varName) and then cuts out a data slice with the correct axis ordering (returning a NumPy masked array).   
        The array is then rescaled, converted to a 1D unsigned short array, and then wrapped as a vtkUnsignedShortArray using the vtkdata.SetVoidArray method call.  
        The vtk data array is then attached as point data to a vtkImageData object, which is returned.
        The CDAT metadata is serialized, wrapped as a vtkStringArray, and then attached as field data to the vtkImageData object.  
        """
        varList = orec.varList
        npts = -1
        if len( varList ) == 0: return False
        varDataIds = []
        intersectedRoi = args.get('roi', None )
        if intersectedRoi: self.cdmsDataset.setRoi( intersectedRoi )
        exampleVarDataSpecs = None
        dsid = None
        if (self.outputType == CDMSDataType.Vector ) and len(varList) < 3:
            if len(varList) == 2: 
                imageDataName = getItem( varList[0] )
                dsid = imageDataName.split('*')[0]
                varList.append( '*'.join( [ dsid, '__zeros__' ] ) )
            else: 
                print>>sys.stderr, "Not enough components for vector plot: %d" % len(varList)
#        print " Get Image Data: varList = %s " % str( varList )
        for varRec in varList:
            range_min, range_max, scale, shift  = 0.0, 0.0, 1.0, 0.0   
            imageDataName = getItem( varRec )
            varNameComponents = imageDataName.split('*')
            if len( varNameComponents ) == 1:
                dsid = self.cdmsDataset.getReferenceDsetId() 
                varName = varNameComponents[0]
            else:
                dsid = varNameComponents[0]
                varName = varNameComponents[1]
            ds = self.cdmsDataset[ dsid ]
            if ds:
                var = ds.getVariable( varName )
                self.setupTimeAxis( var, **args )
            portName = orec.name
            selectedLevel = orec.getSelectedLevel() if ( self.currentLevel == None ) else self.currentLevel
            ndim = 3 if ( orec.ndim == 4 ) else orec.ndim
            default_dtype = np.float32
            scalar_dtype = args.get( "dtype", default_dtype )
            self._max_scalar_value = getMaxScalarValue( scalar_dtype )
            self._range = [ 0.0, self._max_scalar_value ]  
            datatype = getDatatypeString( scalar_dtype )
            if (self.outputType == CDMSDataType.Hoffmuller):
                if ( selectedLevel == None ):
                    varDataIdIndex = 0
                else:
                    varDataIdIndex = selectedLevel  
                                      
            iTimestep = self.timeIndex if ( varName <> '__zeros__' ) else 0
            varDataIdIndex = iTimestep  
            cell_coords = (0,0)
            roiStr = ":".join( [ ( "%.1f" % self.cdmsDataset.gridBounds[i] ) for i in range(4) ] ) if self.cdmsDataset.gridBounds else ""
            varDataId = '%s;%s;%d;%s;%s' % ( dsid, varName, self.outputType, str(varDataIdIndex), roiStr )
            varDataIds.append( varDataId )
            varDataSpecs = self.getCachedData( varDataId, cell_coords ) 
            flatArray = None
            if varDataSpecs == None:
                if varName == '__zeros__':
                    assert( npts > 0 )
                    newDataArray = np.zeros( npts, dtype=scalar_dtype ) 
                    varDataSpecs = copy.deepcopy( exampleVarDataSpecs )
                    varDataSpecs['newDataArray'] = newDataArray.ravel('F')  
                    self.setCachedData( varName, cell_coords, varDataSpecs ) 
                else: 
                    tval = None if (self.outputType == CDMSDataType.Hoffmuller) else [ self.timeValue, iTimestep, self.useTimeIndex ] 
                    varDataMasked = self.cdmsDataset.getVarDataCube( dsid, varName, tval, selectedLevel, cell=cell_coords )
                    if varDataMasked.id <> 'NULL':
                        varDataSpecs = self.getGridSpecs( varDataMasked, self.cdmsDataset.gridBounds, self.cdmsDataset.zscale, self.outputType, ds )
                        if (exampleVarDataSpecs == None) and (varDataSpecs <> None): exampleVarDataSpecs = varDataSpecs
                        range_min = varDataMasked.min()
                        if type( range_min ).__name__ == "MaskedConstant": range_min = 0.0
                        range_max = varDataMasked.max()
                        if type( range_max ).__name__ == 'MaskedConstant': range_max = 0.0
                        var_md = copy.copy( varDataMasked.attributes )
                                                          
                        if ( scalar_dtype == np.float32 ) or ( scalar_dtype == np.float64 ):
                            varData = varDataMasked.filled( 1.0e-15 * range_min ).astype(scalar_dtype).ravel('F')
                        else:
                            shift = -range_min
                            scale = ( self._max_scalar_value ) / ( range_max - range_min ) if  ( range_max > range_min ) else 1.0        
                            varData = ( ( varDataMasked + shift ) * scale ).astype(scalar_dtype).filled( 0 ).ravel('F')                          
                        del varDataMasked                          
                        
                        array_size = varData.size
                        if npts == -1:  npts = array_size
                        else: assert( npts == array_size )
                            
                        var_md[ 'range' ] = ( range_min, range_max )
                        var_md[ 'scale' ] = ( shift, scale )   
                        varDataSpecs['newDataArray'] = varData 
#                        print " ** Allocated data array for %s, size = %.2f MB " % ( varDataId, (varData.nbytes /(1024.0*1024.0) ) )                    
                        md =  varDataSpecs['md']                 
                        md['datatype'] = datatype
                        md['timeValue']= self.timeValue.value
                        md['latLonGrid']= self.cdmsDataset.latLonGrid
                        md['timeUnits' ] = self.referenceTimeUnits
                        md[ 'attributes' ] = var_md
                        md[ 'plotType' ] = 'zyt' if (self.outputType == CDMSDataType.Hoffmuller) else 'xyz'
                                        
                self.setCachedData( varDataId, cell_coords, varDataSpecs )  
        
        if not varDataSpecs: return None            

        cachedImageDataName = '-'.join( varDataIds )
        image_data = self.getCachedImageData( cachedImageDataName ) 
        if not image_data:
#            print 'Building Image for cache: %s ' % cachedImageDataName
            image_data = vtk.vtkImageData() 
            outputOrigin = varDataSpecs[ 'outputOrigin' ]
            outputExtent = varDataSpecs[ 'outputExtent' ]
            gridSpacing = varDataSpecs[ 'gridSpacing' ]
#             if   scalar_dtype == np.ushort: image_data.SetScalarType(vtk.VTK_UNSIGNED_SHORT)
#             elif scalar_dtype == np.ubyte:  image_data.SetScalarType(vtk.VTK_UNSIGNED_CHAR)
#             elif scalar_dtype == np.float32:  image_data.SetScalarType(vtk.VTK_FLOAT)
#             elif scalar_dtype == np.float64:  image_data.SetScalarType(vtk.VTK_DOUBLE)
            image_data.SetOrigin( outputOrigin[0], outputOrigin[1], outputOrigin[2] )
#            image_data.SetOrigin( 0.0, 0.0, 0.0 )
            if ndim == 3: 
                extent = [ int(outputExtent[i]) for i in range(6) ]   
            elif ndim == 2:
                extent = [ int(outputExtent[i]) for i in range(4) ]   
                extent = extent + [ 0, 0 ]
            image_data.SetExtent( extent )
#            image_data.SetWholeExtent( extent )
            image_data.SetSpacing(  gridSpacing[0], gridSpacing[1], gridSpacing[2] )
#            print " ********************* Create Image Data, extent = %s, spacing = %s ********************* " % ( str(extent), str(gridSpacing) )
#            offset = ( -gridSpacing[0]*gridExtent[0], -gridSpacing[1]*gridExtent[2], -gridSpacing[2]*gridExtent[4] )
            self.setCachedImageData( cachedImageDataName, image_data )
                
        nVars = len( varList )
#        npts = image_data.GetNumberOfPoints()
        pointData = image_data.GetPointData()
        for aname in range( pointData.GetNumberOfArrays() ): 
            pointData.RemoveArray( pointData.GetArrayName(aname) )
        fieldData = self.getFieldData()
        if fieldData:
            na = fieldData.GetNumberOfArrays()
            for ia in range(na):
                aname = fieldData.GetArrayName(ia)
                if (aname <> None) and aname.startswith('metadata'):
                    fieldData.RemoveArray(aname)
    #                print 'Remove fieldData Array: %s ' % aname
        extent = image_data.GetExtent()    
        scalars, nTup = None, 0
        vars = [] 
        for varDataId in varDataIds:
            try: 
                varDataSpecs = self.getCachedData( varDataId, cell_coords )   
                newDataArray = varDataSpecs.get( 'newDataArray', None )
                md = varDataSpecs[ 'md' ] 
                varName = varDataId.split(';')[1]
                var_md = md[ 'attributes' ]            
                if newDataArray <> None:
                    vars.append( varName ) 
                    md[ 'valueRange'] = var_md[ 'range' ] 
                    vtkdata = getNewVtkDataArray( scalar_dtype )
                    nTup = newDataArray.size
                    vtkdata.SetNumberOfTuples( nTup )
                    vtkdata.SetNumberOfComponents( 1 )
                    vtkdata.SetVoidArray( newDataArray, newDataArray.size, 1 )
                    vtkdata.SetName( varName )
                    vtkdata.Modified()
                    pointData.AddArray( vtkdata )
#                    print "Add array to PointData: %s " % ( varName  )  
                    if (scalars == None) and (varName <> '__zeros__'):
                        scalars = varName
                        pointData.SetActiveScalars( varName  ) 
                        md[ 'scalars'] = varName 
            except Exception, err:
                print>>sys.stderr, "Error creating variable metadata: %s " % str(err)
                traceback.print_exc()
#         for iArray in range(2):
#             scalars = pointData.GetArray(iArray) 
# #            print "Add array %d to PointData: %s (%s)" % ( iArray, pointData.GetArrayName(iArray), scalars.GetName()  )       
        try:                           
            if (self.outputType == CDMSDataType.Vector ): 
                vtkdata = getNewVtkDataArray( scalar_dtype )
                vtkdata.SetNumberOfComponents( 3 )
                vtkdata.SetNumberOfTuples( nTup )
                iComp = 0
                for varName in vars:
                    fromArray =  pointData.GetArray( varName )
                    fromNTup = fromArray.GetNumberOfTuples()
                    tup0 = fromArray.GetValue(0)
                    toNTup = vtkdata.GetNumberOfTuples()
                    vtkdata.CopyComponent( iComp, fromArray, 0 )
                    if iComp == 0: 
                        md[ 'scalars'] = varName 
                    iComp = iComp + 1                    
                vtkdata.SetName( 'vectors' )
                md[ 'vectors'] = ','.join( vars ) 
                vtkdata.Modified()
                pointData.SetVectors(vtkdata)
                pointData.SetActiveVectors( 'vectors'  )         
            if len( vars )== 0: raise Exception( 'No dataset variables selected for output %s.' % orec.name) 
            for varDataId in varDataIds:
                varDataFields = varDataId.split(';')
                dsid = varDataFields[0] 
                varName = varDataFields[1] 
                if varName <> '__zeros__':
                    varDataSpecs = self.getCachedData( varDataId, cell_coords )
                    vmd = varDataSpecs[ 'md' ] 
                    var_md = md[ 'attributes' ]               
#                    vmd[ 'vars' ] = vars               
                    vmd[ 'title' ] = getTitle( dsid, varName, var_md )                 
                    enc_mdata = encodeToString( vmd ) 
                    if enc_mdata and fieldData: fieldData.AddArray( getStringDataArray( 'metadata:%s' % varName,   [ enc_mdata ]  ) ) 
            if enc_mdata and fieldData: fieldData.AddArray( getStringDataArray( 'varlist',  vars  ) )                       
            image_data.Modified()
        except Exception, err:
            print>>sys.stderr, "Error encoding variable metadata: %s " % str(err)
            traceback.print_exc()
        return cachedImageDataName


    def getAxisValues( self, axis, roi ):
        values = axis.getValue()
        bounds = None
        if roi:
            if   axis.isLongitude():  bounds = [ roi[0], roi[2] ]
            elif axis.isLatitude():   bounds = [ roi[1], roi[3] ] if ( roi[3] > roi[1] ) else [ roi[3], roi[1] ] 
        if bounds:
            if len( values ) < 2: values = bounds
            else:
                if axis.isLongitude() and (values[0] > values[-1]):
                    values[-1] = values[-1] + 360.0 
                value_bounds = [ min(values[0],values[-1]), max(values[0],values[-1]) ]
                mid_value = ( value_bounds[0] + value_bounds[1] ) / 2.0
                mid_bounds = ( bounds[0] + bounds[1] ) / 2.0
                offset = (360.0 if mid_bounds > mid_value else -360.0)
                trans_val = mid_value + offset
                if (trans_val > bounds[0]) and (trans_val < bounds[1]):
                    value_bounds[0] = value_bounds[0] + offset
                    value_bounds[1] = value_bounds[1] + offset           
                bounds[0] = max( [ bounds[0], value_bounds[0] ] )
                bounds[1] = min( [ bounds[1], value_bounds[1] ] )
        return bounds, values

    def getCoordType( self, axis, outputType ):
        iCoord = -2
        if axis.isLongitude(): 
            self.lon = axis
            iCoord  = 0
        if axis.isLatitude(): 
            self.lat = axis
            iCoord  = 1
        if axis.isLevel() or PlotType.isLevelAxis(  axis.id ): 
            self.lev = axis
            iCoord  = 2 if ( outputType <> CDMSDataType.Hoffmuller ) else -1
        if axis.isTime():
            self.time = axis
            iCoord  = 2 if ( outputType == CDMSDataType.Hoffmuller ) else -1
        return iCoord

    def getIntersectedRoi( self, var, current_roi ):   
        try:
            newRoi = [ 0.0 ] * 4
            varname = getVarName( var )
            tvar = self.cdmsDataset.getTransientVariable( varname )
            if id( tvar ) == id( None ): return current_roi
            current_roi_size = getRoiSize( current_roi )
            for iCoord in range(2):
                axis = None
                if iCoord == 0: axis = tvar.getLongitude()
                if iCoord == 1: axis = tvar.getLatitude()
                if axis == None: return current_roi
                axisvals = axis.getValue()          
                if ( len( axisvals.shape) > 1 ):
                    return current_roi
                newRoi[ iCoord ] = axisvals[0] # max( current_roi[iCoord], roiBounds[0] ) if current_roi else roiBounds[0]
                newRoi[ 2+iCoord ] = axisvals[-1] # min( current_roi[2+iCoord], roiBounds[1] ) if current_roi else roiBounds[1]
            if ( current_roi_size == 0 ): return newRoi
            new_roi_size = getRoiSize( newRoi )
            return newRoi if ( ( current_roi_size > new_roi_size ) and ( new_roi_size > 0.0 ) ) else current_roi
        except:
            print>>sys.stderr, "Error getting ROI for input variable"
            traceback.print_exc()
            return current_roi
       
    def getGridSpecs( self, var, roi, zscale, outputType, dset ):   
        dims = var.getAxisIds()
        gridOrigin = [ 0.0 ] * 3
        outputOrigin = [ 0.0 ] * 3
        gridBounds = [ 0.0 ] * 6
        gridSpacing = [ 0.0 ] * 3
        gridExtent = [ 0.0 ] * 6
        outputExtent = [ 0.0 ] * 6
        gridShape = [ 0.0 ] * 3
        gridSize = 1
        domain = var.getDomain()
        self.lev = var.getLevel()
        axis_list = var.getAxisList()
        isCurvilinear = False
        for axis in axis_list:
            size = len( axis )
            iCoord = self.getCoordType( axis, outputType )
            roiBounds, values = self.getAxisValues( axis, roi )
            if iCoord >= 0:
                iCoord2 = 2*iCoord
                gridShape[ iCoord ] = size
                gridSize = gridSize * size
                outputExtent[ iCoord2+1 ] = gridExtent[ iCoord2+1 ] = size-1 
                vmax =  max( values[0], values[-1] )                   
                vmin =  min( values[0], values[-1] )                   
                if iCoord < 2:
                    lonOffset = 0.0 #360.0 if ( ( iCoord == 0 ) and ( roiBounds[0] < -180.0 ) ) else 0.0
                    outputOrigin[ iCoord ] = gridOrigin[ iCoord ] = vmin + lonOffset
                    spacing = (vmax - vmin)/(size-1)
                    if roiBounds:
                        if ( roiBounds[1] < 0.0 ) and  ( roiBounds[0] >= 0.0 ): roiBounds[1] = roiBounds[1] + 360.0
                        gridExtent[ iCoord2 ] = int( round( ( roiBounds[0] - vmin )  / spacing ) )                
                        gridExtent[ iCoord2+1 ] = int( round( ( roiBounds[1] - vmin )  / spacing ) )
                        if gridExtent[ iCoord2 ] > gridExtent[ iCoord2+1 ]:
                            geTmp = gridExtent[ iCoord2+1 ]
                            gridExtent[ iCoord2+1 ] = gridExtent[ iCoord2 ] 
                            gridExtent[ iCoord2 ] = geTmp
                        outputExtent[ iCoord2+1 ] = gridExtent[ iCoord2+1 ] - gridExtent[ iCoord2 ]
                        outputOrigin[ iCoord ] = lonOffset + roiBounds[0]
                    roisize = gridExtent[ iCoord2+1 ] - gridExtent[ iCoord2 ] + 1                  
                    gridSpacing[ iCoord ] = spacing
                    gridBounds[ iCoord2 ] = roiBounds[0] if roiBounds else vmin 
                    gridBounds[ iCoord2+1 ] = (roiBounds[0] + roisize*spacing) if roiBounds else vmax
                else:                                             
                    gridSpacing[ iCoord ] = 1.0
#                    gridSpacing[ iCoord ] = zscale
                    gridBounds[ iCoord2 ] = vmin  # 0.0
                    gridBounds[ iCoord2+1 ] = vmax # float( size-1 )
        if gridBounds[ 2 ] > gridBounds[ 3 ]:
            tmp = gridBounds[ 2 ]
            gridBounds[ 2 ] = gridBounds[ 3 ]
            gridBounds[ 3 ] = tmp
        gridSpecs = {}
        md = { 'datasetId' : self.datasetId,  'bounds':gridBounds } # , 'lat':self.lat, 'lon':self.lon, 'lev':self.lev, 'time': self.timeAxis }
        gridSpecs['gridOrigin'] = gridOrigin
        gridSpecs['outputOrigin'] = outputOrigin
        gridSpecs['gridBounds'] = gridBounds
        gridSpecs['gridSpacing'] = gridSpacing
        gridSpecs['gridExtent'] = gridExtent
        gridSpecs['outputExtent'] = outputExtent
        gridSpecs['gridShape'] = gridShape
        gridSpecs['gridSize'] = gridSize
        gridSpecs['md'] = md
        if dset:  gridSpecs['attributes'] = dset.dataset.attributes
        return gridSpecs   
                 
