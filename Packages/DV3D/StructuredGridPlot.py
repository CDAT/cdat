'''
Created on Apr 23, 2014

@author: tpmaxwell
'''
    
import vtk, sys, os
from ColorMapManager import *
from Shapefile import shapeFileReader   
from StructuredVariableReader import StructuredDataReader
from DV3DPlot import *
      
class StructuredGridPlot(DV3DPlot):  
    
    def __init__( self,  **args ):
        DV3DPlot.__init__( self,  **args )
        self.iOrientation = 0

        self.variables = {}
        self.metadata = {}

        self.isValid = True
        self.configDialog = None
        self.stereoEnabled = 0
        self.inputSpecs = {}


        self.pipelineBuilt = False
        self.baseMapActor = None
        self.enableBasemap = True
        self.map_opacity = [ 0.4, 0.4 ]
        self.skipIndex = 1
        self.roi = None
        self.shapefilePolylineActors = {}
        self.basemapLineSpecs = {}

    def processToggleClippingCommand( self, args, config_function ):
        if args and args[0] == "InitConfig": 
            self.toggleClipping( args[1] )

    def processVerticalScalingCommand( self, args, config_function ):
        verticalScale = config_function.value
        if args and args[0] == "StartConfig":
            pass
        elif args and args[0] == "Init":
            self.setZScale( config_function.initial_value )
            verticalScale.setValue( 'count', 1 )
        elif args and args[0] == "EndConfig":
            pass
        elif args and args[0] == "InitConfig":
            self.updateTextDisplay( config_function.label )
            bbar = self.getInteractionButtons()
            self.skipIndex = 5
            for islider in range(4): bbar.setSliderVisibility(  islider, islider < len(config_function.sliderLabels)  )
        elif args and args[0] == "Open":
            pass
        elif args and args[0] == "Close":
            pass
        elif args and args[0] == "UpdateConfig":
            count = verticalScale.incrementValue( 'count' )
            if count % self.skipIndex == 0:
                value = args[2].GetValue()
                vscale = verticalScale.getValues()
                vscale[ args[1] ] = value
                self.setZScale( vscale )
                verticalScale.setValues( vscale )

#     def onKeyEvent(self, eventArgs ):
#         key = eventArgs[0]
#         md = self.getInputSpec().getMetadata()
#         if (  key == 'r'  ):
#             self.resetCamera()              
#         elif ( md and ( md.get('plotType','')=='xyz' ) and ( key == 't' )  ):
#             self.showInteractiveLens = not self.showInteractiveLens 
#             self.render() 
#         else:
#             return DV3DPlot.onKeyEvent( self, eventArgs )
#         return 1

    def getRangeBounds( self, input_index = 0 ):
        ispec = self.inputSpecs[ input_index ] 
        return ispec.getRangeBounds()  

    def setZScale( self, zscale_data, **args ):
        self.setInputZScale( zscale_data, **args )

    def setRangeBounds( self, rbounds, input_index = 0 ):
        ispec = self.inputSpecs[ input_index ] 
        ispec.rangeBounds[:] = rbounds[:] 

    def setMaxScalarValue(self, iDType ):  
        if iDType   == vtk.VTK_UNSIGNED_CHAR:   self._max_scalar_value = 255
        elif iDType == vtk.VTK_UNSIGNED_SHORT:  self._max_scalar_value = 256*256-1
        elif iDType == vtk.VTK_SHORT:           self._max_scalar_value = 256*128-1
        else:                                   self._max_scalar_value = self.getRangeBounds()[1]  

    def decimateImage( self, image, decx, decy ):
        image.Update()
        dims = image.GetDimensions()
        image_size = dims[0] * dims[1]
        result = image
        if image_size > MAX_IMAGE_SIZE:
            resample = vtk.vtkImageShrink3D()
            resample.SetInput( image )
            resample.SetShrinkFactors( decx, decy, 1 )
            result = resample.GetOutput() 
            result.Update()
        return result

    def getScaleBounds(self):
        return [ 0.5, 100.0 ]

    def setInputZScale( self, zscale_data, input_index=0, **args  ):
        ispec = self.inputSpecs[ input_index ] 
        if ispec.input() <> None:
            input = ispec.input()
            ns = input.GetNumberOfScalarComponents()
            spacing = input.GetSpacing()
            ix, iy, iz = spacing
            sz = zscale_data[0]
            if iz <> sz:
#                print " PVM >---------------> Change input zscale: %.4f -> %.4f" % ( iz, sz )
                input.SetSpacing( ix, iy, sz )  
                input.Modified() 
                self.processScaleChange( spacing, ( ix, iy, sz ) )
                return True
        return False
    
    def getDataRangeBounds(self, inputIndex=0 ):
        ispec = self.getInputSpec( inputIndex )
        return ispec.getDataRangeBounds() if ispec else None

    def onSlicerLeftButtonPress( self, caller, event ):
        self.currentButton = self.LEFT_BUTTON   
        return 0

    def processScaleChange( self, old_spacing, new_spacing ):
        pass
#        self.updateModule()

    def onSlicerRightButtonPress( self, caller, event ):
        self.currentButton = self.RIGHT_BUTTON
        return 0
        
    def getAxes(self):
        pass

    def input( self, iIndex = 0 ):
        return self.variable_reader.output( iIndex )

    def isBuilt(self):
        return self.pipelineBuilt
    
    def initializeInputs( self, **args ):
        nOutputs = self.variable_reader.nOutputs()
        for inputIndex in range( nOutputs ):
            ispec = self.variable_reader.outputSpec( inputIndex )
#             fd = ispec.input().GetPointData()
#             nc = fd.GetNumberOfComponents ()
#             nt = fd.GetNumberOfTuples ()
            self.inputSpecs[inputIndex] = ispec 
            if self.roi == None:  
                self.roi = ispec.metadata.get( 'bounds', None )  
            self.intiTime( ispec, **args )
        self.initMetadata()
        
        
    def initMetadata(self):
        spec = self.inputSpecs[0]
        attributes = spec.metadata.get( 'attributes' , None )
        if attributes:
            self.metadata['var_name'] = attributes[ 'long_name']
            self.metadata['var_units'] = attributes[ 'units']

    def intiTime(self, ispec, **args):
        try:
            t = cdtime.reltime( 0, self.variable_reader.referenceTimeUnits )
            if t.cmp( cdtime.reltime( 0, ispec.referenceTimeUnits ) ) == 1:
                self.variable_reader.referenceTimeUnits = ispec.referenceTimeUnits 
            tval = args.get( 'timeValue', None )
            if tval: self.timeValue = cdtime.reltime( float( args[ 'timeValue' ] ), ispec.referenceTimeUnits )
        except:
            self.timeValue = 0.0

    def execute(self, **args ):
        if not self.isBuilt(): 
            self.initializeInputs()        
            self.buildPipeline()
            self.buildBaseMap()
            self.initializeConfiguration() 
            self.pipelineBuilt = True
                           
        self.updateModule( **args )                
        self.render()

    def updateModule( self, input_index = 0, **args  ):
        ispec = self.inputSpecs[ input_index ] 
        mapper = self.volume.GetMapper()
        mapper.SetInput( ispec.input() )
        mapper.Modified()

    def terminate( self ):
        pass
    
    def getScalarRange( self, input_index = 0 ):
        ispec = self.inputSpecs[ input_index ] 
        return ispec.scalarRange
    
    def basemapLinesVisibilityOn(self):
        for actor_list in self.shapefilePolylineActors.values():
            for actor in actor_list: 
                if actor: actor.VisibilityOn()  

    def basemapLinesVisibilityOff(self):
        for actor_list in self.shapefilePolylineActors.values():
            for actor in actor_list: 
                if actor: actor.VisibilityOff()  

    def createBasemapPolyline( self, type, **args ):
        ispec = self.getInputSpec(0)  
        md = ispec.getMetadata()
        latLonGrid = md.get( 'latLonGrid', True )
        if latLonGrid:
            line_specs = self.basemapLineSpecs.get( type, None )
            thickness = int( round( line_specs[0] ) ) if line_specs else 0
            density = int( round( line_specs[1] ) ) if line_specs else 1
            resTypes = [ "invisible", "low", "medium", "high" ]
            if (thickness > 0) and ( density > 0 ):
                rgb=self.getLayerColor( type ) 
                textFilePath = os.path.join( os.path.dirname(__file__), "data", type, "index.txt" )
                s=shapeFileReader()
                s.setColors(rgb)
                s.setWidth( thickness )
                polys=s.getPolyLines( self.roi, textFilePath, resTypes[ density ] )        
                self.renderer.AddActor(polys)
                origin = self.planeWidgetZ.GetOrigin()
                pos = polys.GetPosition()
                pos1 = [ pos[0], pos[1], origin[2] ]
                polys.SetPosition( pos1 )
                polys_list = self.shapefilePolylineActors.get( type, [ None, None, None, None, None ] ) 
                polys_list[ density ] = polys
                self.shapefilePolylineActors[ type ] = polys_list   
 
    def setBasemapLineSpecs( self, shapefile_type, value ):
        self.basemapLineSpecs[shapefile_type] = value
        npixels = int( round( value[0] ) )
        density = int( round( value[1] ) )
        polys_list = self.shapefilePolylineActors.get( shapefile_type, [ None, None, None, None, None ] ) 
        try:
            selected_polys = polys_list[ density ]
            if not selected_polys:
                if npixels: 
                    self.createBasemapPolyline( shapefile_type )
            else:
                for polys in polys_list:
                    if polys:
                        polys.SetVisibility( npixels and ( id(polys) == id(selected_polys) ) )
                selected_polys.GetProperty().SetLineWidth( npixels )           
            self.render()
        except IndexError:
            print>>sys.stderr, " setBasemapLineSpecs: Density too large: %d " % density

    def setBasemapCoastlineLineSpecs( self, value, **args ):
        self.setBasemapLineSpecs('coastline', value )

    def setBasemapStatesLineSpecs( self, value, **args ):
        self.setBasemapLineSpecs('states', value )

    def setBasemapLakesLineSpecs( self, value, **args ):
        self.setBasemapLineSpecs('lakes', value )
        
    def setBasemapCountriesLineSpecs( self, value, **args ):
        self.setBasemapLineSpecs('countries', value )

    def getBasemapLineSpecs( self, shapefile_type ):
        return self.basemapLineSpecs.get( shapefile_type, None )
        
    def getBasemapCoastlineLineSpecs( self, **args ):
        return self.getBasemapLineSpecs('coastline' )
        
    def getBasemapStatesLineSpecs( self, **args ):
        return self.getBasemapLineSpecs('states' )

    def getBasemapLakesLineSpecs( self, **args ):
        return self.getBasemapLineSpecs('lakes' )

    def getBasemapCountriesLineSpecs( self, **args ):
        return self.getBasemapLineSpecs('countries' )



    def getInputSpec( self, input_index=0 ):
        return self.inputSpecs.get( input_index, None )

    def getDataValue( self, image_value, input_index = 0 ):
        ispec = self.inputSpecs[ input_index ] 
        return ispec.getDataValue( image_value )

    def getTimeAxis(self):
        ispec = self.getInputSpec()     
        timeAxis = ispec.getMetadata('time') if ispec else None
        return timeAxis
                    
    def getDataValues( self, image_value_list, input_index = 0 ):
        ispec = self.inputSpecs[ input_index ] 
        return ispec.getDataValues( image_value_list )  
        
    def getImageValue( self, data_value, input_index = 0 ):
        ispec = self.inputSpecs[ input_index ] 
        return ispec.getImageValue( data_value )  
    
    def getImageValues( self, data_value_list, input_index = 0 ):
        ispec = self.inputSpecs[ input_index ] 
        return ispec.getImageValues( data_value_list )  

    def scaleToImage( self, data_value, input_index = 0 ):
        ispec = self.inputSpecs[ input_index ] 
        return ispec.scaleToImage( data_value )  

#     def finalizeLeveling( self, cmap_index=0 ):
#         ispec = self.inputSpecs[ cmap_index ] 
#         ispec.addMetadata( { 'colormap' : self.getColormapSpec(), 'orientation' : self.iOrientation } ) 
#         return DV3DPlot.finalizeLeveling( self, cmap_index=0 )
 

    def initializeConfiguration( self, cmap_index=0, **args ):
        ispec = self.inputSpecs[ cmap_index ] 
        args['units'] = ispec.units
        self.buttonBarHandler.initializeConfigurations( **args )
        ispec.addMetadata( { 'colormap' : self.getColormapSpec(), 'orientation' : self.iOrientation } ) 
#        self.updateSliceOutput()


    def getMapOpacity(self):
        return self.map_opacity
    
    def setMapOpacity(self, opacity_vals, **args ):
        self.map_opacity = opacity_vals
        self.updateMapOpacity() 

    def updateMapOpacity(self, cmap_index=0 ):
        if self.baseMapActor:
            self.baseMapActor.SetOpacity( self.map_opacity[0] )
            self.render()

    def showInteractiveLens(self): 
        pass

    def updateLensDisplay(self, screenPos, coord):
        pass
           
    def buildBaseMap(self):
        if self.baseMapActor <> None: self.renderer.RemoveActor( self.baseMapActor )               
        world_map =  None  
        map_border_size = 20 
            
        self.y0 = -90.0  
        self.x0 =  0.0  
        dataPosition = None
        if world_map == None:
            self.map_file = defaultMapFile
            self.map_cut = defaultMapCut
        else:
            self.map_file = world_map[0].name
            self.map_cut = world_map[1]
        
        self.world_cut =  -1 
        if  (self.roi <> None): 
            roi_size = [ self.roi[1] - self.roi[0], self.roi[3] - self.roi[2] ] 
            scale = (self.roi[1] - self.roi[0])/300.0
            border_size = map_border_size * scale
            map_cut_size = [ roi_size[0] + 2*border_size, roi_size[1] + 2*border_size ]
            if map_cut_size[0] > 360.0: map_cut_size[0] = 360.0
            if map_cut_size[1] > 180.0: map_cut_size[1] = 180.0
        else:
            map_cut_size = [ 360, 180 ]
            
                  
        if self.world_cut == -1: 
            if  (self.roi <> None): 
                if roi_size[0] > 180:             
                    self.ComputeCornerPosition()
                    self.world_cut = self.NormalizeMapLon( self.x0 )
                else:
                    dataPosition = [ ( self.roi[1] + self.roi[0] ) / 2.0, ( self.roi[3] + self.roi[2] ) / 2.0 ]
            else:
                dataPosition = [ 180, 0 ] # [ ( self.roi[1] + self.roi[0] ) / 2.0, ( self.roi[3] + self.roi[2] ) / 2.0 ]
        else:
            self.world_cut = self.map_cut
        
        self.imageInfo = vtk.vtkImageChangeInformation()        
        image_reader = vtk.vtkJPEGReader()      
        image_reader.SetFileName(  self.map_file )
        image_reader.Update()
        baseImage = image_reader.GetOutput() 
        new_dims, scale = None, None
        if dataPosition == None: 
            old_dims = baseImage.GetDimensions()   
            baseImage = self.RollMap( baseImage ) 
            new_dims = baseImage.GetDimensions()
            scale = [ 360.0/new_dims[0], 180.0/new_dims[1], 1 ]
        else:                       
            baseImage, new_dims = self.getBoundedMap( baseImage, dataPosition, map_cut_size, border_size )             
            scale = [ map_cut_size[0]/new_dims[0], map_cut_size[1]/new_dims[1], 1 ]
                          
        self.baseMapActor = vtk.vtkImageActor()
        self.baseMapActor.SetOrigin( 0.0, 0.0, 0.0 )
        self.baseMapActor.SetScale( scale )
        self.baseMapActor.SetOrientation( 0.0, 0.0, 0.0 )
        self.baseMapActor.SetOpacity( self.map_opacity[0] )
        mapCorner = [ self.x0, self.y0 ]
                
        self.baseMapActor.SetPosition( mapCorner[0], mapCorner[1], 0.1 )
        if vtk.VTK_MAJOR_VERSION <= 5:  self.baseMapActor.SetInput(baseImage)
        else:                           self.baseMapActor.SetInputData(baseImage)        
        self.mapCenter = [ self.x0 + map_cut_size[0]/2.0, self.y0 + map_cut_size[1]/2.0 ]        
        self.renderer.AddActor( self.baseMapActor )


    def ComputeCornerPosition( self ):
        if (self.roi[0] >= -180) and (self.roi[1] <= 180) and (self.roi[1] > self.roi[0]):
            self.x0 = -180
            return 180
        if (self.roi[0] >= 0) and (self.roi[1] <= 360) and (self.roi[1] > self.roi[0]):
            self.x0 = 0
            return 0
        self.x0 = int( round( self.roi[0] / 10.0 ) ) * 10
#        print "Set Corner pos: %s, roi: %s " % ( str(self.x0), str(self.roi) )
        
    def GetScaling( self, image_dims ):
        return 360.0/image_dims[0], 180.0/image_dims[1],  1

    def GetFilePath( self, cut ):
        filename = "%s_%d.jpg" % ( self.world_image, cut )
        return os.path.join( self.data_dir, filename ) 
        
    def RollMap( self, baseImage ):
#        baseImage.Update()
        if self.world_cut  == self.map_cut: return baseImage
        baseExtent = baseImage.GetExtent()
        baseSpacing = baseImage.GetSpacing()
        x0 = baseExtent[0]
        x1 = baseExtent[1]
        newCut = self.NormalizeMapLon( self.world_cut )
        delCut = newCut - self.map_cut
#        print "  %%%%%% Roll Map %%%%%%: world_cut=%.1f, map_cut=%.1f, newCut=%.1f " % ( float(self.world_cut), float(self.map_cut), float(newCut) )
        imageLen = x1 - x0 + 1
        sliceSize =  imageLen * ( delCut / 360.0 )
        sliceCoord = int( round( x0 + sliceSize) )        
        extent = list( baseExtent ) 
        
        extent[0:2] = [ x0, x0 + sliceCoord - 1 ]
        clip0 = vtk.vtkImageClip()
        if vtk.VTK_MAJOR_VERSION <= 5:  clip0.SetInput( baseImage )
        else:                           clip0.SetInputData( baseImage )                
        clip0.SetOutputWholeExtent( extent[0], extent[1], extent[2], extent[3], extent[4], extent[5] )
        
        extent[0:2] = [ x0 + sliceCoord, x1 ]
        clip1 = vtk.vtkImageClip()
        if vtk.VTK_MAJOR_VERSION <= 5:  clip1.SetInput( baseImage )
        else:                           clip1.SetInputData( baseImage )                
        clip1.SetOutputWholeExtent( extent[0], extent[1], extent[2], extent[3], extent[4], extent[5] )
        
        append = vtk.vtkImageAppend()
        append.SetAppendAxis( 0 )
        append.SetInputConnection ( clip1.GetOutputPort() )
        append.AddInputConnection ( clip0.GetOutputPort() )           
        
        imageInfo = vtk.vtkImageChangeInformation()
        imageInfo.SetInputConnection( append.GetOutputPort() ) 
        imageInfo.SetOutputOrigin( 0.0, 0.0, 0.0 )
        imageInfo.SetOutputExtentStart( 0, 0, 0 )
        imageInfo.SetOutputSpacing( baseSpacing[0], baseSpacing[1], baseSpacing[2] )
        
        imageInfo.Update()
        result = imageInfo.GetOutput() 
        return result

    def NormalizeMapLon( self, lon ): 
        while ( lon < ( self.map_cut - 0.01 ) ): lon = lon + 360
        return ( ( lon - self.map_cut ) % 360 ) + self.map_cut

    def getBoundedMap( self, baseImage, dataLocation, map_cut_size, map_border_size ):
        baseImage.Update()
        baseExtent = baseImage.GetExtent()
        baseSpacing = baseImage.GetSpacing()
        x0 = baseExtent[0]
        x1 = baseExtent[1]
        y0 = baseExtent[2]
        y1 = baseExtent[3]
        imageLen = [ x1 - x0 + 1, y1 - y0 + 1 ]
        selectionDim = [ map_cut_size[0]/2, map_cut_size[1]/2 ]
        dataXLoc = dataLocation[0]
        imageInfo = vtk.vtkImageChangeInformation()
        dataYbounds = [ dataLocation[1]-selectionDim[1], dataLocation[1]+selectionDim[1] ]
        vertExtent = [ y0, y1 ]
        bounded_dims = None
        if dataYbounds[0] > -90.0:
            yOffset = dataYbounds[0] + 90.0
            extOffset = int( round( ( yOffset / 180.0 ) * imageLen[1] ) )
            vertExtent[0] = y0 + extOffset
            self.y0 = dataYbounds[0]
        if dataYbounds[1] < 90.0:
            yOffset = 90.0 - dataYbounds[1]
            extOffset = int( round( ( yOffset / 180.0 ) * imageLen[1] ) )
            vertExtent[1] = y1 - extOffset
            
        overlapsBorder = ( self.NormalizeMapLon(dataLocation[0]-selectionDim[0]) > self.NormalizeMapLon(dataLocation[0]+selectionDim[0]) )
        if overlapsBorder:
            cut0 = self.NormalizeMapLon( dataXLoc + selectionDim[0] )
            sliceSize =  imageLen[0] * ( ( cut0 - self.map_cut ) / 360.0 )
            sliceCoord = int( round( x0 + sliceSize) )        
            extent = list( baseExtent )         
            extent[0:2] = [ x0, x0 + sliceCoord - 1 ]
            clip0 = vtk.vtkImageClip()
            clip0.SetInput( baseImage )
            clip0.SetOutputWholeExtent( extent[0], extent[1], vertExtent[0], vertExtent[1], extent[4], extent[5] )
            size0 = extent[1] - extent[0] + 1
        
            self.x0 = dataLocation[0] - selectionDim[0]
            cut1 = self.NormalizeMapLon( self.x0 ) 
            sliceSize =  imageLen[0] * ( ( cut1 - self.map_cut )/ 360.0 )
            sliceCoord = int( round( x0 + sliceSize) )       
            extent[0:2] = [ x0 + sliceCoord, x1 ]
            clip1 = vtk.vtkImageClip()
            clip1.SetInput( baseImage )
            clip1.SetOutputWholeExtent( extent[0], extent[1], vertExtent[0], vertExtent[1], extent[4], extent[5] )
            size1 = extent[1] - extent[0] + 1
#            print "Set Corner pos: %s, cuts: %s " % ( str(self.x0), str( (cut0, cut1) ) )
        
            append = vtk.vtkImageAppend()
            append.SetAppendAxis( 0 )
            append.AddInput( clip1.GetOutput() )          
            append.AddInput( clip0.GetOutput() )
            bounded_dims = ( size0 + size1, vertExtent[1] - vertExtent[0] + 1 )
            
            imageInfo.SetInputConnection( append.GetOutputPort() ) 

        else:
                        
            self.x0 = dataXLoc - selectionDim[0]
            cut0 = self.NormalizeMapLon( self.x0 )
            sliceSize =  imageLen[0] * ( ( cut0 - self.map_cut ) / 360.0 )
            sliceCoord = int( round( x0 + sliceSize) )        
            extent = list( baseExtent )         
            extent[0] = x0 + sliceCoord - 1
        
            cut1 = self.NormalizeMapLon( dataXLoc + selectionDim[0] )
            sliceSize =  imageLen[0] * ( ( cut1 - self.map_cut ) / 360.0 )
            sliceCoord = int( round( x0 + sliceSize) )       
            extent[1] = x0 + sliceCoord
            clip = vtk.vtkImageClip()
            clip.SetInput( baseImage )
            clip.SetOutputWholeExtent( extent[0], extent[1], vertExtent[0], vertExtent[1], extent[4], extent[5] )
            bounded_dims = ( extent[1] - extent[0] + 1, vertExtent[1] - vertExtent[0] + 1 )
#            print "Set Corner pos: %s, dataXLoc: %s " % ( str(self.x0), str( (dataXLoc, selectionDim[0]) ) )

            imageInfo.SetInputConnection( clip.GetOutputPort() ) 
                       
        imageInfo.SetOutputOrigin( 0.0, 0.0, 0.0 )
        imageInfo.SetOutputExtentStart( 0, 0, 0 )
        imageInfo.SetOutputSpacing( baseSpacing[0], baseSpacing[1], baseSpacing[2] )
        
        result = imageInfo.GetOutput() 
        result.Update()
        return result, bounded_dims

    def init(self, **args ):
        init_args = args[ 'init' ]      
        n_cores = args.get( 'n_cores', 32 )    
        lut = self.getLUT()
        self.variable_reader = StructuredDataReader( init_specs=init_args, **args )
        self.variable_reader.execute( )       
        self.createRenderer( **args )
        interface = init_args[2]
        self.execute( )
        self.initializePlots()
        self.initCamera( 700.0 )
        self.start()

    def gminit( self, var1, var2, **args ): 
        var_list = [ var1 ]
        if id(var2) <> id(None): var_list.append( var2 )
        self.variable_reader = StructuredDataReader( vars=var_list, otype=self.type, **args )
        self.variable_reader.execute( )       
        self.createRenderer( **args )
        self.execute( )
        self.initializePlots()
        self.initCamera( 700.0 )
        self.start()

    def onResizeEvent(self):
        self.updateTextDisplay( None, True )
        
    def updateTextDisplay( self, text=None, render=False ):
        if text <> None:
            metadata = self.getMetadata()
            var_name = metadata.get( 'var_name', '')
            var_units = metadata.get( 'var_units', '')
            self.labelBuff = "%s (%s)\n%s" % ( var_name, var_units, str(text) )
        DV3DPlot.updateTextDisplay( self, None, render )
            
    def toggleClipping(self, clipping_on ):
        if clipping_on:   self.clipOn()
        else:             self.clipOff()
        
    def clipOn(self):
        self.clipper.On()
        self.executeClip()

    def clipOff(self):
        self.clipper.Off()      
        
        
