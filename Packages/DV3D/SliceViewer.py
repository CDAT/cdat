'''
Created on May 14, 2014

@author: tpmaxwel
'''

'''
Created on Apr 23, 2014

@author: tpmaxwell
'''
    
import sys, vtk, cdms2, traceback, os, cdtime 
from ColorMapManager import *  
from ImagePlaneWidget import ImagePlaneWidget   
from DistributedPointCollections import kill_all_zombies
from StructuredGridPlot import  *
 
class SlicePlot(StructuredGridPlot): 

    global_coords = [-1, -1, -1]
    
    def __init__( self, **args ):
        StructuredGridPlot.__init__( self, **args  )
#         self.addUVCDATConfigGuiFunction( 'contourColormap', ColormapConfigurationDialog, 'K', label='Choose Contour Colormap', setValue=lambda data: self.setColormap(data,1) , getValue=lambda: self.getColormap(1), layerDependent=True, isValid=self.hasContours, group=ConfigGroup.Color )
        self.sliceOutputShape = args.get( 'slice_shape', [ 100, 50 ] )
        self.polygonActor = None
        self.opacity = [ 1.0, 1.0 ]
        self.updatingPlacement = False
        self.isSlicing = False
        self.planeWidgetX = None
        self.planeWidgetY = None
        self.planeWidgetZ = None
        self.opacityUpdateCount = 0
        self.generateContours = False
        self.contourLineActors = {}
        self.contourLineMapperer = None
        self.contours = None
        self.NumContours = 10.0
        self.showOutlineMap = True
        self.zincSkipIndex = 1

        self.addConfigurableSliderFunction( 'colorScale', 'C', label='Colormap Scale', interactionHandler=self.processColorScaleCommand )
        self.addConfigurableSliderFunction( 'opacityScale', 'o', label='Opacity Scale', range_bounds=[ 0.0, 1.0 ], initValue=[ 1.0, 1.0 ], interactionHandler=self.processOpacityScalingCommand )
        self.addConfigurableSliderFunction( 'xSlider', 'x', sliderLabels='X Slice Position', label="Slicing", position=[0,3], interactionHandler=self.processSlicingCommand )
        self.addConfigurableSliderFunction( 'ySlider', 'y', sliderLabels='Y Slice Position', label="Slicing", position=[1,3], interactionHandler=self.processSlicingCommand )
        self.addConfigurableSliderFunction( 'zSlider', 'z', sliderLabels='Z Slice Position', label="Slicing", position=[2,3], interactionHandler=self.processSlicingCommand )

#         self.addConfigurableLevelingFunction( 'colorScale', 'C', label='Colormap Scale', units='data', setLevel=self.scaleColormap, getLevel=self.getDataRangeBounds, layerDependent=True, adjustRangeInput=0, group=ConfigGroup.Color )
#         self.addConfigurableLevelingFunction( 'opacity', 'O', label='Slice Plane Opacity', rangeBounds=[ 0.0, 1.0 ],  setLevel=self.setOpacity, activeBound='min',  getLevel=self.getOpacity, isDataValue=False, layerDependent=True, bound = False, group=ConfigGroup.Rendering )
#         self.addConfigurableLevelingFunction( 'contourDensity', 'g', label='Contour Density', activeBound='max', setLevel=self.setContourDensity, getLevel=self.getContourDensity, layerDependent=True, windowing=False, rangeBounds=[ 3.0, 30.0, 1 ], bound=False, isValid=self.hasContours, group=ConfigGroup.Rendering )
#         self.addConfigurableLevelingFunction( 'contourColorScale', 'S', label='Contour Colormap Scale', units='data', setLevel=self.scaleContourColormap, getLevel=lambda:self.getDataRangeBounds(1), layerDependent=True, adjustRangeInput=1, isValid=self.hasContours, group=ConfigGroup.Color )
#         self.addConfigurableLevelingFunction( 'coastlines_Line', 'm0', label='Coastline Line', setLevel=self.setBasemapCoastlineLineSpecs, getLevel=self.getBasemapCoastlineLineSpecs, sliderLabels=[ 'Thickness', 'Density' ], layerDependent=False, rangeBounds=[ 0.0, 3.49 ], initRange=[ 1.0, 1.0, 1 ], group=ConfigGroup.BaseMap )
#         self.addConfigurableLevelingFunction( 'countries_Line', 'm1', label='Countries Line', setLevel=self.setBasemapCountriesLineSpecs, getLevel=self.getBasemapCountriesLineSpecs, sliderLabels=[ 'Thickness', 'Density' ], layerDependent=False, rangeBounds=[ 0.0, 3.49 ], initRange=[ 0.0, 1.0, 0 ], group=ConfigGroup.BaseMap )
#         self.addConfigurableLevelingFunction( 'states_Line', 'm2', label='States Line', setLevel=self.setBasemapStatesLineSpecs, getLevel=self.getBasemapStatesLineSpecs, sliderLabels=[ 'Thickness', 'Density' ], layerDependent=False, rangeBounds=[ 0.0, 3.49 ], initRange=[ 0.0, 1.0, 0 ], group=ConfigGroup.BaseMap )
#         self.addConfigurableLevelingFunction( 'lakes_Line', 'm3', label='Lakes Line', setLevel=self.setBasemapLakesLineSpecs, getLevel=self.getBasemapLakesLineSpecs, sliderLabels=[ 'Thickness', 'Density' ], layerDependent=False, rangeBounds=[ 0.0, 3.49 ], initRange=[ 0.0, 1.0, 0 ], group=ConfigGroup.BaseMap )
# 
#     def activateWidgets( self, iren ):
#         if self.baseMapActor:
#             bounds = [ 0.0 ]*6
#             self.baseMapActor.GetBounds( bounds )

    def onKeyEvent(self, eventArgs ):
        key = eventArgs[0]
        md = self.getInputSpec().getMetadata()
        return StructuredGridPlot.onKeyEvent( self, eventArgs )

    def processSlicingCommand( self, args, config_function = None ):
        plane_widget = self.getPlaneWidget( config_function.key )
        slicePosition = config_function.value
        if args and args[0] == "StartConfig":
            pass
        elif args and args[0] == "Init":
            primaryInput = self.input()
            bounds = list( primaryInput.GetBounds() ) 
            bindex = 2*config_function.position[0]
            init_range = [ bounds[bindex], bounds[bindex+1] ]
            config_function.range_bounds = init_range  
            config_function.initial_value = init_range[0] 
            slicePosition.setValues( init_range ) 
            plane_widget.SetSlicePosition( config_function.initial_value )
        elif args and args[0] == "EndConfig":
            pass
        elif args and args[0] == "InitConfig":
            self.updateTextDisplay( config_function.label )
            self.modifySlicePlaneVisibility( config_function.key )                       
            self.render() 
        elif args and args[0] == "Open":
            pass
        elif args and args[0] == "Close":
            pass
        elif args and args[0] == "UpdateConfig":
#            print "Process Slicing Command: ", str( args )
            plane_widget.SetSlicePosition( args[2] )

    def processOpacityScalingCommand( self, args, config_function = None ):
        opacityRange = config_function.value
        if args and args[0] == "StartConfig":
            pass
        elif args and args[0] == "Init":
            self.setOpacity( config_function.initial_value )
        elif args and args[0] == "EndConfig":
            pass
        elif args and args[0] == "InitConfig":
            self.updateTextDisplay( config_function.label )
        elif args and args[0] == "Open":
            pass
        elif args and args[0] == "Close":
            pass
        elif args and args[0] == "UpdateConfig":
            opacityRange.setValue( args[1], args[2] )
            orange = opacityRange.getValues()
            self.setOpacity( orange )

    def processColorScaleCommand( self, args, config_function = None ):
        colorScaleRange = config_function.value
        if args and args[0] == "StartConfig":
            pass
        elif args and args[0] == "Init":
            init_range = self.getDataRangeBounds()
            config_function.range_bounds = init_range  
            config_function.initial_value = init_range  
            self.scaleColormap( init_range )
            colorScaleRange.setValues( init_range )
        elif args and args[0] == "EndConfig":
            pass
        elif args and args[0] == "InitConfig":
            self.updateTextDisplay( config_function.label )
        elif args and args[0] == "Open":
            pass
        elif args and args[0] == "Close":
            pass
        elif args and args[0] == "UpdateConfig":
            colorScaleRange.setValue( args[1], args[2] )
            cscale = colorScaleRange.getValues()
            self.scaleColormap( cscale )

       
    def clearReferrents(self):
        print " **************************************** VolumeSlicer:clearReferrents, id = %d  **************************************** " % self.moduleID
        sys.stdout.flush()
        del self.planeWidgetX
        del self.planeWidgetY
        del self.planeWidgetZ
        self.planeWidgetX = None
        self.planeWidgetY = None
        self.planeWidgetZ = None
        self.latLonGrid = True
        del self.sliceOutput
        self.sliceOutput = None 
        if self.contours:
            del self.contours
            self.contours = None    
            del self.contourLineMapperer 
            self.contourLineMapperer = None
        ispec = self.getInputSpec( 0 ) 
        input0 = ispec.input() 
        print " VolumeSlicer: Input refs = %d " % input0.GetReferenceCount()
        sys.stdout.flush()

        
    def scaleContourColormap(self, data, **args ):
        return self.scaleColormap( data, 1, **args )
        
    def hasContours(self):
        return self.generateContours
        
    def setContourDensity( self, ctf_data, **args ):
        if self.NumContours <> ctf_data[1]:
            self.NumContours = ctf_data[1]
            self.updateContourDensity()

    def getContourDensity( self ):
        return [ 3.0, self.NumContours, 1 ]
    
    def setZScale( self, zscale_data, **args ):
        self.setInputZScale( zscale_data, **args )
        if self.planeWidgetX <> None:
            primaryInput = self.input()
            bounds = list( primaryInput.GetBounds() ) 
            if not self.planeWidgetX.MatchesBounds( bounds ):
                self.planeWidgetX.PlaceWidget( bounds )        
                self.planeWidgetY.PlaceWidget( bounds ) 
                self.render()               

    def setInputZScale( self, zscale_data, **args  ):
        StructuredGridPlot.setInputZScale( self, zscale_data, **args  )
        ispec = self.getInputSpec(  1 )       
        if (ispec <> None) and (ispec.input() <> None):
            contourInput = ispec.input() 
            ix, iy, iz = contourInput.GetSpacing()
            sz = zscale_data[1]
            contourInput.SetSpacing( ix, iy, sz )  
            contourInput.Modified() 

              
    def getOpacity(self):
        return self.opacity
    
    def setOpacity(self, range, **args ):
        self.opacity = range
#        printArgs( " Leveling: ", opacity=self.opacity, range=range ) 
        self.updateOpacity() 

    def updateOpacity(self, cmap_index=0 ):
        colormapManager = self.getColormapManager( index=cmap_index )
        colormapManager.setAlphaRange( [ bound( self.opacity[i], [ 0.0, 1.0 ] ) for i in (0,1) ] )
        if (self.opacityUpdateCount % 5) == 0: self.render()
        self.opacityUpdateCount = self.opacityUpdateCount + 1  
#        self.lut.SetAlpha( self.opacity[1] )
#        self.lut.SetAlphaRange ( self.opacity[0], self.opacity[1] )
#        print "  ---> Set Opacity = %s " % str( self.opacity )
#        self.UpdateWidgetPlacement()
        
#    def UpdateWidgetPlacement(self):
#        self.updatingPlacement = True
#        self.planeWidgetX.UpdatePlacement() 
#        self.planeWidgetX.PlaceWidget()
#        self.planeWidgetY.UpdatePlacement() 
#        self.planeWidgetY.PlaceWidget()
#        self.planeWidgetZ.UpdatePlacement() 
#        self.planeWidgetZ.PlaceWidget()
#        self.updatingPlacement = False

    def enableVisualizationInteraction(self): 
#        print>>sys.stderr, "enable Visualization Interaction"
        if self.planeWidgetX <> None: self.planeWidgetX.EnableInteraction()                                                
        if self.planeWidgetY <> None:self.planeWidgetY.EnableInteraction()                                                
        if self.planeWidgetZ <> None:self.planeWidgetZ.EnableInteraction()  

    def disableVisualizationInteraction(self):
#        print>>sys.stderr, "disable Visualization Interaction" 
        if self.planeWidgetX <> None: self.planeWidgetX.DisableInteraction()                                                
        if self.planeWidgetY <> None:self.planeWidgetY.DisableInteraction()                                                
        if self.planeWidgetZ <> None:self.planeWidgetZ.DisableInteraction()  

    def updatingColormap( self, cmap_index, colormapManager ):
        if cmap_index == 0:
            if self.planeWidgetX <> None: self.planeWidgetX.SetTextureInterpolate( colormapManager.smoothColormap )
            if self.planeWidgetY <> None: self.planeWidgetY.SetTextureInterpolate( colormapManager.smoothColormap )
            if self.planeWidgetZ <> None: self.planeWidgetZ.SetTextureInterpolate( colormapManager.smoothColormap )
            self.updateModule()
            
    def getPlaneWidget( self, plane ):       
        if plane == 'x': return self.planeWidgetX
        if plane == 'y': return self.planeWidgetY
        if plane == 'z': return self.planeWidgetZ
        return None 
    
    def modifySlicePlaneVisibility( self, plane, make_visible=None ):
        plane_widget = self.getPlaneWidget( plane )
        if make_visible == None:  make_visible = not plane_widget.IsVisible()
        if make_visible:    plane_widget.VisibilityOn()
        else:               plane_widget.VisibilityOff()
        if plane == 'z':
            if make_visible:    self.basemapLinesVisibilityOn()
            else:               self.basemapLinesVisibilityOff()
                                                                        
    def buildPipeline(self):
        """ execute() -> None
        Dispatch the vtkRenderer to the actual rendering widget
        """     
#        contourModule = self.wmod.forceGetInputFromPort( "contours", None )
#        if self.input() == None:
#            if contourModule <> None:
#                self.input() = contourModule.getOutput() 
#            else:
#                print>>sys.stderr, "Error, must provide an input to the Volume Slicer module!"
 #       self.intersectInputExtents()
        contour_ispec = None # self.getInputSpec(  1 )       

        contourInput = contour_ispec.input() if contour_ispec <> None else None
        primaryInput = self.input()
        md = self.getInputSpec().getMetadata()
        self.latLonGrid = md.get( 'latLonGrid', True )

#        self.contourInput = None if contourModule == None else contourModule.getOutput() 
        # The 3 image plane widgets are used to probe the dataset.    
#        print " Volume Slicer buildPipeline, id = %s " % str( id(self) )
        self.sliceOutput = vtk.vtkImageData()  
        xMin, xMax, yMin, yMax, zMin, zMax = primaryInput.GetWholeExtent()       
        self.slicePosition = [ (xMax-xMin)/2, (yMax-yMin)/2, (zMax-zMin)/2  ]       
        dataType = primaryInput.GetScalarTypeAsString()
        bounds = list(primaryInput.GetBounds()) 
        origin = primaryInput.GetOrigin()
        if (dataType <> 'float') and (dataType <> 'double'):
             self.setMaxScalarValue( primaryInput.GetScalarType() )
#        print "Data Type = %s, range = (%f,%f), extent = %s, origin = %s, bounds=%s, slicePosition=%s" % ( dataType, self.rangeBounds[0], self.rangeBounds[1], str(self.input().GetWholeExtent()), str(origin), str(bounds), str(self.slicePosition)  )
      
        # The shared picker enables us to use 3 planes at one time
        # and gets the picking order right
        lut = self.getLut()
        picker = None
        useVtkImagePlaneWidget = False
        textureColormapManager = self.getColormapManager( index=0 )
        picker = vtk.vtkCellPicker()
        picker.SetTolerance(0.005) 
                
        if self.planeWidgetZ == None:
            self.planeWidgetZ = ImagePlaneWidget( self, picker, 2 )  
            self.planeWidgetZ.SetRenderer( self.renderer )
#            self.observerTargets.add( self.planeWidgetZ )
            prop3 = self.planeWidgetZ.GetPlaneProperty()
            prop3.SetColor(0, 0, 1)
            self.planeWidgetZ.SetUserControlledLookupTable(1)
            self.planeWidgetZ.SetLookupTable( lut )
       
        self.planeWidgetZ.SetInput( primaryInput, contourInput )
        self.planeWidgetZ.SetPlaneOrientationToZAxes()
        self.planeWidgetZ.PlaceWidget( bounds )
       
        if self.planeWidgetZ.HasThirdDimension(): 
            if (self.planeWidgetX == None): 
                self.planeWidgetX = ImagePlaneWidget( self, picker, 0 )
#               self.observerTargets.add( self.planeWidgetX )
                self.planeWidgetX.SetRenderer( self.renderer )
                prop1 = self.planeWidgetX.GetPlaneProperty()
                prop1.SetColor(1, 0, 0)
                self.planeWidgetX.SetUserControlledLookupTable(1)
                self.planeWidgetX.SetLookupTable( lut )
                
            self.planeWidgetX.SetInput( primaryInput, contourInput )
            self.planeWidgetX.SetPlaneOrientationToXAxes()
            self.planeWidgetX.PlaceWidget( bounds )     
                    
            if self.planeWidgetY == None: 
                self.planeWidgetY = ImagePlaneWidget( self, picker, 1)
                self.planeWidgetY.SetRenderer( self.renderer )
                self.planeWidgetY.SetUserControlledLookupTable(1)
#                self.observerTargets.add( self.planeWidgetY )
                prop2 = self.planeWidgetY.GetPlaneProperty()
                prop2.SetColor(1, 1, 0)
                self.planeWidgetY.SetUserControlledLookupTable(1)
                self.planeWidgetY.SetLookupTable( lut )
            
            self.planeWidgetY.SetInput( primaryInput, contourInput )
            self.planeWidgetY.SetPlaneOrientationToYAxes()       
            self.planeWidgetY.PlaceWidget(  bounds  )     

        self.renderer.SetBackground( VTK_BACKGROUND_COLOR[0], VTK_BACKGROUND_COLOR[1], VTK_BACKGROUND_COLOR[2] )
        self.updateOpacity() 
        self.setColormap( [ 'jet', 1, 0, 0 ] )
        
        if (contour_ispec <> None) and (contour_ispec.input() <> None) and (self.contours == None):
            rangeBounds = self.getRangeBounds(1)
            colormapManager = self.getColormapManager( index=1 )
            self.generateContours = True   
            self.contours = vtk.vtkContourFilter()
            self.contours.GenerateValues( self.NumContours, rangeBounds[0], rangeBounds[1] )
     
            self.contourLineMapperer = vtk.vtkPolyDataMapper()
            self.contourLineMapperer.SetInputConnection( self.contours.GetOutputPort() )
            self.contourLineMapperer.SetScalarRange( rangeBounds[0], rangeBounds[1] )
            self.contourLineMapperer.SetColorModeToMapScalars()
            self.contourLineMapperer.SetLookupTable( colormapManager.lut )
            self.contourLineMapperer.UseLookupTableScalarRangeOn()
        else:
            self.setBasemapCoastlineLineSpecs( [ 1, 1 ] )
            self.setBasemapCountriesLineSpecs( [ 0, 1 ] )
         
        self.modifySlicePlaneVisibility( 'x', False )   
        self.modifySlicePlaneVisibility( 'y', False )   
        self.modifySlicePlaneVisibility( 'z', False )  
         
        self.onKeyEvent( [ 'x', 'x' ] )
        self.render()

#        self.set3DOutput() 

        # Add the times series only in regular volume slicer and not in Hovmoller Slicer
#         if self.getInputSpec().getMetadata()['plotType']=='xyz':
#             self.addConfigurableFunction('Show Time Series', None, 't' )

    def buildOutlineMap(self):
        # This function load a binary image (black and white)
        # and create a default grid for it. Then it uses re-gridding algorithms 
        # to scale in the correct domain.
        from pylab import imread
        import vtk.util.vtkImageImportFromArray as vtkUtil

        # read outline image and convert to gray scale
        try:
            data = imread(defaultOutlineMapFile)
            data = data.mean(axis=2)
    
    #        # create a variable using the data loaded in the image and an uniform grid
            dims = data.shape
            reso = [180.0/dims[0], 360.0/dims[1]]
            var = cdms2.createVariable(data)
            lat = cdms2.createUniformLatitudeAxis(90, dims[0], -reso[0])
            lon = cdms2.createUniformLongitudeAxis(-180, dims[1], reso[1])
            var.setAxis(0, lat)
            var.setAxis(1, lon)
    
            # create the final map using the ROI
            ROI = self.roi[:]
            if ROI[2] < -90.0: ROI[2] = -90.0
            if ROI[3] >  90.0: ROI[3] =  90.0
            odims = [ (ROI[3]-ROI[2])/reso[0] , (ROI[1]-ROI[0])/reso[1] ]
            ogrid = cdms2.createUniformGrid( ROI[2], odims[0], reso[0], ROI[0], odims[1], reso[1] )
            ovar = var.regrid(ogrid, regridTool='regrid2')
            
            # replace outlier numbers
            d = ovar.data
            d[d==1e+20] = d[d<>1e+20].max()
            
            img = vtkUtil.vtkImageImportFromArray()
            img.SetArray(ovar.data)
            img.Update()
            
        except Exception:
            print>>sys.stderr, "Error building Outline Map"
            traceback.print_exc()
            return None
        
        # convert to vtkImageData       
        return img.GetOutput()

                
    def updateContourDensity(self):
        if self.generateContours:
            rangeBounds = self.getRangeBounds(1)
            self.contours.GenerateValues( self.NumContours, rangeBounds[0], rangeBounds[1] )
            self.contours.Modified()
            self.render()
                        
    def updateModule(self, **args ):
        primaryInput = self.input()
        contour_ispec = self.getInputSpec(  1 )       
        contourInput = contour_ispec.input() if contour_ispec <> None else None
        if self.planeWidgetX <> None: self.planeWidgetX.SetInput( primaryInput, contourInput )         
        if self.planeWidgetY <> None: self.planeWidgetY.SetInput( primaryInput, contourInput )         
        if self.planeWidgetZ <> None: self.planeWidgetZ.SetInput( primaryInput, contourInput ) 
        if self.baseMapActor: self.baseMapActor.SetVisibility( int( self.enableBasemap ) )
        self.render()
#        self.set3DOutput()
           
    def TestObserver( self, caller=None, event = None ):
        print " Volume Slicer TestObserver: event = %s, " % ( event )
    
    def getLayerColor( self, type ):
        if type == 'coastline':
            return ( 0, 0, 0 )
        if type == 'countries':
            return ( 0.7, 0.2, 0.2 )
        if type == 'states':
            return ( 0.5, 0.5, 0.3 )
        if type == 'lakes':
            return ( 0, 0, 0.6 )
        return ( 0, 0, 0 )
            
    def ProcessIPWAction( self, caller, event, **args ):
        action = args.get( 'action', caller.State )
        iAxis = caller.PlaneIndex

        if event == ImagePlaneWidget.InteractionUpdateEvent:
            
            if action == ImagePlaneWidget.Cursoring:   
                if not self.isSlicing:
                    self.isSlicing = True
                ispec = self.inputSpecs[ 0 ] 
                cursor_data = caller.GetCursorData()
                image_value = cursor_data[3] 
                cpos = cursor_data[0:3]     
                dataValue = ispec.getDataValue( image_value )
                wpos = ispec.getWorldCoords( cpos )
                if self.generateContours:
                    contour_image_value = cursor_data[4]
                    if  contour_image_value:
                        contour_value = self.getDataValue( contour_image_value, 1 )
                        contour_units = self.getUnits(1)
                        textDisplay = " Position: (%s, %s, %s), Value: %.3G %s, Contour Value: %.3G %s" % ( wpos[0], wpos[1], wpos[2], dataValue, ispec.units, contour_value, contour_units )
                    else:
                        textDisplay = " Position: (%s, %s, %s), Value: %.3G %s" % ( wpos[0], wpos[1], wpos[2], dataValue, ispec.units )
#                        print " >>>>> Current Image Value: %d %d, contour value: %.3G, pos = %s " % ( image_value, contour_image_value,  contour_value, str(cpos) ) # , str(wpos) )
                else:
                    textDisplay = " Position: (%s, %s, %s), Value: %.3G %s." % ( wpos[0], wpos[1], wpos[2], dataValue, ispec.units )
#                    print " >>>>> Current Image Value: %d,  pos = %s " % ( image_value, str(cpos) ) # , str(wpos) )
                sliceIndex = caller.GetSliceIndex() 
                self.slicePosition[iAxis] = sliceIndex
                self.updateTextDisplay( textDisplay )
                
                coord = ispec.getWorldCoordsAsFloat(cpos)
                screenPos = caller.GetCurrentScreenPosition()
                self.updateLensDisplay(screenPos, coord)
                
            if action == ImagePlaneWidget.Pushing: 
                ispec = self.inputSpecs[ 0 ] 
                if not self.isSlicing:
                    self.isSlicing = True 
                sliceIndex = caller.GetSliceIndex() 
                axisName, spos = ispec.getWorldCoord( sliceIndex, iAxis, True )
                textDisplay = " %s = %s ." % ( axisName, spos )
                if iAxis == 0:
                    p1 = caller.GetPoint1()
#                    print " >++++++++++++++++++> Slicing: Set Slice[%d], index=%d, pos=%.2f, " % ( iAxis, sliceIndex, p1[0] ), textDisplay
                self.slicePosition[ iAxis ] = sliceIndex                  
                self.updateTextDisplay( textDisplay ) 

                if (iAxis == 2):              
                    origin = caller.GetOrigin()
                    for type in ( 'coastline', 'countries', 'states', 'lakes' ): 
                        line_specs = self.basemapLineSpecs.get( type, None )
                        polys_list = self.shapefilePolylineActors.get( type, None )
                        density = int( round( line_specs[1] ) ) if line_specs else 1
                        polys = polys_list[ density ] if polys_list else None
                        if polys:
                            pos = polys.GetPosition()
                            pos1 = [ pos[0], pos[1], origin[2] ]
                            polys.SetPosition( pos1 )
            
                if self.generateContours:
                    slice_data = caller.GetReslice2Output()
                    if slice_data:
                        slice_data.Update()    
                        iextent =  slice_data.GetExtent()            
                        ispacing =  slice_data.GetSpacing()            
                        self.contours.SetInput( slice_data )
                        self.contours.Modified()
                        origin = caller.GetOrigin()
                        contourLineActor = self.getContourActor( iAxis )
                        contourLineActor.SetPosition( origin[0], origin[1], origin[2] )
        #                contourLineActor.SetOrigin( origin[0], origin[1], origin[2] )
                        self.setVisibleContour( iAxis )
#                print " Generate Contours, data dims = %s, origin = %s, pos = %s, extent = %s" % ( str( slice_data.GetDimensions() ), str(slice_data.GetOrigin()), str(origin), str(slice_data.GetExtent()) )
                
            self.render()
#                print " Generate Contours, data dims = %s, pos = %s %s %s " % ( str( slice_data.GetDimensions() ), str(pos1), str(pos2), str(origin) )

    def setContourActorOrientation( self, iAxis, contourLineActor ):
        if iAxis == 1: 
            contourLineActor.SetOrientation(90,0,0)
        elif iAxis == 0: 
            contourLineActor.SetOrientation(90,0,90)   

    def updateContourActorOrientations( self ):
        for contourLineActorItem in self.contourLineActors.items():
            if contourLineActorItem[1].GetVisibility( ): 
                self.setContourActorOrientation( contourLineActorItem[0], contourLineActorItem[1] )
        self.render()
        pass

                                     
    def getContourActor( self, iAxis, **args ):
        contourLineActor = self.contourLineActors.get( iAxis, None )
        if contourLineActor == None:
            contourLineActor = vtk.vtkActor()
            contourLineActor.SetMapper(self.contourLineMapperer)
            contourLineActor.GetProperty().SetLineWidth(2)     
            self.renderer.AddActor( contourLineActor ) 
            self.contourLineActors[iAxis] = contourLineActor
            self.setContourActorOrientation( iAxis, contourLineActor )
#            print " GetContourActor %d, origin = %s, position = %s " % ( iAxis, str( contourLineActor.GetOrigin() ), str( contourLineActor.GetPosition() ) )
        return contourLineActor

            
    def setVisibleContour( self, iAxis ):
        for contourLineActorItem in self.contourLineActors.items():
            if iAxis == contourLineActorItem[0]:    contourLineActorItem[1].VisibilityOn( )
            else:                                   contourLineActorItem[1].VisibilityOff( )
       
    def getAdjustedSliceExtent( self ):
        ext = None
        if self.iOrientation == 1:      ext = [ 0, self.sliceOutputShape[1]-1,  0, self.sliceOutputShape[0]-1, 0, 0 ]  
        else:                           ext = [ 0, self.sliceOutputShape[0]-1,  0, self.sliceOutputShape[1]-1, 0, 0 ]  
#        print " Slice Extent = %s " % str( ext )
        return ext       

    def getAdjustedSliceSpacing( self, outputData ):
        padded_extent = outputData.GetWholeExtent()
        padded_shape = [ padded_extent[1]-padded_extent[0]+1, padded_extent[3]-padded_extent[2]+1, 1 ]
        padded_spacing = outputData.GetSpacing()
        scale_factor = [ padded_shape[0]/float(self.sliceOutputShape[0]), padded_shape[1]/float(self.sliceOutputShape[1]) ]
        if self.iOrientation == 1:      spacing = [ padded_spacing[1]*scale_factor[1], padded_spacing[0]*scale_factor[0], 1.0 ]
        else:                           spacing = [ padded_spacing[0]*scale_factor[0], padded_spacing[1]*scale_factor[1], 1.0 ]
#        print " Slice Spacing = %s " % str( spacing )
        return spacing
                       
    def initColorScale( self, caller, event ): 
        x, y = caller.GetEventPosition()
        self.ColorLeveler.startWindowLevel( x, y )

    def scaleColormap( self, ctf_data, cmap_index=0, **args ):
        ispec = self.inputSpecs.get( cmap_index, None )
        if ispec and ispec.input(): 
            colormapManager = self.getColormapManager( index=cmap_index )
#            if not colormapManager.matchDisplayRange( ctf_data ):
            imageRange = self.getImageValues( ctf_data[0:2], cmap_index ) 
            colormapManager.setScale( imageRange, ctf_data )
            if self.contourLineMapperer: 
                self.contourLineMapperer.Modified()
            ispec.addMetadata( { 'colormap' : self.getColormapSpec(), 'orientation' : self.iOrientation } )
#            print '-'*50
#            print " Volume Slicer[%d]: Scale Colormap: ( %.4g, %.4g ) " % ( self.moduleID, ctf_data[0], ctf_data[1] )
#            print '-'*50
                

    def updateColorScale( self, caller, event ):
        x, y = caller.GetEventPosition()
        wsize = self.renderer.GetSize()
        range = self.ColorLeveler.windowLevel( x, y, wsize )
        return range
                  
#     def onKeyPress( self, caller, event ):
#         key = caller.GetKeyCode() 
#         keysym = caller.GetKeySym()
#         print " -- Key Press: %s ( %s ), event = %s " % ( key, str(keysym), str( event ) )
#         if keysym == None: return
#         alt = ( keysym.lower().find('alt') == 0 )
#         ctrl = caller.GetControlKey() 
#         shift = caller.GetShiftKey() 
#        print " -- Key Press: %c ( %d: %s ), ctrl: %s, shift: %s, alt: %s, event = %s " % ( key, ord(key), str(keysym), bool(ctrl), bool(shift), bool(alt), str( event ) )
#        if ( key == 'x' ): 
#            self.planeWidgetX.SetPlaneOrientationToXAxes() 
#            self.planeWidgetX.SetSliceIndex( 0 ) #self.slicePosition[0] )
#            self.render()      
#        elif ( key == 'y' ):  
#            self.planeWidgetY.SetPlaneOrientationToYAxes()
#            self.planeWidgetY.SetSliceIndex( 0 ) #self.slicePosition[1] )
#            self.render()       
#        elif ( key == 'z' ):  
#            self.planeWidgetZ.SetPlaneOrientationToZAxes()
#            self.planeWidgetZ.SetSliceIndex( 0 ) #self.slicePosition[2] )
#            self.render() 



