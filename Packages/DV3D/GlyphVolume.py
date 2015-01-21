import vtk, sys

def getUnscaledWorldExtent( extent, spacing, origin ):
    return [ ( ( extent[ i ] * spacing[ i/2 ] ) + origin[i/2]  ) for i in range(6) ]

class GlyphVolumeWidget:

    def __init__( self,  **args ):
        self.glyphMapper = None
        self.glyphDecimationFactorBounds = [ 1.0, 20.0 ]
        self.glyphDecimationFactor = 3.0
        self.lowResGlyphDecimationFactor = 10.0
        self.hiResGlyphDecimationFactor = self.glyphDecimationFactor
        self.hiResGlyphScale = None
        self.nSkipCount = 4
        self.updateCount = 0
        self.UserControlledLookupTable= 0
        self.LookupTable  = 0

    def SetUserControlledLookupTable( self, value ):
        self.UserControlledLookupTable = value

    def SetLookupTable( self, table ):

        if (self.LookupTable <> table):
            self.LookupTable = table
            if (self.LookupTable == None): self.LookupTable = self.CreateDefaultLookupTable()
#            self.LookupTable.AddObserver( 'AnyEvent', self.LookupTableObserver )
#            print " Image Plane Widget %x: SetLookupTable: %x " % ( id(self), id( self.LookupTable ) )

        if( self.ImageData and  not self.UserControlledLookupTable):
            scalar_range = self.ImageData.GetScalarRange()
            self.LookupTable.SetTableRange(scalar_range[0],scalar_range[1])
            self.LookupTable.Build()

    def SetInput(self, inputData ):
        self.ImageData = inputData
        self.initialOrigin = self.ImageData.GetOrigin()
        self.initialExtent = self.ImageData.GetExtent()
        self.initialSpacing = self.ImageData.GetSpacing()
        self.dataBounds = getUnscaledWorldExtent( self.initialExtent, self.initialSpacing, self.initialOrigin )
        self.dataExtents = ( (self.dataBounds[1]-self.dataBounds[0])/2.0, (self.dataBounds[3]-self.dataBounds[2])/2.0, (self.dataBounds[5]-self.dataBounds[4])/2.0 )
        self.centroid = ( (self.dataBounds[0]+self.dataBounds[1])/2.0, (self.dataBounds[2]+self.dataBounds[3])/2.0, (self.dataBounds[4]+self.dataBounds[5])/2.0  )
        self.pos = [ self.initialSpacing[i]*self.initialExtent[2*i] for i in range(3) ]
        if ( (self.initialOrigin[0] + self.pos[0]) < 0.0): self.pos[0] = self.pos[0] + 360.0

        scalar_range = self.ImageData.GetScalarRange()

        if (  not self.UserControlledLookupTable ):
            self.LookupTable.SetTableRange( scalar_range[0], scalar_range[1] )
            self.LookupTable.Build()

        dims = self.ImageData.GetDimensions()
        self.InputDims = 3 if ( ( len(dims) > 2 ) and ( dims[2] > 1 ) ) else 2

        self.UpdateInputs()

    def SetPicker( self, picker):
        pass

    def initGlyphMapper(self):
        if self.glyphMapper == None:
#            scaleGlyphsByMagnitudeCF = CfgManager.getParameter( 'ScaleGlyphsByMagnitude' )
            self.scaleByMag = True # scaleGlyphsByMagnitudeCF.getInitValue( True )

            pointData = self.ImageData.GetPointData()
            vectorsArray = pointData.GetVectors()
            self.resample = vtk.vtkExtractVOI()
            if vtk.VTK_MAJOR_VERSION <= 5:  self.resample.SetInput(self.ImageData)
            else:                           self.resample.SetInputData(self.ImageData)
            self.resample.SetVOI( self.initialExtent )
            self.LookupTable.SetVectorModeToMagnitude()
            self.glyphMapper = vtk.vtkGlyph3DMapper()
#
            if self.scaleByMag:
                self.glyphMapper.SetScaleModeToScaleByMagnitude()
                self.glyphScaleBounds = [ 0.1, 1.0 ]
                self.glyphScale = 0.3
                self.lowresGlyphScale = 0.5
            else:
                self.glyphMapper.SetScaleModeToNoDataScaling()
                self.glyphScaleBounds = [ 1.0, 10.0 ]
                self.glyphScale = 3.0
                self.lowresGlyphScale = 5.0
            self.glyphMapper.ScalingOn()
            self.glyphMapper.SetUseLookupTableScalarRange(1)
            self.glyphMapper.OrientOn ()
#            self.glyphMapper.ClampingOn()
            self.glyphMapper.SourceIndexingOff()
            self.glyphMapper.NestedDisplayListsOff()
            self.glyphMapper.UseSelectionIdsOff()
            self.glyphMapper.SetInputConnection( self.resample.GetOutputPort() )
            self.glyphMapper.SetLookupTable( self.LookupTable )
            self.glyphMapper.ScalarVisibilityOn()
            self.glyphMapper.SetScalarModeToUsePointFieldData()
            self.glyphMapper.SelectColorArray( vectorsArray.GetName() )

            self.createArrowSources()
            self.updateScaling()

            self.glyphActor = vtk.vtkActor()
            self.glyphActor.SetMapper( self.glyphMapper )

            self.CurrentRenderer.AddActor( self.glyphActor )
            self.ApplyGlyphDecimationFactor()

    def createArrowSources( self, scaleRange=[ 1.0, 10.0 ], n_sources=10 ):
        trans = vtk.vtkTransform()
        arrowSource = vtk.vtkArrowSource()
        arrowSource.SetTipResolution(3)
        arrowSource.SetShaftResolution(3)
        arrowSource.Update()
        arrow = arrowSource.GetOutput()
        sourcePts = arrow.GetPoints()
        dScale = ( scaleRange[1] - scaleRange[0] ) / ( n_sources - 1 )
        for iScale in range( n_sources ):
            scale = scaleRange[0] + iScale * dScale
            trans.Identity()
            trans.Scale( scale, 1.0, 1.0 )
            newPts = vtk.vtkPoints()
            trans.TransformPoints( sourcePts, newPts )
            scaledArrow = vtk.vtkPolyData()
            scaledArrow.CopyStructure(arrow)
            scaledArrow.SetPoints( newPts )
            if vtk.VTK_MAJOR_VERSION <= 5:  self.glyphMapper.SetSource( iScale, scaledArrow )
            else:                           self.glyphMapper.SetSourceData( iScale, scaledArrow )

    def UpdateInputs(self):
        self.initGlyphMapper()
        return True

    def ApplyGlyphDecimationFactor(self):
        sampleRate =  int( round( abs( self.glyphDecimationFactor ) )  )
#        print "Sample rate: %s " % str( sampleRate )
        self.resample.SetSampleRate( sampleRate, sampleRate, 1 )

    def processGlyphScaleCommand( self, args, config_function = None ):
        glyphScale = config_function.value
        if args and args[0] == "StartConfig":
            pass
        elif args and args[0] == "Init":
            config_function.setRangeBounds( self.glyphScaleBounds )
            if config_function.initial_value == None:
                config_function.initial_value = self.glyphScale
            glyphScale.setValue( 0, config_function.initial_value )
            self.updateScaling( True )
        elif args and args[0] == "EndConfig":
            pass
        elif args and args[0] == "InitConfig":
            pass
        elif args and args[0] == "Open":
            pass
        elif args and args[0] == "Close":
            pass
        elif args and args[0] == "UpdateConfig":
            value = args[2].GetValue()
            glyphScale.setValue( 0, value )
            self.glyphScale = abs( value )
            if self.isActiveUpdate():
                self.updateScaling( True )

    def processGlyphDensityCommand( self, args, config_function = None ):
        glyphDensity = config_function.value
        if args and args[0] == "StartConfig":
            pass
        elif args and args[0] == "Init":
            config_function.setRangeBounds( self.glyphDecimationFactorBounds )
            if config_function.initial_value == None:
                config_function.initial_value = self.glyphDecimationFactor
            glyphDensity.setValue( 0, config_function.initial_value )
            self.ApplyGlyphDecimationFactor()
        elif args and args[0] == "EndConfig":
            pass
        elif args and args[0] == "InitConfig":
            pass
        elif args and args[0] == "Open":
            pass
        elif args and args[0] == "Close":
            pass
        elif args and args[0] == "UpdateConfig":
            value = args[2].GetValue()
            glyphDensity.setValue( 0, value )
            self.glyphDecimationFactor = value
            if self.isActiveUpdate():
                self.ApplyGlyphDecimationFactor()

    def isActiveUpdate(self):
        self.updateCount = self.updateCount + 1
        return ( (self.updateCount % self.nSkipCount) == 0 )

    def updateScaling( self, render = False ):
        self.glyphMapper.SetScaleFactor( self.glyphScale )
#        self.glyphMapper.SetRange( 0.0, self.glyphRange )
        if render:
            self.glyphMapper.Update()
            self.Interactor.Render()

    def getGlyphScale( self ):
        return [ self.glyphScale ]

    def getGlyphDensity(self):
        return self.glyphDecimationFactorBounds

    def scaleColormap( self, ctf_data, cmap_index=0, **args ):
        colormapManager = self.getColormapManager( 'Slice', index=cmap_index )
        colormapManager.setScale( ctf_data, ctf_data )
        ispec = self.inputSpecs[ cmap_index ]
        ispec.addMetadata( { '-'.join( [ 'colormap', 'Slice' ] ) : self.getColormapSpec('Slice') } )
        self.glyphMapper.SetLookupTable( colormapManager.lut )
        self.Interactor.Render()


# class PM_VectorVolume:
#     """Takes an arbitrary slice of the input data using an implicit cut
#     plane and places glyphs according to the vector field data.  The
#     glyphs may be colored using either the vector magnitude or the scalar
#     attributes.
#     """
#     def __init__( self, mid, **args ):
#         self.glyphScaleBounds = [ 0.02, 1.0 ]
#         self.glyphScale = [ 0.0, 0.5 ]
#         self.glyphRange = None
#         self.glyphDecimationFactor = [ 2.0, 20.0 ]
#         self.glyphDecimationFactorBounds = [ 1.0, 50.0 ]
#         self.primaryInputPorts = [ 'volume' ]
#         self.resample = None
#         self.addConfigurableLevelingFunction( 'colorScale', 'C', label='Colormap Scale', units='data', setLevel=self.scaleColormap, getLevel=self.getDataRangeBounds, layerDependent=True, adjustRangeInput=0 )
#         self.addConfigurableLevelingFunction( 'glyphScale', 'Z', label='Glyph Size', setLevel=self.setGlyphScale, getLevel=self.getGlyphScale, activeBound='max', layerDependent=True, bound=False )
#         self.addConfigurableLevelingFunction( 'glyphDensity', 'G', label='Glyph Density', sliderLabels=["Vertical Sample Spacing", "Horizontal Sample Spacing"], setLevel=self.setGlyphDensity, getLevel=self.getGlyphDensity, layerDependent=True, windowing=False, bound=False )
#         self.addConfigurableLevelingFunction( 'zScale', 'z', label='Vertical Scale', setLevel=self.setInputZScale, getLevel=self.getScaleBounds, activeBound='max', windowing=False, sensitivity=(10.0,10.0), initRange=[ 2.0, 2.0, 1 ] )
#
#     def scaleColormap( self, ctf_data, cmap_index=0, **args ):
#         colormapManager = self.getColormapManager( index=cmap_index )
#         colormapManager.setScale( ctf_data, ctf_data )
#         ispec = self.inputSpecs[ cmap_index ]
#         ispec.addMetadata( { 'colormap' : self.getColormapSpec() } )
#         self.glyph.SetLookupTable( colormapManager.lut )
# #        self.glyph.Modified()
# #        self.glyph.Update()
#         self.render()
#
#     def setGlyphScale( self, ctf_data ):
#         self.glyphScale = ctf_data
#         self.glyph.SetScaleFactor( self.glyphScale[1] )
#         self.glyph.Update()
#         self.render()
#
#     def getGlyphScale( self ):
#         return self.glyphScaleBounds
#
#     def setGlyphDensity( self, ctf_data ):
#         self.glyphDecimationFactor = ctf_data
#         self.ApplyGlyphDecimationFactor()
#
#     def getGlyphDensity(self):
#         return self.glyphDecimationFactorBounds
#
#     def buildPipeline(self):
#         """ execute() -> None
#         Dispatch the vtkRenderer to the actual rendering widget
#         """
#         self.colorInputModule = self.wmod.forceGetInputFromPort( "colors", None )
#
#         if self.input() == None:
#             print>>sys.stderr, "Must supply 'volume' port input to VectorCutPlane"
#             return
#
#         xMin, xMax, yMin, yMax, zMin, zMax = self.input().GetExtent()
#         spacing = self.input().GetSpacing()
#         sx, sy, sz = spacing
#         origin = self.input().GetOrigin()
#         ox, oy, oz = origin
#
#         cellData = self.input().GetCellData()
#         pointData = self.input().GetPointData()
#         vectorsArray = pointData.GetVectors()
#
#         if vectorsArray == None:
#             print>>sys.stderr, "Must supply point vector data for 'volume' port input to VectorVolume"
#             return
#
#         self.setRangeBounds( list( vectorsArray.GetRange(-1) ) )
#         self.nComponents = vectorsArray.GetNumberOfComponents()
#         for iC in range(-1,3): print "Value Range %d: %s " % ( iC, str( vectorsArray.GetRange( iC ) ) )
#         for iV in range(10): print "Value[%d]: %s " % ( iV, str( vectorsArray.GetTuple3( iV ) ) )
#
#         self.initialOrigin = self.input().GetOrigin()
#         self.initialExtent = self.input().GetExtent()
#         self.initialSpacing = self.input().GetSpacing()
#         self.dataBounds = self.getUnscaledWorldExtent( self.initialExtent, self.initialSpacing, self.initialOrigin )
#         dataExtents = ( (self.dataBounds[1]-self.dataBounds[0])/2.0, (self.dataBounds[3]-self.dataBounds[2])/2.0, (self.dataBounds[5]-self.dataBounds[4])/2.0 )
#         centroid = ( (self.dataBounds[0]+self.dataBounds[1])/2.0, (self.dataBounds[2]+self.dataBounds[3])/2.0, (self.dataBounds[4]+self.dataBounds[5])/2.0  )
#         self.pos = [ self.initialSpacing[i]*self.initialExtent[2*i] for i in range(3) ]
#         if ( (self.initialOrigin[0] + self.pos[0]) < 0.0): self.pos[0] = self.pos[0] + 360.0
#
#         self.resample = vtk.vtkExtractVOI()
#         versionAgnosticSetInput( self.resample, self.input() )
#         self.resample.SetVOI( self.initialExtent )
#         self.ApplyGlyphDecimationFactor()
#         lut = self.getLut()
#
#         if self.colorInputModule <> None:
#             self.color_resample = vtk.vtkExtractVOI()
#             self.color_resample.SetInputConnection( self.colorInputModule.getOutputPort() )
#             self.color_resample.SetVOI( self.initialExtent )
#             self.color_resample.SetSampleRate( sampleRate, sampleRate, 1 )
# #            self.probeFilter = vtk.vtkProbeFilter()
# #            self.probeFilter.SetSourceConnection( self.resample.GetOutputPort() )
# #            colorInput = self.colorInputModule.getOutput()
# #            self.probeFilter.SetInput( colorInput )
#             shiftScale = vtk.vtkImageShiftScale()
#             shiftScale.SetOutputScalarTypeToFloat ()
#             shiftScale.SetInputConnection( self.color_resample.GetOutputPort() )
#             valueRange = self.getScalarRange()
#             shiftScale.SetShift( valueRange[0] )
#             shiftScale.SetScale ( (valueRange[1] - valueRange[0]) / 65535 )
#             shiftScale.Update()
#             colorFloatInput = shiftScale.GetOutput()
#             colorInput_pointData = colorFloatInput.GetPointData()
#             self.colorScalars = colorInput_pointData.GetScalars()
#             self.colorScalars.SetName('color')
#             lut.SetTableRange( valueRange )
#
#         self.glyph = vtk.vtkGlyph3DMapper()
# #        if self.colorInputModule <> None:   self.glyph.SetColorModeToColorByScalar()
# #        else:                               self.glyph.SetColorModeToColorByVector()
#         scalarRange = self.getScalarRange()
#         self.glyph.SetScaleModeToScaleByMagnitude()
#         self.glyph.SetColorModeToMapScalars()
#         self.glyph.SetUseLookupTableScalarRange(1)
#         self.glyph.SetOrient( 1 )
# #        self.glyph.ClampingOn()
#         self.glyph.SetRange( scalarRange[0:2] )
#         self.glyph.SetInputConnection( self.resample.GetOutputPort()  )
#         self.arrow = vtk.vtkArrowSource()
#         self.glyph.SetSourceConnection( self.arrow.GetOutputPort() )
#         self.glyph.SetLookupTable( lut )
#         self.glyphActor = vtk.vtkActor()
#         self.glyphActor.SetMapper( self.glyph )
#         self.renderer.AddActor( self.glyphActor )
#         self.renderer.SetBackground( VTK_BACKGROUND_COLOR[0], VTK_BACKGROUND_COLOR[1], VTK_BACKGROUND_COLOR[2] )
#         self.set3DOutput(wmod=self.wmod)
#
#     def updateModule(self, **args ):
#         versionAgnosticSetInput( self.resample, self.input() )
#         self.glyph.Modified()
#         self.glyph.Update()
#         self.set3DOutput(wmod=self.wmod)
#
#     def ApplyGlyphDecimationFactor(self):
#         sampleRate = [ int( round( abs( self.glyphDecimationFactor[1] ) )  ), int( round( abs( self.glyphDecimationFactor[0] ) ) )  ]
# #        print "Sample rate: %s " % str( sampleRate )
#         self.resample.SetSampleRate( sampleRate[0], sampleRate[0], sampleRate[1] )
#
#     def dumpData( self, label, dataArray ):
#         nt = dataArray.GetNumberOfTuples()
#         valArray = []
#         for iT in range( 0, nt ):
#             val = dataArray.GetTuple3(  iT  )
#             valArray.append( "(%.3g,%.3g,%.3g)" % ( val[0], val[1], val[2] )  )
#         print " _________________________ %s _________________________ " % label
#         print ' '.join( valArray )
#
#     def getUnscaledWorldExtent( self, extent, spacing, origin ):
#         return [ ( ( extent[ i ] * spacing[ i/2 ] ) + origin[i/2]  ) for i in range(6) ]
#
#
