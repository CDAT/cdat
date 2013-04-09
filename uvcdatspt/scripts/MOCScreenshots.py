
try: paraview.simple
except: from paraview.simple import *

import sys
import os
import paraview

if len(sys.argv) != 3:
    print 'Usage: pvbatch --symmetric MOCScreenshots.py <output file name> "<input file names>"'
    sys.exit(1)

print 'input file names are: ', sys.argv[2]
print 'output file name is: ', sys.argv[1]

# trying to import the library where I can specify the global and subcontrollers
try:
    import libvtkParallelPython as vtkParallel # requires LD_LIBRARY_PATH being properly set
except ImportError:
    import vtkParallelPython as vtkParallel # for a static build, i.e. jaguarpf, use this instead and don't worry about LD_LIBRARY_PATH

paraview.options.batch = True # this may not be necessary
paraview.simple._DisableFirstRenderCameraReset()

def CreateTimeCompartments(globalController, timeCompartmentSize):
    if globalController.GetNumberOfProcesses() == 1:
        print 'single process'
        return
    elif globalController.GetNumberOfProcesses() % timeCompartmentSize != 0:
        print 'number of processes must be an integer multiple of time compartment size'
        return
    elif timeCompartmentSize == globalController.GetNumberOfProcesses():
        return globalController

    gid = globalController.GetLocalProcessId()
    timeCompartmentGroupId = int (gid / timeCompartmentSize )
    newController = globalController.PartitionController(timeCompartmentGroupId, gid % timeCompartmentSize)
    # must unregister if the reference count is greater than 1
    if newController.GetReferenceCount() > 1:
        newController.UnRegister(None)

    #print gid, timeCompartmentGroupId, gid % timeCompartmentSize
    print gid, ' of global comm is ', newController.GetLocalProcessId()
    globalController.SetGlobalController(newController)
    return newController

def CheckReader(reader):
    if hasattr(reader, "FileName") == False:
        print "ERROR: Don't know how to set file name for ", reader.SMProxy.GetXMLName()
        sys.exit(-1)

    if hasattr(reader, "TimestepValues") == False:
        print "ERROR: ", reader.SMProxy.GetXMLName(), " doesn't have time information"
        sys.exit(-1)

def CreateControllers(timeCompartmentSize):
    pm = paraview.servermanager.vtkProcessModule.GetProcessModule()
    globalController = pm.GetGlobalController()
    if timeCompartmentSize > globalController.GetNumberOfProcesses():
        timeCompartmentSize = globalController.GetNumberOfProcesses()

    temporalController = CreateTimeCompartments(globalController, timeCompartmentSize)
    return globalController, temporalController, timeCompartmentSize

currentTimeStep = -1
def UpdateCurrentTimeStep(globalController, timeCompartmentSize):
    global currentTimeStep
    if currentTimeStep == -1:
        currentTimeStep = globalController.GetLocalProcessId() / timeCompartmentSize
        return currentTimeStep

    numTimeStepsPerIteration = globalController.GetNumberOfProcesses() / timeCompartmentSize
    currentTimeStep = currentTimeStep + numTimeStepsPerIteration
    return currentTimeStep

def WriteImages(currentTimeStep, currentTime, views):
    for view in views:
        filename = view.tpFileName.replace("%t", str(currentTimeStep))
        view.ViewTime = currentTime
        WriteImage(filename, view, Magnification=view.tpMagnification)

def WriteFiles(currentTimeStep, currentTime, writers):
    for writer in writers:
        originalfilename = writer.FileName
        fname = originalfilename.replace("%t", str(currentTimeStep))
        writer.FileName = fname
        writer.UpdatePipeline(currentTime)
        writer.FileName = originalfilename

def IterateOverTimeSteps(globalController, timeCompartmentSize, timeSteps, writers, views):
    currentTimeStep = UpdateCurrentTimeStep(globalController, timeCompartmentSize)
    while currentTimeStep < len(timeSteps):
        print globalController.GetLocalProcessId(), " is working on ", currentTimeStep
        WriteImages(currentTimeStep, timeSteps[currentTimeStep], views)
        WriteFiles(currentTimeStep, timeSteps[currentTimeStep], writers)
        currentTimeStep = UpdateCurrentTimeStep(globalController, timeCompartmentSize)

def CreateReader(ctor, args, fileInfo):
    "Creates a reader, checks if it can be used, and sets the filenames"
    reader = ctor()
    CheckReader(reader)
    import glob
    files = glob.glob(fileInfo)
    files.sort() # assume there is a logical ordering of the filenames that corresponds to time ordering
    reader.FileName = files
    for a in args:
        s = "reader."+a
        exec (s)

    return reader

def CreateWriter(ctor, filename, tp_writers):
    writer = ctor()
    writer.FileName = filename
    tp_writers.append(writer)
    return writer

def CreateView(proxy_ctor, filename, magnification, width, height, tp_views):
    view = proxy_ctor()
    view.add_attribute("tpFileName", filename)
    view.add_attribute("tpMagnification", magnification)
    tp_views.append(view)
    view.ViewSize = [width, height]
    return view

tp_writers = []
tp_views = []
# ==================== end of specialized temporal parallelism sections ==================

timeCompartmentSize = 16
globalController, temporalController, timeCompartmentSize = CreateControllers(timeCompartmentSize)

RenderView1 = CreateView( CreateRenderView, sys.argv[1], 1, 549, 583, tp_views )
RenderView1.LightSpecularColor = [1.0, 1.0, 1.0]
RenderView1.InteractionMode = '3D'
RenderView1.UseTexturedBackground = 0
RenderView1.UseLight = 1
RenderView1.CameraPosition = [15.0, -2624.999755859375, 14496.62787197619]
RenderView1.FillLightKFRatio = 3.0
RenderView1.Background2 = [0.0, 0.0, 0.16470588235294117]
RenderView1.FillLightAzimuth = -10.0
RenderView1.LODResolution = 50.0
RenderView1.BackgroundTexture = []
RenderView1.KeyLightAzimuth = 10.0
RenderView1.StencilCapable = 1
RenderView1.LightIntensity = 1.0
RenderView1.CameraFocalPoint = [15.0, -2624.999755859375, 0.0]
RenderView1.ImageReductionFactor = 2
RenderView1.CameraViewAngle = 30.0
RenderView1.CameraParallelScale = 3766.3151510583625
RenderView1.EyeAngle = 2.0
RenderView1.HeadLightKHRatio = 3.0
RenderView1.StereoRender = 0
RenderView1.KeyLightIntensity = 0.75
RenderView1.BackLightAzimuth = 110.0
RenderView1.OrientationAxesInteractivity = 0
RenderView1.UseInteractiveRenderingForSceenshots = 0
RenderView1.UseOffscreenRendering = 0
RenderView1.Background = [1.0, 1.0, 1.0]
RenderView1.UseOffscreenRenderingForScreenshots = 1
RenderView1.NonInteractiveRenderDelay = 2
RenderView1.CenterOfRotation = [15.0, -2624.999755859375, 0.0]
RenderView1.CameraParallelProjection = 0
RenderView1.CompressorConfig = 'vtkSquirtCompressor 0 3'
RenderView1.HeadLightWarmth = 0.5
RenderView1.MaximumNumberOfPeels = 4
RenderView1.LightDiffuseColor = [1.0, 1.0, 1.0]
RenderView1.StereoType = 'Red-Blue'
RenderView1.DepthPeeling = 1
RenderView1.BackLightKBRatio = 3.5
RenderView1.StereoCapableWindow = 1
RenderView1.CameraViewUp = [0.0, 1.0, 0.0]
RenderView1.LightType = 'HeadLight'
RenderView1.LightAmbientColor = [1.0, 1.0, 1.0]
RenderView1.RemoteRenderThreshold = 3.0
RenderView1.KeyLightElevation = 50.0
RenderView1.CenterAxesVisibility = 0
RenderView1.MaintainLuminance = 0
RenderView1.StillRenderImageReductionFactor = 1
RenderView1.BackLightWarmth = 0.5
RenderView1.FillLightElevation = -75.0
RenderView1.MultiSamples = 0
RenderView1.FillLightWarmth = 0.4
RenderView1.AlphaBitPlanes = 1
RenderView1.LightSwitch = 0
RenderView1.OrientationAxesVisibility = 0
RenderView1.CameraClippingRange = [14351.66159325643, 14714.077290055833]
RenderView1.BackLightElevation = 0.0
RenderView1.ViewTime = 0.0
RenderView1.OrientationAxesOutlineColor = [1.0, 1.0, 1.0]
RenderView1.LODThreshold = 5.0
RenderView1.CollectGeometryThreshold = 100.0
RenderView1.UseGradientBackground = 0
RenderView1.KeyLightWarmth = 0.6
RenderView1.OrientationAxesLabelColor = [1.0, 1.0, 1.0]

in_msf_moc = CreateReader( MOCFileSeriesReader, [],  sys.argv[2])
timeSteps = GetActiveSource().TimestepValues if len(GetActiveSource().TimestepValues)!=0 else [0]
Threshold1 = Threshold( guiName="Threshold1", Scalars=['POINTS', 'reader_moc_global'], ThresholdRange=[-1000.0, 592.3663330078125], AllScalars=1 )

Transform1 = Transform( guiName="Transform1", Transform="Transform" )
Transform1.Transform.Scale = [40.0, -1.0, 1.0]
Transform1.Transform.Rotate = [0.0, 0.0, 0.0]
Transform1.Transform.Translate = [0.0, 0.0, 0.0]

a1_reader_moc_global_PiecewiseFunction = CreatePiecewiseFunction( Points=[0.0, 0.0, 1.0, 1.0] )

a1_reader_moc_global_PVLookupTable = GetLookupTableForArray( "reader_moc_global", 1, Discretize=1, RGBPoints=[-151.5101776123047, 0.23, 0.299, 0.754, 592.3663330078125, 0.706, 0.016, 0.15], UseLogScale=0, VectorComponent=0, NanColor=[0.25, 0.0, 0.0], NumberOfTableValues=256, ColorSpace='Diverging', VectorMode='Magnitude', HSVWrap=0, ScalarRangeInitialized=1.0, LockScalarRange=0 )

SetActiveSource(in_msf_moc)
DataRepresentation1 = Show()
DataRepresentation1.CubeAxesZAxisVisibility = 1
DataRepresentation1.SelectionPointLabelColor = [0.5, 0.5, 0.5]
DataRepresentation1.SelectionPointFieldDataArrayName = 'vtkOriginalPointIds'
DataRepresentation1.SuppressLOD = 0
DataRepresentation1.CubeAxesXGridLines = 0
DataRepresentation1.CubeAxesYAxisTickVisibility = 1
DataRepresentation1.Position = [0.0, 0.0, 0.0]
DataRepresentation1.BackfaceRepresentation = 'Follow Frontface'
DataRepresentation1.SelectionOpacity = 1.0
DataRepresentation1.SelectionPointLabelShadow = 0
DataRepresentation1.CubeAxesYGridLines = 0
DataRepresentation1.OrientationMode = 'Direction'
DataRepresentation1.Source.TipResolution = 6
DataRepresentation1.ScaleMode = 'No Data Scaling Off'
DataRepresentation1.Diffuse = 1.0
DataRepresentation1.SelectionUseOutline = 0
DataRepresentation1.CubeAxesZTitle = 'Z-Axis'
DataRepresentation1.Specular = 0.1
DataRepresentation1.SelectionVisibility = 1
DataRepresentation1.InterpolateScalarsBeforeMapping = 1
DataRepresentation1.CubeAxesZAxisTickVisibility = 1
DataRepresentation1.Origin = [0.0, 0.0, 0.0]
DataRepresentation1.CubeAxesVisibility = 0
DataRepresentation1.Scale = [1.0, 1.0, 1.0]
DataRepresentation1.SelectionCellLabelJustification = 'Left'
DataRepresentation1.DiffuseColor = [1.0, 1.0, 1.0]
DataRepresentation1.SelectionCellLabelOpacity = 1.0
DataRepresentation1.Source = "Arrow"
DataRepresentation1.Source.Invert = 0
DataRepresentation1.Masking = 0
DataRepresentation1.Opacity = 1.0
DataRepresentation1.LineWidth = 1.0
DataRepresentation1.MeshVisibility = 0
DataRepresentation1.Visibility = 0
DataRepresentation1.SelectionCellLabelFontSize = 18
DataRepresentation1.CubeAxesCornerOffset = 0.0
DataRepresentation1.SelectionPointLabelJustification = 'Left'
DataRepresentation1.Ambient = 0.0
DataRepresentation1.SelectOrientationVectors = ''
DataRepresentation1.CubeAxesTickLocation = 'Inside'
DataRepresentation1.BackfaceDiffuseColor = [1.0, 1.0, 1.0]
DataRepresentation1.CubeAxesYAxisVisibility = 1
DataRepresentation1.SelectionPointLabelFontFamily = 'Arial'
DataRepresentation1.Source.ShaftResolution = 6
DataRepresentation1.CubeAxesFlyMode = 'Closest Triad'
DataRepresentation1.SelectScaleArray = ''
DataRepresentation1.CubeAxesYTitle = 'Y-Axis'
DataRepresentation1.ColorAttributeType = 'POINT_DATA'
DataRepresentation1.SpecularPower = 100.0
DataRepresentation1.Texture = []
DataRepresentation1.SelectionCellLabelShadow = 0
DataRepresentation1.AmbientColor = [1.0, 1.0, 1.0]
DataRepresentation1.MapScalars = 1
DataRepresentation1.PointSize = 2.0
DataRepresentation1.Source.TipLength = 0.35
DataRepresentation1.SelectionCellLabelFormat = ''
DataRepresentation1.Scaling = 0
DataRepresentation1.StaticMode = 0
DataRepresentation1.SelectionCellLabelColor = [0.0, 1.0, 0.0]
DataRepresentation1.Source.TipRadius = 0.1
DataRepresentation1.EdgeColor = [0.0, 0.0, 0.5000076295109483]
DataRepresentation1.CubeAxesXAxisTickVisibility = 1
DataRepresentation1.SelectionCellLabelVisibility = 0
DataRepresentation1.NonlinearSubdivisionLevel = 1
DataRepresentation1.CubeAxesColor = [1.0, 1.0, 1.0]
DataRepresentation1.Representation = 'Surface'
DataRepresentation1.CustomBounds = [0.0, 1.0, 0.0, 1.0, 0.0, 1.0]
DataRepresentation1.CubeAxesXAxisMinorTickVisibility = 1
DataRepresentation1.Orientation = [0.0, 0.0, 0.0]
DataRepresentation1.CubeAxesXTitle = 'X-Axis'
DataRepresentation1.CubeAxesInertia = 1
DataRepresentation1.BackfaceOpacity = 1.0
DataRepresentation1.SelectionCellFieldDataArrayName = 'vtkOriginalCellIds'
DataRepresentation1.SelectionColor = [1.0, 0.0, 1.0]
DataRepresentation1.SelectionPointLabelVisibility = 0
DataRepresentation1.SelectionPointLabelFontSize = 18
DataRepresentation1.ScaleFactor = 1.0
DataRepresentation1.BackfaceAmbientColor = [1.0, 1.0, 1.0]
DataRepresentation1.Source.ShaftRadius = 0.03
DataRepresentation1.SelectMaskArray = ''
DataRepresentation1.SelectionLineWidth = 2.0
DataRepresentation1.CubeAxesZAxisMinorTickVisibility = 1
DataRepresentation1.CubeAxesXAxisVisibility = 1
DataRepresentation1.Interpolation = 'Gouraud'
DataRepresentation1.SelectionCellLabelFontFamily = 'Arial'
DataRepresentation1.SelectionCellLabelItalic = 0
DataRepresentation1.CubeAxesYAxisMinorTickVisibility = 1
DataRepresentation1.CubeAxesZGridLines = 0
DataRepresentation1.SelectionPointLabelFormat = ''
DataRepresentation1.SelectionPointLabelOpacity = 1.0
DataRepresentation1.Pickable = 1
DataRepresentation1.CustomBoundsActive = [0, 0, 0]
DataRepresentation1.SelectionRepresentation = 'Wireframe'
DataRepresentation1.SelectionPointLabelBold = 0
DataRepresentation1.ColorArrayName = 'reader_moc_global'
DataRepresentation1.SelectionPointLabelItalic = 0
DataRepresentation1.SpecularColor = [1.0, 1.0, 1.0]
DataRepresentation1.LookupTable = a1_reader_moc_global_PVLookupTable
DataRepresentation1.SelectionPointSize = 5.0
DataRepresentation1.SelectionCellLabelBold = 0
DataRepresentation1.Orient = 0

SetActiveSource(Threshold1)
DataRepresentation2 = Show()
DataRepresentation2.CubeAxesZAxisVisibility = 1
DataRepresentation2.SelectionPointLabelColor = [0.5, 0.5, 0.5]
DataRepresentation2.SelectionPointFieldDataArrayName = 'vtkOriginalPointIds'
DataRepresentation2.SuppressLOD = 0
DataRepresentation2.CubeAxesXGridLines = 0
DataRepresentation2.CubeAxesYAxisTickVisibility = 1
DataRepresentation2.Position = [0.0, 0.0, 0.0]
DataRepresentation2.BackfaceRepresentation = 'Follow Frontface'
DataRepresentation2.SelectionOpacity = 1.0
DataRepresentation2.SelectionPointLabelShadow = 0
DataRepresentation2.CubeAxesYGridLines = 0
DataRepresentation2.OrientationMode = 'Direction'
DataRepresentation2.Source.TipResolution = 6
DataRepresentation2.ScaleMode = 'No Data Scaling Off'
DataRepresentation2.Diffuse = 1.0
DataRepresentation2.SelectionUseOutline = 0
DataRepresentation2.SelectionPointLabelFormat = ''
DataRepresentation2.CubeAxesZTitle = 'Z-Axis'
DataRepresentation2.Specular = 0.1
DataRepresentation2.SelectionVisibility = 1
DataRepresentation2.InterpolateScalarsBeforeMapping = 1
DataRepresentation2.CubeAxesZAxisTickVisibility = 1
DataRepresentation2.Origin = [0.0, 0.0, 0.0]
DataRepresentation2.CubeAxesVisibility = 0
DataRepresentation2.Scale = [1.0, 1.0, 1.0]
DataRepresentation2.SelectionCellLabelJustification = 'Left'
DataRepresentation2.DiffuseColor = [1.0, 1.0, 1.0]
DataRepresentation2.SelectionCellLabelOpacity = 1.0
DataRepresentation2.CubeAxesInertia = 1
DataRepresentation2.Source = "Arrow"
DataRepresentation2.Source.Invert = 0
DataRepresentation2.Masking = 0
DataRepresentation2.Opacity = 1.0
DataRepresentation2.LineWidth = 1.0
DataRepresentation2.MeshVisibility = 0
DataRepresentation2.Visibility = 0
DataRepresentation2.SelectionCellLabelFontSize = 18
DataRepresentation2.CubeAxesCornerOffset = 0.0
DataRepresentation2.SelectionPointLabelJustification = 'Left'
DataRepresentation2.SelectionPointLabelVisibility = 0
DataRepresentation2.SelectOrientationVectors = ''
DataRepresentation2.CubeAxesTickLocation = 'Inside'
DataRepresentation2.BackfaceDiffuseColor = [1.0, 1.0, 1.0]
DataRepresentation2.CubeAxesYAxisVisibility = 1
DataRepresentation2.SelectionPointLabelFontFamily = 'Arial'
DataRepresentation2.Source.ShaftResolution = 6
DataRepresentation2.CubeAxesFlyMode = 'Closest Triad'
DataRepresentation2.SelectScaleArray = ''
DataRepresentation2.CubeAxesYTitle = 'Y-Axis'
DataRepresentation2.ColorAttributeType = 'POINT_DATA'
DataRepresentation2.SpecularPower = 100.0
DataRepresentation2.Texture = []
DataRepresentation2.SelectionCellLabelShadow = 0
DataRepresentation2.AmbientColor = [1.0, 1.0, 1.0]
DataRepresentation2.MapScalars = 1
DataRepresentation2.PointSize = 2.0
DataRepresentation2.Source.TipLength = 0.35
DataRepresentation2.SelectionCellLabelFormat = ''
DataRepresentation2.Scaling = 0
DataRepresentation2.StaticMode = 0
DataRepresentation2.SelectionCellLabelColor = [0.0, 1.0, 0.0]
DataRepresentation2.Source.TipRadius = 0.1
DataRepresentation2.EdgeColor = [0.0, 0.0, 0.5000076295109483]
DataRepresentation2.CubeAxesXAxisTickVisibility = 1
DataRepresentation2.SelectionCellLabelVisibility = 0
DataRepresentation2.NonlinearSubdivisionLevel = 1
DataRepresentation2.CubeAxesColor = [1.0, 1.0, 1.0]
DataRepresentation2.Representation = 'Surface'
DataRepresentation2.CustomBounds = [0.0, 1.0, 0.0, 1.0, 0.0, 1.0]
DataRepresentation2.CubeAxesXAxisMinorTickVisibility = 1
DataRepresentation2.Orientation = [0.0, 0.0, 0.0]
DataRepresentation2.CubeAxesXTitle = 'X-Axis'
DataRepresentation2.ScalarOpacityUnitDistance = 287.4628538795667
DataRepresentation2.BackfaceOpacity = 1.0
DataRepresentation2.SelectionCellFieldDataArrayName = 'vtkOriginalCellIds'
DataRepresentation2.SelectionColor = [1.0, 0.0, 1.0]
DataRepresentation2.Ambient = 0.0
DataRepresentation2.SelectionPointLabelFontSize = 18
DataRepresentation2.ScaleFactor = 1.0
DataRepresentation2.BackfaceAmbientColor = [1.0, 1.0, 1.0]
DataRepresentation2.Source.ShaftRadius = 0.03
DataRepresentation2.ScalarOpacityFunction = a1_reader_moc_global_PiecewiseFunction
DataRepresentation2.SelectMaskArray = ''
DataRepresentation2.SelectionLineWidth = 2.0
DataRepresentation2.CubeAxesZAxisMinorTickVisibility = 1
DataRepresentation2.CubeAxesXAxisVisibility = 1
DataRepresentation2.Interpolation = 'Gouraud'
DataRepresentation2.SelectMapper = 'Projected tetra'
DataRepresentation2.SelectionCellLabelFontFamily = 'Arial'
DataRepresentation2.SelectionCellLabelItalic = 0
DataRepresentation2.CubeAxesYAxisMinorTickVisibility = 1
DataRepresentation2.CubeAxesZGridLines = 0
DataRepresentation2.ExtractedBlockIndex = 0
DataRepresentation2.SelectionPointLabelOpacity = 1.0
DataRepresentation2.Pickable = 1
DataRepresentation2.CustomBoundsActive = [0, 0, 0]
DataRepresentation2.SelectionRepresentation = 'Wireframe'
DataRepresentation2.SelectionPointLabelBold = 0
DataRepresentation2.ColorArrayName = 'reader_moc_global'
DataRepresentation2.SelectionPointLabelItalic = 0
DataRepresentation2.SpecularColor = [1.0, 1.0, 1.0]
DataRepresentation2.LookupTable = a1_reader_moc_global_PVLookupTable
DataRepresentation2.SelectionPointSize = 5.0
DataRepresentation2.SelectionCellLabelBold = 0
DataRepresentation2.Orient = 0

SetActiveSource(Transform1)
DataRepresentation3 = Show()
DataRepresentation3.CubeAxesZAxisVisibility = 1
DataRepresentation3.SelectionPointLabelColor = [0.5, 0.5, 0.5]
DataRepresentation3.SelectionPointFieldDataArrayName = 'vtkOriginalPointIds'
DataRepresentation3.SuppressLOD = 0
DataRepresentation3.CubeAxesXGridLines = 0
DataRepresentation3.CubeAxesYAxisTickVisibility = 1
DataRepresentation3.Position = [0.0, 0.0, 0.0]
DataRepresentation3.BackfaceRepresentation = 'Follow Frontface'
DataRepresentation3.SelectionOpacity = 1.0
DataRepresentation3.SelectionPointLabelShadow = 0
DataRepresentation3.CubeAxesYGridLines = 0
DataRepresentation3.OrientationMode = 'Direction'
DataRepresentation3.Source.TipResolution = 6
DataRepresentation3.ScaleMode = 'No Data Scaling Off'
DataRepresentation3.Diffuse = 1.0
DataRepresentation3.SelectionUseOutline = 0
DataRepresentation3.SelectionPointLabelFormat = ''
DataRepresentation3.CubeAxesZTitle = 'Z-Axis'
DataRepresentation3.Specular = 0.1
DataRepresentation3.SelectionVisibility = 1
DataRepresentation3.InterpolateScalarsBeforeMapping = 1
DataRepresentation3.CubeAxesZAxisTickVisibility = 1
DataRepresentation3.Origin = [0.0, 0.0, 0.0]
DataRepresentation3.CubeAxesVisibility = 0
DataRepresentation3.Scale = [1.0, 1.0, 1.0]
DataRepresentation3.SelectionCellLabelJustification = 'Left'
DataRepresentation3.DiffuseColor = [1.0, 1.0, 1.0]
DataRepresentation3.SelectionCellLabelOpacity = 1.0
DataRepresentation3.CubeAxesInertia = 1
DataRepresentation3.Source = "Arrow"
DataRepresentation3.Source.Invert = 0
DataRepresentation3.Masking = 0
DataRepresentation3.Opacity = 1.0
DataRepresentation3.LineWidth = 1.0
DataRepresentation3.MeshVisibility = 0
DataRepresentation3.Visibility = 1
DataRepresentation3.SelectionCellLabelFontSize = 18
DataRepresentation3.CubeAxesCornerOffset = 0.0
DataRepresentation3.SelectionPointLabelJustification = 'Left'
DataRepresentation3.SelectionPointLabelVisibility = 0
DataRepresentation3.SelectOrientationVectors = ''
DataRepresentation3.CubeAxesTickLocation = 'Inside'
DataRepresentation3.BackfaceDiffuseColor = [1.0, 1.0, 1.0]
DataRepresentation3.CubeAxesYAxisVisibility = 1
DataRepresentation3.SelectionPointLabelFontFamily = 'Arial'
DataRepresentation3.Source.ShaftResolution = 6
DataRepresentation3.CubeAxesFlyMode = 'Closest Triad'
DataRepresentation3.SelectScaleArray = ''
DataRepresentation3.CubeAxesYTitle = 'Y-Axis'
DataRepresentation3.ColorAttributeType = 'POINT_DATA'
DataRepresentation3.SpecularPower = 100.0
DataRepresentation3.Texture = []
DataRepresentation3.SelectionCellLabelShadow = 0
DataRepresentation3.AmbientColor = [1.0, 1.0, 1.0]
DataRepresentation3.MapScalars = 1
DataRepresentation3.PointSize = 2.0
DataRepresentation3.Source.TipLength = 0.35
DataRepresentation3.SelectionCellLabelFormat = ''
DataRepresentation3.Scaling = 0
DataRepresentation3.StaticMode = 0
DataRepresentation3.SelectionCellLabelColor = [0.0, 1.0, 0.0]
DataRepresentation3.Source.TipRadius = 0.1
DataRepresentation3.EdgeColor = [0.0, 0.0, 0.5000076295109483]
DataRepresentation3.CubeAxesXAxisTickVisibility = 1
DataRepresentation3.SelectionCellLabelVisibility = 0
DataRepresentation3.NonlinearSubdivisionLevel = 1
DataRepresentation3.CubeAxesColor = [1.0, 1.0, 1.0]
DataRepresentation3.Representation = 'Surface'
DataRepresentation3.CustomBounds = [0.0, 1.0, 0.0, 1.0, 0.0, 1.0]
DataRepresentation3.CubeAxesXAxisMinorTickVisibility = 1
DataRepresentation3.Orientation = [0.0, 0.0, 0.0]
DataRepresentation3.CubeAxesXTitle = 'X-Axis'
DataRepresentation3.ScalarOpacityUnitDistance = 388.2163580108114
DataRepresentation3.BackfaceOpacity = 1.0
DataRepresentation3.SelectionCellFieldDataArrayName = 'vtkOriginalCellIds'
DataRepresentation3.SelectionColor = [1.0, 0.0, 1.0]
DataRepresentation3.Ambient = 0.0
DataRepresentation3.SelectionPointLabelFontSize = 18
DataRepresentation3.ScaleFactor = 1.0
DataRepresentation3.BackfaceAmbientColor = [1.0, 1.0, 1.0]
DataRepresentation3.Source.ShaftRadius = 0.03
DataRepresentation3.ScalarOpacityFunction = a1_reader_moc_global_PiecewiseFunction
DataRepresentation3.SelectMaskArray = ''
DataRepresentation3.SelectionLineWidth = 2.0
DataRepresentation3.CubeAxesZAxisMinorTickVisibility = 1
DataRepresentation3.CubeAxesXAxisVisibility = 1
DataRepresentation3.Interpolation = 'Gouraud'
DataRepresentation3.SelectMapper = 'Projected tetra'
DataRepresentation3.SelectionCellLabelFontFamily = 'Arial'
DataRepresentation3.SelectionCellLabelItalic = 0
DataRepresentation3.CubeAxesYAxisMinorTickVisibility = 1
DataRepresentation3.CubeAxesZGridLines = 0
DataRepresentation3.ExtractedBlockIndex = 0
DataRepresentation3.SelectionPointLabelOpacity = 1.0
DataRepresentation3.Pickable = 1
DataRepresentation3.CustomBoundsActive = [0, 0, 0]
DataRepresentation3.SelectionRepresentation = 'Wireframe'
DataRepresentation3.SelectionPointLabelBold = 0
DataRepresentation3.ColorArrayName = 'reader_moc_global'
DataRepresentation3.SelectionPointLabelItalic = 0
DataRepresentation3.SpecularColor = [1.0, 1.0, 1.0]
DataRepresentation3.LookupTable = a1_reader_moc_global_PVLookupTable
DataRepresentation3.SelectionPointSize = 5.0
DataRepresentation3.SelectionCellLabelBold = 0
DataRepresentation3.Orient = 0



IterateOverTimeSteps(globalController, timeCompartmentSize, timeSteps, tp_writers, tp_views)


print 'ending'
