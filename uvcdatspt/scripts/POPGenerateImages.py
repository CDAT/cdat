# Spatio-temporal script for generating images for POP NetCDF
# output files. This one pseudo-colors by TEMP. It has a
# time compartment size of 4 so the number of processes
# also needs to be a multiple of 4. To run it, do:
# mpirun -np <numprocs> ./pvbatch --symmetric POPGenerateImages.py

try: paraview.simple
except: from paraview.simple import *

import sys
import os
import paraview

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

timeCompartmentSize = 4
globalController, temporalController, timeCompartmentSize = CreateControllers(timeCompartmentSize)

RenderView1 = CreateView( CreateRenderView, "POP_TEMP_%t.png", 1, 549, 583, tp_views )
RenderView1.LightSpecularColor = [1.0, 1.0, 1.0]
RenderView1.InteractionMode = '3D'
RenderView1.UseTexturedBackground = 0
RenderView1.UseLight = 1
RenderView1.CameraPosition = [24413625.828416377, -24592716.541236263, 5758186.884780747]
RenderView1.FillLightKFRatio = 3.0
RenderView1.Background2 = [0.0, 0.0, 0.165]
RenderView1.FillLightAzimuth = -10.0
RenderView1.LODResolution = 50.0
RenderView1.BackgroundTexture = []
RenderView1.KeyLightAzimuth = 10.0
RenderView1.StencilCapable = 1
RenderView1.LightIntensity = 1.0
RenderView1.CameraFocalPoint = [1.78529588937719e-12, 1.4505529101189668e-12, 64147.750000000015]
RenderView1.ImageReductionFactor = 2
RenderView1.CameraViewAngle = 30.0
RenderView1.CameraParallelScale = 30343845.664423227
RenderView1.EyeAngle = 2.0
RenderView1.HeadLightKHRatio = 3.0
RenderView1.StereoRender = 0
RenderView1.KeyLightIntensity = 0.75
RenderView1.BackLightAzimuth = 110.0
RenderView1.OrientationAxesInteractivity = 0
RenderView1.UseInteractiveRenderingForSceenshots = 0
RenderView1.UseOffscreenRendering = 0
RenderView1.Background = [0.31999694819562063, 0.3400015259021897, 0.4299992370489052]
RenderView1.UseOffscreenRenderingForScreenshots = 1
RenderView1.NonInteractiveRenderDelay = 2
RenderView1.CenterOfRotation = [0.0, 0.0, 64147.75]
RenderView1.CameraParallelProjection = 0
RenderView1.CompressorConfig = 'vtkSquirtCompressor 0 3'
RenderView1.HeadLightWarmth = 0.5
RenderView1.MaximumNumberOfPeels = 4
RenderView1.LightDiffuseColor = [1.0, 1.0, 1.0]
RenderView1.StereoType = 'Red-Blue'
RenderView1.DepthPeeling = 1
RenderView1.BackLightKBRatio = 3.5
RenderView1.StereoCapableWindow = 1
RenderView1.CameraViewUp = [0.0471859955443886, 0.2695389330828218, 0.9618327533293193]
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
RenderView1.CameraClippingRange = [15039199.876017962, 60476974.08593859]
RenderView1.BackLightElevation = 0.0
RenderView1.ViewTime = 0.0
RenderView1.OrientationAxesOutlineColor = [1.0, 1.0, 1.0]
RenderView1.LODThreshold = 5.0
RenderView1.CollectGeometryThreshold = 100.0
RenderView1.UseGradientBackground = 0
RenderView1.KeyLightWarmth = 0.6
RenderView1.OrientationAxesLabelColor = [1.0, 1.0, 1.0]

TEMP_t_t0_1_42l_oilspill12c_00060101_pop_nc = CreateReader( UnstructuredNetCDFPOPreader, ['Stride=[10, 10, 10]', 'VerticalVelocity=0', 'VOI=[0, -1, 0, -1, 0, -1]'], "/home/acbauer/DATA/UVCDAT/TEMP.t.t0.1_42l_oilspill12c.*.pop.nc" )
timeSteps = GetActiveSource().TimestepValues if len(GetActiveSource().TimestepValues)!=0 else [0]
a1_TEMP_PiecewiseFunction = CreatePiecewiseFunction( Points=[0.0, 0.0, 1.0, 1.0] )

a1_TEMP_PVLookupTable = GetLookupTableForArray( "TEMP", 1, Discretize=1, RGBPoints=[-20.0, 0.23, 0.299, 0.754, 31.338409423828125, 0.706, 0.016, 0.15], UseLogScale=0, VectorComponent=0, NanColor=[0.25, 0.0, 0.0], NumberOfTableValues=256, ColorSpace='Diverging', VectorMode='Magnitude', HSVWrap=0, ScalarRangeInitialized=1.0, LockScalarRange=0 )

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
DataRepresentation1.SelectionPointLabelFormat = ''
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
DataRepresentation1.CubeAxesInertia = 1
DataRepresentation1.Source = "Arrow"
DataRepresentation1.Source.Invert = 0
DataRepresentation1.Masking = 0
DataRepresentation1.Opacity = 1.0
DataRepresentation1.LineWidth = 1.0
DataRepresentation1.MeshVisibility = 0
DataRepresentation1.Visibility = 1
DataRepresentation1.SelectionCellLabelFontSize = 18
DataRepresentation1.CubeAxesCornerOffset = 0.0
DataRepresentation1.SelectionPointLabelJustification = 'Left'
DataRepresentation1.SelectionPointLabelVisibility = 0
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
DataRepresentation1.ScalarOpacityUnitDistance = 313870.26193506655
DataRepresentation1.BackfaceOpacity = 1.0
DataRepresentation1.SelectionCellFieldDataArrayName = 'vtkOriginalCellIds'
DataRepresentation1.SelectionColor = [1.0, 0.0, 1.0]
DataRepresentation1.Ambient = 0.0
DataRepresentation1.SelectionPointLabelFontSize = 18
DataRepresentation1.ScaleFactor = 1.0
DataRepresentation1.BackfaceAmbientColor = [1.0, 1.0, 1.0]
DataRepresentation1.Source.ShaftRadius = 0.03
DataRepresentation1.ScalarOpacityFunction = a1_TEMP_PiecewiseFunction
DataRepresentation1.SelectMaskArray = ''
DataRepresentation1.SelectionLineWidth = 2.0
DataRepresentation1.CubeAxesZAxisMinorTickVisibility = 1
DataRepresentation1.CubeAxesXAxisVisibility = 1
DataRepresentation1.Interpolation = 'Gouraud'
DataRepresentation1.SelectMapper = 'Projected tetra'
DataRepresentation1.SelectionCellLabelFontFamily = 'Arial'
DataRepresentation1.SelectionCellLabelItalic = 0
DataRepresentation1.CubeAxesYAxisMinorTickVisibility = 1
DataRepresentation1.CubeAxesZGridLines = 0
DataRepresentation1.ExtractedBlockIndex = 0
DataRepresentation1.SelectionPointLabelOpacity = 1.0
DataRepresentation1.Pickable = 1
DataRepresentation1.CustomBoundsActive = [0, 0, 0]
DataRepresentation1.SelectionRepresentation = 'Wireframe'
DataRepresentation1.SelectionPointLabelBold = 0
DataRepresentation1.ColorArrayName = 'TEMP'
DataRepresentation1.SelectionPointLabelItalic = 0
DataRepresentation1.SpecularColor = [1.0, 1.0, 1.0]
DataRepresentation1.LookupTable = a1_TEMP_PVLookupTable
DataRepresentation1.SelectionPointSize = 5.0
DataRepresentation1.SelectionCellLabelBold = 0
DataRepresentation1.Orient = 0



IterateOverTimeSteps(globalController, timeCompartmentSize, timeSteps, tp_writers, tp_views)
