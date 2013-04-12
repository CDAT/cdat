try: paraview.simple
except: from paraview.simple import *

import sys
import os
import paraview

if len(sys.argv) != 3:
    print 'Usage: pvbatch --symmetric MHTScreenshots.py <output file name> "<input file names>"'
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

timeCompartmentSize = 8
globalController, temporalController, timeCompartmentSize = CreateControllers(timeCompartmentSize)

in_msf_moc = CreateReader( MHTFileSeriesReader, [], sys.argv[2] )
timeSteps = GetActiveSource().TimestepValues if len(GetActiveSource().TimestepValues)!=0 else [0]

XYChartView1 = CreateView( CreateXYPlotView, sys.argv[1], 1, 549, 583, tp_views )
XYChartView1.ShowAxis = [1, 1, 0, 0]
XYChartView1.ShowAxisGrid = [1, 1, 0, 0]
XYChartView1.AxisLabelsBottom = []
XYChartView1.LegendLocation = 1
XYChartView1.AxisLabelsLeft = []
XYChartView1.ViewTime = 0.0
XYChartView1.ShowLegend = 1
XYChartView1.AxisRange = [0.0, 1.0, 0.0, 1.0, 0.0, 1.0, 0.0, 1.0]
XYChartView1.AxisTitleFont = ['Arial', '12', '1', '0', 'Arial', '12', '1', '0', 'Arial', '12', '1', '0', 'Arial', '12', '1', '0']
XYChartView1.AxisLabelColor = [0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0]
XYChartView1.AxisTitleColor = [0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.5, 0.0, 0.0, 0.5]
XYChartView1.ChartTitleColor = [0.0, 0.0, 0.0]
XYChartView1.ChartTitleAlignment = 1
XYChartView1.AxisColor = [0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0]
XYChartView1.AxisLabelsTop = []
XYChartView1.AxisLabelFont = ['Arial', '12', '0', '0', 'Arial', '12', '0', '0', 'Arial', '12', '0', '0', 'Arial', '12', '0', '0']
XYChartView1.ShowAxisLabels = [1, 1, 1, 1]
XYChartView1.AxisLabelNotation = [0, 0, 0, 0]
XYChartView1.AxisLabelPrecision = [2, 2, 2, 2]
XYChartView1.AxisGridColor = [0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95]
XYChartView1.ChartTitle = ''
XYChartView1.AxisLabelsRight = []
XYChartView1.AxisBehavior = [0, 0, 0, 0]
XYChartView1.AxisTitle = ['', '', '', '']
XYChartView1.ChartTitleFont = ['Arial', '14', '0', '0']
XYChartView1.AxisLogScale = [0, 0, 0, 0]

DataRepresentation1 = Show() #GetDisplayProperties(in_msf_moc)
DataRepresentation1.XArrayName = 'reader_mht_global'
DataRepresentation1.SeriesVisibility = ['vtkOriginalIndices', '0']
DataRepresentation1.SeriesVisibility = ['reader_mht_global', '1']

IterateOverTimeSteps(globalController, timeCompartmentSize, timeSteps, tp_writers, tp_views)

print 'ending'
