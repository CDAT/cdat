
try: paraview.simple
except: from paraview.simple import *

import sys
import os
import paraview

import benchmark

# trying to import the library where I can specify the global and subcontrollers
try:
    import libvtkParallelPython as vtkParallel # requires LD_LIBRARY_PATH being properly set
except ImportError:
    import vtkParallelPython as vtkParallel # for a static build, i.e. jaguarpf, use this instead and don't worry about LD_LIBRARY_PATH


# global variables
timeCompartmentSize = 4
input_files = "/home/boonth/Desktop/spatio/ocean_4/SALT*"
iso_files = "/home/boonth/Desktop/spatio/ocean/salt_%i.pvtp"

currentTimeStep = -1
log_lines_per_file = 5


# some initial setup
benchmark.maximize_logs()

pm = paraview.servermanager.vtkProcessModule.GetProcessModule()
timer = paraview.vtk.vtkTimerLog()
if len(sys.argv) < 1:
    print 'usage: <num files>'
else:
    num_files = int(sys.argv[1])
numprocs = pm.GetGlobalController().GetNumberOfProcesses()
timer.SetMaxEntries(log_lines_per_file * num_files * numprocs + 2)
pm.GetGlobalController().Barrier()
timer.StartTimer()

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

def WriteFiles(currentTimeStep, currentTime, writers, reader, contour):
    for writer in writers:
        originalfilename = writer.FileName
        fname = originalfilename.replace("%t", str(currentTimeStep))
        #folder = (currentTimeStep % 3) + 1
        #fname = originalfilename % (folder, currentTimeStep)
        writer.FileName = fname
        writer.UpdatePipeline(currentTime)
        writer.FileName = originalfilename

def IterateOverTimeSteps(globalController, timeCompartmentSize, timeSteps, writers, views, reader, contour):
    currentTimeStep = UpdateCurrentTimeStep(globalController, timeCompartmentSize)
    while currentTimeStep < len(timeSteps):
        print globalController.GetLocalProcessId(), " is working on ", currentTimeStep
        sys.stdout.flush()
        WriteImages(currentTimeStep, timeSteps[currentTimeStep], views)
        WriteFiles(currentTimeStep, timeSteps[currentTimeStep], writers, reader, contour)
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

def main():

    global timer
    global timeCompartmentSize

    tp_writers = []
    tp_views = []

    # ============ end of specialized temporal parallelism sections ==========

    globalController, temporalController, timeCompartmentSize = CreateControllers(timeCompartmentSize)

    reader = CreateReader( NetCDFReader, ["Dimensions='(depth_t, t_lat, t_lon)'", 'ReplaceFillValueWithNan=0', 'VerticalBias=0.0', "OutputType='Automatic'", 'SphericalCoordinates=1', 'VerticalScale=1.0'], input_files )
    timeSteps = GetActiveSource().TimestepValues
    if len(timeSteps) == 0:
        timeSteps = [0.0]
    contour = Contour( guiName="contour", Isosurfaces=[0.03], ComputeNormals=1, ComputeGradients=0, ComputeScalars=0, ContourBy=['POINTS', 'SALT'], PointMergeMethod="Uniform Binning" )
    contour.PointMergeMethod.Numberofpointsperbucket = 8
    contour.PointMergeMethod.Divisions = [50, 50, 50]

    ParallelPolyDataWriter2 = CreateWriter(XMLPPolyDataWriter,iso_files,tp_writers)

    IterateOverTimeSteps(globalController, timeCompartmentSize, timeSteps, tp_writers, tp_views, reader, contour)

    globalController.Barrier()
    timer.StopTimer()

    gid = globalController.GetLocalProcessId()
    if gid == 0:
        print 'all done! -- total time is', timer.GetElapsedTime(), 'seconds'

    benchmark.get_logs()
    if gid == 0:
        benchmark.print_logs()

if __name__ == '__main__':
    if len(sys.argv) < 1:
        print 'usage: <num files>'
    else:
        main()
    
