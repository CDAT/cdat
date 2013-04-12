# Script for computing temporal statistics (average, minimum, maximum
# and standard deviation) on hopper.nersc.gov. The input is a single
# file that contains multipe time steps. The time compartment size is
# a command line argument.

import sys
import time
start = time.time()

try: paraview.simple
except: from paraview.simple import *
paraview.simple._DisableFirstRenderCameraReset()

import libvtkParallelPython
import paraview
pm = paraview.servermanager.vtkProcessModule.GetProcessModule()
globalController = pm.GetGlobalController()
pid = globalController.GetLocalProcessId()

tcsize = sys.argv[1]

fileName = "statsmwhenertwod.vtm"

if pid == 0:
    print 'starting script with tcsize of ', tcsize, ' and output filename using ', fileName

V_cam5_1_amip_run2_cam2_h0_1994_nc = NetCDFReader( FileName=['/global/project/projectdirs/m1517/ACE/cam5.1/control/0.25_degre
e/monthly/run2/zg_Amon_CAM5.1_0.25degree_control_v1.0_run2_197901-200512.nc'] )

V_cam5_1_amip_run2_cam2_h0_1994_nc.Dimensions = '(plev, lat, lon)'
V_cam5_1_amip_run2_cam2_h0_1994_nc.SphericalCoordinates = 0

MultiBlockTemporalStatistics1 = MultiBlockTemporalStatistics()
MultiBlockTemporalStatistics1.TimeStepType = 'Months'
#MultiBlockTemporalStatistics1.SamplingMethod = 'Consecutive'
MultiBlockTemporalStatistics1.SamplingMethod = 'Climatology'
#MultiBlockTemporalStatistics1.TimeSpan = 'Year'
MultiBlockTemporalStatistics1.TimeSpan = 'AllTimeSteps'
MultiBlockTemporalStatistics1.TimeCompartmentSize = int(tcsize)

writer = XMLMultiBlockDataWriter()
writer.FileName = fileName

writer.UpdatePipeline()
if pid == 0:
    print 'finished run in ', time.time()-start

