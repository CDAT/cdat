print 'starting'
import sys
from paraview.simple import *

if len(sys.argv) < 3:
    print 'Usage: pvbatch MHTTemporalStatistics.py <output file name> <input file names>'
    sys.exit(1)

paraview.simple._DisableFirstRenderCameraReset()
reader = MHTFileSeriesReader()
print 'input file names are: ', sys.argv[2:len(sys.argv)]
print 'output file name is: ', sys.argv[1]
reader.FileName = sys.argv[2:len(sys.argv)]

MultiBlockTemporalStatistics1 = MultiBlockTemporalStatistics()
MultiBlockTemporalStatistics1.TimeStepType = 0
MultiBlockTemporalStatistics1.SamplingMethod = 1
MultiBlockTemporalStatistics1.TimeSpan = 0
MultiBlockTemporalStatistics1.TimeStepLength = 1
MultiBlockTemporalStatistics1.TimeCompartmentSize = 8

writer = XMLMultiBlockDataWriter()
writer.FileName = sys.argv[1]
writer.UpdatePipeline()

print 'ending'
