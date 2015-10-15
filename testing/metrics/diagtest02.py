#!/usr/bin/env python
"""" In this file the inputs for the test are defined and passed to diags_test.execute"""
import diags_test
from metrics.packages.amwg.amwg import amwg_plot_set2

print amwg_plot_set2.name

test_str = 'Test 2: Diagnostic contour plots ... '
#run this from command line to get the files required
example = "./diagtest2.py --datadir ~/uvcmetrics_test_data/ --baseline ~/uvcdat-testdata/baselines/metrics/ --keep True"

plotset = 2
filterid = 'f_contains'
obsid = 'NCEP'
varid = 'Ocean_Heat'
seasonid = 'JAN'
modeldir = 'cam_output'
obsdir = 'obs_atmos'
dt = diags_test.DiagTest( modeldir, obsdir, plotset, filterid, obsid, varid, seasonid )

# Test of graphics (png) file match:
# This just looks at combined plot, aka summary plot, which is a compound of three plots.
imagefilename = 'set2_Global_JAN_Ocean_Heat-combined.png'
imagethreshold = None
ncfiles = {}
ncfiles['CAM_and_NCEP_HEAT_TRANSPORT_ATLANTIC_cam_output_.nc'] = ['CAM_HEAT_TRANSPORT_ALL_1']
ncfiles['CAM_and_NCEP_HEAT_TRANSPORT_GLOBAL_cam_output_.nc'] = ['CAM_HEAT_TRANSPORT_ALL_1']
ncfiles['CAM_and_NCEP_HEAT_TRANSPORT_INDIAN_cam_output_.nc'] = ['CAM_HEAT_TRANSPORT_ALL_1']
ncfiles['CAM_and_NCEP_HEAT_TRANSPORT_PACIFIC_cam_output_.nc'] = ['CAM_HEAT_TRANSPORT_ALL_1']
# Test of NetCDF data (nc) file match:
rtol = 1.0e-3
atol = 1.0e-2   # suitable for temperatures

dt.execute(test_str, imagefilename, imagethreshold, ncfiles, rtol, atol)
