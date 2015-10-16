#!/usr/bin/env python
"""" In this file the inputs for the test are defined and passed to diags_test.execute"""
import diags_test
from metrics.packages.amwg.amwg import amwg_plot_set7

print amwg_plot_set7.name

test_str = 'Test 07\n'
#run this from command line to get the files required
example = "./diagtest07.py --datadir ~/uvcmetrics_test_data/ --baseline ~/uvcdat-testdata/baselines/metrics/ --keep True"

plotset = 7
filterid = 'f_contains'
obsid = 'NCEP'
varid = 'T'
seasonid = 'ANN'
modeldir = 'cam_output'
obsdir = 'obs_atmos'
dt = diags_test.DiagTest( modeldir, obsdir, plotset, filterid, obsid, varid, seasonid )

# Test of graphics (png) file match:
# This just looks at combined plot, aka summary plot, which is a compound of three plots.
imagefilename = 'set7_Global_ANN_T-combined.png'
imagethreshold = None
ncfiles = {}
ncfiles['rv_T_ANN_ft0_cam_output__ANN.nc'] = ['rv_T_ANN_ft0_cam_output']
ncfiles['rv_T_ANN_ft1_obs_atmos_NCEP__ANN.nc'] = ['rv_T_ANN_ft1_obs_atmos_NCEP']

# Test of NetCDF data (nc) file match:
rtol = 1.0e-3
atol = 1.0e-2   # suitable for temperatures

dt.execute(test_str, imagefilename, imagethreshold, ncfiles, rtol, atol)
