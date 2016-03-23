#!/usr/bin/env python
"""" In this file the inputs for the test are defined and passed to diags_test.execute"""
import diags_test
from metrics.packages.amwg.amwg import amwg_plot_set9

print amwg_plot_set9.name

test_str = 'Test 09\n'
#run this from command line to get the files required
example = "./diagtest09.py --datadir ~/uvcmetrics_test_data/ --baseline ~/uvcdat-testdata/baselines/metrics/ --keep True"

plotset = 9
filterid = 'f_contains'
obsid = 'NCEP'
varid = 'T'
seasonid = 'ANN'
modeldir = 'cam_output'
obsdir = 'obs_atmos'
dt = diags_test.DiagTest( modeldir, obsdir, plotset, filterid, obsid, varid, seasonid )

# Test of graphics (png) file match:
# This just looks at combined plot, aka summary plot, which is a compound of three plots.
imagefilename = 'figure-set9_Global_ANN_T_plot-combined.png'
imagethreshold = None
ncfiles = {}
ncfiles['rv_T_DJF_ft1_cam_output_DJF-JJA.nc'] = ['rv_T_DJF_ft1_cam_output']
ncfiles['rv_T_JJA_ft2_obs_atmos_NCEP_DJF-JJA.nc'] = ['rv_T_JJA_ft2_obs_atmos_NCEP']

# Test of NetCDF data (nc) file match:
rtol = 1.0e-3
atol = 1.0e-2   # suitable for temperatures

dt.execute(test_str, imagefilename, imagethreshold, ncfiles, rtol, atol)
