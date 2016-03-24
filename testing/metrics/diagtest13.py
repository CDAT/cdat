#!/usr/bin/env python
"""" In this file the inputs for the test are defined and passed to diags_test.execute"""
import diags_test
from metrics.packages.amwg.amwg import amwg_plot_set13

print amwg_plot_set13.name

test_str = 'Test 13\n'
#run this from command line to get the files required
example = "./diagtest13.py --datadir ~/uvcmetrics_test_data/ --baseline ~/uvcdat-testdata/baselines/metrics/ --keep True"

plotset = 13
filterid = 'f_startswith'
obsid = 'ISCCPCOSP'
varid = 'CLISCCP'
seasonid = 'ANN'
modeldir = 'cam35_data_smaller'
obsdir = 'obs_data_13'
dt = diags_test.DiagTest( modeldir, obsdir, plotset, filterid, obsid, varid, seasonid )

# Test of graphics (png) file match:
# This just looks at combined plot, aka summary plot, which is a compound of three plots.
imagefilename = 'figure-set13_Global_ANN_CLISCCP_plot-combined.png'
imagethreshold = None
ncfiles = {}
ncfiles['CLISCCP_ANN_Global_(1)_cam35_data_smaller.nc'] = ['dv_CLISCCP_ANN_ft1_cam35_data_smaller']
ncfiles['CLISCCP_ANN_Global_(2)_obs_data_13_ISCCPCOSP.nc'] = ['rv_CLISCCP_ANN_Global_ft2_obs_data_13_ISCCPCOSP']

# Test of NetCDF data (nc) file match:
rtol = 1.0e-3
atol = 1.0e-2   # suitable for temperatures

dt.execute(test_str, imagefilename, imagethreshold, ncfiles, rtol, atol)
