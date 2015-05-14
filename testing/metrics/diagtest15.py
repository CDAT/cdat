#!/usr/bin/env python
"""" In this file the inputs for the test are defined and passed to diags_test.execute"""
import diags_test
from metrics.packages.amwg.amwg import amwg_plot_set15

print amwg_plot_set15.name

test_str = 'Test 15\n'
#run this from command line to get the files required
example = "./diagtest15.py --datadir ~/uvcmetrics_test_data/ --baseline ~/uvcdat-testdata/baselines/metrics/ --keep True"

plotset = 15
filterid = 'f_contains'
obsid = 'NCEP'
varid = 'T'
seasonid = 'ANN'
modeldir = 'cam_output'
obsdir = 'obs_atmos'
dt = diags_test.DiagTest( modeldir, obsdir, plotset, filterid, obsid, varid, seasonid  )

# Test of graphics (png) file match:
# This just looks at combined plot, aka summary plot, which is a compound of three plots.
imagefilename = 'set15_Global_ANN_T-combined.png'
imagethreshold = 1.0e6
ncfiles = {}
ncfiles['model_.nc'] = ['dv_T_Level model_ANN_ft0_None_None']
ncfiles['obs_.nc'] = ['dv_T_Level obs_ANN_ft1_None_None']

# Test of NetCDF data (nc) file match:
rtol = 1.0e-3
atol = 1.0e-2   # suitable for temperatures

dt.execute(test_str, imagefilename, imagethreshold, ncfiles, rtol, atol)