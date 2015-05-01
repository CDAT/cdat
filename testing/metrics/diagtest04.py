#!/usr/bin/env python
"""" In this file the inputs for the test are defined and passed to diags_test.execute"""

import diags_test
from metrics.packages.amwg.amwg import amwg_plot_set4

print amwg_plot_set4.name

test_str = 'Test 4\n'
#run this from command line to get the files required
example = "./diagtest04.py --datadir ~/uvcmetrics_test_data/ --baseline ~/uvcdat-testdata/baselines/metrics/ --keep True"

plotset = 4
filterid = 'f_contains'
obsid = 'NCEP'
varid = 'T'
seasonid = 'JJA'
modeldir = 'cam_output'
obsdir = 'obs_atmos'
dt = diags_test.DiagTest( modeldir, obsdir, plotset, filterid, obsid, varid, seasonid )

# Test of graphics (png) file match:
# This just looks at combined plot, aka summary plot, which is a compound of three plots.
imagefilename = 'set4_Global_JJA_T-combined.png'
imagethreshold = 1.0e6
ncfiles = {}
ncfiles['T_JJA_(1)_None.nc'] = ['rv_T_JJA_ft0_None']
ncfiles['T_JJA_(2)_None.nc'] = ['rv_T_JJA_ft1_None']

# Test of NetCDF data (nc) file match:
rtol = 1.0e-3
atol = 1.0e-2   # suitable for temperatures

dt.execute(test_str, imagefilename, imagethreshold, ncfiles, rtol, atol)