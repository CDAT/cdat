#!/usr/bin/env python
"""" In this file the inputs for the test are defined and passed to diags_test.execute"""

import diags_test
from metrics.packages.amwg.amwg import amwg_plot_set4

print amwg_plot_set4.name

test_str = 'Test 4\n'
#run this from command line to get the files required
example = "./diagtest04.py --datadir ~/uvcmetrics_test_data/ --baseline ~/uvcdat-testdata/baselines/metrics/ --keep True"

plotset = 4
filterid = 'f_startswith'
obsid = 'NCEP'
varid = 'T'
seasonid = 'ANN'
modeldir = 'cam35_data_smaller'
obsdir = 'obs'
dt = diags_test.DiagTest( modeldir, obsdir, plotset, filterid, obsid, varid, seasonid )

# Test of graphics (png) file match:
# This just looks at combined plot, aka summary plot, which is a compound of three plots.
imagefilename = 'set4_Global_ANN_T-combined.png'
imagethreshold = None
ncfiles = {}
ncfiles['T_ANN_(1)_cam35_data_smaller.nc'] = ['dv_T_levlat_ANN_ft0_cam35_data_smaller']
ncfiles['T_ANN_(2)_obs_NCEP.nc'] = ['rv_T_ANN_ft1_obs_NCEP']

# Test of NetCDF data (nc) file match:
rtol = 1.0e-3
atol = 1.0e-2   # suitable for temperatures

dt.execute(test_str, imagefilename, imagethreshold, ncfiles, rtol, atol)
