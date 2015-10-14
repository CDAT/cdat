#!/usr/bin/env python
"""" In this file the inputs for the test are defined and passed to diags_test.execute"""
import diags_test
from metrics.packages.amwg.amwg import amwg_plot_set41

print amwg_plot_set41.name

test_str = 'Test 41\n'
#run this from command line to get the files required
example = "./diagtest41.py --datadir ~/uvcmetrics_test_data/ --baseline ~/uvcdat-testdata/baselines/metrics/ --keep True"

plotset = 41
#filterid = 'f_contains'
#obsid = 'NCEP'
#varid = 'T'
#seasonid = 'ANN'
#modeldir = 'cam_output'
#obsdir = 'obs_atmos'
filterid = 'f_startswith'
obsid = 'NCEP'
varid = 'T'
seasonid = 'ANN'
modeldir = 'cam35_data'
obsdir = 'obs'
dt = diags_test.DiagTest( modeldir, obsdir, plotset, filterid, obsid, varid, seasonid )

# Test of graphics (png) file match:
# This just looks at combined plot, aka summary plot, which is a compound of three plots.
imagefilename = 'set41_Global_ANN_T-combined.png'
imagethreshold = None
ncfiles = {}
ncfiles['T_ANN_(1)_None.nc'] = ['dv_T_levlon_ANN_ft0_None_None']#rv_T_ANN_ft0_None']
ncfiles['T_ANN_(2)_None.nc'] = ['rv_T_ANN_ft1_None']

# Test of NetCDF data (nc) file match:
rtol = 1.0e-3
atol = 1.0e-2   # suitable for temperatures

dt.execute(test_str, imagefilename, imagethreshold, ncfiles, rtol, atol)
