#!/usr/bin/env python
"""" In this file the inputs for the test are defined and passed to diags_test.execute"""
import diags_test
from metrics.packages.amwg.amwg import amwg_plot_set3

print amwg_plot_set3.name

test_str = 'Test 3: Diagnostic multi-line (spaghetti) plots ... \n'
#run this from command line to get the files required
example = "./diagtest03.py --datadir ~/uvcmetrics_test_data/ --baseline ~/uvcdat-testdata/baselines/metrics/ --keep True"

plotset = 3
filterid = 'f_contains'
obsid = 'NCEP'
varid = 'T'
seasonid = 'JJA'
modeldir = 'cam_output'
obsdir = 'obs_atmos'
dt = diags_test.DiagTest( modeldir, obsdir, plotset, filterid, obsid, varid, seasonid )

# Test of graphics (png) file match:
# This just looks at combined plot, aka summary plot, which is a compound of three plots.
imagefilename = 'set3_Global_JJA_T-combined.png'
imagethreshold = None
ncfiles = {}
ncfiles['set3_T_JJA_cam_output,obs_atmos_NCEP.nc'] = ['set3_T_ft0_cam_output', 'set3_T_ft1_obs_atmos_NCEP']

# Test of NetCDF data (nc) file match:
rtol = 1.0e-3
atol = 1.0e-2   # suitable for temperatures

dt.execute(test_str, imagefilename, imagethreshold, ncfiles, rtol, atol)
