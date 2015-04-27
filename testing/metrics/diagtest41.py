#!/usr/bin/env python
"""" In this file the inputs for the test are defined and passed to diags_test.execute"""
import diags_test
from metrics.packages.amwg.amwg import amwg_plot_set41

print amwg_plot_set41.name

test_str = 'Test 41\n'
#run this from command line to get the files required
example = "./diagtest41.py --datadir ~/uvcmetrics_test_data/ --baseline ~/uvcdat-testdata/baselines/metrics/ --keep True"

plotset = 41
obstype = 'NCEP'
varid = 'T'
season = 'JJA'

# Test of graphics (png) file match:
# This just looks at combined plot, aka summary plot, which is a compound of three plots.
imagefilename = 'set41_Global_JJA_T-combined.png'
imagethreshold = 1.0e6
ncfiles = {}
ncfiles['T_JJA_(1)_None.nc'] = ['rv_T_JJA_ft0_None']
ncfiles['T_JJA_(2)_None.nc'] = ['rv_T_JJA_ft1_None']

# Test of NetCDF data (nc) file match:
rtol = 1.0e-3
atol = 1.0e-2   # suitable for temperatures

diags_test.execute(test_str, plotset, obstype, varid, season, imagefilename, imagethreshold, ncfiles, rtol, atol)