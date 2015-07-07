#!/usr/bin/env python
"""" In this file the inputs for the test are defined and passed to diags_test.execute"""
import diags_test
from metrics.packages.amwg.amwg import amwg_plot_set12

print amwg_plot_set12.name

test_str = 'Test 12\n'
#run this from command line to get the files required
example = "./diagtest12.py --datadir ~/uvcmetrics_test_data/ --baseline ~/uvcdat-testdata/baselines/metrics/ --keep True"

plotset = 12
filterid = 'f_startswith'
obsid = 'RAOBS'
varid = 'T'
seasonid = 'JAN'
modeldir = 'model_data_12' 
obsdir = 'obs_data_12' 
dt = diags_test.DiagTest( modeldir, obsdir, plotset, filterid, obsid, varid, seasonid, extra_parts=['--varopts "SanFrancisco_CA" '] )

# Test of graphics (png) file match:
# This just looks at combined plot, aka summary plot, which is a compound of three plots.
imagefilename = 'set12_Global_JAN_T-combined.png'
imagethreshold = 1.0e6
ncfiles = {}

# Test of NetCDF data (nc) file match:
rtol = 1.0e-3
atol = 1.0e-2   # suitable for temperatures

dt.execute(test_str, imagefilename, imagethreshold, ncfiles, rtol, atol)