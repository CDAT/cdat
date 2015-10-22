#!/usr/bin/env python
"""" In this file the inputs for the test are defined and passed to diags_test.execute"""
import diags_test
from metrics.packages.amwg.amwg import amwg_plot_set11

print amwg_plot_set11.name

test_str = 'Test 11\n'
#run this from command line to get the files required
example = "./diagtest11.py --datadir ~/uvcmetrics_test_data/ --baseline ~/uvcdat-testdata/baselines/metrics/ --keep True"

plotset = 11
filterid = 'f_startswith'
obsid = 'NCEP'
varid = 'LWCF'
seasonid = 'JAN'
modeldir = 'cam35_data'
obsdir = 'obs'
dt = diags_test.DiagTest( modeldir, obsdir, plotset, filterid, obsid, varid, seasonid )

# Test of graphics (png) file match:
# This just looks at combined plot, aka summary plot, which is a compound of three plots.
imagefilename = 'set11_Global_JAN_LWCF-combined.png'
imagethreshold = 1.0e6
ncfiles = {}

# Test of NetCDF data (nc) file match:
rtol = 1.0e-3
atol = 1.0e-2   # suitable for temperatures

dt.execute(test_str, imagefilename, imagethreshold, ncfiles, rtol, atol)