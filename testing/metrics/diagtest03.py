#!/usr/bin/env python

# Compute a set of contour plots using diags (diags.py).
# First argument: --datadir=<data location> - with subdirectories cam_output and obs_atmos and baseline.
# These have sample model output, observation data, and "baseline" output which we should match.
# However, the graphical output (png files) may not match in manner suitable for automated testing.
# So the return value only depends on the numerical values in the .nc files.
# Second argument: '--keep=True' to keep (don't delete) output files*
# No attempt is made to clean up the diagnostics' cache files, which are generally in /tmp.

print 'Test 3: Diagnostic multi-line (spaghetti) plots ... ',

from metrics.common.utilities import *
from pprint import pprint
import sys, os, shutil, tempfile, subprocess
import cdms2, numpy
pth = os.path.join(os.path.dirname(__file__),"..")
sys.path.append(pth)
import checkimage
import argparse

# Silence annoying messages about how to set the NetCDF file type.  Anything will do.
cdms2.setNetcdfShuffleFlag(0)
cdms2.setNetcdfDeflateFlag(0)
cdms2.setNetcdfDeflateLevelFlag(0)

p = argparse.ArgumentParser(description="Basic gm testing code for vcs")
p.add_argument("--datadir", dest="datadir", help="root directory for model and obs data")
p.add_argument("--baseline", dest="baseline", help="directory with baseline files for comparing results")
p.add_argument("--keep", dest="keep", help="Iff True, will keep computed png and nc files")
args = p.parse_args(sys.argv[1:])

def closeness( varname, filename, pathout, baselinepath, rtol, atol ):
    fname = os.path.join( pathout, filename )
    baselinefname = os.path.join( baselinepath, filename )
    f = cdms2.open( fname )
    g = cdms2.open( baselinefname )
    fvar = f(varname)
    gvar = g(varname)
    close = numpy.ma.allclose( fvar, gvar, rtol=rtol, atol=atol )
    if close:
        print "fvar and gvar are close for", varname
    else:
        print "fvar and gvar differ for", varname
        print "max difference", (fvar-gvar).max()
        print "min difference", (fvar-gvar).min()
    f.close()
    g.close()
    return close

datadir = args.datadir
path1 = os.path.join( datadir, 'cam_output' )
path2 = os.path.join( datadir, 'obs_atmos' )
baselinepath = args.baseline
pathout = tempfile.mkdtemp()
print "jfp pathout=",pathout

diagstr = "diags --outputdir '%s' --model path=%s,climos=no --obs path=%s,filter=\"f_contains('NCEP')\",climos=yes --package AMWG --set 3 --var TS --seasons JJA" % (pathout,path1,path2)
# nonstandard, suitable for testing:
proc = subprocess.Popen([diagstr],shell=True)
proc_status = proc.wait()
if proc_status!=0: 
    raise DiagError("diags run failed")

# Test of graphics (png) file match:
# This just looks at combined plot, aka summary plot, which is a compound of three plots.
filename = 'set3_Global_JJA_TS-combined.png'
fname = os.path.join( pathout, filename )
baselinefname = os.path.join( baselinepath, filename )
threshold = 1.0e6
graphics_result = checkimage.check_result_image( fname, baselinefname, threshold )
print "Graphics file",fname,"match difference:",graphics_result

# Test of NetCDF data (nc) file match:
rtol = 1.0e-3
atol = 1.0e-2   # suitable for temperatures
filename = 'set3_TS_JJA_None,None.nc'
varname1 = 'set3_TS_ft0_None'
varname2 = 'set3_TS_ft1_None'
close1 = closeness( varname1, filename, pathout, baselinepath, rtol, atol )
close2 = closeness( varname2, filename, pathout, baselinepath, rtol, atol )
filename = 'set3_TS_JJA_difference_None,None.nc'
varname = 'set3_TS_ft0_None_ft1_None_diff'
close12 = closeness( varname, filename, pathout, baselinepath, rtol, atol )
close = close1 and close2 and close12

if args.keep is True:
    print "saving output in",pathout
else:
    shutil.rmtree(pathout)

# The exit value depends on numerical values in the NetCDF file, not on the plot.
sys.exit( close )
