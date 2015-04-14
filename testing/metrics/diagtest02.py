#!/usr/bin/env python

# Compute a set of contour plots using diags (diags.py).
# First argument: data location - with subdirectories cam_output and obs_atmos and baseline.
# These have sample model output, observation data, and "baseline" output which we should match.
# However, the graphical output (png files) may not match in manner suitable for automated testing.
# Second argument: 'keep' to keep (don't delete) output files*
# At the moment this just creates plots (and .nc data files) without checking them.
# No attempt is made to clean up the diagnostics' cache files, which are generally in /tmp.

print 'Test 2: Diagnostic contour plots ... ',

from metrics.common.utilities import *
from pprint import pprint
import sys, os, shutil, tempfile, subprocess
import cdms2, numpy
pth = os.path.join(os.path.dirname(__file__),"..")
sys.path.append(pth)
import checkimage

#from markError import clearError, markError, reportError
#clearError()

datadir = sys.argv[1]
path1 = os.path.join( datadir, 'cam_output' )
path2 = os.path.join( datadir, 'obs_atmos' )
pathout = tempfile.mkdtemp()
diagstr = "diags --outputdir '%s' --model path=%s,climos=no --obs path=%s,filter=\"f_startswith('NCEP')\",climos=yes --varopts 850 --package AMWG --set 5 --var T --seasons ANN" % (pathout,path1,path2)
# nonstandard, suitable for testing:
#diagstr = "diags --outputdir '%s' --model path=%s,climos=yes --obs path=%s,filter=\"f_startswith('NCEP')\",climos=yes --varopts 850 --package AMWG --set 5 --var T --seasons ANN" % (pathout,os.path.join(datadir,'cam_output_climo'),path2)
proc = subprocess.Popen([diagstr],shell=True)
proc_status = proc.wait()
if proc_status!=0: 
    raise DiagError("diags run failed")

# Test of graphics (png) file match:
# This just looks at combined plot, aka summary plot, which is a compound of three plots.
filename = 'set5_Global_ANN_T-combined.png'
fname = os.path.join( pathout, filename )
baselinefname = os.path.join( datadir, 'baseline', filename )
threshold = 1.0e6
graphics_result = checkimage.check_result_image( fname, baselinefname, threshold )
print "Graphics file",fname,"match difference:",graphics_result

# Test of NetCDF data (nc) file match:
filename = 'T_ANN_at_850_mbar_(1)_None.nc'
varname = 'dv_T_lp_ANN_ft0_None_None'
fname = os.path.join( pathout, filename )
baselinefname = os.path.join( datadir, 'baseline', filename )
f = cdms2.open( fname )
print f.variables.keys()
g = cdms2.open( baselinefname )
fvar = f(varname)
gvar = g(varname)
rtol = 1.0e-3
atol = 1.0e-2   # suitable for temperatures
close = numpy.ma.allclose( fvar, gvar, rtol=rtol, atol=atol )
if close:
    print "fvar and gvar are close for", varname
else:
    print "fvar and gvar differ for", varname
    print "max difference", (fvar-gvar).max()
    print "min difference", (fvar-gvar).min()

if len(sys.argv)>2 and sys.argv[2]=='keep':
    print "saving output in",pathout
else:
    shutil.rmtree(pathout)

# The exit value depends on numerical values in the NetCDF file, not on the plot.
sys.exit( close )
