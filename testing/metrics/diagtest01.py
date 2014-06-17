#!/usr/bin/env python

# simple diagnostics test - compute a single diagnostic and just check for exceptions
# First argument: data location - with subdirectories acme_lores_atm_climo and obs
# Second argument: 'keep' to keep (don't delete) output files in /tmp/diag*

print 'Test 1: Sample Diagnostic ... ',

import hashlib, os, pickle, sys, os, time
from metrics import *
from metrics.fileio.filetable import *
from metrics.fileio.findfiles import *
from metrics.computation.reductions import *
from metrics.packages.amwg import *
from metrics.packages.amwg.derivations.vertical import *
from metrics.packages.amwg.plot_data import plotspec, derived_var
from metrics.packages.amwg.derivations import *
from metrics.packages.diagnostic_groups import *
from metrics.frontend.uvcdat import *
from metrics.frontend.options import *
from pprint import pprint
import cProfile
import tempfile

#from markError import clearError, markError, reportError
#clearError()

datadir = sys.argv[1]
path1 = os.path.join( datadir, 'acme_lores_atm_climo' )
path2 = os.path.join( datadir, 'obs' )
tmppth = tempfile.mkdtemp()
outpath = tempfile.mkdtemp()

filt1 = None
opts1 = Options()
opts1._opts['path']={'model':path1}
opts1._opts['filter']=filt1
opts1._opts['cachepath']=tmppth
datafiles1 = dirtree_datafiles( opts1, pathid='model' )
filetable1 = datafiles1.setup_filetable( "model" )
filt2 = filt1
opts2 = Options()
opts2._opts['path'] = {'obs':path2}
opts2._opts['filter'] = filt2
opts2._opts['cachepath']=tmppth
datafiles2 = dirtree_datafiles( opts2, pathid='obs' )
filetable2 = datafiles2.setup_filetable( "obs" )

dm = diagnostics_menu()
pname = "AMWG"
pclass = dm[pname]
package = pclass()
sm = package.list_diagnostic_sets()
sname = ' 3 - Line Plots of  Zonal Means'
sclass = sm[sname]
seasonid = 'DJF'
varid = 'T'
vard = package.all_variables( filetable1, filetable2, sname )
var = vard[varid]

plot = sclass( filetable1, filetable2, varid, seasonid )
res = plot.compute()
if res is not None:
    if res.__class__.__name__ is 'uvc_composite_plotspec':
        resc = res
    else:
        resc = uvc_composite_plotspec( res )
    filenames = resc.write_plot_data("xml-NetCDF", outpath )
    print "created and deleting files",filenames

    # clean up
    if len(sys.argv)>2 and sys.argv[2]=='keep':
        print "saving output in",outpath," and",tmppth
    else:
        for f in filenames: os.unlink(f)
        datafiles1.clear_filetable()
        datafiles2.clear_filetable()
        # N.B. There's something not cleaned up here - cdscan output, though it may not be
        # present.  If it's there, it's an xml file in $tmppth.
        # But it's dangerous to delete without knowing its actual name,
        # and messy to keep track of its name on a global basis.
        # Rather than deal with that alone, I'll need to add a general facility for keeping
        # track of cache and other files that the diagnostics have written.

