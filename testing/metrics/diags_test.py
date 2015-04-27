# Compute a set of contour plots using diags (diags.py).
# First argument: --datadir=<data location> - with subdirectories cam_output and obs_atmos and baseline.
# These have sample model output, observation data, and "baseline" output which we should match.
# However, the graphical output (png files) may not match in manner suitable for automated testing.
# So the return value only depends on the numerical values in the .nc files.
# Second argument: '--keep=True' to keep (don't delete) output files*
# No attempt is made to clean up the diagnostics' cache files, which are generally in /tmp.

from metrics.common.utilities import *
from pprint import pprint
import sys, os, shutil, tempfile, subprocess
import cdms2, numpy
pth = os.path.join(os.path.dirname(__file__),"..")
sys.path.append(pth)
import checkimage
import argparse, pdb

def closeness( varname, filename, pathout, baselinepath, rtol, atol ):
    print "$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$"
    fname = os.path.join( pathout, filename )
    baselinefname = os.path.join( baselinepath, filename )
    f = cdms2.open( fname )
    g = cdms2.open( baselinefname )
    fvar = f(varname)
    gvar = g(varname)
    print "FVAR:",fvar.shape
    print "GVAR:",gvar.shape
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

def execute(test_str, plotset, obstype, varid, season, imagefilename, imagethreshold, ncfiles, rtol, atol):
    print test_str
    # Silence annoying messages about how to set the NetCDF file type.  Anything will do.
    cdms2.setNetcdfShuffleFlag(0)
    cdms2.setNetcdfDeflateFlag(0)
    cdms2.setNetcdfDeflateLevelFlag(0)
    
    #get commmand line args
    p = argparse.ArgumentParser(description="Basic gm testing code for vcs")
    p.add_argument("--datadir", dest="datadir", help="root directory for model and obs data")
    p.add_argument("--baseline", dest="baseline", help="directory with baseline files for comparing results")
    p.add_argument("--keep", dest="keep", help="Iff True, will keep computed png and nc files")
    args = p.parse_args(sys.argv[1:])
    datadir = args.datadir
    baselinepath = args.baseline
    keep = args.keep

    #setup paths to data
    modelpath = os.path.join( datadir, 'cam_output' )
    obspath = os.path.join( datadir, 'obs_atmos' )   
    outpath = tempfile.mkdtemp() + "/"
    print "outpath=", outpath

    #setup string to be executed and run script
    #diagstr = "diags --outputdir '%s' --model path=%s,climos=no --obs path=%s,filter=\"f_contains('NCEP')\",climos=yes --package AMWG --set 3 --var T --seasons JJA" % (outpath, modelpath, obspath)
    diagstr_parts = [ " --outputdir %s "%(outpath), " --model path=%s,climos=no "%(modelpath), " --obs path=%s,filter=\"f_contains('%s')\",climos=yes "%(obspath, obstype),
                       " --package AMWG ", " --set %s "%(str(plotset)), " --var %s"%(varid), " --seasons %s"%(season)]
    diagstr = "diags "
    for part in diagstr_parts:
        diagstr += part
    print 'executing '
    print diagstr
        
    # nonstandard, suitable for testing:
    proc = subprocess.Popen([diagstr], shell=True)
    proc_status = proc.wait()
    if proc_status!=0: 
        raise DiagError("diags run failed")

    if keep:
        print "save ", imagefilename, ncfiles.keys()
        print "output directory is = ", outpath
    else:    
        # Test of graphics (png) file match:
        # This just looks at combined plot, aka summary plot, which is a compound of three plots.
        
        imagefname = os.path.join( outpath, imagefilename )
        imagebaselinefname = os.path.join( baselinepath, imagefilename )
        graphics_result = checkimage.check_result_image( imagefname, imagebaselinefname, imagethreshold )
        print "Graphics file", imagefname, "match difference:", graphics_result
        
        # Test of NetCDF data (nc) file match:
        CLOSE = True
        for ncfilename, ncvars in ncfiles.items():
            for var in ncvars:
                #print ncfilename, var
                print baselinepath
                try:
                    close = closeness( var, ncfilename, outpath, baselinepath, rtol, atol )
                    if not close:
                        print var, ' in ', ncfilename, ' is not close.'
                except:
                    print 'comparison failed ', ncfilename, var
                    close = False
                CLOSE = CLOSE and close
                
        #cleanup the temp files
        shutil.rmtree(outpath)
        print "CLOSE IS:",CLOSE,close
        assert(CLOSE)#, 'data are not close'
