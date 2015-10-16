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

class DiagTest(object):
    def __init__(self, modeldir, obsdir, plotset, filterid, obsid, varid, seasonid, extra_parts=[] ):
        
        #get commmand line args
        p = argparse.ArgumentParser(description="Basic gm testing code for vcs")
        p.add_argument("--datadir", dest="datadir", help="root directory for model and obs data")
        p.add_argument("--baseline", dest="baseline", help="directory with baseline files for comparing results")
        p.add_argument("--keep", dest="keep", help="If True, will keep computed png and nc files")
        args = p.parse_args(sys.argv[1:])
        self.datadir = args.datadir
        print 'datadir = ', self.datadir
        self.baselinepath = args.baseline + 'plotset' + str(plotset)
        print "baselinepath = ", self.baselinepath
        self.keep = False
        if args.keep:
            self.keep = args.keep
         
    
        #setup paths to data
        self.modelpath = os.path.join( self.datadir, modeldir )
        self.obspath = os.path.join( self.datadir, obsdir )   
        self.outpath = tempfile.mkdtemp() + "/"
        print "outpath=", self.outpath
    
        #setup string to be executed and run script
        #diagstr = "diags --no-antialiasing --outputdir '%s' --model path=%s,climos=no --obs path=%s,filter=\"f_contains('NCEP')\",climos=yes --package AMWG --set 3 --var T --seasons JJA" % (outpath, modelpath, obspath)
        diagstr_parts = [" --outputdir %s "%(self.outpath), 
 			             " --no-antialiasing",
                         " --model path=%s,climos=no "%(self.modelpath), 
                         " --obs path=%s,filter=\"%s('%s')\",climos=yes "%(self.obspath, filterid, obsid),
                         " --package AMWG ", 
                         " --set %s "%(str(plotset)), 
                         " --var %s"%(varid), 
                         " --seasons %s "%(seasonid)]
        self.diagstr = "diags "
        for part in diagstr_parts:
            #print part
            self.diagstr += part
        for part in extra_parts:
            self.diagstr += part
        print 'executing '
        print self.diagstr
    def closeness( self, varname, filename, rtol, atol ):
        #pdb.set_trace()
        testfname = os.path.join( self.outpath, filename )
        #print '>>>>>>>>>>>>>>>>>>> ', testfname
        baselinefname = os.path.join( self.baselinepath, filename )
        #print '>>>>>>>>>>>>>>>>>>> ', baselinefname
        f = cdms2.open( testfname )
        g = cdms2.open( baselinefname )
        fvar = f(varname)
        gvar = g(varname)
        #print '>>>>>>>>>>>>>>>>>>> fvar', fvar.shape
        #print '>>>>>>>>>>>>>>>>>>> gvar', gvar.shape
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
    def execute(self, test_str, imagefilename, imagethreshold, ncfiles, rtol, atol):
        print test_str
        if imagethreshold is None:  # user didn't specify a value
     	    imagethreshold = checkimage.defaultThreshold
        # Silence annoying messages about how to set the NetCDF file type.  Anything will do.
        cdms2.setNetcdfShuffleFlag(0)
        cdms2.setNetcdfDeflateFlag(0)
        cdms2.setNetcdfDeflateLevelFlag(0)
        
        # nonstandard, suitable for testing:
        proc = subprocess.Popen([self.diagstr], shell=True)
        proc_status = proc.wait()
        if proc_status!=0: 
            raise DiagError("diags run failed")
    
        if self.keep:
            print "save ", imagefilename, ncfiles.keys()
            print "output directory is = ", self.outpath
        else:    
            # Test of graphics (png) file match:
            # This just looks at combined plot, aka summary plot, which is a compound of three plots.
            
            imagefname = os.path.join( self.outpath, imagefilename )
            imagebaselinefname = os.path.join( self.baselinepath, imagefilename )
            #pdb.set_trace()
            print "OK THRESHOLD IS:",imagethreshold
            graphics_result = checkimage.check_result_image( imagefname, imagebaselinefname, imagethreshold )
            print "Graphics file", imagefname, "match difference:", graphics_result
            
            #initialize to successful graphics check
            GR_CLOSE = (graphics_result == 0)
            assert(GR_CLOSE), 'graphic images are not close'
            
            # Test of NetCDF data (nc) file match:
            NC_CLOSE = True
            for ncfilename, ncvars in ncfiles.items():
                for var in ncvars:
                    #print ncfilename, var
                    try:
                        #print ">>>>>>>>>>>>>", var, ncfilename
                        close = self.closeness( var, ncfilename, rtol, atol )
                        if not close:
                            print var, ' in ', ncfilename, ' is not close.'
                    except:
                        print 'comparison failed for ', var, ' in file: ', ncfilename
                        close = False
                    NC_CLOSE = NC_CLOSE and close
            assert(NC_CLOSE), 'NetCDF files are not close'
            
            #cleanup the temp files
            if GR_CLOSE and NC_CLOSE:
                shutil.rmtree(self.outpath)
    
if __name__ == "__main__":
    dt = diag_test('a', 'b', 1, 'c', 'd', 'e', 'f')
    print dt.modelpath
    print dt.obspath
    print dt.outpath
    print dt.keep
