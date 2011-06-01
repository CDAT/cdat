import sys
import os
import os.path
import numpy
from  ctypes import CDLL, c_char_p, c_int, byref, c_double, c_uint, pointer, create_string_buffer
import cdms2
import time
import config
sys.path.append("/home/painter1/libcf/")
libcf = CDLL(config.prefix + '/lib/libcf.so')

class GS_Regridder:

    def __init__( self, ingrid, outgrid,
                  infile=None, outfile=None, remapfile=None ):
        # Save the grids, make and save a Gridspec remap file.
        # For now, we are using the libCF/Gridspec API which operates only on files;
        # thus temporary files are written out and read back in.  That requires the
        # input grids to support a "write_gridspec" method.
        # If a path is provided, it prefixes whatever filenames were input.
        # If there is no path provided, each filename must include its path.
        # Note: it's a bit messy to keep filenames and paths separate internally,
        # but the presnet Gridspec function expects lots of directories.

        self.ingrid = ingrid
        self.outgrid = outgrid
        if ( not hasattr(ingrid,"gsfile") ):
            ingrid.gsfile = None
            ingrid.gspath = None
        if ( infile==None ):
            self.infile = ingrid.gsfile
            self.inpath = ingrid.gspath
        else:
            self.infile = os.path.basename(infile)
            self.inpath = os.path.dirname(infile)
        if ( not os.path.isfile( self.inpath + "/" + self.infile ) ):
            raise OSError,"cannot open infile " + self.inpath + "/" + self.infile

        if ( not hasattr(outgrid,"gsfile") ):
            outgrid.gsfile = None
            outgrid.gspath = None
        if ( outfile==None ):
            self.outfile = outgrid.gsfile
            self.outpath = outgrid.gspath
        else:
            self.outfile = os.path.basename(outfile)
            self.outpath = os.path.dirname(outfile)
        if ( not os.path.isfile( self.outpath + "/" + self.outfile ) ):
            raise OSError, "cannot open outfile " + self.outpath + "/" + self.outfile

        if ( remapfile==None ):
            timestr = str(int(time.time()))
            self.remapfile = "remap" + timestr
            self.remappath = "/tmp"
        else:
            self.remapfile = os.path.basename(remapfile)
            self.remappath = os.path.dirname(remapfile)
        if ( not os.path.isdir( self.remappath + "/" ) ):
            raise OSError, "cannot open remapfile directory " + self.remappath + "/"

        ingrid.write_gridspec( self.inpath + "/" + self.infile )
        outgrid.write_gridspec( self.outpath + "/" + self.outfile )

        history="GS_Regridder"
        mosaic_in = self.inpath + "/" + self.infile
        mosaic_out = self.outpath + "/" + self.outfile


        # No variables to interpolate; the gs_fregrid call will be just to
        # make a remap file...
        dir_in = 256*"\x00"
        dir_out = 256*"\x00"
        input_file = 256*"\x00"
        nfiles = 0
        output_file = 256*"\x00"
        nfiles_out = 0
        scalar_name = 256*"\x00"
        nscalar = 0
        u_name = 256*"\x00"
        v_name = 256*"\x00"
        nvector = 0
        nvector2 = 0

        # For a call which only writes the remap files, gs_fregrid
        # expects remapfile to be a full path.  For a call in which
        # remapping takes place, gs_fregrid expects remapfile to be
        # a pure filename, in a path it gets from elsewhere, maybe dir_in.
        # (ARRGH - but with the API slated to be replaced, I'll live with it)
        remapf = os.path.abspath( self.remappath + "/" + self.remapfile ) + "\0"*256

        interp_method = "conserve_order2"
        test_case = None
        test_param = c_double(1.0)
        opcode = c_uint(0)
        AGRID = 64
        grid_type = AGRID
        finer_step = c_uint(0)
        fill_missing = 0
        nlon = 0
        nlat = 0
        check_conserve = 0
        y_at_center = 0
        lonbegin = c_double(0.)
        lonend = c_double(360.)
        latbegin = c_double(-90.)
        latend = c_double(90.)
        lbegin = 0
        lend = -1
        kbegin = 0
        kend = -1

        libcf.gs_fregrid( history, mosaic_in, mosaic_out, dir_in, dir_out,
                          input_file, nfiles, output_file, nfiles_out,
                          remapf,
                          scalar_name, nscalar, u_name, nvector, v_name,
                          nvector2, interp_method, test_case, test_param, opcode,
                          grid_type, finer_step, fill_missing, nlon, nlat,
                          check_conserve, y_at_center, lonbegin, lonend, latbegin,
                          latend, kbegin, kend, lbegin, lend )

    def __call__( self, ar ):
        # Interpolate the input variable to the new grid using the Gridspec
        # remap file generated when this GS_Remapper object was initialized.
        # >>>for now, ar is required to be a variable (MV) <<<

        #  Here, convert ar into a file for gs_fregrid 
        # There's no special Gridspec way to write a variable; you write to any
        # *.nc file.  The tough part is making sure you have there exactly what's
        # needed for the gs_fregrid call, especially because the grids are
        # supposedly supergrids and you just have some keywords to use to figure
        # out what goes where.  And note that we'll need to check whether ar
        # really lives on self.ingrid .  Supposedly a future API will work
        # better in this respect

        # Write the variable to a temporary file, as required by gs_fregrid.
        # The remap path should be suitable.
        # >>> this should be done more carefully, e.g. deal with failures;
        # >>> more worth doing when we have mosaic variables.
        timestr = str(int(time.time()))
        varfile=cdms2.open( self.inpath+"/"+"invvar"+timestr+".nc", 'w' )
        varfile.write(ar)
        varfile.close()

        history="GS_Regridder"
        mosaic_in = self.inpath + "/" + self.infile
        mosaic_out = self.outpath + "/" + self.outfile

        dir_in = 256*"\x00"  # path is already encoded in input_file
        dir_out = 256*"\x00"  # path is already encoded in output_file
        input_file = varfile.id + 256*"\x00"
        nfiles = 1
        output_file = self.outpath+'/'+"outvar"+timestr+".nc" +256*"\x00"
        nfiles_out = 1
        scalar_name = ar.id +256*"\x00"
        nscalar = 1
        u_name = 256*"\x00"
        v_name = 256*"\x00"
        nvector = 0
        nvector2 = 0

        interp_method = "conserve_order2"
        test_case = None
        test_param = c_double(1.0)
        opcode = c_uint(0)
        AGRID = 64
        grid_type = AGRID
        finer_step = c_uint(0)
        fill_missing = 0
        nlon = 0
        nlat = 0
        check_conserve = 0
        y_at_center = 0
        lonbegin = c_double(0.)
        lonend = c_double(360.)
        latbegin = c_double(-90.)
        latend = c_double(90.)
        lbegin = 0
        lend = -1
        kbegin = 0
        kend = -1

        print "__call__  remapfile=", self.remapfile
        libcf.gs_fregrid( history, mosaic_in, mosaic_out, dir_in, dir_out,
                          input_file, nfiles, output_file, nfiles_out,
                          self.remapfile,
                          scalar_name, nscalar, u_name, nvector, v_name,
                          nvector2, interp_method, test_case, test_param, opcode,
                          grid_type, finer_step, fill_missing, nlon, nlat,
                          check_conserve, y_at_center, lonbegin, lonend, latbegin,
                          latend, kbegin, kend, lbegin, lend )

        # Read the output_file into a variable, and return the variable
        f = cdms2.open( path + "/" + output_file )
        vout = f(scalar_name)
        f.close()
        return vout
