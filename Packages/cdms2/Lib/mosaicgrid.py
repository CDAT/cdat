"""CDMS Mosaic Grid objects, Gridspec-compatible"""

import sys
import os
import os.path
from  ctypes import CDLL, c_char_p, c_int, byref, c_double, c_uint, pointer, create_string_buffer
import cdms2
sys.path.append("/home/painter1/libcf/libcf-1.0-alpha6-snapshot2010041411/testpy")
# ... This is very much temporary, the necessary functions there need to be moved<<<<
from gscall import *
# ... This is very much temporary, the necessary functions there need to be moved<<<<

sys.path.append("/home/painter1/libcf/")  # <<< temporary
import config

libcf = CDLL(config.prefix + '/lib/libcf.so')  # <<< doesn't seem very portable
from cdms2.hgrid import AbstractHorizontalGrid
from cdms2.grid import AbstractGrid
import numpy
from error import CDMSError

MethodNotImplemented = "Method not yet implemented"

class AbstractMosaicGrid( AbstractGrid ):
    def __init__( self ):
        AbstractGrid.__init__(self,None)

class FileMosaicGrid( AbstractMosaicGrid ):
    def __init__( self, filename ):
        if ( filename ):
            try:
                f = cdms2.open( filename )
                if ( not f['mosaic'] ):
                    print "File " + filename + " is not a Gridspec mosaic"
                    f.close()
                    raise IOError
                fm = f('mosaic')
                if ( not hasattr(fm,'standard_name') or \
                     fm.standard_name != 'grid_mosaic_spec' ):
                    print "File " + filename + " is not a Gridspec mosaic file"
                    f.close()
                    raise IOError
                ftiles = f('tile_files')
                fcontacts = f('contact_files')
                self.gsfile = os.path.basename(filename)
                self.gspath = os.path.dirname(filename) + "/"
            except IOError:
                self.gsfile = None
                self.gspath = None
                self.tiles = None
                self.contacts = None
                return
        # Presently the file format uses an array of strings for tile_files and a
        # string for contact_files (hence there's really only one contact_file).
        # If this changes in the future, the following code will need changes.
        if ( not hasattr(ftiles,'data') ):
            self.tiles = None
            self.contacts = None
            return
        self.tiles = map( lambda ca: ca.tostring().rstrip('\x00'), ftiles.data )
        if ( not hasattr(fcontacts,'data') ):
            self.contacts = None
            return
        self.contacts = fcontacts.data.tostring().rstrip('\x00')

        f.close()
        AbstractMosaicGrid.__init__( self )
        
        
class GridspecMixin():
    "grid compatible with Gridspec files"
    def write_gridspec( path, filename ):
        """writes this grid to a Gridspec-compliant file"""
        raise CDMSError, MethodNotImplemented

class TransientMosaicGrid( AbstractMosaicGrid, GridspecMixin ):

    def __init__( self, filegrid=None, Tiles=None, Contacts=None ):
        "create a mosaic grid, from a FileMosaicGrid or from a list of tiles and contacts"
        # >>> For now, I expect tiles to be CurveGrid grids <<<<
        # eventually, add more optional arguments as used in AbstractGrid.__init__
        if ( Tiles ):   # init from local data
            self.tiles = Tiles
            self.contacts = Contacts
            AbstractMosaicGrid.__init__( self, None )    
            return
        if ( filegrid ): # init from a FileMosaicGrid
            self.tiles = []
            tilefilenames = filegrid.tiles
            tilecontactnames = filegrid.contacts
            path = filegrid.gspath
            self.gsfile = filegrid.gsfile
            self.gspath = filegrid.gspath

            for tilefilename in tilefilenames:
                # In the future this will be AbstractGrid( path, tilefilename )>>>
                # or, if I don't support RectGrid, AbstractGrid(...) >>>
                tile = GridspecFile_to_TransientCurveGrid( tilefilename, path )
                self.tiles.append( tile )
                # The tiles don't a common set of axes in the CDAT/CDMS2 sense (i.e.,
                # the x axis is the set of all gridpoints' x values), but they
                # do have a common coordinate system.  This coordinate system
                # (normally lat,lon in degrees) is not in Gridspec but should
                # be, and should be identified and set at this point.

                # >>> I'm not reading any contact files, and should once I have a use for them. <<<

                AbstractMosaicGrid.__init__( self )

    def write_gridspec( self, filename ):
        "writes this grid to a Gridspec-compliant file.  The filename should be a complete path."
        # This needs to be wrapped with something that looks like the existing
        # pattern        file=cdms2.open(...);   file.write(object) (or gswrite(object))
        i = 0
        tilename_root = os.path.basename(filename)  #<<<better to strip any final .nc
        path = os.path.dirname(filename)
        for tile in self.tiles:
            if ( not hasattr(tile,"gsfile") or tile.gsfile==None ):
                tilefile = self.generate_tilename( tilename_root, i )
                # >>> will be: tile.write_gridspec( path, tilefile ) # but now is >>>
                tile.write_gridspec( path + "/" + tilefile )
            i += 1
        if ( not hasattr(self,"gsfile") ):
             self.gsfile=None
             self.gspath=None
        if ( self.gsfile==None ):
            history = "Py mosaic experiment"
            ntile = len(self.tiles)
            mosaic_name = os.path.abspath(filename)
            nmosaic = 1
            mosaic_file = "experimental Py mosaic"
            grid_descriptor = "no grid descriptor yet"
            n_tilefile = 1  # 1 says to generate filenames based on tilefile
            # It would be much better to provide an array of tilefile names
            # as computed above, but the API as of April 2010 isn't able to
            # get an array of strings from Python.
            periodx = c_double(0.0)  # this is one of the many things I'll have to revisit<<<
            periody = c_double(0.0)
            generate_contact = 0
            tilefile = tilename_root
            tile_dir = path
            # >>>  phps many or all of the strings should be passed as ctypes string buffers <<<
            libcf.gs_make_mosaic( history, ntile, mosaic_name, nmosaic, mosaic_file,
                                  grid_descriptor, n_tilefile, tilefile,
                                  periodx, periody, generate_contact, tile_dir )
            self.gsfile = os.path.basename(filename)
            self.gspath = path
        return ( self.gsfile, self.gspath)

    def generate_tilename( self, tilename_root, i ):
        "generates a tile file name compatible with Gridspec"
        # When Gridspec gets a better API we won't need filename conventions.
        # For now, we need 256 characters, for workspace or something
        return tilename_root + str(i) + "\0"*(255-len(tilename_root))
    
class MosaicGrid( TransientMosaicGrid ):
  "same as TransientMosaicGrid, defined temporarily for backwards compatibility"

