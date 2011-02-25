#/usr/bin/env python

"""
A file-like object to access mosaic and time aggregated data
$Id: gsFile.py 1728 2011-02-04 21:26:11Z dkindig $
"""

import os
from re import search
from ctypes import c_float, c_char_p, c_int, CDLL, byref, POINTER
import cdms2
import gsStatVar
import gsTimeVar

# local imports
import config

LIBCF = config.LIBCFDIR + "/lib/libcf"

def open(uri, mode = 'r'):
    """
    Open mosaic file
    @param mosaicfile mosaic file
    @param mode valid cdms2 open file mode
    @param inCdmsFile Mosaic file cdms2 object
    """

    outMosaicFile = gsMosaic(uri, mode)
    return outMosaicFile

class gsMosaic:

    def __init__(self, uri, mode = 'r'):
        """
        Constructor, no arg
        """

        self.id      = uri
        self.mode    = mode
        self.uri     = uri
        self._status = 'Open'

        self.mosaicId_t = c_int(-1)
        self.lib = None
        for sosuffix in '.so', '.dylib', '.dll', '.a':
            self.lib = CDLL(LIBCF + sosuffix)
            if self.lib:
                break

        libcf = self.lib

        self.file_type           = "" 
        self.tile_contacts       = {}
        self.tile_contacts_compl = {}
        tile_names               = []

        status = libcf.nccf_def_mosaic_from_file(uri, "", byref(self.mosaicId_t))

        if status != 0:
            print "ERROR: File %s doesn't exist or is not a valid mosaic file" % \
                mosaicfile
            print "error code: ", status
            return

        ngrids = c_int(-1)
        ncontacts = c_int(-1)
        separator_t = c_char_p(" " * (config.NC_MAX_NAME+1))
        contact_map_t = c_char_p(" " * (config.NC_MAX_NAME+1))
        tile_contact_t = c_char_p(" " * (config.NC_MAX_NAME+1))
        tile_name_t = c_char_p(" " * (config.NC_MAX_NAME+1))
        coord_t = c_char_p(" " * (config.NC_MAX_NAME+1))

        libcf.nccf_get_mosaic_ngrids(self.mosaicId_t, byref(ngrids))
        libcf.nccf_get_mosaic_ncontacts(self.mosaicId_t, byref(ncontacts))
        print 'HERE', ngrids, ncontacts

        # Get the coordinate names for the grids
        libcf.nccf_get_mosaic_coord_names(self.mosaicId_t, coord_t)
        for iCrd in range(len(coord_t)):
            self.coordinate_names.append(coord_t[0].value)
        print 'HERE', ngrids, ncontacts

        # Get the tile names
        for iGrid in range(ngrids):
            libcf.nccf_get_mosaic_grid_name(self.mosaicId_t, tile_name_t)
            tile_names.append(tile_name_t.value)

        # Get the contact map information
        for iContact in range(ncontacts):
            status = libcf.nccf_get_mosaic_contact_map(self.mosaicId_t, contact_map_t)
            self.contact_map.append(contact_map_t.value)
            status = libcf.nccf_get_mosaic_tile_contact(self.mosaicId_t, tile_contact_t)
            self.tile_contact.append(tile_contact_t.value)

            tileName1, tileName2 = tile_contact_t.value.split(separator_t.value)
            slab1, slab2         = contact_map_t.value.split(separator_t.value)

            # Create the tile contact dictionary. Non symmetric
            if not self.tile_contacts.has_key(tileName1.strip()):
                self.tile_contacts[tileName1.strip()] = {}
            # The complement to tile_contacts
            if not self.tile_contacts_compl.has_key(tileName2.strip()):
                self.tile_contacts[tileName2.strip()] = {}

            self.tile_contacts[tileName1.strip()][tileName2.strip()] = (slab1.strip(), slab2.strip())
            self.tile_contacts[tileName2.strip()][tileName1.strip()] = (slab2.strip(), slab1.strip())

    def getContactMap(self):
        return self.tile_contacts

    def getTileNames(self):
        return self.tile_names

    def getCoordinateNames(self):
        return self.coordinate_names

    def __repr__(self):
        res = "<Mosaic: '%s',  URI: '%s', mode: '%s', status: '%s' >" % \
            ( self.id, self.uri, self.mode, self._status)
        return res

    # Not sure yet if these are needed.
    def __get_item__(self):
        pass

    def __call__(self):
        pass

    # Write? Only if self.mode is 'w'
    def __set_item__(self):
        if self.mode == 'w':
            # self.lib.nccf_put_mosaic( ncid, self.mosaicId_t )
            pass
        else:
            print 'Mode set to %s' % self.mode

    def __del__(self):
        self.lib.nccf_free_mosaic(self.mosaicId_t)

#############################################################################

def test():
    import os.path
    from sys import exit

    mfile = '/home/kindig/projects/modave/examples/ex2_mosaic.nc'
    if not os.path.exists(mfile):
        print "File '%s' does not exist. Check path"
        return

    m = open(mfile)

    print m

if __name__ == "__main__": test()
