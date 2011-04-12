#/usr/bin/env python

"""
A file-like object to access mosaic and time aggregated data
$Id: gsFile.py 1728 2011-02-04 21:26:11Z dkindig $
"""

import os
from re import search, sub
from ctypes import c_float, c_char_p, c_int, CDLL, byref, POINTER
import cdms2
from numpy import zeros, float64, asarray
from pycf import libCFConfig, __path__

LIBCFDIR  = __path__[0] + "/libcf"
libCF  = libCFConfig

def open(uri, mode = 'r'):
    """
    Open mosaic file
    @param mosaicfile mosaic file
    @param mode valid cdms2 open file mode
    @param inCdmsFile Mosaic file cdms2 object
    """

    outMosaicFile = GsMosaic(uri, mode)
    return outMosaicFile

def getSlab(str):
    """
    From a string return a tuple of slice objects
    @param str input string in the format "1:2 7:-1" for instance
    @return slice tuple, eg (slice(1, 2, 1), slice(7, -1, -1))
    """
    res = []
    # remove extra spaces
    str = sub(r'\s+', ' ', str)
    # remove leading/trailing spaces
    str = sub(r'^\s+', '', str)
    str = sub(r'\s+$', '', str)
    for index_range in str.split(libCF.CF_INDEX_SEPARATOR):
        m = search(r'([\-\d]+):([\-\d]+)', index_range)
        if m:
            step = 1
            startIndex = int(m.group(1))
            endIndex = int(m.group(2))
            if endIndex < startIndex: step = -1
            slc = slice(startIndex, endIndex, step)
            res.append(slc)
    return tuple(res)

class GsMosaic:
    """
    Define a mosaic.
    """

    def __init__(self, uri, mode = 'r'):
        """
        Constructor
        @param uri Filename with path
        @param mode read/write. Currently only read is supported
        """

        self.id      = uri
        self.mode    = mode
        self.uri     = uri
        self._status = 'Open'

        self.mosaicId_t = c_int(-1)
        self.lib = None
        for sosuffix in '.so', '.dylib', '.dll', '.a':
            self.lib = CDLL(LIBCFDIR + sosuffix)
            if self.lib:
                break

        libcfdll = self.lib

        self.file_type           = ""
        self.contact_map         = {}
        self.tile_contacts       = {}
        self.tile_contacts_compl = {}
        self.coordinate_names    = []
        tile_names               = []

        status = libcfdll.nccf_def_mosaic_from_file(uri, "", byref(self.mosaicId_t))

        if status != 0:
            print "ERROR: File %s doesn't exist or is not a valid mosaic file" % \
                mosaicfile
            print "error code: ", status
            return

        # Get some sizes
        ngrids         = c_int(-1)
        ndims          = c_int(-1)
        ncontacts      = c_int(-1)
        libcfdll.nccf_get_mosaic_ndims(self.mosaicId_t, byref(ndims))
        libcfdll.nccf_get_mosaic_ngrids(self.mosaicId_t, byref(ngrids))
        libcfdll.nccf_get_mosaic_ncontacts(self.mosaicId_t, byref(ncontacts))

        # Build the character arrays
        separator_t = libCF.CF_TILE_SEPARATOR
        contact_map_t  = c_char_p(" " * (libCF.NC_MAX_NAME+1))
        tile_contact_t = c_char_p(" " * (libCF.NC_MAX_NAME+1))
        tile_name_t    = c_char_p(" " * (libCF.NC_MAX_NAME+1))
        coord_t = (c_char_p * ndims.value)()
        for iDim in range(ndims.value):
            coord_t[iDim] = " " * (libCF.NC_MAX_NAME+1)

        # Get the coordinate names for the grids
        libcfdll.nccf_get_mosaic_coord_names(self.mosaicId_t, coord_t)

        for iCrd in range(len(coord_t)):
            self.coordinate_names.append(coord_t[iCrd])

        # Get the contact map information
        for iContact in range(ncontacts.value):
            status = libcfdll.nccf_get_mosaic_contact_map(self.mosaicId_t, \
                                                       iContact, contact_map_t)
            status = libcfdll.nccf_get_mosaic_tile_contact(self.mosaicId_t, \
                                                        iContact, tile_contact_t)

            tN1, tN2             = tile_contact_t.value.split(separator_t)
            tileName1, tileName2 = tN1.strip(), tN2.strip()
            s1, s2               = contact_map_t.value.split(separator_t)

            # slice objects
            slab1 = getSlab(s1.strip())
            slab2 = getSlab(s2.strip())

            # Create the tile contact dictionary. Non symmetric.
            if not self.tile_contacts.has_key(tileName1):
                self.tile_contacts[tileName1] = {}
            # The complement to tile_contacts
            if not self.tile_contacts_compl.has_key(tileName2):
                self.tile_contacts_compl[tileName2] = {}

            # Attach the contact map (slab) between the tiles
            self.tile_contacts[tileName1][tileName2] = (slab1, slab2)
            self.tile_contacts_compl[tileName2][tileName1] = (slab2, slab1)

    def getContactMap(self):
        return self.tile_contacts

    def getTileNames(self):
        return self.tile_names

    def getCellCenteredSlab(self, slab1, slab2):
        # adjust for cell centers
        newslabs = []
        for slab in slab1, slab2:
            newslab = []
            for sl in slab:
                b = sl.start
                e = sl.stop + 1
                newsl = slice(max(b-1, 0), max(e-1, -1), sl.step)
                newslab.append(newsl)
            newslabs.append(tuple(newslab))
        slab1, slab2 = newslabs
    
        return (slab1, slab2)

    def getGrids(self, coordData):
        res = []
        for tn1 in self.tile_contacts.keys():
            for tn2 in self.tile_contacts[tn1].keys():
                tmp = self.getSeamData(tn1, tn2, coordData)
                res.append(tmp)
    
        return res

    def getSeamData(self, tileName, otherTileName, inputData):
        """
        @param tileName Name for the first tile
        @param otherTileName Name for the other tile
        @param inputData Dictionary containing lon-lat names and their flat
                          coordinate data values
        @return newData Grid of data on slice
        """
        slab1, slab2 = self.tile_contacts[tileName][otherTileName]
    
        # Convert to cell centered slabs
        slab1, slab2 = self.getCellCenteredSlab(slab1, slab2)

        d1 = inputData[tileName]
        d2 = inputData[otherTileName]
        data1 = d1[slab1].flatten()
        data2 = d2[slab2].flatten()
        n = len(data1)
        newData = zeros( (n, 2), float64 )
        newData[:, 0] = data1
        newData[:, 1] = data2

        return newData

    def getSeamSlice(self, tileName, otherTileName, inputData):
        """
        @param tileName Name for the first tile
        @param otherTileName Name for the other tile
        @param inputData Dictionary containing lon-lat names and their flat
                          coordinate data values
        @return coords dictionary of seam sliced coordinates
        """
        slab1, slab2 = self.tile_contacts[tileName][otherTileName]
    
        # Convert to cell centered slabs
        slab1, slab2 = self.getCellCenteredSlab(slab1, slab2)
    
        coords = {}
    
        for coordName in inputData.keys():
            d1 = inputData[coordName][tileName]
            d2 = inputData[coordName][otherTileName]
            data1 = d1[slab1].flatten()
            data2 = d2[slab2].flatten()
            n = len(data1)
            d = zeros( (n, 2), float64 )
            d[:, 0] = data1
            d[:, 1] = data2
            coords[coordName] = d
        return coords

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

    from optparse import OptionParser

    cfdir = '/home/kindig/projects/libcf/libcf/build/examples/'
    mfile = cfdir + 'ex2_mosaic.nc'
    usage = """
    Full path to mosaic file.
    e.g. python gsMosaic.py -f /home/kindig/projects/libcf/libcf/build/examples/ex2_mosaic.nc
    """
    parser = OptionParser()
    parser.add_option("-f", "--file", dest="mfile",
                  help="full path to mosaic file")

    options, args = parser.parse_args()
    if not options.mfile:
        print usage
        exit(1)
    # Use the libcf examples directory.
    if not os.path.exists(mfile):
        print "File '%s' does not exist. Check path"
        return

    m = open(mfile)

    print "\nCoordinate Names"
    for c in m.coordinate_names: print c

    print "\nTile Contacts"
    for t in m.tile_contacts: print "%s -> %s" % (t, m.tile_contacts[t])
    print "\nTile Contacts Complement"
    for t in m.tile_contacts_compl: print "%s -> %s" % (t, m.tile_contacts_compl[t])
    print

if __name__ == "__main__": test()
