"CDMS Mosaic Variable objects, Gridspec-compatible"

import sys
import cdms2
sys.path.append("/home/painter1/libcf/")  # <<< temporary
import config

libcf = CDLL(config.prefix + '/lib/libcf.so')
from cdms2.hgrid import AbstractHorizontalGrid
from cdms2.grid import AbstractGrid
import numpy
from error import CDMSError

class MosaicVariable:

    def __init__( self, mosaicgrid, tilevars ):
        self._grid_ = mosaicgrid
        self._tilevars_ = tilevars
