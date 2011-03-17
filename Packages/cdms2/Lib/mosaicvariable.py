"CDMS Mosaic Variable objects, Gridspec-compatible"

import sys
import cdms2

from cdms2.hgrid import AbstractHorizontalGrid
from cdms2.grid import AbstractGrid
import numpy
from error import CDMSError

import pycf
LIBCF = pycf.__path__[0] + '/libcf'

class MosaicVariable:

    def __init__( self, mosaicgrid, tilevars ):
        self._grid_ = mosaicgrid
        self._tilevars_ = tilevars
