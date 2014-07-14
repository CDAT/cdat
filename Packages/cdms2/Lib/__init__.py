"""
CDMS module-level API
"""
import cdat_info
cdat_info.pingPCMDIdb("cdat","cdms2")

__all__ = ["cdmsobj", "axis", "coord", "grid", "hgrid", "avariable", \
"sliceut", "error", "variable", "fvariable", "tvariable", "dataset", \
"database", "cache", "selectors", "MV2", "convention", "bindex", \
"auxcoord", "gengrid", "gsHost", "gsStaticVariable", "gsTimeVariable", \
"mvBaseWriter", "mvSphereMesh", "mvVsWriter", "mvCdmsRegrid"]

# Errors
from error import CDMSError

# CDMS datatypes
from cdmsobj import CdArray, CdChar, CdByte, CdDouble, CdFloat, CdFromObject, CdInt, CdLong, CdScalar, CdShort, CdString

# Functions which operate on all objects or groups of objects
from cdmsobj import Unlimited, getPathFromTemplate, matchPattern, matchingFiles, searchPattern, searchPredicate, setDebugMode

# Axis functions and classes
from axis import AbstractAxis, axisMatches, axisMatchAxis, axisMatchIndex
from axis import createAxis, createEqualAreaAxis, createGaussianAxis, createUniformLatitudeAxis, createUniformLongitudeAxis, setAutoBounds, getAutoBounds

# Grid functions
from grid import createGenericGrid, createGlobalMeanGrid, createRectGrid, createUniformGrid, createZonalGrid, setClassifyGrids, createGaussianGrid, writeScripGrid, isGrid

# Dataset functions
from dataset import createDataset, openDataset, useNetcdf3, getNetcdfClassicFlag, getNetcdfShuffleFlag, getNetcdfDeflateFlag, getNetcdfDeflateLevelFlag, setNetcdfClassicFlag, setNetcdfShuffleFlag, setNetcdfDeflateFlag, setNetcdfDeflateLevelFlag, setCompressionWarnings
open = openDataset

# Database functions
from database import connect, Base, Onelevel, Subtree

#Selectors
import selectors
from selectors import longitude, latitude, time, level, required, \
                      longitudeslice, latitudeslice, levelslice, timeslice

from avariable import order2index, orderparse, setNumericCompatibility, getNumericCompatibility
# TV
from tvariable import asVariable, createVariable, isVariable

from mvSphereMesh import SphereMesh
from mvBaseWriter import BaseWriter
from mvVsWriter import VsWriter
from mvVTKSGWriter import VTKSGWriter
from mvVTKUGWriter import VTKUGWriter
from mvCdmsRegrid import CdmsRegrid

# Gridspec is not installed by default so just pass on if it isn't installed
try:
    from gsStaticVariable import StaticFileVariable
    from gsTimeVariable import TimeFileVariable
except:
    pass

from restApi import esgfConnection,esgfDataset,FacetConnection

MV = MV2

ESMP_HAS_BEEN_INITIALIZED = False
if not ESMP_HAS_BEEN_INITIALIZED:
    try:
        import ESMP
        ESMP.ESMP_Initialize(ESMP.ESMP_LOGKIND_NONE)
        # this turns off the PET file logs
        ESMP.ESMP_LogSet(False)
        ESMP_HAS_BEEN_INITIALIZED = True
    except:
        pass
