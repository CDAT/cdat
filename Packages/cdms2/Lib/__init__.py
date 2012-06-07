"""
CDMS module-level API
"""
import cdat_info
cdat_info.pingPCMDIdb("cdat","start")

__all__ = ["cdmsobj", "axis", "coord", "grid", "hgrid", "avariable", \
"sliceut", "error", "variable", "fvariable", "tvariable", "dataset", \
"database", "cache", "selectors", "MV2", "convention", "bindex", \
"auxcoord", "gengrid", "gsHost", "gsStaticVariable", "gsTimeVariable", \
"mvBaseWriter", "mvSphereMesh", "mvVsWriter"]

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
from dataset import createDataset, openDataset, useNetcdf3, getNetcdfShuffleFlag, getNetcdfDeflateFlag, getNetcdfDeflateLevelFlag, setNetcdfShuffleFlag, setNetcdfDeflateFlag, setNetcdfDeflateLevelFlag, setCompressionWarnings
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

# Gridspec is not installed by default so just pass on if it isn't installed
try:
    from gsStaticVariable import StaticFileVariable
    from gsTimeVariable import TimeFileVariable
    from mvSphereMesh import SphereMesh
    from mvBaseWriter import BaseWriter
    from mvVsWriter import VsWriter
    from mvVTKSGWriter import VTKSGWriter
    from mvVTKUGWriter import VTKUGWriter
except:
    pass

from restApi import esgfConnection

MV = MV2
