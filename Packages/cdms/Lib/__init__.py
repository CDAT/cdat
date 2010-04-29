"""
CDMS module-level API
"""
__all__ = ["cdmsobj", "axis", "coord", "grid", "hgrid", "avariable", "sliceut", "error", "variable", "fvariable", "tvariable", "dataset", "database", "cache", "selectors", "MV", "convention", "bindex", "auxcoord", "gengrid"]

try:
    dir(regrid)
except NameError:
    import warnings
    warnings.warn("The cdms, MV, and regrid modules are deprecated, please use cdms2, MV2, and regrid2 instead. The convertcdms.py script can convert scripts to numpy, ma, cdms2, and regrid2", DeprecationWarning)

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
from grid import createGenericGrid, createGlobalMeanGrid, createRectGrid, createUniformGrid, createZonalGrid, setClassifyGrids, createGaussianGrid, writeScripGrid

# Dataset functions
from dataset import createDataset, openDataset
open = openDataset

# Database functions
from database import connect, Base, Onelevel, Subtree

#Selectors
import selectors
from selectors import longitude, latitude, time, level, required, \
                      longitudeslice, latitudeslice, levelslice, timeslice

from avariable import order2index, orderparse
# TV
from tvariable import asVariable, createVariable, isVariable
