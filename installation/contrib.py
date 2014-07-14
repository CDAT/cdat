import os
dostandard = force
## try:
##     import Numeric, cdms
## except ImportError:
##     dostandard = 1
dostandard = 1
import sys
if not 'clist' in locals().keys():
    clist=[]
## Format is [path,description,licence_file]
Clist = [
#    ['contrib/Sphinx','sphinx documentation builder','GNU'],
##     ['contrib/zope-interface','zope interface','GNU'],
#    ['contrib/Twisted','network computing tools','GNU'],
#    ['contrib/Foolscap','RPC protocol for Python+Twisted','GNU'],
#    ['contrib/ipython','an Enhanced Python Shell','GNU'],
#    ['contrib/scipy','Scientific tools for Python (core only)','GNU'],
    ['contrib/SP','A collection of Python modules that are useful for scientific computing.','LICENSE'],
    ['contrib/cssgrid','An interpolation package for random data on the surface of a sphere based on the work of Robert Renka. cssgrid uses cubic splines to calculate its interpolation function.',''],
    ['contrib/lmoments','56 routines for statistical analysis using L-moments','UC'],
    ['contrib/ort','Reads in Oort data files','UC'],
#    ['contrib/spherepack','A collection of programs for computing certain common differential operators and performing related manipulations on a sphere.',''],
    ['contrib/asciidata','Reads in ASCII files with the ability to specify tab or comma or space delimited fields','Lib/ASV.py'],
    ['contrib/eof','Calculates Empirical Orthogonal Functions of either one variable or two variables jointly','UC'],
    ['contrib/trends','Computes variance estimate taking auto-correlation into account.',''],
    ['contrib/binaryio','Handles binary or unformatted data',''],
    ['contrib/regridpack','A collection of programs for linear or cubic interpolation in one, two, three or four dimensions.',''],
    ['contrib/shgrid','An interpolation package for random data in 3-space based on the work of Robert Renka. shgrid uses a modified Shepard\'s algorithm to calculate its interpolation function',''],
    ['contrib/dsgrid','A three-dimensional random data interpolator based on a simple inverse distance weighting algorithm.',''],
    ['contrib/pyclimate','Provides functions to perform some simple IO operations, operations with COARDS-compliant netCDF files, EOF analysis, SVD and CCA analysis of coupled data sets, some linear digital filters, kernel based probability density function estimation and access to DCDFLIB.C library from Python.','GNU'],
    ['contrib/ComparisonStatistics','Calculates statistics (e.g., correlations and RMS differences) that quantify differences between two datasets. Allows for ellaborated masking and regridding operations','UC'],
    ['contrib/IaGraph','Package for Quick Interactive Graphing','GNU'],
    ['contrib/MSU','Package to compute Equivalent MSU Temperatures','UC'],
    ['contrib/EzTemplate','Package to generate VCS templates easily','GNU'],
    ['contrib/ZonalMeans','Package to compute zonal means on any grid (requires f90 compiler)','GNU'],
    ['contrib/HDF5Tools','Package to read HDF5 files into CDAT (requires h5dump binary utility)','GNU'],
# following is now built via externals
#    ['contrib/eof2','',''],
#    ['contrib/eofs','',''],
#    ['contrib/windspharm','','GNU'],
]

# natgrid has illegal C comments but gcc lets them through...
# we need to fix it.
NCARG_ROOT = os.environ.get('NCARG_ROOT')
NCARG_COLORMAP_PATH = os.environ.get('NCARG_COLORMAP_PATH')
if NCARG_COLORMAP_PATH or NCARG_ROOT :
    Clist.append(['contrib/pyncl','Generate NCL plots of cdms transient variables',''])


if sys.platform == "linux2" or sys.platform == 'darwin':
    Clist.append(['contrib/natgrid','A two-dimensional random data interpolation package based on Dave Watson\'s nngridr',''])

if '--enable-R' in sys.argv or '--enable-r' in sys.argv:
    Clist.append(['contrib/Rpy','Python Interface to the R library','GNU'])

if '--enable-ioapi' in sys.argv :
    Clist.append(['contrib/pyIoapi','Python Interface to the IoAPI library','GNU'])
    Clist.append(['contrib/egenix',"Collection of  tools which enhance Python's usability in many important areas such as ODBC database connectivity, fast text processing, date/time processing and web site programming.",'LICENSE'])
    Clist.append(['contrib/ioapiTools','ioapiTools developped by Alexis Zubrow form University of Chicago','GNU'])

if '--enable-spanlib' in sys.argv :
    Clist.append(['contrib/spanlib','Package to do Spectral analysis','GNU'],)
    
if not dostandard:
    packages = []

for c in Clist:
    clist.append(c)
    packages.append(c[0])

    
