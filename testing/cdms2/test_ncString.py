import cdms2
import os
import sys
baseline = sys.argv[1]

from markError import NTIME,NLAT,NLON,x,clearError,markError,reportError
clearError()

try:
    f = cdms2.open(baseline+"/../data/prcp_1951.nc")
    f.institution
    f['prcp'].long_name
except:
    markError("ncString property failed in prcp_1951.nc")
reportError()

