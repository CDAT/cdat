import cdms2
import os
import sys
datadir = sys.argv[1]

from markError import NTIME,NLAT,NLON,x,clearError,markError,reportError
clearError()

try:
    f = cdms2.open(datadir+"/prcp_1951.nc")
    f.institution
    f['prcp'].long_name
except:
    markError("ncString property failed in prcp_1951.nc")
reportError()

