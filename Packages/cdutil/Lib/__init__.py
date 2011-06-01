"""Module cdutil contains miscellaneous routines for manipulating variables.
"""
import region
#import continent_fill
from genutil.averager import averager, AveragerError, area_weights, getAxisWeight, getAxisWeightByName,__check_weightoptions
from times import *
from retrieve import WeightsMaker,  WeightedGridMaker, VariableConditioner, VariablesMatcher
from vertical import sigma2Pressure, reconstructPressureFromHybrid, logLinearInterpolation, linearInterpolation
from create_landsea_mask import generateLandSeaMask
from sftbyrgn import generateSurfaceTypeByRegionMask
try:
    import cdat_info
    if cdat_info.ping is False:
        import urllib2,os,sys
        urllib2.urlopen("http://uv-cdat.llnl.gov/UVCDATLogger/%s/%s/cdat/start" % (os.getlogin(),sys.platform))
        cdat_info.ping = True
except:
    cdat_info.ping = False
    pass
