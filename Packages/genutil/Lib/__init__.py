"""
genutil -- General utility modules for scientific computing
"""
from grower import grower
import xmgrace
import statistics
from minmax import minmax
from statusbar import statusbar
from selval import picker
import filters
import salstat
import arrayindexing
import ASCII
from unidata import udunits
from Filler import Filler,StringConstructor
from averager import averager, AveragerError, area_weights, getAxisWeight, getAxisWeightByName,__check_weightoptions
#from Statusbar_Pmw import Statusbar
try:
    import cdat_info
    if cdat_info.ping is False:
        import urllib2,os,sys
        urllib2.urlopen("http://uv-cdat.llnl.gov/UVCDATLogger/%s/%s/cdat/start" % (os.getlogin(),sys.platform))
        cdat_info.ping = True
except:
    cdat_info.ping = False
    pass
