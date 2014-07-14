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
import cdat_info
from ASCII import get_parenthesis_content
cdat_info.pingPCMDIdb("cdat","genutil")

