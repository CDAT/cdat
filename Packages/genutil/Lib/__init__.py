"""
genutil -- General utility modules for scientific computing
"""
## Lean mode does not install xmgrace module
from grower import grower
try:
  import xmgrace
except:
  pass
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
import cdat_info
from ASCII import get_parenthesis_content
cdat_info.pingPCMDIdb("cdat","genutil")

