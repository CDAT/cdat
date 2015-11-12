"""Interface to regridding facilities
"""

__all__ = ["horizontal", "pressure", "crossSection", "scrip", 
           "error", "mvGenericRegrid",]

from error import RegridError
from horizontal import Horizontal, Regridder
from pressure import PressureRegridder
from crossSection import CrossSectionRegridder
from scrip import ConservativeRegridder, BilinearRegridder, BicubicRegridder 
from scrip import DistwgtRegridder, readRegridder
from regrid2 import gsRegrid
from mvGenericRegrid import GenericRegrid
from mvLibCFRegrid import LibCFRegrid
try:
    import ESMP
    from mvESMFRegrid import ESMFRegrid
except:
    pass

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
                                                                                                                                                                                         80,9          Bot

