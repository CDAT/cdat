
#  Usage:
#
#    python setup.py install 
#
#
from distutils.core import setup
import os, sys

import cdat_info

try:
    externals = cdat_info.externals
except:
    #externals = os.path.join(sys.prefix,"Externals")
    externals = os.environ.get("EXTERNALS",externals)
    
os.environ['PATH']=os.environ['PATH']+':'+os.path.join(externals,'bin')

os.environ['PKG_CONFIG_PATH']=os.path.join(externals,'lib','pkgconfig')+':'+os.environ.get("PKG_CONFIG_PATH","")

setup (name = "vcs",
       version=cdat_info.Version,
       description = "Visualization and Control System",
       url = "http://uvcdat.llnl.gov",
       packages = ['vcs', ],
       package_dir = {'vcs': 'Lib',
                     },
       data_files = [('share/vcs',('Share/wmo_symbols.json',
                                   'Share/data_continent_coarse',
                                   'Share/data_continent_political',
                                   'Share/data_continent_river',
                                   'Share/data_continent_states',
                                   'Share/data_continent_other7',
                                   'Share/data_continent_fine',
                                   )),],
       )

