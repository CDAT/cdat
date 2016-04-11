
#  Usage:
#
#    python setup.py install
#
#
from setuptools import setup, find_packages
import os

import cdat_info

try:
    externals = cdat_info.externals
except:
    externals = os.environ.get("EXTERNALS", externals)

os.environ['PATH'] = os.environ['PATH'] + ':' + os.path.join(externals, 'bin')

os.environ['PKG_CONFIG_PATH'] = os.path.join(
    externals, 'lib', 'pkgconfig') + \
    ':' + os.environ.get("PKG_CONFIG_PATH", "")

setup(name="vcs",
      version=cdat_info.Version,
      description="Visualization and Control System",
      url="http://uvcdat.llnl.gov",
      packages=find_packages(),
      package_dir={'vcs': 'vcs',
                   },
      data_files=[('share/vcs', ('Share/wmo_symbols.json',
                                 'Share/data_continent_coarse',
                                 'Share/data_continent_political',
                                 'Share/data_continent_river',
                                 'Share/data_continent_states',
                                 'Share/data_continent_other7',
                                 'Share/data_continent_fine',
                                 'Share/initial.attributes',
                                 'Share/uvcdat.png',
                                 'Share/marker_icon.png',
                                 'Share/text_icon.png',
                                 'Share/fill_icon.png',
                                 'Share/line_icon.png'
                                 )), ],
      )
