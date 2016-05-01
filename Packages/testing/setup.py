import os, sys
from distutils.core import setup
import cdat_info

sys.path.append(os.environ.get('BUILD_DIR',"build"))

setup(name="testing",
      version=cdat_info.Version,
      description="Testing infrastructure for cdat",
      url="http://uvcdat.llnl.gov",
      packages=['testing'],
      package_dir={'testing': 'testing',},
      install_requires=['numpy','vcs', 'vtk'],
)
