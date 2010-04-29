import os
import sys
import sys,os
target_prefix = sys.prefix
for i in range(len(sys.argv)):
    a = sys.argv[i]
    if a=='--prefix':
        target_prefix=sys.argv[i+1]
    sp = a.split("--prefix=")
    if len(sp)==2:
        target_prefix=sp[1]
sys.path.insert(0,os.path.join(target_prefix,'lib','python%i.%i' % sys.version_info[:2],'site-packages')) 

from getopt import gnu_getopt


# Gather up all the files we need.
files = ['Src/compall.pyf','Src/compall_lib.F',]

## scypy_distutils Script
from numpy.distutils.core import setup, Extension

# Some useful directories.  
## from distutils.sysconfig import get_python_inc, get_python_lib

## python_incdir = os.path.join( get_python_inc(plat_specific=1) )
## python_libdir = os.path.join( get_python_lib(plat_specific=1) )

extra_link_args=[]
if sys.platform=='darwin':
    extra_link_args = ['-bundle','-bundle_loader '+sys.prefix+'/bin/python']
## setup the python module
setup(name="ComparisonStatistics",
      version='1.1',
      description="Karl Taylor's compall code",
      author="Fortran: K.E.Taylor, Python: C.  Doutriaux",
      author_email="taylor13@llnl.gov , doutriaux1@llnl.gov",
      maintainer="C. Doutriaux",
      maintainer_email="doutriaux1@llnl.gov",

      ## Build fortran wrappers, uses f2py
      ## directories to search for libraries defined in setup.cfg
      ext_modules = [Extension('ComparisonStatistics.compall',
                               files,
#                               libraries=["ioapi", "netcdf"],
#                               library_dirs=libDirs,
			       include_dirs=['Src'],
#                               extra_link_args=extra_link_args,
                               ),
                     ],
      license="GNU GPL",
      
     ## Install these to their own directory
     package_dir={'ComparisonStatistics':'Lib'},
     packages = ["ComparisonStatistics"],
      
     )




