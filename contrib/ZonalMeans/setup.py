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
files = ['Src/zonebasin.f90', 'Src/mask4d.f90', 'Src/meme_cote.f90',  'Src/intersecte.f90' , 'Src/echange.f90', 'Src/SphericalPolyArea.f90', 'Src/cal_int.f90', 'Src/cfraction.f90']
## Get package version info
version_file = os.path.join(os.curdir,"Lib","package_version.py")
execfile(version_file)

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
setup(name="ZonalMeans",
      version=version,
      description=description,
      author=author,
      author_email=author_email,
      maintainer=author,
      maintainer_email=author_email,

      ## Build fortran wrappers, uses f2py
      ## directories to search for libraries defined in setup.cfg
      ext_modules = [Extension('ZonalMeans._gengridzmean',
                               files,
#                               f2py_options = ["--fcompiler=gfortran",],
#                               libraries=["ioapi", "netcdf"],
#                               library_dirs=libDirs,
			       include_dirs=['Src'],
                               extra_link_args=extra_link_args,
                               ),
                     ],
      license="GNU GPL",
      
     ## Install these to their own directory
     package_dir={'ZonalMeans':'Lib'},
     packages = ["ZonalMeans"],
      
     )




