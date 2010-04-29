######################################################################
## SpanLib, Raynaud 2006-2009
######################################################################

import os
import sys

from getopt import gnu_getopt


# Gather up all the files we need.
files = ['spanlib.pyf','spanlib_pywrap.f90','spanlib.f90']
libDirs=_LIBDIRS_	# like ['/usr/local/lib']
incDirs=_INCDIRS_	# like ['/usr/local/include']
libs=_LIBS_		# like ['lapack95', 'lapack','blas']
extra_link_args=[]

## Get package version info
version='_VERSION_' # Like 0.1
description='Python extension to spanlib fortran library'
author = 'Stephane Raynaud and Charles Doutriaux'
author_email = 'stephane.raynaud@gmail.com'
url="http://spanlib.sf.net"
description
## scypy_distutils Script
from numpy.distutils.core import setup, Extension

# Some useful directories.  
## from distutils.sysconfig import get_python_inc, get_python_lib
## python_incdir = os.path.join( get_python_inc(plat_specific=1) )
## python_libdir = os.path.join( get_python_lib(plat_specific=1) )

if sys.platform=='darwin':
    extra_link_args += ['-bundle','-bundle_loader '+sys.prefix+'/bin/python']
## setup the python module
setup(name="spanlib",
	version=version,
	description=description,
	author=author,
	author_email=author_email,
	maintainer=author,
	maintainer_email=author_email,

	## Build fortran wrappers, uses f2py
	## directories to search for libraries defined in setup.cfg
	ext_modules = [
		Extension('spanlib.spanlib_fort',
			files,
			libraries=libs,
			library_dirs=libDirs,
			include_dirs=incDirs,
			extra_link_args=extra_link_args,
		),],
	license="GNU LGPL",
      
	## Install these to their own directory
	package_dir={'spanlib':'../lib'},
	packages = ["spanlib"],
      
)




