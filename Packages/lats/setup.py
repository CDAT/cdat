#!/usr/bin/env python
from distutils.core import setup, Extension
import os, sys
netcdf_library_dir = os.path.join(sys.exec_prefix, 'lib')
netcdf_include_dir = os.path.join(sys.prefix, 'include')
lats_library_dir = '/usr/local/lib'
setup (name = "lats",
       version='3.2',
       description = "Climate Data Management System",
       url = "http://www-pcmdi.llnl.gov/software",
       packages = ['lats'],
       package_dir = {'lats': 'Lib'},
       include_dirs = ['Include', netcdf_include_dir],
       ext_modules = [
                      Extension('lats.slats',
				['Src/slatsmodule.c'],
				library_dirs = [lats_library_dir, netcdf_library_dir],
				libraries = ['lats', 'netcdf']
				)
		     ]
      )
    
