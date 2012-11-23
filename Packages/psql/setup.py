#!/usr/bin/env python
from distutils.core import setup, Extension
import os, sys
sys.path.append(os.environ['BUILD_DIR'])
import cdat_info

setup (name = "psql",
       description = "PSQL Interface",
       version='3.2',
       url = "http://cdat.sourceforge.net",
       include_dirs = ['Include'] + cdat_info.cdunif_include_directories,
       ext_modules = [Extension('psql_interface', 
                                ['Src/psqlmodule.c'],
                                library_dirs = cdat_info.cdunif_library_directories,
                                libraries = cdat_info.cdunif_libraries)
       ]
)
    
