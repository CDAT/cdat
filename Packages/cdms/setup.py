#!/usr/bin/env python
from distutils.core import setup, Extension
import os, sys
unm = os.uname()[-1]
if unm.find('64')>-1:
  print 'cdms will not build on 64bit system, instead use cdms2, regrid2, MV2, numpy'
  sys.exit()

target_prefix = sys.prefix
for i in range(len(sys.argv)):
    a = sys.argv[i]
    if a=='--prefix':
        target_prefix=sys.argv[i+1]
    sp = a.split("--prefix=")
    if len(sp)==2:
        target_prefix=sp[1]
        print 'Target is:',target_prefix
sys.path.insert(0,os.path.join(target_prefix,'lib','python%i.%i' % sys.version_info[:2],'site-packages')) 
import cdat_info

setup (name = "cdms",
       version='3.2',
       description = "Climate Data Management System",
       url = "http://cdat.sf.net",
       packages = ['cdms'],
       package_dir = {'cdms': 'Lib'},
       include_dirs = ['Include'] + cdat_info.cdunif_include_directories,
       ext_modules = [Extension('cdms.Cdunif',
                                ['Src/Cdunifmodule.c'],
                                library_dirs = cdat_info.cdunif_library_directories,
                                libraries = cdat_info.cdunif_libraries,
                                ),
                      Extension('cdms._bindex',
                                ['Src/_bindexmodule.c', 'Src/bindex.c'],
                                ) 
                     ]
      )

setup (name = "MV",
       version = '1.0',
       description="Alias for cdms.MV",
       url = "http://cdat.sf.net",
       py_modules=['MV']
       )
