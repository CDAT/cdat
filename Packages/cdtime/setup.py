#!/usr/bin/env python
from distutils.core import setup, Extension
import os, sys
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
        
setup (name = "cdtime",
       description = "Time utilities",
       version='3.2',
       url = "http://cdat.sf.net",
       include_dirs = ['Include'] + cdat_info.cdunif_include_directories,
       ext_modules = [Extension('cdtime', 
                       ['Src/cdtimemodule.c'],
                       library_dirs = cdat_info.cdunif_library_directories,
                       libraries = cdat_info.cdunif_libraries)
       ]
)
    
