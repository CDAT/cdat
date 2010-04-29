#!/usr/bin/env python
from numpy.distutils.core import setup, Extension
import glob

sources=glob.glob('Src/*.c')

setup (name = "dsgrid",
       version='1.0',
       description = "dsgrid",
       url = "http://cdat.sf.net",
       packages = [''],
       package_dir = {'': 'Lib'},
       include_dirs = ['Include'], 
       ext_modules = [Extension('dsgridmodule', sources),
		     ]
      )
