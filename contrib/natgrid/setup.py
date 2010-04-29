#!/usr/bin/env python
from numpy.distutils.core import setup, Extension
import glob,sys

sources=glob.glob('Src/*.c')

setup (name = "natgrid",
       version='1.0',
       description = "natgrid",
       url = "http://cdat.sf.net",
       packages = [''],
       package_dir = {'': 'Lib'},
       include_dirs = ['Include',], 
       ext_modules = [Extension('natgridmodule', sources),
		     ]
      )
