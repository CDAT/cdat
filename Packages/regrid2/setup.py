#!/usr/bin/env python
from numpy.distutils.core import setup, Extension
import os, sys
import numpy

setup (name = "regrid2",
       version='3.3',
       description = "Remap Package",
       url = "http://www-pcmdi.llnl.gov/software",
       packages = ['regrid2'],
       package_dir = {'regrid2': 'Lib'},
       include_dirs = [numpy.lib.utils.get_include()],
       ext_modules = [Extension('regrid2._regrid', ['Src/_regridmodule.c']),
                      Extension('regrid2._scrip', ['Src/scrip.pyf','Src/regrid.c'])]
      )
    
