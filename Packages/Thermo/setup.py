#!/usr/bin/env python
from distutils.core import setup, Extension
import os, sys, time
here = os.getcwd()
os.chdir(os.path.join('..','..','..'))
#from cdat_configuration import *
os.chdir(here)

setup (name = "thermo",
       version='1.0',
       description = "Package to draw Themrodynamic diagrams with VCS",
       url = "http://cdat.sf.net",
       packages = ['thermo'],
       package_dir = {'thermo': 'Lib'},
       
      )
