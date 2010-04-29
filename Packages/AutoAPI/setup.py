#!/usr/bin/env python
from distutils.core import setup
## import os, sys, time
## here = os.getcwd()
## os.chdir(os.path.join('..','..','..'))
## #from cdat_configuration import *
## os.chdir(here)

setup (name = "AutoAPI",
       version='1.0',
       description = "Package to automagically generate API to modules/diagnosis",
       url = "http://cdat.sf.net",
       packages = ['AutoAPI'],
       package_dir = {'AutoAPI': 'Lib'},
       
      )
