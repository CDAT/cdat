#!/usr/bin/env python
from distutils.core import setup, Extension
import os, sys, time
here = os.getcwd()
os.chdir(os.path.join('..','..','..'))
#from cdat_configuration import *
os.chdir(here)

setup (name = "EzTemplate",
       version='1.0',
       description = "Package to create templates for VCS",
       url = "http://cdat.sf.net",
       packages = ['EzTemplate'],
       package_dir = {'EzTemplate': 'Lib'},
       
      )
