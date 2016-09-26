#!/usr/bin/env python
from distutils.core import setup
import os, sys

setup (name = "ncml",
       version='1.0',
       description = "netCDF Markup Language",
       url = "http://cdat.sf.net",
       packages = ['ncml'],
       package_dir = {'ncml': 'Lib'},
      )
