#!/usr/bin/env python
from distutils.core import setup
import os, sys

setup (name = "esg",
       version='1.0',
       description = "netCDF Markup Language",
       url = "http://cdat.sf.net",
       packages = ['esg'],
       package_dir = {'esg': 'Lib'},
       scripts = ['Script/esscan'],
      )
