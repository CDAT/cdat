#!/usr/bin/env python
from distutils.core import setup
import os, sys

setup (name = "help",
       version='1.0',
       description = "Help for CDAT",
       url = "http://cdat.sf.net",
       packages = ['help'],
       package_dir = {'help': 'Lib'},
       scripts = ['Script/cdathelp']
      )
