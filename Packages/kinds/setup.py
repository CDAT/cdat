#!/usr/bin/env python
# To use:
#       python setup.py install
#
import os, sys, string, re, time
from glob import glob

# Check for an advanced enough Distutils.
import distutils
from distutils.core import setup, Extension

sourcelist = ['Src/_kindsmodule.c']
s=os.path.join('Lib','kinds_version.py')
execfile(s)
setup (name = "kinds",
       version=version,
       author="Paul F. Dubois",
       description = "Numeric Kinds Reference Implementation (draft)",
       maintainer = "Numerical Python Developers",
       maintainer_email = "numpy-developers@lists.sourceforge.net",
       url = "http://python.org/peps/pep-0242.html",
       packages = ['kinds'],
       package_dir = {'kinds': 'Lib'},
       ext_modules = [ Extension('kinds._kinds', sourcelist)]
       )
