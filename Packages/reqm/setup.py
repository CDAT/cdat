#!/usr/bin/env python
from distutils.core import setup
import __init__
setup (name = "apps",
       version = __init__.__version__,
       description = "ESG/NGI Request Manager interface",
       packages = ['reqm','reqm._GlobalIDL'],
       package_dir = {'reqm': '.', 'reqm._GlobalIDL':'_GlobalIDL'}
      )
    
