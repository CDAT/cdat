#!/usr/bin/env python
from distutils.core import setup

setup (name = "distarray",
       version = '1.0',
       author = 'Alexander Pletzer',
       author_email = 'pletzer@txcorp.com',
       description = "Distributed array library",
       url = "http://cdat.sf.net",
       packages = ['distarray'],
       package_dir = {'distarray': 'Lib'},
      )
