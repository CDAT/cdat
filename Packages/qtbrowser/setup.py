from distutils.core import setup
import os

setup (name = "qtbrowser",
       version='1.0',
       description = "Qt-based data browser",
       url = "http://cdat.sourceforge.net",
       packages = ['qtbrowser'],
       package_dir = {'qtbrowser': 'Lib'},
       scripts=['Scripts/vcdat',]
      )
