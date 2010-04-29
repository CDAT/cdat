from distutils.core import setup, Extension
import os,sys,string


setup (name = "xmgrace",
       version='1.1',
       author='doutriaux1@llnl.gov',
       description = "wrapping package for xmgrace",
       url = "http://www-pcmdi.llnl.gov/software",
       packages = ['xmgrace'],
       package_dir = {'xmgrace': 'Lib'},    
      )

