from distutils.core import setup
import os

setup (name = "browser",
       version='3.2',
       description = "Tk-based data browser from PCMDI",
       url = "http://cdat.sourceforge.net",
       packages = ['browser'],
       package_dir = {'browser': 'Lib'},
      )
