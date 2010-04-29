from distutils.core import setup

setup (name = "HDF5Tools",
       author="Charles Doutriaux",
       version='1.0',
       description = "Utilities for HDF5 manipulation, requires h5dump to be present on system",
       url = "http://cdat.sourceforge.net",
       packages = ['HDF5Tools'],
       package_dir = {'HDF5Tools': 'Lib'},
      )
    
