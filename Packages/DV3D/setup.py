from distutils.core import setup
import os, sys

package_data = {'DV3D': ['data/earth2k.jpg', 'data/colormaps.pkl', 'data/buttons/*', 'data/coastline/index.txt', 'data/coastline/low/*', 'data/coastline/medium/*', 'data/coastline/high/*']}

setup (name = "DV3D",
       description = "Climate Data Interactive Visualization System",
       url = "http://portal.nccs.nasa.gov/DV3D/",
       packages = ['DV3D', ],
       package_dir = {'DV3D': '', },
       package_data = package_data,
       data_files = [ ('sample_data', ('data/geos5-sample.nc',) ), ]
       )

