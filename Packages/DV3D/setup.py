from distutils.core import setup
import os, sys

package_data = {'dv3d': ['data/earth2k.jpg', 'data/colormaps.pkl', 'data/buttons/*', 'data/coastline/index.txt', 'data/coastline/low/*', 'data/coastline/medium/*', 'data/coastline/high/*']}

setup (name = "dv3d",
       description = "Climate Data Interactive Visualization System",
       url = "http://portal.nccs.nasa.gov/DV3D/",
       packages = ['dv3d', ],
       package_dir = {'dv3d': '', },
       package_data = package_data
       )

