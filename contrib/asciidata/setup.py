from distutils.core import setup
import time
setup (name = "asciidata",
       version=time.asctime(time.gmtime(time.time())),
       description = "ascii separated values data file reader",
       packages = ['asciidata'],
       package_dir = {'asciidata': 'Lib'},
      )
    
