from distutils.core import setup
import os
current_dir = os.path.dirname(__file__)
setup (name = "gui_support",
       version='3.2',
       author='PCMDI',
       description = "Common components for vcs gui's",
       url = "http://cdat.sourceforge.net",
       packages = ['gui_support'],
       package_dir = {'gui_support': os.path.join(current_dir,'Lib')},
      )

