import os, string
from distutils.core import setup
execfile(os.path.join('Lib', 'Properties_version.py'))
setup (name = "PropertiedClasses",
       version=version,
       author="Paul F. Dubois",
       description = "Properties",
       maintainer = "Numerical Python Developers",
       maintainer_email = "numpy-developers@lists.sourceforge.net",
       url = "http://sourceforge.net/projects/numpy",
       packages = ['PropertiedClasses'],
       package_dir = {'PropertiedClasses': 'Lib'},
      )
