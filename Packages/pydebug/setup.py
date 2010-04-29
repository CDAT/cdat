from distutils.core import setup
import os,sys,string


setup (name = "pydebug",
       version='1.5.4',
       author='Ulrich.Herold@ProConsult-Online.com',
       description = "Debugger for Python programs with a graphical user interface. It is inherited from 'bdb' but uses a GUI and has some powerful features like object browser, windows for variables, classes, functions, exceptions, stack, conditional breakpoints, etc. ",
       url = "http://home.t-online.de/home/Ulrich.Herold/PyDIntro.htm",
       packages = ['pydebug'],
       package_dir = {'pydebug': 'Lib'},
      )
