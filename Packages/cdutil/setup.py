from distutils.core import setup
try:
    import cdat_info
    Version=cdat_info.Version
except:
    Version="???"

setup (name = "cdutil",
       author="PCMDI Software Team",
       version=Version,
       description = "Utilities for climate data manipulation",
       url = "http://cdat.sourceforge.net",
       packages = ['cdutil'],
       package_dir = {'cdutil': 'Lib'},
      )
    
