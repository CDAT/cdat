from distutils.core import setup
try:
    sys.path.append(os.environ['BUILD_DIR'])
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
       data_files = [ ("share/cdutil",("data/sftbyrgn.nc","data/navy_land.nc"))],
      )
    
