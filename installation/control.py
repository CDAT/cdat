# This file is used to control the behavior of install.py.

# The search path is used if the X11 directories aren't configured.
x11search = ['/usr/X11R6', '/usr/X11R6.5.1', 
             '/usr/X11R6.4','/usr','/usr/openwin','/opt']
# Here is where they are on OSF1 and perhaps similar systems
x11OSF1lib = ['/usr/lib/X11', '/usr/lib']
x11OSF1include = ['/usr/include/X11']

# Controlling the install itself
force=0    # Force a complete recompilation?
norun=0   # Cause _install just to echo command?
echo=0     # Echo installation commands before executing?
log=1      # Make logs?
silent = 0 # Report progress?

import os,sys
current_dir = os.path.dirname(__file__)
build_dir = os.getcwd()
sys.path.append(build_dir)
src_dir = os.path.join(current_dir, '..')
installation_script_dir = os.path.join(src_dir, 'installation')

sys.path.append(src_dir)
sys.path.append(installation_script_dir)

# Configuration
do_configure = 1
if os.path.isfile(os.path.join(build_dir,'cdat_info.py')):
    try:
      import cdat_info
      do_configure = 0
    except:
      pass

finish="""
******************************************************
Success! CDAT has been installed in %s .
Make sure all Packages built successfully
******************************************************

""" %(sys.prefix,)

# Options used for building setup.py, install_script, make
if os.environ.has_key('MAKE'):
    make_code = os.environ['MAKE']
else:
    make_code = 'make'

# List of packages to be built
packages = [ 
    "Packages/pydebug",
    "Packages/AutoAPI",
    "Packages/cdtime",
    "Packages/demo",
    "Packages/help",
    "Packages/regrid2",
    "Packages/cdms2", 
    "Packages/esg", 
    "Packages/ncml",
    "Packages/vcs", 
    "Packages/vcsaddons",
    "Packages/cdutil",
    "Packages/unidata",
    "Packages/xmgrace",
    "Packages/genutil", 
    "Packages/Thermo",
    "Packages/WK",
    "Packages/gui_support",
    "Packages/distarray",
    ]

if "--enable-vcs-legacy" in sys.argv:
  packages.append("Packages/vcs_legacy")

