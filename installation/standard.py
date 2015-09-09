# DO NOT EDIT THIS FILE
# Instead, make your own configuration file to override these values 
# and use the -c option to read it.

# This is the standard configuration file. It is read first by install.py.
# In your own configuration file you can use any Python statements to modify
# these values. 

# File pcmdi.txt is an example that shows the changes we use at PCMDI.

# Append to packages to build additional packages, such as
# packages.append('cu')

#This file is executed as Python input so you can compute values depending on
#platform, etc. Modules os, sys will be imported already.

current_dir = os.path.dirname(__file__)
src_dir = os.path.join(current_dir, '..')
libcdms_dir = os.path.join(src_dir, 'libcdms')

## This part figures out the target thing
target_prefix = sys.prefix
for i in range(len(sys.argv)):
    a = sys.argv[i]
    if a=='--prefix':
        target_prefix=sys.argv[i+1]
    sp = a.split("--prefix=")
    if len(sp)==2:
        target_prefix=sp[1]


# This is where we build netcdf, if you let us
#netcdf_directory = os.popen('%s --prefix' % os.environ.get("LOCNCCONFIG","nc-config")).readlines()[0]
#netcdf_include_directory = os.popen('%s --includedir' % os.environ.get("LOCNCCONFIG","nc-config")).readlines()[0]
#netcdf_include_directory= os.path.join(os.environ.get("EXTERNALS",os.path.join(sys.prefix,'Externals')),'include')

#  Control of the CDMS build
drs_file = '/usr/local/libdrs.a'  # only needed if next line is 'yes'
CDMS_INCLUDE_DRS='no'    # Change to 'yes' to include DRS. If yes:
                         # Assumes /usr/local/lib/libdrs.a exists.
                         # Assumes you have a Fortran compiler.
CDMS_INCLUDE_QL='no'     # Include QL in build?
                         # Caution: if set to yes, CDMS library compile
                         # may fail on certain platforms, including AIX.
CDMS_INCLUDE_HDF='no'    # Don't set to yes, doesn't work.
CDMS_INCLUDE_PP='no'     # Met Office PP format is built in to cdunif.
#  Location of X11 library
#     If you set x11libdir (that is two ones and an el) AND x11include to point
#     to the lib and include directories, they will be used.
#     Otherwise a search is made for common locations.
x11libdir='/opt/X11/lib'
x11include='/opt/X11/include'

#  List of math libraries
#    We attempt to get the C math libraries right but if we don't fix it.
mathlibs= ['m']  #i.e., libm.a
if sys.platform in ['win32', 'mac', 'beos5']:
    mathlibs = []

# Build actions
action = {}
## Commenting out pyfort not used anylonger (it's been years)
#if os.path.exists(os.path.join(target_prefix, 'bin', 'pyfort')):
#    action['*.pfp'] = os.path.join(target_prefix, 'bin', 'pyfort') + " -i %(file)s ; "
#elif os.path.exists(os.path.join(sys.exec_prefix, 'bin', 'pyfort')):
#    action['*.pfp'] = os.path.join(sys.exec_prefix, 'bin', 'pyfort') + " -i %(file)s ; "
#else:
#    action['*.pfp'] = "pyfort  -i %(file)s ; "
    
# matplotlib depends on pkg-config
action['setup.py'] = 'PATH=%s/bin:$PATH  %s setup.py install --prefix=%s ; ' \
    % (sys.exec_prefix, sys.executable, target_prefix)
install_script_path = os.path.join(libcdms_dir, 'install_script')
action['install_script'] = install_script_path + ' %s %s ; ' % (target_prefix, sys.executable)
for k in ['makefile','Makefile','MAKEFILE']:
    action[k] = make_code + " PYPREFIX='%s' PREFIX='%s' install ; " % (sys.exec_prefix,target_prefix)
action['autogen.sh'] = "autogen.sh ; ./configure --prefix=%s  --with-python=%s ; make -j1 ; make -j1 install ;" % (os.environ.get("EXTERNALS",os.path.join(sys.prefix,'Externals')), os.path.join(sys.exec_prefix,'bin','python'))
