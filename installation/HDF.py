# To build on Linux with HDF:
# express_install /usr/local/cdat/somewhere --force --configuration installation/hdf.py
import os
current_dir = os.path.dirname(__file__)
src_dir = os.path.join(current_dir, '..')
installation_script_dir = os.path.join(src_dir, 'installation')

sys.path.append(src_dir)
sys.path.append(installation_script_dir)

CDMS_INCLUDE_HDF='yes'
CDMS_HDF_DIR=""
try:
    import cdat_info
    externals = cdat_info.externals
except:
    externals = os.path.join(sys.prefix,"Externals")
externals = os.environ.get("EXTERNALS",externals)

for o in sys.argv[1:]:
    pth = o.lower().split('with-hdf4=')
    if len(pth)>1:
        CDMS_HDF_DIR=pth[1]

if CDMS_HDF_DIR is "":
    CDMS_HDF_DIR=os.path.join(externals,'HDF')
