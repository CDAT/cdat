# Edit this configuration file before building.
# Always build with --force after changing a configuration.
# You do not need to rebuild Python itself.
#print 'DAP'

CDMS_INCLUDE_DAP='yes'
CDMS_DAP_DIR=""
try:
    import cdat_info
    externals = cdat_info.externals
except:
    externals = os.path.join(sys.prefix,"Externals")
externals = os.environ.get("EXTERNALS",externals)

for o in sys.argv[1:]:
    pth = o.lower().split('with-opendap=')
    if len(pth)>1:
        CDMS_DAP_DIR=pth[1]
        
if CDMS_DAP_DIR is "":
    CDMS_DAP_DIR=os.path.join(externals,'OpenDAP')
