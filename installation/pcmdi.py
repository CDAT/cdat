# Edit this configuration file before building.
# Always build with --force after changing a configuration.
# You do not need to rebuild Python itself.
CDMS_INCLUDE_DRS='yes'
if sys.platform=="linux2":
  COMPILER_EXTRA_LIBS=['pgftnrtl','pgc']
else:
  COMPILER_EXTRA_LIBS=[]
#if sys.platform[0:3] == "aix":  # and probably other platforms...
#    CMDS_INCLUDE_QL = 'no'
#else:
#    CDMS_INCLUDE_QL ='yes'

# These don't actually get respected by the libcdms build yet.
# drs_file = '/usr/local/lib/libdrs.a'

# Add on additional packages
#packages.append('Packages/psql')
#packages.append('Packages/cu')
#packages.append('Packages/pcmdi')

