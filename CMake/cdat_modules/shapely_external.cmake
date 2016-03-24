# create an external project to install MyProxyClient,
# and configure and build it
set(nm Shapely)
set(USR_ENVS "GEOS_CONFIG=${cdat_EXTERNALS}/bin/geos-config")
include(pipinstaller)
