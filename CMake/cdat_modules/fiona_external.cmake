# create an external project to install Fiona
# and configure and build it
set(nm Fiona)
set(USR_ENVS "GDAL_CONFIG=${cdat_EXTERNALS}/bin/gdal-config")
#set(USER_BUILD_EXT_OPTS "build_ext -I${cdat_EXTERNALS}/include -L${cdat_EXTERNALS}/lib -lgdal")
include(pipinstaller)
