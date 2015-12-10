set(gdal_source "${CMAKE_CURRENT_BINARY_DIR}/build/gdal")
set(gdal_install "${cdat_EXTERNALS}")
set(gdal_configure_args "--prefix=${cdat_EXTERNALS}^^--with-hdf5=${cdat_EXTERNALS}^^--with-netcdf=${cdat_EXTERNALS}^^--with-curl=${cdat_EXTERNALS}^^--with-geos=${cdat_EXTERNALS}/bin/geos-config^^--with-python=${PYTHON_EXECUTABLE}^^--with-jpeg=no^^--with-libtiff=internal^^--without-jpeg12^^--with-geotiff=internal^^--with-static-proj4=${cdat_EXTERNALS}/proj4")

if (CDAT_BUILD_PARALLEL)
  set(configure_file "cdatmpi_configure_step.cmake")
else()
  set(configure_file "cdat_configure_step.cmake")
endif()
message("[GDAL] CONF FILE IS:"${configure_file})
ExternalProject_Add(gdal
  LIST_SEPARATOR ^^
  DOWNLOAD_DIR ${CDAT_PACKAGE_CACHE_DIR}
  SOURCE_DIR ${gdal_source}
  INSTALL_DIR ${gdal_install}
  URL ${GDAL_URL}/${GDAL_GZ}
  URL_MD5 ${GDAL_MD5}
  BUILD_IN_SOURCE 1
  PATCH_COMMAND ""
  CONFIGURE_COMMAND sh configure --prefix=${cdat_EXTERNALS} --with-hdf5=${cdat_EXTERNALS} --with-netcdf=${cdat_EXTERNALS} --with-curl=${cdat_EXTERNALS} --with-geos=${cdat_EXTERNALS}/bin/geos-config --with-python=${PYTHON_EXECUTABLE} --with-jpeg=no --with-libtiff=internal --without-jpeg12 --with-geotiff=internal
  DEPENDS "${gdal_deps}"
  ${ep_log_options}
)
