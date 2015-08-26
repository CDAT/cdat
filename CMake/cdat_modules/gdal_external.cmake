set(gdal_source "${CMAKE_CURRENT_BINARY_DIR}/build/gdal")
set(gdal_install "${cdat_EXTERNALS}")
set(gdal_configure_args "--prefix=${cdat_EXTERNALS}^^--with-hdf5=${cdat_EXTERNALS}^^--with-netcdf=${cdat_EXTERNALS}^^--with-curl=${cdat_EXTERNALS}^^--with-geos=${cdat_EXTERNALS}/bin/geos-config^^--with-python=${PYTHON_EXECUTABLE}")

ExternalProject_Add(gdal
  LIST_SEPARATOR ^^
  DOWNLOAD_DIR ${CDAT_PACKAGE_CACHE_DIR}
  SOURCE_DIR ${gdal_source}
  INSTALL_DIR ${gdal_install}
  URL ${GDAL_URL}/${GDAL_GZ}
  URL_MD5 ${GDAL_MD5}
  BUILD_IN_SOURCE 1
  PATCH_COMMAND ""
  CONFIGURE_COMMAND <SOURCE_DIR>/configure --prefix=<INSTALL_DIR> --with-hdf5=${cdat_EXTERNALS} --with-netcdf=${cdat_EXTERNALS} --with-curl=${cdat_EXTERNALS} --with-geos=${cdat_EXTERNALS}/bin/geos-config --with-python=${PYTHON_EXECUTABLE} --with-jpeg=no --with-libtiff=internal --without-jpeg12
  DEPENDS "${gdal_deps}"
  ${ep_log_options}
)
