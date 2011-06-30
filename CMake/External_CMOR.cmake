
set(cmor_source "${cdat_SOURCE_DIR}/Packages/cmor")
set(cmor_install "${cdat_EXTERNALS}")

set(cmor_configure_args --with-netcdf=${netcdf_install} --with-udunits2=${udunits_install} --with-uuid=${uuid_install} --with-zlib=${zlib_install} --enable-fortran=no)

# it appears currently we only configure cmor but not build it.
ExternalProject_Add(CMOR
  SOURCE_DIR ${cmor_source}
  INSTALL_DIR ${cmor_install}
  BUILD_IN_SOURCE 1
  PATCH_COMMAND ""
  CONFIGURE_COMMAND sh configure --prefix=<INSTALL_DIR> ${cmor_configure_args}
  BUILD_COMMAND ""
  INSTALL_COMMAND ""
  DEPENDS ${CMOR_DEPENDENCIES}
)
