
set(cmor_source "${CMAKE_CURRENT_BINARY_DIR}/build/cmor")
set(cmor_install "${CMAKE_CURRENT_BINARY_DIR}/Externals")

set(cmor_configure_args --with-netcdf=${netcdf_install} --with-udunits2=${udunits_install} --with-uuid=${uuid_install} --with-zlib=${zlib_install} --enable-fortran=no)

# it appears currently we only configure cmor but not build it.
ExternalProject_Add(CMOR
  GIT_REPOSITORY http://uv-cdat.llnl.gov/git/cmor.git
  SOURCE_DIR ${cmor_source}
  INSTALL_DIR ${cmor_install}
  BUILD_IN_SOURCE 1
  PATCH_COMMAND ""
  CONFIGURE_COMMAND sh configure --prefix=<INSTALL_DIR> ${cmor_configure_args}
  BUILD_COMMAND ""
  INSTALL_COMMAND ""
  DEPENDS ${cmor_DEPENDENCIES}
)

set(cmor_DIR "${cmor_binary}" CACHE PATH "cmor binary directory" FORCE)
mark_as_advanced(cmor_DIR)
