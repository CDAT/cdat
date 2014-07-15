set(cmor_source_dir "${cdat_SOURCE_DIR}/Packages/cmor")
set(cmor_binary_dir  "${CMAKE_CURRENT_BINARY_DIR}/build/CMOR-build")
set(cmor_install_dir "${cdat_EXTERNALS}")

set(cmor_configure_args --with-netcdf=${netcdf_install} --with-udunits2=${udunits_install} --with-uuid=${uuid_install}  --enable-fortran=no)

# it appears currently we only configure cmor but not build it.
ExternalProject_Add(CMOR
  SOURCE_DIR ${cmor_source_dir}
  BINARY_DIR ${cmor_binary_dir}
  INSTALL_DIR ${cmor_install_dir}
  BUILD_IN_SOURCE 0
  PATCH_COMMAND ""
  CONFIGURE_COMMAND sh ${cmor_source_dir}/configure --prefix=<INSTALL_DIR> ${cmor_configure_args}
  BUILD_COMMAND ""
  INSTALL_COMMAND ""
  DEPENDS ${CMOR_deps}
  ${ep_log_options}
)
