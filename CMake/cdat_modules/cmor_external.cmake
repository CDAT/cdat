set(cmor_source_dir  "${CMAKE_CURRENT_BINARY_DIR}/build/cmor")
set(cmor_binary_dir  "${CMAKE_CURRENT_BINARY_DIR}/build/cmor")
set(cmor_install_dir "${cdat_EXTERNALS}")

set(cmor_configure_args --with-netcdf=${netcdf_install} --with-udunits2=${udunits_install} --with-uuid=${uuid_install} --enable-fortran=yes --with-python=${CMAKE_INSTALL_PREFIX} --prefix=${CMAKE_INSTALL_PREFIX})

# it appears currently we only configure cmor but not build it.
ExternalProject_Add(CMOR
  DOWNLOAD_DIR ${CDAT_PACKAGE_CACHE_DIR}
  SOURCE_DIR ${cmor_source_dir}
  BUILD_IN_SOURCE 1
  ${GIT_CMD_STR_CMOR}
  ${GIT_TAG}
  INSTALL_DIR ${cmor_install_dir}
  PATCH_COMMAND ""
  CONFIGURE_COMMAND sh ${cmor_source_dir}/configure ${cmor_configure_args}
  DEPENDS ${CMOR_deps}
  ${ep_log_options}
)
if (DEFINED GIT_CMD_STR_CMOR)
  unset(GIT_CMD_STR_CMOR)
endif()
