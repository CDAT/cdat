
set(xgks_source "${CMAKE_CURRENT_BINARY_DIR}/build/xgks")
set(xgks_install "${CMAKE_CURRENT_BINARY_DIR}/Externals")

ExternalProject_Add(xgks
  DOWNLOAD_DIR ${CMAKE_CURRENT_BINARY_DIR}
  SOURCE_DIR ${xgks_source}
  INSTALL_DIR ${xgks_install}
  URL ${XGKS_URL}/${XGKS_GZ}
  URL_MD5 ${XGKS_MD5}
  BUILD_IN_SOURCE 1
  PATCH_COMMAND ""
  CONFIGURE_COMMAND ${CMAKE_COMMAND} -DINSTALL_DIR=<INSTALL_DIR> -DWORKING_DIR=<SOURCE_DIR> -P ${cdat_CMAKE_BINARY_DIR}/cdat_configure_step.cmake
)

set(xgks_DIR "${xgks_binary}" CACHE PATH "xgks binary directory" FORCE)
mark_as_advanced(xgks_DIR)
