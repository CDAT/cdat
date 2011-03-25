
set(xgks_source "${CMAKE_CURRENT_BINARY_DIR}/build/xgks")
set(xgks_install "${CMAKE_CURRENT_BINARY_DIR}/Externals")

configure_file(${cdat_CMAKE_SOURCE_DIR}/xgks_configure_step.cmake.in
  ${cdat_CMAKE_BINARY_DIR}/xgks_configure_step.cmake
  @ONLY)

#cp -f build/xgks*/port/misc/udposix.h /home/partyd/Projects/uv-cdat/make-install/Externals/include

ExternalProject_Add(xgks
  DOWNLOAD_DIR ${CMAKE_CURRENT_BINARY_DIR}
  SOURCE_DIR ${xgks_source}
  INSTALL_DIR ${xgks_install}
  URL ${XGKS_URL}/${XGKS_GZ}
  URL_MD5 ${XGKS_MD5}
  BUILD_IN_SOURCE 1
  PATCH_COMMAND ""
  CONFIGURE_COMMAND ${CMAKE_COMMAND} -DINSTALL_DIR=<INSTALL_DIR> -DWORKING_DIR=<SOURCE_DIR> -P ${cdat_CMAKE_BINARY_DIR}/xgks_configure_step.cmake
)
