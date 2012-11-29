
set(libcf_source "${CMAKE_CURRENT_BINARY_DIR}/build/libcf")
set(libcf_install "${cdat_EXTERNALS}")

configure_file(${cdat_CMAKE_SOURCE_DIR}/cdat_modules_extra/libcf_make_step.cmake.in
  ${cdat_CMAKE_BINARY_DIR}/libcf_make_step.cmake
  @ONLY)
  
configure_file(${cdat_CMAKE_SOURCE_DIR}/cdat_modules_extra/libcf_install_step.cmake.in
  ${cdat_CMAKE_BINARY_DIR}/libcf_install_step.cmake
  @ONLY)

set(libcf_build_command ${CMAKE_COMMAND} -P ${cdat_CMAKE_BINARY_DIR}/libcf_make_step.cmake)
set(libcf_install_command ${CMAKE_COMMAND} -P ${cdat_CMAKE_BINARY_DIR}/libcf_install_step.cmake)

ExternalProject_Add(libcf
  DOWNLOAD_DIR ${CDAT_PACKAGE_CACHE_DIR}
  SOURCE_DIR ${libcf_source}
  INSTALL_DIR ${libcf_install}
  URL ${LIBCF_URL}/${LIBCF_GZ}
  URL_MD5 ${LIBCF_MD5}
  BUILD_IN_SOURCE 1
  CONFIGURE_COMMAND ${CMAKE_COMMAND} -DINSTALL_DIR=<INSTALL_DIR> -DWORKING_DIR=<SOURCE_DIR> -P ${cdat_CMAKE_BINARY_DIR}/cdat_configure_step.cmake
  PATCH_COMMAND ""
  BUILD_COMMAND ${libcf_build_command}
  INSTALL_COMMAND ${libcf_install_command}
  DEPENDS ${libcf_deps}
  ${ep_log_options}
)
