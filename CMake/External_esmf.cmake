
set(esmf_source "${CMAKE_CURRENT_BINARY_DIR}/build/esmf" CACHE INTERNAL "")
set(esmf_install "${cdat_EXTERNALS}" CACHE INTERNAL "")
set(esmf_comm "mpiuni")
set(esmf_pthreads "OFF")

configure_file(${cdat_CMAKE_SOURCE_DIR}/esmf_make_step.cmake.in
  ${cdat_CMAKE_BINARY_DIR}/esmf_make_step.cmake
  @ONLY)
  
configure_file(${cdat_CMAKE_SOURCE_DIR}/esmf_install_step.cmake.in
  ${cdat_CMAKE_BINARY_DIR}/esmf_install_step.cmake
  @ONLY)

set(esmf_build_command ${CMAKE_COMMAND} -P ${cdat_CMAKE_BINARY_DIR}/esmf_make_step.cmake)
set(esmf_install_command ${CMAKE_COMMAND} -P ${cdat_CMAKE_BINARY_DIR}/esmf_install_step.cmake)

ExternalProject_Add(ESMF
  DOWNLOAD_DIR ${CMAKE_CURRENT_BINARY_DIR}
  SOURCE_DIR ${esmf_source}
  INSTALL_DIR ${esmf_install}
  URL ${ESMF_URL}/${ESMF_GZ}
#  URL_MD5 ${ESMF_MD5}
  URL_MD5 ""
  BUILD_IN_SOURCE 1
  PATCH_COMMAND ""
  CONFIGURE_COMMAND ""
  BUILD_COMMAND ${esmf_build_command}
  INSTALL_COMMAND ${esmf_install_command}
  DEPENDS ${esmf_DEPENDENCIES}
  ${EP_LOG_OPTIONS}
)
