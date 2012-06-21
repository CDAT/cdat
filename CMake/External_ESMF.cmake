
set(ESMF_source "${CMAKE_CURRENT_BINARY_DIR}/build/ESMF" CACHE INTERNAL "")
set(ESMF_install "${cdat_EXTERNALS}" CACHE INTERNAL "")
set(ESMF_pthreads "OFF")

set(ESMF_os "${CDAT_BUILD_ESMF_OS}")
set(ESMF_compiler "${CDAT_BUILD_ESMF_COMPILER}")
set(ESMF_abi "${CDAT_BUILD_ESMF_ABI}")

# check if ESMF should be built in parallel
set(emsf_enable_mpi FALSE)
if(CDAT_BUILD_ESMF_PARALLEL)
  set(emsf_enable_mpi TRUE)
endif()

if("${emsf_enable_mpi}")
  set(ESMF_comm "${CDAT_BUILD_ESMF_COMM}")
else()
  message("Will build ESMF serial")
  set(ESMF_comm "mpiuni")
endif()

configure_file(${cdat_CMAKE_SOURCE_DIR}/ESMF_make_step.cmake.in
  ${cdat_CMAKE_BINARY_DIR}/ESMF_make_step.cmake
  @ONLY)
  
configure_file(${cdat_CMAKE_SOURCE_DIR}/ESMF_install_step.cmake.in
  ${cdat_CMAKE_BINARY_DIR}/ESMF_install_step.cmake
  @ONLY)

set(ESMF_build_command ${CMAKE_COMMAND} -P ${cdat_CMAKE_BINARY_DIR}/ESMF_make_step.cmake)
set(ESMF_install_command ${CMAKE_COMMAND} -P ${cdat_CMAKE_BINARY_DIR}/ESMF_install_step.cmake)

ExternalProject_Add(ESMF
  DOWNLOAD_DIR ${CMAKE_CURRENT_BINARY_DIR}
  SOURCE_DIR ${ESMF_source}
  INSTALL_DIR ${ESMF_install}
  URL ${ESMF_URL}/${ESMF_GZ}
  URL_MD5 ${ESMF_MD5}
  BUILD_IN_SOURCE 1
  PATCH_COMMAND ""
  CONFIGURE_COMMAND ""
  BUILD_COMMAND ${ESMF_build_command}
  INSTALL_COMMAND ${ESMF_install_command}
  DEPENDS ${ESMF_DEPENDENCIES}
  ${EP_LOG_OPTIONS}
)
