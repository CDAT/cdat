set(ESMF_source_dir "${CMAKE_CURRENT_BINARY_DIR}/build/ESMF" CACHE INTERNAL "")
set(ESMF_source "${CMAKE_CURRENT_BINARY_DIR}/build/ESMF" CACHE INTERNAL "")
set(ESMF_install "${cdat_EXTERNALS}" CACHE INTERNAL "")
set(ESMF_pthreads "OFF")
set(ESMF_os "${CDAT_BUILD_ESMF_OS}")
set(ESMF_compiler "${CDAT_BUILD_ESMF_COMPILER}")
set(ESMF_abi "${CDAT_BUILD_ESMF_ABI}")
set(ESMF_openmp "ON")

if(APPLE)
  if("${CMAKE_C_COMPILER_ID}" STREQUAL "Clang" AND ${CMAKE_C_COMPILER_VERSION} VERSION_GREATER 4.2)
    # xcode 5 clang does not support openmp
    set(ESMF_openmp "OFF")
  endif()
endif()

# Check if ESMF should be built in parallel
set(emsf_enable_mpi FALSE)
if(CDAT_BUILD_ESMF_PARALLEL)
  set(emsf_enable_mpi TRUE)
endif()

if("${emsf_enable_mpi}")
  set(ESMF_comm "${CDAT_BUILD_ESMF_COMM}")
else()
  message("[INFO] CDAT will build ESMF serial")
  set(ESMF_comm "mpiuni")
endif()

configure_file(
  ${cdat_CMAKE_SOURCE_DIR}/cdat_modules_extra/ESMF_make_step.cmake.in
  ${cdat_CMAKE_BINARY_DIR}/ESMF_make_step.cmake
  @ONLY
)

configure_file(
  ${cdat_CMAKE_SOURCE_DIR}/cdat_modules_extra/ESMF_install_step.cmake.in
  ${cdat_CMAKE_BINARY_DIR}/ESMF_install_step.cmake
  @ONLY
)

set(ESMF_build_command ${CMAKE_COMMAND} -P ${cdat_CMAKE_BINARY_DIR}/ESMF_make_step.cmake)
set(ESMF_install_command ${CMAKE_COMMAND} -P ${cdat_CMAKE_BINARY_DIR}/ESMF_install_step.cmake)

# ESMF Python interface. Install after ESMF is done.
set(ESMP_source "${ESMF_source_dir}/ESMP")

configure_file(
  ${cdat_CMAKE_SOURCE_DIR}/cdat_modules_extra/ESMP_install_step.cmake.in
  ${cdat_CMAKE_BINARY_DIR}/ESMP_install_step.cmake
  @ONLY
)

configure_file(
  ${cdat_CMAKE_SOURCE_DIR}/cdat_modules_extra/ESMP_patch_step.cmake.in
  ${cdat_CMAKE_BINARY_DIR}/ESMP_patch_step.cmake
  @ONLY
)

set(ESMP_install_command ${CMAKE_COMMAND} -P ${cdat_CMAKE_BINARY_DIR}/ESMP_install_step.cmake)
set(ESMP_patch_command ${CMAKE_COMMAND} -P ${cdat_CMAKE_BINARY_DIR}/ESMP_patch_step.cmake)

ExternalProject_Add(ESMF
  DOWNLOAD_DIR ${CDAT_PACKAGE_CACHE_DIR}
  SOURCE_DIR ${ESMF_source_dir}
  INSTALL_DIR ${ESMF_install}
  URL ${ESMF_URL}/${ESMF_GZ}
  URL_MD5 ${ESMF_MD5}
  BUILD_IN_SOURCE 1
  PATCH_COMMAND ""
  CONFIGURE_COMMAND ""
  BUILD_COMMAND ${ESMF_build_command}
  INSTALL_COMMAND ${ESMF_install_command}
  INSTALL_COMMAND ${ESMP_install_command}
  PATCH_COMMAND ${ESMP_patch_command}
  DEPENDS ${ESMF_deps}
  ${ep_log_options}
)
