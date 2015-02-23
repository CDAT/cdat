# h5py
#
set(h5py_source_dir "${CMAKE_CURRENT_BINARY_DIR}/build/h5py")

configure_file(
  "${cdat_CMAKE_SOURCE_DIR}/cdat_modules_extra/h5py_build_step.cmake.in"
  "${cdat_CMAKE_BINARY_DIR}/h5py_build_step.cmake"
  @ONLY
)

set(h5py_build_command ${CMAKE_COMMAND} -P ${cdat_CMAKE_BINARY_DIR}/h5py_build_step.cmake)

ExternalProject_Add(h5py
  DOWNLOAD_DIR ${CDAT_PACKAGE_CACHE_DIR}
  SOURCE_DIR ${h5py_source_dir}
  URL ${H5PY_URL}/${H5PY_GZ}
  URL_MD5 ${H5PY_MD5}
  BUILD_IN_SOURCE 1
  CONFIGURE_COMMAND ""
  BUILD_COMMAND ${h5py_build_command}
  INSTALL_COMMAND ""
  DEPENDS ${h5py_deps}
  ${ep_log_options}
)
