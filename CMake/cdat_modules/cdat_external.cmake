set(CDAT_source "${cdat_SOURCE_DIR}")
set(WORKING_DIR "${cdat_CMAKE_BINARY_DIR}")

ExternalProject_Add(CDAT
  DOWNLOAD_DIR ""
  SOURCE_DIR ${cdat_SOURCE_DIR}
  BINARY_DIR ${cdat_build_dir}
  BUILD_IN_SOURCE 0
  PATCH_COMMAND ""
  CONFIGURE_COMMAND ""
  BUILD_COMMAND ""
  INSTALL_COMMAND ${cdat_CMAKE_BINARY_DIR}/install_cdat_from_conda.bash
  DEPENDS ${CDAT_deps}
  ${ep_log_options}
)
