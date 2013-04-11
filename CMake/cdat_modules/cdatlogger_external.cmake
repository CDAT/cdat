
configure_file(${cdat_CMAKE_SOURCE_DIR}/cmake_modules/CDATLogger.cmake.in
  ${cdat_CMAKE_BINARY_DIR}/CDATLogger.cmake
  @ONLY)

ExternalProject_Add(CDATLogger
  DOWNLOAD_DIR ""
  SOURCE_DIR ${cdat_SOURCE_DIR}
  BINARY_DIR ${cdat_build_dir}
  BUILD_IN_SOURCE 0
  PATCH_COMMAND ""
  CONFIGURE_COMMAND ""
  BUILD_COMMAND ""
  INSTALL_COMMAND ${CMAKE_COMMAND} -P ${cdat_CMAKE_BINARY_DIR}/CDATLogger.cmake
  DEPENDS ${CDATLogger_deps}
)
