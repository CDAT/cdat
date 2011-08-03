
# VisTrails

set(VisTrails_source ${CMAKE_CURRENT_BINARY_DIR}/build/VisTrails)

configure_file(${cdat_CMAKE_SOURCE_DIR}/vistrails_make_step.cmake.in
  ${cdat_CMAKE_BINARY_DIR}/vistrails_make_step.cmake
  @ONLY)

ExternalProject_Add(VisTrails
  GIT_REPOSITORY git://vistrails.org/vistrails.git
  GIT_TAG pcmdi
  SOURCE_DIR ${VisTrails_source}
  BUILD_IN_SOURCE 1
  CONFIGURE_COMMAND ${CMAKE_COMMAND} -P ${cdat_CMAKE_BINARY_DIR}/vistrails_make_step.cmake
  DEPENDS ${VisTrails_DEPENDENCIES}
  ${EP_LOG_OPTIONS}
  )

