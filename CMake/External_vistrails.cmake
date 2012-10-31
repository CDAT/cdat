# create an external project to clone vistrails,
# and configure and build it

set(vistrails_url "git://vistrails.org/vistrails.git")

# NOTE: vistrails has a different URL for http
if("${GIT_PROTOCOL}" STREQUAL "http://")
  set(vistrails_url "http://vistrails.org/git/vistrails.git")
endif()

ExternalProject_Add(vistrails
  DOWNLOAD_DIR ${CMAKE_CURRENT_BINARY_DIR}
  SOURCE_DIR ${CMAKE_INSTALL_PREFIX}
  BUILD_IN_SOURCE 1
  CONFIGURE_COMMAND ""
  BUILD_COMMAND ""
  INSTALL_COMMAND ${GIT_EXECUTABLE} clone --depth 1 -b ${VISTRAILS_TAG_POINT}  ${vistrails_url}
  DEPENDS ${vistrails_DEPENDENCIES}
  ${EP_LOG_OPTIONS}
)

ExternalProject_Add_Step(vistrails after_install
  COMMAND ${CMAKE_COMMAND} -E remove_directory ${CMAKE_INSTALL_PREFIX}/vistrails/.git
  DEPENDEES install
  WORKING_DIRECTORY ${CMAKE_INSTALL_PREFIX}/vistrails
)
