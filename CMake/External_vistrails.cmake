# create an external project to clone vistrails,
# and configure and build it

set(VISTRAILS_TAG_POINT uvcdat-next CACHE STRING "Specify branch of vistrails to be used for UVCDAT")
set(vistrails_url "${GIT_PROTOCOL}vistrails.org/git/vistrails.git")

ExternalProject_Add(vistrails
  DOWNLOAD_DIR ${CMAKE_CURRENT_BINARY_DIR}
  SOURCE_DIR ${CMAKE_INSTALL_PREFIX}
  BUILD_IN_SOURCE 1
  CONFIGURE_COMMAND ""
  BUILD_COMMAND ""
  INSTALL_COMMAND ${GIT_EXECUTABLE} clone -b ${VISTRAILS_TAG_POINT}  ${vistrails_url}
  DEPENDS ${vistrails_DEPENDENCIES}
  ${EP_LOG_OPTIONS}
  )

