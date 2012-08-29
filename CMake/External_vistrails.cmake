# create an external project to clone vistrails,
# and configure and build it

ExternalProject_Add(vistrails
  DOWNLOAD_DIR ${CMAKE_CURRENT_BINARY_DIR}
  SOURCE_DIR ${CMAKE_INSTALL_PREFIX}
  BUILD_IN_SOURCE 1
  CONFIGURE_COMMAND ""
  BUILD_COMMAND ""
  INSTALL_COMMAND ${GIT_EXECUTABLE} clone -b ${VISTRAILS_TAG_POINT}  git://vistrails.org/vistrails.git
  DEPENDS ${vistrails_DEPENDENCIES}
  ${EP_LOG_OPTIONS}
  )

