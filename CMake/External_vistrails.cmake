# create an external project to clone vistrails,
# and configure and build it

ExternalProject_Add(vistrails
  DOWNLOAD_DIR ${CMAKE_CURRENT_BINARY_DIR}
  SOURCE_DIR ${CMAKE_INSTALL_PREFIX}
  BUILD_IN_SOURCE 1
  CONFIGURE_COMMAND ""
  BUILD_COMMAND ${GIT_EXECUTABLE} clone  git://vistrails.org/vistrails.git
  INSTALL_COMMAND ${GIT_EXECUTABLE} checkout ${VISTRAILS_TAG_POINT}
  DEPENDS ${vistrails_DEPENDENCIES}
  ${EP_LOG_OPTIONS}
  )

