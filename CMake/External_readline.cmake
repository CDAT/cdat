

set(readline_binary "${CMAKE_CURRENT_BINARY_DIR}/build/readline")
set(readline_install "${CMAKE_CURRENT_BINARY_DIR}/Externals")

ExternalProject_Add(readline
  DOWNLOAD_DIR ${CMAKE_CURRENT_BINARY_DIR}
  SOURCE_DIR ${readline_source}
  INSTALL_DIR ${readline_install}
  URL ${READLINE_URL}/${READLINE_GZ}
  URL_MD5 ${READLINE_MD5}
  BUILD_IN_SOURCE 1
  PATCH_COMMAND ""
  CONFIGURE_COMMAND ${CMAKE_COMMAND} -DINSTALL_DIR=<INSTALL_DIR> -DWORKING_DIR=<SOURCE_DIR> -P ${cdat_CMAKE_BINARY_DIR}/cdat_configure_step.cmake
  DEPENDS ${readline_DEPENDENCIES}
)

