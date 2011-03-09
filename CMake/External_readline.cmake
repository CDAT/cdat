
set(readline_source "${CMAKE_CURRENT_BINARY_DIR}/readline")
set(readline_binary "${CMAKE_CURRENT_BINARY_DIR}/readline-build")
set(readline_install "${CMAKE_CURRENT_BINARY_DIR}/readline-install")

ExternalProject_Add(readline
  DOWNLOAD_DIR ${CMAKE_CURRENT_BINARY_DIR}
  SOURCE_DIR ${readline_source}
  BINARY_DIR ${readline_build}
  INSTALL_DIR ${readline_install}
  URL ${READLINE_URL}/${READLINE_GZ}
  URL_MD5 ${READLINE_MD5}
  BUILD_IN_SOURCE 1
  PATCH_COMMAND ""
  CONFIGURE_COMMAND <SOURCE_DIR>/configure --prefix=<INSTALL_DIR>
)

set(readline_DIR "${readline_binary}" CACHE PATH "readline binary directory" FORCE)
mark_as_advanced(readline_DIR)
