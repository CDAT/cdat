
set(pixman_source "${CMAKE_CURRENT_BINARY_DIR}/pixman")
set(pixman_binary "${CMAKE_CURRENT_BINARY_DIR}/pixman-build")
set(pixman_install "${CMAKE_CURRENT_BINARY_DIR}/pixman-install")

ExternalProject_Add(pixman
  DOWNLOAD_DIR ${CMAKE_CURRENT_BINARY_DIR}
  SOURCE_DIR ${pixman_source}
  BINARY_DIR ${pixman_build}
  INSTALL_DIR ${pixman_install}
  URL ${PIX_URL}/${PIX_GZ}
  URL_MD5 ${PIX_MD5}
  BUILD_IN_SOURCE 1
  PATCH_COMMAND ""
  CONFIGURE_COMMAND <SOURCE_DIR>/configure --prefix=<INSTALL_DIR>
)

set(pixman_DIR "${pixman_binary}" CACHE PATH "pixman binary directory" FORCE)
mark_as_advanced(pixman_DIR)
