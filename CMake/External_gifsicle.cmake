
set(gifsicle_source "${CMAKE_CURRENT_BINARY_DIR}/build/gifsicle")
set(gifsicle_install "${CMAKE_CURRENT_BINARY_DIR}/Externals")

ExternalProject_Add(gifsicle
  DOWNLOAD_DIR ${CMAKE_CURRENT_BINARY_DIR}
  SOURCE_DIR ${gifsicle_source}
  INSTALL_DIR ${gifsicle_install}
  URL ${GIFSICLE_URL}/${GIFSICLE_GZ}
  URL_MD5 ${GIFSICLE_MD5}
  BUILD_IN_SOURCE 1
  PATCH_COMMAND ""
  CONFIGURE_COMMAND <SOURCE_DIR>/configure --prefix=<INSTALL_DIR>
)

set(gifsicle_DIR "${gifsicle_binary}" CACHE PATH "gifsicle binary directory" FORCE)
mark_as_advanced(gifsicle_DIR)
