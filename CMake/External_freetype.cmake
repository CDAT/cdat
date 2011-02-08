
set(freetype_source "${CMAKE_CURRENT_BINARY_DIR}/build/freetype")
set(freetype_install "${CMAKE_CURRENT_BINARY_DIR}/Externals")

ExternalProject_Add(freetype
  DOWNLOAD_DIR ${CMAKE_CURRENT_BINARY_DIR}
  SOURCE_DIR ${freetype_source}
  BINARY_DIR ${freetype_build}
  INSTALL_DIR ${freetype_install}
  URL ${FT_URL}/${FT_GZ}
  URL_MD5 ${FT_MD5}
  BUILD_IN_SOURCE 1
  PATCH_COMMAND ""
  CONFIGURE_COMMAND <SOURCE_DIR>/configure --prefix=<INSTALL_DIR>
)

set(freetype_DIR "${freetype_binary}" CACHE PATH "freetype binary directory" FORCE)
mark_as_advanced(freetype_DIR)
