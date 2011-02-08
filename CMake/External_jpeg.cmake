
set(jpeg_source "${CMAKE_CURRENT_BINARY_DIR}/build/jpeg")
set(jpeg_install "${CMAKE_CURRENT_BINARY_DIR}/Externals")

ExternalProject_Add(jpeg
  DOWNLOAD_DIR ${CMAKE_CURRENT_BINARY_DIR}
  SOURCE_DIR ${jpeg_source}
  BINARY_DIR ${jpeg_build}
  INSTALL_DIR ${jpeg_install}
  URL ${JPEG_URL}/${JPEG_GZ}
  URL_MD5 ${JPEG_MD5}
  BUILD_IN_SOURCE 1
  PATCH_COMMAND ""
  CONFIGURE_COMMAND <SOURCE_DIR>/configure --prefix=<INSTALL_DIR>
)

set(jpeg_DIR "${jpeg_binary}" CACHE PATH "jpeg binary directory" FORCE)
mark_as_advanced(jpeg_DIR)
