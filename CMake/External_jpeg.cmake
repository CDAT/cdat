
set(jpeg_source "${CMAKE_CURRENT_BINARY_DIR}/build/jpeg")
set(jpeg_install "${CMAKE_CURRENT_BINARY_DIR}/Externals")

ExternalProject_Add(jpeg
  DOWNLOAD_DIR ${CMAKE_CURRENT_BINARY_DIR}
  SOURCE_DIR ${jpeg_source}
  INSTALL_DIR ${jpeg_install}
  URL ${JPEG_URL}/${JPEG_GZ}
  URL_MD5 ${JPEG_MD5}
  BUILD_IN_SOURCE 1
  PATCH_COMMAND ""
  CONFIGURE_COMMAND ${CMAKE_COMMAND} -DINSTALL_DIR=<INSTALL_DIR> -DWORKING_DIR=<SOURCE_DIR> -P ${cdat_CMAKE_BINARY_DIR}/cdat_configure_step.cmake
)

set(jpeg_DIR "${jpeg_binary}" CACHE PATH "jpeg binary directory" FORCE)
mark_as_advanced(jpeg_DIR)
