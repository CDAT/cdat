
set(HDF4_source "${CMAKE_CURRENT_BINARY_DIR}/HDF4")
set(HDF4_binary "${CMAKE_CURRENT_BINARY_DIR}/HDF4-build")
set(HDF4_install "${CMAKE_CURRENT_BINARY_DIR}/HDF4-install")

ExternalProject_Add(HDF4
  DOWNLOAD_DIR ${CMAKE_CURRENT_BINARY_DIR}
  SOURCE_DIR ${HDF4_source}
  BINARY_DIR ${HDF4_build}
  INSTALL_DIR ${HDF4_install}
  URL ${HDF4_URL}/${HDF4_GZ}
  URL_MD5 ${HDF4_MD5}
  BUILD_IN_SOURCE 1
  PATCH_COMMAND ""
  CONFIGURE_COMMAND <SOURCE_DIR>/configure --prefix=<INSTALL_DIR>
)

set(HDF4_DIR "${HDF4_binary}" CACHE PATH "HDF4 binary directory" FORCE)
mark_as_advanced(HDF4_DIR)
