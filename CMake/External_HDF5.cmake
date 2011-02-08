

set(HDF5_source "${CMAKE_CURRENT_BINARY_DIR}/HDF5")
set(HDF5_binary "${CMAKE_CURRENT_BINARY_DIR}/HDF5-build")
set(HDF5_install "${CMAKE_CURRENT_BINARY_DIR}/HDF5-install")

ExternalProject_Add(HDF5
  DOWNLOAD_DIR ${CMAKE_CURRENT_BINARY_DIR}
  SOURCE_DIR ${HDF5_source}
  BINARY_DIR ${HDF5_build}
  INSTALL_DIR ${HDF5_install}
  URL ${HDF5_URL}/${HDF5_GZ}
  URL_MD5 ${HDF5_MD5}
  BUILD_IN_SOURCE 1
  PATCH_COMMAND ""
  CONFIGURE_COMMAND <SOURCE_DIR>/configure --prefix=<INSTALL_DIR>
)

set(PyOpenGL_DIR "${HDF5_binary}" CACHE PATH "HDF5 binary directory" FORCE)
mark_as_advanced(HDF5_DIR)