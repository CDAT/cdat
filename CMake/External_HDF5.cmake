
set(HDF5_source "${CMAKE_CURRENT_BINARY_DIR}/build/HDF5")
set(HDF5_install "${CMAKE_CURRENT_BINARY_DIR}/Externals/HDF5")

# we disable HDF5 warnings because it has way too many of them.
ExternalProject_Add(HDF5
  DOWNLOAD_DIR ${CMAKE_CURRENT_BINARY_DIR}
  SOURCE_DIR ${HDF5_source}
  INSTALL_DIR ${HDF5_install}
  URL ${HDF5_URL}/${HDF5_GZ}
  URL_MD5 ${HDF5_MD5}
  BUILD_IN_SOURCE 1
  PATCH_COMMAND ""
  CONFIGURE_COMMAND ${CMAKE_COMMAND} -DADDITIONAL_CFLAGS=-w -DADDITIONAL_CPPFPAGS=-w -DINSTALL_DIR=<INSTALL_DIR> -DWORKING_DIR=<SOURCE_DIR> -P ${cdat_CMAKE_BINARY_DIR}/cdat_configure_step.cmake
)

set(HDF5_DIR "${HDF5_binary}" CACHE PATH "HDF5 binary directory" FORCE)
mark_as_advanced(HDF5_DIR)
