
set(setuptools_source "${CMAKE_CURRENT_BINARY_DIR}/setuptools")
set(setuptools_binary "${CMAKE_CURRENT_BINARY_DIR}/setuptools-build")
set(setuptools_install "${CMAKE_CURRENT_BINARY_DIR}/setuptools-install")

ExternalProject_Add(setuptools
  DOWNLOAD_DIR ${CMAKE_CURRENT_BINARY_DIR}
  SOURCE_DIR ${setuptools_source}
  BINARY_DIR ${setuptools_build}
  INSTALL_DIR ${setuptools_install}
  URL ${SETUPTOOLS_URL}/${SETUPTOOLS_GZ}
  URL_MD5 ${SETUPTOOLS_MD5}
  BUILD_IN_SOURCE 1
  PATCH_COMMAND ""
  CONFIGURE_COMMAND <SOURCE_DIR>/configure --prefix=<INSTALL_DIR>
)

set(setuptools_DIR "${setuptools_binary}" CACHE PATH "setuptools binary directory" FORCE)
mark_as_advanced(setuptools_DIR)
