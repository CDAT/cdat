
set(setuptools_source "${CMAKE_CURRENT_BINARY_DIR}/build/setuptools")
set(setuptools_install "${CMAKE_CURRENT_BINARY_DIR}/Externals")

ExternalProject_Add(setuptools
  DOWNLOAD_DIR ${CMAKE_CURRENT_BINARY_DIR}
  SOURCE_DIR ${setuptools_source}
  INSTALL_DIR ${setuptools_install}
  URL ${SETUPTOOLS_URL}/${SETUPTOOLS_GZ}
  URL_MD5 ${SETUPTOOLS_MD5}
  BUILD_IN_SOURCE 1
  PATCH_COMMAND ""
  CONFIGURE_COMMAND ${CMAKE_COMMAND} -DINSTALL_DIR=<INSTALL_DIR> -DWORKING_DIR=<SOURCE_DIR> -P ${cdat_CMAKE_BINARY_DIR}/cdat_configure_step.cmake
  DEPENDS ${setuptools_DEPENDENCIES}
)

set(setuptools_DIR "${setuptools_binary}" CACHE PATH "setuptools binary directory" FORCE)
mark_as_advanced(setuptools_DIR)
