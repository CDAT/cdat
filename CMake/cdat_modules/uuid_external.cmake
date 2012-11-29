
set(uuid_source "${CMAKE_CURRENT_BINARY_DIR}/build/uuid")
set(uuid_install "${cdat_EXTERNALS}")

ExternalProject_Add(uuid
  DOWNLOAD_DIR ${CDAT_PACKAGE_CACHE_DIR}
  SOURCE_DIR ${uuid_source}
  INSTALL_DIR ${uuid_install}
  URL ${UUID_URL}/${UUID_GZ}
  URL_MD5 ${UUID_MD5}
  BUILD_IN_SOURCE 1
  PATCH_COMMAND ""
  CONFIGURE_COMMAND ${CMAKE_COMMAND} -DINSTALL_DIR=<INSTALL_DIR> -DWORKING_DIR=<SOURCE_DIR> -P ${cdat_CMAKE_BINARY_DIR}/cdat_configure_step.cmake
  DEPENDS ${uuid_DEPENDENCY}
  ${ep_log_options}
)

set(uuid_DIR "${uuid_binary}" CACHE PATH "uuid binary directory" FORCE)
mark_as_advanced(uuid_DIR)
