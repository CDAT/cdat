
set(uuid_source "${CMAKE_CURRENT_BINARY_DIR}/uuid")
set(uuid_binary "${CMAKE_CURRENT_BINARY_DIR}/uuid-build")
set(uuid_install "${CMAKE_CURRENT_BINARY_DIR}/uuid-install")

ExternalProject_Add(uuid
  DOWNLOAD_DIR ${CMAKE_CURRENT_BINARY_DIR}
  SOURCE_DIR ${uuid_source}
  BINARY_DIR ${uuid_build}
  INSTALL_DIR ${uuid_install}
  URL ${UUID_URL}/${UUID_GZ}
  URL_MD5 ${UUID_MD5}
  BUILD_IN_SOURCE 1
  PATCH_COMMAND ""
  CONFIGURE_COMMAND <SOURCE_DIR>/configure --prefix=<INSTALL_DIR>
)

set(uuid_DIR "${uuid_binary}" CACHE PATH "uuid binary directory" FORCE)
mark_as_advanced(uuid_DIR)
