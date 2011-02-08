
set(pbmplus_source "${CMAKE_CURRENT_BINARY_DIR}/build/pbmplus")
set(pbmplus_install "${CMAKE_CURRENT_BINARY_DIR}/Externals")

ExternalProject_Add(pbmplus
  DOWNLOAD_DIR ${CMAKE_CURRENT_BINARY_DIR}
  SOURCE_DIR ${pbmplus_source}
  INSTALL_DIR ${pbmplus_install}
  URL ${PBMPLUS_URL}/${PBMPLUS_GZ}
  URL_MD5 ${PBMPLUS_MD5}
  BUILD_IN_SOURCE 1
  PATCH_COMMAND ""
  CONFIGURE_COMMAND <SOURCE_DIR>/configure --prefix=<INSTALL_DIR>
)

set(pbmplus_DIR "${pbmplus_binary}" CACHE PATH "pbmplus binary directory" FORCE)
mark_as_advanced(pbmplus_DIR)
