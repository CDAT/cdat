
set(R_source "${CMAKE_CURRENT_BINARY_DIR}/build/R")
set(R_install "${cdat_EXTERNALS}")

ExternalProject_Add(R
  DOWNLOAD_DIR ${CMAKE_CURRENT_BINARY_DIR}
  SOURCE_DIR ${R_source}
  INSTALL_DIR ${R_install}
  URL ${R_URL}/${R_GZ}
  URL_MD5 ${R_MD5}
  BUILD_IN_SOURCE 1
  PATCH_COMMAND ""
  CONFIGURE_COMMAND <SOURCE_DIR>/configure --prefix=<INSTALL_DIR>
)

set(R_DIR "${R_binary}" CACHE PATH "R binary directory" FORCE)
mark_as_advanced(R_DIR)
