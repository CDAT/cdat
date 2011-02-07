
set(udunits_source "${CMAKE_CURRENT_BINARY_DIR}/udunits")
set(udunits_binary "${CMAKE_CURRENT_BINARY_DIR}/udunits-build")
set(udunits_install "${CMAKE_CURRENT_BINARY_DIR}/udunits-install")

ExternalProject_Add(udunits
  DOWNLOAD_DIR ${CMAKE_CURRENT_BINARY_DIR}
  SOURCE_DIR ${udunits_source}
  BINARY_DIR ${udunits_build}
  INSTALL_DIR ${udunits_install}
  URL ${UDUNITS2_URL}/${UDUNITS2_GZ}
  URL_MD5 ${UDUNITS2_MD5}
  BUILD_IN_SOURCE 1
  PATCH_COMMAND ""
  CONFIGURE_COMMAND <SOURCE_DIR>/configure --prefix=<INSTALL_DIR>
)

set(udunits_DIR "${udunits_binary}" CACHE PATH "udunits binary directory" FORCE)
mark_as_advanced(udunits_DIR)
