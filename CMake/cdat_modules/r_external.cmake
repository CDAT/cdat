
set(R_source "${CMAKE_CURRENT_BINARY_DIR}/build/R")
set(R_install "${cdat_EXTERNALS}")
if (APPLE)
    message("ON APPLE WILL NOT BUILD X11")
    set(WITHX "no")
else ()
    set(WITHX "yes")
endif()

ExternalProject_Add(R
  DOWNLOAD_DIR ${CDAT_PACKAGE_CACHE_DIR}
  SOURCE_DIR ${R_source}
  INSTALL_DIR ${R_install}
  URL ${R_URL}/${R_GZ}
  URL_MD5 ${R_MD5}
  BUILD_IN_SOURCE 1
  PATCH_COMMAND ""
  CONFIGURE_COMMAND <SOURCE_DIR>/configure --prefix=<INSTALL_DIR> LIBnn=lib --without-jpeglib --disable-R-framework --enable-R-shlib --disable-openmp --without-cairo --without-ICU --without-libpng --without-system-xz --without-aqua --without-tcltk --without-readline --with-x=${WITHX}
  INSTALL_COMMAND ${CMAKE_MAKE_PROGRAM} -j1 install
  ${ep_log_options}
)

set(R_DIR "${R_binary}" CACHE PATH "R binary directory" FORCE)
mark_as_advanced(R_DIR)
