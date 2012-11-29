
set(freetype_source "${CMAKE_CURRENT_BINARY_DIR}/build/freetype")
set(freetype_install "${cdat_EXTERNALS}")

ExternalProject_Add(freetype
  DOWNLOAD_DIR ${CDAT_PACKAGE_CACHE_DIR}
  SOURCE_DIR ${freetype_source}
  INSTALL_DIR ${freetype_install}
  URL ${FT_URL}/${FT_GZ}
  URL_MD5 ${FT_MD5}
  BUILD_IN_SOURCE 1
  PATCH_COMMAND ""
  CONFIGURE_COMMAND ${CMAKE_COMMAND} -DINSTALL_DIR=<INSTALL_DIR> -DWORKING_DIR=<SOURCE_DIR> -P ${cdat_CMAKE_BINARY_DIR}/cdat_configure_step.cmake
  DEPENDS ${freetype_deps}
  ${ep_log_options}
)

#ln -sf @EXTERNALS@/include/freetype2/freetype @EXTERNALS@/include/freetype

ExternalProject_Add_Step(freetype symlink
  COMMAND ${CMAKE_COMMAND} -E create_symlink @cdat_EXTERNALS@/include/freetype2/freetype @cdat_EXTERNALS@/include/freetype
  COMMENT "Symlink include/freetype2/freetype include directory as include/freetype"
  DEPENDEES install
)
