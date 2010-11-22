
set(proj pkgconfig)

ExternalProject_Add(${proj}
  URL ${PKG_URL}/${PKG_GZ}
  URL_MD5 ${PKG_MD5}
  UPDATE_COMMAND ""
  SOURCE_DIR ${proj}
  BINARY_DIR ${proj}-build
  CONFIGURE_COMMAND <SOURCE_DIR>/configure
  BUILD_COMMAND make
  INSTALL_COMMAND ""
)

