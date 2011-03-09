
set(proj pkgconfig)

ExternalProject_Add(${proj}
  URL ${PKG_URL}/${PKG_GZ}
  URL_MD5 ${PKG_MD5}
  SOURCE_DIR ${proj}
  BINARY_DIR ${proj}-build
  INSTALL_DIR ${proj}-install
  CONFIGURE_COMMAND <SOURCE_DIR>/configure --prefix=<INSTALL_DIR>
  BUILD_COMMAND make
  INSTALL_COMMAND make install
)

