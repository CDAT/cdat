
set(tcl_source "${CMAKE_CURRENT_BINARY_DIR}/build/tcl")
set(tk_source "${CMAKE_CURRENT_BINARY_DIR}/build/tk")
set(tcltk_install "${CMAKE_CURRENT_BINARY_DIR}/Externals")

# tcl
#
set(proj tcl-${TCLTK_MAJOR_SRC}.${TCLTK_MINOR_SRC})

ExternalProject_Add(${proj}
  DOWNLOAD_DIR ${CMAKE_CURRENT_BINARY_DIR}
  SOURCE_DIR ${tk_source}
  INSTALL_DIR ${tcltk_install}
  URL ${TCLTK_URL}/${TCL_GZ}
  URL_MD5 ${TCL_MD5}
  BUILD_IN_SOURCE 1
  CONFIGURE_COMMAND <SOURCE_DIR>/unix/configure --prefix=<INSTALL_DIR>
  BUILD_COMMAND make
)

# tk
#
set(proj tk-${TCLTK_MAJOR_SRC}.${TCLTK_MINOR_SRC})

ExternalProject_Add(${proj}
  DOWNLOAD_DIR ${CMAKE_CURRENT_BINARY_DIR}
  SOURCE_DIR ${tk_source}
  INSTALL_DIR ${tcltk_install}
  URL ${TCLTK_URL}/${TK_GZ}
  URL_MD5 ${TK_MD5}
  BUILD_IN_SOURCE 1
  CONFIGURE_COMMAND <SOURCE_DIR>/unix/configure --prefix=<INSTALL_DIR>  --with-tcl=<INSTALL_DIR>/lib
  BUILD_COMMAND make
  DEPENDS tcl-${TCLTK_MAJOR_SRC}.${TCLTK_MINOR_SRC}
)

ExternalProject_Add_Step(${proj} symlink
  COMMAND ${CMAKE_COMMAND} -E create_symlink "wish${TCLTK_MAJOR_SRC}.${TCLTK_MINOR_SRC}" wish
  WORKING_DIRECTORY ${tcltk_install}/bin
  COMMENT "Linking wish${TCLTK_MAJOR_SRC}.${TCLTK_MINOR_SRC} to wish"
  DEPENDEES install
)

# tcltk
#
set(proj tcltk)

ExternalProject_Add(${proj}
  DOWNLOAD_COMMAND ""
  CONFIGURE_COMMAND ""
  BUILD_COMMAND ""
  INSTALL_COMMAND ""
  DEPENDS tk-${TCLTK_MAJOR_SRC}.${TCLTK_MINOR_SRC}
)

