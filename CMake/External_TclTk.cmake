
# tcl
#
set(proj tcl-${TCLTK_MAJOR_SRC}.${TCLTK_MINOR_SRC})

ExternalProject_Add(${proj}
  URL ${TCLTK_URL}/${TCL_GZ}
  URL_MD5 ${TCL_MD5}
  INSTALL_DIR tcltk-install
  CONFIGURE_COMMAND <SOURCE_DIR>/unix/configure --prefix=<INSTALL_DIR>
  BUILD_COMMAND make
)

# tk
#
set(proj tk-${TCLTK_MAJOR_SRC}.${TCLTK_MINOR_SRC})

ExternalProject_Add(${proj}
  URL ${TCLTK_URL}/${TK_GZ}
  URL_MD5 ${TK_MD5}
  INSTALL_DIR tcltk-install
  CONFIGURE_COMMAND <SOURCE_DIR>/unix/configure --prefix=<INSTALL_DIR>  --with-tcl=<INSTALL_DIR>/lib
  BUILD_COMMAND make
  DEPENDS tcl-${TCLTK_MAJOR_SRC}.${TCLTK_MINOR_SRC}
)

ExternalProject_Add_Step(${proj} symlink
  COMMAND ${CMAKE_COMMAND} -E create_symlink "wish${TCLTK_MAJOR_SRC}.${TCLTK_MINOR_SRC}" wish
  WORKING_DIRECTORY tcltk-install/bin
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

# other projects need to know where tcltk gets installed:
#
#set(CSE_TCLTK_HOME ${prefix} PARENT_SCOPE)
#set(CSE_TCLTK_PACKAGE "${name}-${version}" PARENT_SCOPE)
#set(CSE_TCLTK_NAME "${name}" PARENT_SCOPE)
#set(CSE_TCLTK_VERSIONXX "${version_major}.${version_minor}" PARENT_SCOPE)

#set(extras
#  "setenv TCLTK_INCLUDE_DIR  $prefix/include"
#  "setenv TCLTK_LIB_DIR $prefix/lib"
#  "setenv TCL_LIBRARY $prefix/lib/tcl8.4"
#)
