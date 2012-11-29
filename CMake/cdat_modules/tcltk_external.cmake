
set(tcl_source "${CMAKE_CURRENT_BINARY_DIR}/build/tcl")
set(tk_source "${CMAKE_CURRENT_BINARY_DIR}/build/tk")
set(tcltk_install "${cdat_EXTERNALS}")

set(tcltk_configure_args --enable-shared)

# tcl
#
set(proj tcl-${TCLTK_MAJOR}.${TCLTK_MINOR})

ExternalProject_Add(${proj}
  DOWNLOAD_DIR ${CDAT_PACKAGE_CACHE_DIR}
  SOURCE_DIR ${tcl_source}
  INSTALL_DIR ${tcltk_install}
  URL ${TCLTK_URL}/${TCL_GZ}
  URL_MD5 ${TCL_MD5}
  BUILD_IN_SOURCE 1
  CONFIGURE_COMMAND ${CMAKE_COMMAND} -DINSTALL_DIR=<INSTALL_DIR> -DWORKING_DIR=<SOURCE_DIR>/unix -DCONFIGURE_ARGS=${tcltk_configure_args} -P ${cdat_CMAKE_BINARY_DIR}/cdat_configure_step.cmake
  BUILD_COMMAND ${CMAKE_COMMAND} -Dmake=$(MAKE) -DWORKING_DIR=<SOURCE_DIR>/unix -P ${cdat_CMAKE_BINARY_DIR}/cdat_make_step.cmake
  INSTALL_COMMAND ${CMAKE_COMMAND} -DWORKING_DIR=<SOURCE_DIR>/unix -P ${cdat_CMAKE_BINARY_DIR}/cdat_install_step.cmake
  DEPENDS ${TclTk_deps}
  ${ep_log_options}
)

# tk
#
set(proj tk-${TCLTK_MAJOR}.${TCLTK_MINOR})

ExternalProject_Add(${proj}
  DOWNLOAD_DIR ${CDAT_PACKAGE_CACHE_DIR}
  SOURCE_DIR ${tk_source}
  INSTALL_DIR ${tcltk_install}
  URL ${TCLTK_URL}/${TK_GZ}
  URL_MD5 ${TK_MD5}
  BUILD_IN_SOURCE 1
  CONFIGURE_COMMAND ${CMAKE_COMMAND} -DINSTALL_DIR=<INSTALL_DIR> -DWORKING_DIR=<SOURCE_DIR>/unix -P ${cdat_CMAKE_BINARY_DIR}/cdat_configure_step.cmake
  BUILD_COMMAND ${CMAKE_COMMAND} -Dmake=$(MAKE) -DWORKING_DIR=<SOURCE_DIR>/unix -P ${cdat_CMAKE_BINARY_DIR}/cdat_make_step.cmake
  INSTALL_COMMAND ${CMAKE_COMMAND} -DWORKING_DIR=<SOURCE_DIR>/unix -P ${cdat_CMAKE_BINARY_DIR}/cdat_install_step.cmake
  DEPENDS tcl-${TCLTK_MAJOR}.${TCLTK_MINOR}
  ${ep_log_options}
)

ExternalProject_Add_Step(${proj} symlink
  COMMAND ${CMAKE_COMMAND} -E create_symlink "wish${TCLTK_MAJOR}.${TCLTK_MINOR}" wish
  WORKING_DIRECTORY ${tcltk_install}/bin
  COMMENT "Linking wish${TCLTK_MAJOR}.${TCLTK_MINOR} to wish"
  DEPENDEES install
)

# tcltk
#

ExternalProject_Add(TclTk
  DOWNLOAD_COMMAND ""
  CONFIGURE_COMMAND ""
  BUILD_COMMAND ""
  INSTALL_COMMAND ""
  DEPENDS tk-${TCLTK_MAJOR}.${TCLTK_MINOR}
  ${ep_log_options}
)

