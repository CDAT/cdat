set(pvfiledialog_source "${CMAKE_CURRENT_BINARY_DIR}/pvdialog")
set(pvfiledialog_binary "${CMAKE_CURRENT_BINARY_DIR}/pvdialog-build")
set(pvfiledialog_install "${cdat_EXTERNALS}")

set(PVFileDialog_install_command "")

ExternalProject_Add(pvfiledialog
  DOWNLOAD_DIR ${CDAT_PACKAGE_CACHE_DIR}
  SOURCE_DIR ${pvdialog_source}
  BINARY_DIR ${pvdialog_build}
  INSTALL_DIR ${pvfiledialog_install}
  GIT_REPOSITORY ${GIT_PROTOCOL}github.com/aashish24/pvfiledialog.git
  GIT_TAG master
  PATCH_COMMAND ""
  CMAKE_CACHE_ARGS
    -DParaView_DIR:PATH=${ParaView_binary}
    -DQT_QMAKE_EXECUTABLE:PATH=${QT_QMAKE_EXECUTABLE}
    -DVTK_DIR:PATH=${ParaView_binary}/VTK
    -DPYTHON_EXECUTABLE:PATH=${PYTHON_EXECUTABLE}
    -DPYTHON_INCLUDE_DIR:PATH=${PYTHON_INCLUDE}
    -DPYTHON_LIBRARIES:PATH=${PYTHON_LIBRARY}
    -DCMAKE_CXX_FLAGS:STRING=${cdat_tpl_cxx_flags}
    -DCMAKE_C_FLAGS:STRING=${cdat_tpl_c_flags}
    -DCMAKE_BUILD_TYPE:STRING=${CMAKE_CFG_INTDIR}
    ${cdat_compiler_args}
  CMAKE_ARGS
    -DCMAKE_INSTALL_PREFIX:PATH=<INSTALL_DIR>
  INSTALL_COMMAND ${PVFileDialog_install_command}
  DEPENDS ${PVFileDialog_deps}
  ${ep_log_options}
)
