
set(gui_support_source ${cdat_SOURCE_DIR}/Packages/gui_support)
set(runtime_library_path ${CMAKE_INSTALL_PREFIX}/lib:${cdat_EXTERNALS}/lib:${cdat_EXTERNALS}/lib64)



#  BUILD_COMMAND env EXTERNALS=${cdat_EXTERNALS} ${LIBRARY_PATH}=${runtime_library_path} ${PYTHON_EXECUTABLE} setup.py build
#  INSTALL_COMMAND env EXTERNALS=${cdat_EXTERNALS} ${LIBRARY_PATH}=${runtime_library_path} ${PYTHON_EXECUTABLE} setup.py install --prefix=${CMAKE_INSTALL_PREFIX}
ExternalProject_Add(gui_support
  DOWNLOAD_DIR ""
  SOURCE_DIR ${gui_support_source}
  BUILD_IN_SOURCE 1
  BUILD_COMMAND env PYTHONPATH=$ENV{PYTHONPATH} LD_LIBRARY_PATH=$ENV{LD_LIBRARY_PATH} EXTERNALS=${cdat_EXTERNALS}  ${PYTHON_EXECUTABLE} setup.py build
  INSTALL_COMMAND env PYTHONPATH=$ENV{PYTHONPATH} LD_LIBRARY_PATH=$ENV{LD_LIBRARY_PATH} EXTERNALS=${cdat_EXTERNALS} ${PYTHON_EXECUTABLE} setup.py install ${PYTHON_EXTRA_PREFIX}
  PATCH_COMMAND ""
  CONFIGURE_COMMAND ""
  DEPENDS ${gui_support_DEPENDENCIES}
  ${EP_LOG_OPTIONS}
)

