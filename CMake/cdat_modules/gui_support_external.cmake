set(gui_support_source_dir "${cdat_SOURCE_DIR}/Packages/gui_support")
set(gui_support_binary_dir "${CMAKE_CURRENT_BINARY_DIR}/build/gui_support-build")
set(runtime_library_path ${CMAKE_INSTALL_PREFIX}/lib:${cdat_EXTERNALS}/lib)

#  BUILD_COMMAND env EXTERNALS=${cdat_EXTERNALS} ${LIBRARY_PATH}=${runtime_library_path} ${PYTHON_EXECUTABLE} setup.py build
#  INSTALL_COMMAND env EXTERNALS=${cdat_EXTERNALS} ${LIBRARY_PATH}=${runtime_library_path} ${PYTHON_EXECUTABLE} setup.py install --prefix=${CMAKE_INSTALL_PREFIX}
ExternalProject_Add(gui_support
  DOWNLOAD_DIR ""
  SOURCE_DIR ${gui_support_source_dir}
  BINARY_DIR ${gui_support_binary_dir}
  BUILD_IN_SOURCE 0
  BUILD_COMMAND ""
#  BUILD_COMMAND env PYTHONPATH=$ENV{PYTHONPATH} LD_LIBRARY_PATH=$ENV{LD_LIBRARY_PATH} EXTERNALS=${cdat_EXTERNALS}  ${PYTHON_EXECUTABLE} ${gui_support_source_dir}/setup.py build -b ${gui_support_binary_dir}
  INSTALL_COMMAND env PYTHONPATH=$ENV{PYTHONPATH} LD_LIBRARY_PATH=$ENV{LD_LIBRARY_PATH} EXTERNALS=${cdat_EXTERNALS} ${PYTHON_EXECUTABLE} ${gui_support_source_dir}/setup.py build -b ${gui_support_binary_dir} install ${PYTHON_EXTRA_PREFIX}
  PATCH_COMMAND ""
  CONFIGURE_COMMAND ""
  DEPENDS ${gui_support_deps}
  ${ep_log_options}
)

