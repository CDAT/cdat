
set(gui_support_source ${cdat_SOURCE_DIR}/Packages/gui_support)
set(runtime_library_path ${CMAKE_INSTALL_PREFIX}/lib:${cdat_EXTERNALS}/lib)

ExternalProject_Add(gui_support
  DOWNLOAD_DIR ""
  SOURCE_DIR ${gui_support_source}
  BUILD_IN_SOURCE 1
  PATCH_COMMAND ""
  CONFIGURE_COMMAND ""
  BUILD_COMMAND ""
  INSTALL_COMMAND env EXTERNALS=${cdat_EXTERNALS} ${LIBRARY_PATH}=${runtime_library_path} ${PYTHON_EXECUTABLE} setup.py build --force install
  DEPENDS ${gui_support_DEPENDENCIES}
  ${EP_LOG_OPTIONS}
)

