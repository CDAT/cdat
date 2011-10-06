
set(CDAT_source "${cdat_SOURCE_DIR}")

set(RUNTIME_FLAGS ${cdat_EXTERNALS}/lib)
set(LDFLAGS -L${cdat_EXTERNALS}/lib)

if(QT_QMAKE_EXECUTABLE)
  get_filename_component(QT_BINARY_DIR ${QT_QMAKE_EXECUTABLE} PATH)
  get_filename_component(QT_ROOT ${QT_BINARY_DIR} PATH)
endif()

#env EXTERNALS=/Users/partyd/Kitware/uv-cdat/make-file-install//Externals  LDFLAGS="${LDFLAGS/"/} -undefined dynamic_lookup"  /Users/partyd/Kitware/uv-cdat/make-file-install//bin/python install.py  --enable-qt-framework  --with-qt=/Users/partyd/Kitware/uv-cdat/make-file-install//Externals

# -with-qt-lib=/Users/partyd/Dashboards/Support/qt-4.6.3-10.5/install/lib --with-qt-inc=/Users/partyd/Dashboards/Support/qt-4.6.3-10.5/install/include --enable-qt-framework  --with-qt-bin=/Users/partyd/Dashboards/Support/qt-4.6.3-10.5/install/bin/

if(APPLE)
  set(qt_flags "--enable-qt-framework --with-qt=${QT_ROOT} --with-qt-lib=${QT_ROOT}/lib --with-qt-inc=${QT_ROOT}/include --with-qt-bin=${QT_ROOT}/bin")
else()
  set(qt_flags "--with-qt=${QT_ROOT} --with-qt-lib=${QT_ROOT}/lib --with-qt-inc=${QT_ROOT}/include --with-qt-bin=${QT_ROOT}/bin")
endif()

if(CDAT_USE_SYSTEM_QT AND QT_QTCORE_INCLUDE_DIR)
  if(APPLE)
    #get_filename_component(QT_FRAMEWORK_ROOT ${QT_QTCORE_LIBRARY} PATH)
    #set(ADDITIONAL_CFLAGS "-F${QT_FRAMEWORK_ROOT} -I${QT_QTCORE_INCLUDE_DIR} -I${QT_QTGUI_INCLUDE_DIR}")
    #set(ADDITIONAL_CXXFLAGS "-F${QT_FRAMEWORK_ROOT} -I${QT_QTCORE_INCLUDE_DIR} -I${QT_QTGUI_INCLUDE_DIR}")
    #set(ADDITIONAL_CPPFLAGS "-F${QT_FRAMEWORK_ROOT} -I${QT_QTCORE_INCLUDE_DIR} -I${QT_QTGUI_INCLUDE_DIR}")
  else()
    get_filename_component(QT_INCLUDE_ROOT ${QT_QTCORE_INCLUDE_DIR} PATH)
    set(ADDITIONAL_CFLAGS "-I${QT_INCLUDE_ROOT}") 
    set(ADDITIONAL_CPPFLAGS "-I${QT_INCLUDE_ROOT}")
    set(ADDITIONAL_CXXFLAGS "-I${QT_INCLUDE_ROOT}")
  endif()
endif()

set(WORKING_DIR ${cdat_SOURCE_DIR})

configure_file(${cdat_CMAKE_SOURCE_DIR}/cdat_python_install_step.cmake.in
  ${cdat_CMAKE_BINARY_DIR}/cdat_python_install_step.cmake
  @ONLY)

ExternalProject_Add(CDAT
  DOWNLOAD_DIR ""
  SOURCE_DIR ${cdat_SOURCE_DIR}
  BUILD_IN_SOURCE 1
  PATCH_COMMAND ""
  CONFIGURE_COMMAND ""
  BUILD_COMMAND ""
  #INSTALL_COMMAND env EXTERNALS=${cdat_EXTERNALS} LDFLAGS=${LDFLAGS} ${LIBRARY_PATH}=${RUNTIME_FLAGS} ${PYTHON_EXECUTABLE} install.py ${qt_flags}
  INSTALL_COMMAND ${CMAKE_COMMAND} -DPYTHON_INSTALL_ARGS=${qt_flags} -P ${cdat_CMAKE_BINARY_DIR}/cdat_python_install_step.cmake
  DEPENDS ${CDAT_DEPENDENCIES}
)

