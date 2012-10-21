
set(CDAT_source "${cdat_SOURCE_DIR}")

set(RUNTIME_FLAGS ${cdat_EXTERNALS}/lib)
set(LDFLAGS -L${cdat_EXTERNALS}/lib)

if(QT_QMAKE_EXECUTABLE)
  get_filename_component(QT_BINARY_DIR ${QT_QMAKE_EXECUTABLE} PATH)
  get_filename_component(QT_ROOT ${QT_BINARY_DIR} PATH)
  get_filename_component(QT_INC_DIR ${QT_QTCORE_INCLUDE_DIR} PATH)
  get_filename_component(QT_LIB_DIR ${QT_QTCORE_LIBRARY} PATH)
endif()

# Mac requires special argument
if(APPLE)
  set(qt_flags "--enable-qt-framework")
endif()

set(qt_flags "${qt_flags} --with-qt=${QT_ROOT} --with-qt-lib=${QT_LIB_DIR} --with-qt-inc=${QT_INC_DIR}" --with-qt-bin=${QT_BINARY_DIR})

if (CDAT_BUILD_WITH_LIBDRS)
 set(qt_flags "${qt_flags} -c pcmdi.py")
endif()

if(CDAT_USE_SYSTEM_QT AND QT_QTCORE_INCLUDE_DIR)
  if(NOT APPLE)
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
  INSTALL_COMMAND ${CMAKE_COMMAND} -DPYTHON_INSTALL_ARGS=${qt_flags} -P ${cdat_CMAKE_BINARY_DIR}/cdat_python_install_step.cmake
  DEPENDS ${CDAT_deps}
)

