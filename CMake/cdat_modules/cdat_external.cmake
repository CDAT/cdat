set(CDAT_source "${cdat_SOURCE_DIR}")

set(RUNTIME_FLAGS ${cdat_EXTERNALS}/lib)
set(LDFLAGS -L${cdat_EXTERNALS}/lib)

if (CDAT_BUILD_VCS_LEGACY)
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

  set(cdat_xtra_flags "--enable-vcs-legacy ${qt_flags} --with-qt=${QT_ROOT} --with-qt-lib=${QT_LIB_DIR} --with-qt-inc=${QT_INC_DIR}" --with-qt-bin=${QT_BINARY_DIR})
endif()

if (CDAT_BUILD_WITH_LIBDRS)
 set(cdat_xtra_flags "${cdat_xtra_flags} -c pcmdi.py")
endif()

set(cdat_build_dir ${CMAKE_CURRENT_BINARY_DIR}/cdat-build)

configure_file(${cdat_CMAKE_SOURCE_DIR}/cdat_modules_extra/cdat_python_install_step.cmake.in
  ${cdat_CMAKE_BINARY_DIR}/cdat_python_install_step.cmake
  @ONLY)

ExternalProject_Add(CDAT
  DOWNLOAD_DIR ""
  SOURCE_DIR ${cdat_SOURCE_DIR}
  BINARY_DIR ${cdat_build_dir}
  BUILD_IN_SOURCE 0
  PATCH_COMMAND ""
  CONFIGURE_COMMAND ""
  BUILD_COMMAND ""
  INSTALL_COMMAND ${CMAKE_COMMAND} -DPYTHON_INSTALL_ARGS=${cdat_xtra_flags} -P ${cdat_CMAKE_BINARY_DIR}/cdat_python_install_step.cmake
  DEPENDS ${CDAT_deps}
)

#configure_file(${cdat_CMAKE_SOURCE_DIR}/cdat_modules_extra/cdat_installed_pingdb_step.cmake.in
#  ${cdat_CMAKE_BINARY_DIR}/cdat_installed_pingdb_step.cmake
#  @ONLY)

#ExternalProject_Add_Step(CDAT PINGDB
#  COMMAND ${CMAKE_COMMAND} -P ${cdat_CMAKE_BINARY_DIR}/cdat_installed_pingdb_step.cmake
#  DEPENDEES install
#  WORKING_DIRECTORY ${cdat_CMAKE_BINARY_DIR}
#  )
