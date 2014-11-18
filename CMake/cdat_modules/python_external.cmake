#-----------------------------------------------------------------------------
set(proj Python)

set(python_SOURCE_DIR ${cdat_BINARY_DIR}/build/Python)
set(python_BUILD_IN_SOURCE 1)

set(python_aqua_cdat no)

configure_file(${cdat_CMAKE_SOURCE_DIR}/cdat_modules_extra/python_configure_step.cmake.in
  ${cdat_CMAKE_BINARY_DIR}/python_configure_step.cmake
  @ONLY)

configure_file(${cdat_CMAKE_SOURCE_DIR}/cdat_modules_extra/python_make_step.cmake.in
  ${cdat_CMAKE_BINARY_DIR}/python_make_step.cmake
  @ONLY)

configure_file(${cdat_CMAKE_SOURCE_DIR}/cdat_modules_extra/python_install_step.cmake.in
  ${cdat_CMAKE_BINARY_DIR}/python_install_step.cmake
  @ONLY)

set(python_PATCH_COMMAND ${CMAKE_COMMAND} -P ${cdat_CMAKE_BINARY_DIR}/python_patch_step.cmake)

if(APPLE)
  set(python_CONFIGURE_COMMAND ${CMAKE_COMMAND} -P ${cdat_CMAKE_BINARY_DIR}/python_configure_step.cmake)
  set(python_BUILD_COMMAND ${CMAKE_COMMAND} -P ${cdat_CMAKE_BINARY_DIR}/python_make_step.cmake)
  set(python_INSTALL_COMMAND ${CMAKE_COMMAND} -P ${cdat_CMAKE_BINARY_DIR}/python_install_step.cmake)
else()
  set(python_CONFIGURE_COMMAND ${CMAKE_COMMAND} -P ${cdat_CMAKE_BINARY_DIR}/python_configure_step.cmake)
  set(python_BUILD_COMMAND ${CMAKE_COMMAND} -P ${cdat_CMAKE_BINARY_DIR}/python_make_step.cmake)
  set(python_INSTALL_COMMAND ${CMAKE_COMMAND} -P ${cdat_CMAKE_BINARY_DIR}/python_install_step.cmake)
endif()

ExternalProject_Add(${proj}
  URL ${PYTHON_URL}/${PYTHON_GZ}
  URL_MD5 ${PYTHON_MD5}
  DOWNLOAD_DIR ${CDAT_PACKAGE_CACHE_DIR}
  SOURCE_DIR ${python_SOURCE_DIR}
  BUILD_IN_SOURCE ${python_BUILD_IN_SOURCE}
  UPDATE_COMMAND pwd
  CONFIGURE_COMMAND ${python_CONFIGURE_COMMAND}
  BUILD_COMMAND ${python_BUILD_COMMAND}
  INSTALL_COMMAND ${python_INSTALL_COMMAND}
  DEPENDS ${Python_deps}
  ${ep_log_options}
)

#-----------------------------------------------------------------------------
# Set PYTHON_INCLUDE and PYTHON_LIBRARY variables
#
set(PYTHON_SITE_PACKAGES ${CMAKE_INSTALL_PREFIX}/lib/python${PYVER}/site-packages)

if(APPLE)
  ExternalProject_Add_Step(${proj} change_plist_name
    COMMAND ${CMAKE_INSTALL_PREFIX}/Library/Frameworks/Python.framework/Versions/${PYVER}/bin/python ${cdat_CMAKE_SOURCE_DIR}/fixName.py
    DEPENDEES install
  )
  set(PYTHON_INCLUDE ${CMAKE_INSTALL_PREFIX}/Library/Frameworks/Python.framework/Versions/${PYVER}/Headers)
  set(PYTHON_LIBRARY ${CMAKE_INSTALL_PREFIX}/Library/Frameworks/Python.framework/Versions/${PYVER}/Python)
  set(PYTHON_LIBRARY_DIR ${CMAKE_INSTALL_PREFIX}/lib)
  set(PYTHON_EXECUTABLE ${CMAKE_INSTALL_PREFIX}/bin/python)
  #set(PYTHON_EXECUTABLE ${CMAKE_INSTALL_PREFIX}/Library/Frameworks/Python.framework/Versions/${PYTHON_MAJOR_SRC}.${PYTHON_MINOR_SRC}/bin/python)
  set(PYTHON_SITE_PACKAGES ${CMAKE_INSTALL_PREFIX}/Library/Frameworks/Python.framework/Versions/${PYVER}/lib/python${PYVER}/site-packages)
else()
  set(PYTHON_INCLUDE ${CMAKE_INSTALL_PREFIX}/include/python${PYVER})
  set(PYTHON_LIBRARY ${CMAKE_INSTALL_PREFIX}/lib/libpython${PYVER}.so)
endif()
