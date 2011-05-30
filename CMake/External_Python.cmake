#-----------------------------------------------------------------------------
set(proj Python)

  set(python_SOURCE_DIR ${cdat_BINARY_DIR}/build/Python)
  set(python_BUILD_IN_SOURCE 1)

  set(python_aqua_cdat no)

  configure_file(${cdat_CMAKE_SOURCE_DIR}/python_patch_step.cmake.in
    ${CMAKE_CURRENT_BINARY_DIR}/python_patch_step.cmake
    @ONLY)
    
  configure_file(${cdat_CMAKE_SOURCE_DIR}/python_configure_step.cmake.in
    ${CMAKE_CURRENT_BINARY_DIR}/python_configure_step.cmake
    @ONLY)
  
  configure_file(${cdat_CMAKE_SOURCE_DIR}/python_make_step.cmake.in
    ${CMAKE_CURRENT_BINARY_DIR}/python_make_step.cmake
    @ONLY)
    
  configure_file(${cdat_CMAKE_SOURCE_DIR}/python_install_step.cmake.in
    ${CMAKE_CURRENT_BINARY_DIR}/python_install_step.cmake
    @ONLY)

  set(python_PATCH_COMMAND ${CMAKE_COMMAND} -P ${CMAKE_CURRENT_BINARY_DIR}/python_patch_step.cmake)
  set(python_CONFIGURE_COMMAND ${CMAKE_COMMAND} -P ${CMAKE_CURRENT_BINARY_DIR}/python_configure_step.cmake)
  set(python_BUILD_COMMAND ${CMAKE_COMMAND} -P ${CMAKE_CURRENT_BINARY_DIR}/python_make_step.cmake)
  set(python_INSTALL_COMMAND ${CMAKE_COMMAND} -P ${CMAKE_CURRENT_BINARY_DIR}/python_install_step.cmake)

if(APPLE)
  set(library_param --enable-framework=${cdat_EXTERNALS}/Library/Frameworks)
elseif(UNIX)
  set(library_param --enable-shared)
endif()
  
  ExternalProject_Add(${proj}
    URL ${PYTHON_URL}/${PYTHON_GZ}
    URL_MD5 ${PYTHON_MD5}
    DOWNLOAD_DIR ${CMAKE_CURRENT_BINARY_DIR}
    SOURCE_DIR ${python_SOURCE_DIR}
    BUILD_IN_SOURCE ${python_BUILD_IN_SOURCE}
    UPDATE_COMMAND pwd
    #PATCH_COMMAND ${CMAKE_COMMAND} -E copy_if_different ${cdat_SOURCE_DIR}/pysrc/src/setup-${PYTHON_VERSION}.py ${python_SOURCE_DIR}/setup.py
    #CONFIGURE_COMMAND ${python_CONFIGURE_COMMAND}
    PATCH_COMMAND export
    CONFIGURE_COMMAND unset MAKEFLAGS && env EXTERNALS=${cdat_EXTERNALS} <SOURCE_DIR>/configure ${library_param} && make && make install
    #BUILD_COMMAND make
    BUILD_COMMAND ""
    INSTALL_COMMAND ""
    #BUILD_COMMAND ${python_BUILD_COMMAND}
    #INSTALL_COMMAND ${python_INSTALL_COMMAND}
    DEPENDS ${Python_DEPENDENCIES}
    )

# ExternalProject_Add_Step(${proj} PythonMakeAgain
#    COMMAND make
#    COMMAND make install
#    DEPENDEES install
#    WORKING_DIRECTORY ${python_SOURCE_DIR}
#    )

#-----------------------------------------------------------------------------
# Set PYTHON_INCLUDE and PYTHON_LIBRARY variables
#

set(PYTHON_INCLUDE)
set(PYTHON_LIBRARY)
set(PYTHON_EXECUTABLE)
set(PYTHON_SITE_PACKAGES ${CMAKE_BINARY_DIR}/python-build/lib/python${PYTHON_MAJOR_SRC}.${PYTHON_MINOR_SRC}/site-packages)

if(APPLE)
  set(PYTHON_INCLUDE ${cdat_EXTERNALS}/Library/Frameworks/Python.framework/Versions/${PYTHON_MAJOR_SRC}.${PYTHON_MINOR_SRC}/Headers)
  set(PYTHON_LIBRARY ${cdat_EXTERNALS}/Library/Frameworks/Python.framework/Versions/${PYTHON_MAJOR_SRC}.${PYTHON_MINOR_SRC}/Python)
  set(PYTHON_LIBRARY_DIR ${cdat_EXTERNALS}/lib)
  #set(PYTHON_EXECUTABLE ${cdat_EXTERNALS}/bin/python)
  set(PYTHON_EXECUTABLE ${cdat_EXTERNALS}/Library/Frameworks/Python.framework/Versions/${PYTHON_MAJOR_SRC}.${PYTHON_MINOR_SRC}/bin/python)
else()
  set(PYTHON_INCLUDE ${CMAKE_BINARY_DIR}/Externals/include/python${PYTHON_MAJOR_SRC}.${PYTHON_MINOR_SRC})
  set(PYTHON_LIBRARY ${CMAKE_BINARY_DIR}/Externals/lib/libpython${PYTHON_MAJOR_SRC}.${PYTHON_MINOR_SRC}.so)
  set(PYTHON_LIBRARY_DIR ${CMAKE_BINARY_DIR}/Externals/lib)
  set(PYTHON_EXECUTABLE ${CMAKE_BINARY_DIR}/Externals/bin/python)
endif()



