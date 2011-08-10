

set(ParaView_source "${CMAKE_CURRENT_BINARY_DIR}/build/ParaView")
set(ParaView_binary "${CMAKE_CURRENT_BINARY_DIR}/build/ParaView-build")
set(ParaView_install "${cdat_EXTERNALS}")

if(QT_QMAKE_EXECUTABLE)
  get_filename_component(QT_BINARY_DIR ${QT_QMAKE_EXECUTABLE} PATH)
  get_filename_component(QT_ROOT ${QT_BINARY_DIR} PATH)
endif()

set(ParaView_install_command "")

if(NOT APPLE)
  set(ParaView_install_command make install)
  set(ParaView_tpl_args 
    # hdf5
    -DVTK_USE_SYSTEM_HDF5:BOOL=ON
    -DHDF5_INCLUDE_DIR:PATH=${cdat_EXTERNALS}/include
    -DHDF5_LIBRARY:FILEPATH=${cdat_EXTERNALS}/lib/libhdf5${_LINK_LIBRARY_SUFFIX}
    # zlib
    -DVTK_USE_SYSTEM_ZLIB:BOOL=ON
    -DZLIB_INCLUDE_DIR:PATH=${cdat_EXTERNALS}/include
    -DZLIB_LIBRARY:FILEPATH=${cdat_EXTERNALS}/lib/libz${_LINK_LIBRARY_SUFFIX}
    # libxml2
    -DVTK_USE_SYSTEM_LIBXML2:BOOL=ON
    -DLIBXML2_INCLUDE_DIR:PATH=${cdat_EXTERNALS}/include/libxml2
    -DLIBXML2_LIBRARIES:FILEPATH=${cdat_EXTERNALS}/lib/libxml2${_LINK_LIBRARY_SUFFIX}
    -DLIBXML2_XMLLINT_EXECUTABLE:FILEPATH=${cdat_EXTERNALS}/bin/xmllint
   )
endif()

set(pv_rpath_linker_flags "${cdat_rpath_flag}${CMAKE_INSTALL_PREFIX}/lib ${cdat_rpath_flag}${cdat_EXTERNALS}/lib ${cdat_rpath_flag}${ParaView_install}/lib/paraview-${PARAVIEW_MAJOR}.${PARAVIEW_MINOR}")

ExternalProject_Add(ParaView
  DOWNLOAD_DIR ${CMAKE_CURRENT_BINARY_DIR}
  SOURCE_DIR ${ParaView_source}
  BINARY_DIR ${ParaView_binary}
  INSTALL_DIR ${ParaView_install}
  URL ${PARAVIEW_URL}/${PARAVIEW_GZ}
  URL_MD5 ${PARAVIEW_MD5}
  PATCH_COMMAND ""
  CMAKE_CACHE_ARGS
    -DBUILD_SHARED_LIBS:BOOL=ON
    -DBUILD_TESTING:BOOL=OFF
    -DPARAVIEW_DISABLE_VTK_TESTING:BOOL=ON
    -DPARAVIEW_TESTING_WITH_PYTHON:BOOL=OFF
    -DPARAVIEW_BUILD_AS_APPLICATION_BUNDLE:BOOL=OFF
    -DCMAKE_CXX_FLAGS:STRING=${cdat_tpl_cxx_flags}
    -DCMAKE_C_FLAGS:STRING=${cdat_tpl_c_flags}
    -DCMAKE_BUILD_TYPE:STRING=${CMAKE_CFG_INTDIR}
    -DPARAVIEW_INSTALL_THIRD_PARTY_LIBRARIES:BOOL=OFF
    ${cdat_compiler_args}
    ${ParaView_tpl_args}
    # Qt
    -DQT_QMAKE_EXECUTABLE:FILEPATH=${QT_QMAKE_EXECUTABLE}
    -DQT_QTUITOOLS_INCLUDE_DIR:PATH=${QT_ROOT}/include/QtUiTools
    # Python
    -DPARAVIEW_ENABLE_PYTHON:BOOL=ON
    -DPYTHON_EXECUTABLE:FILEPATH=${PYTHON_EXECUTABLE}
    -DPYTHON_INCLUDE_DIR:PATH=${PYTHON_INCLUDE}
    -DPYTHON_LIBRARY:FILEPATH=${PYTHON_LIBRARY}
    -DCMAKE_EXE_LINKER_FLAGS:STRING=${pv_rpath_linker_flags}
    -DCMAKE_MODULE_LINKER_FLAGS:STRING=${pv_rpath_linker_flags}
    -DCMAKE_SHARED_LINKER_FLAGS:STRING=${pv_rpath_linker_flags}
  CMAKE_ARGS
    -DCMAKE_INSTALL_PREFIX:PATH=<INSTALL_DIR>
  BUILD_COMMAND ${CMAKE_COMMAND} -DWORKING_DIR=<BINARY_DIR> -Dmake=$(MAKE) -P ${cdat_CMAKE_BINARY_DIR}/cdat_cmake_make_step.cmake
  INSTALL_COMMAND ${ParaView_install_command}
  DEPENDS ${ParaView_DEPENDENCIES}
  ${EP_LOG_OPTIONS}
)

# Install ParaView and VTK python modules via their setup.py files.

configure_file(${cdat_CMAKE_SOURCE_DIR}/vtk_install_python_module.cmake.in
  ${cdat_CMAKE_BINARY_DIR}/vtk_install_python_module.cmake
  @ONLY)

configure_file(${cdat_CMAKE_SOURCE_DIR}/paraview_install_python_module.cmake.in
  ${cdat_CMAKE_BINARY_DIR}/paraview_install_python_module.cmake
  @ONLY)

 ExternalProject_Add_Step(ParaView InstallParaViewPythonModule
    COMMAND ${CMAKE_COMMAND} -P ${cdat_CMAKE_BINARY_DIR}/paraview_install_python_module.cmake
    DEPENDEES install
    WORKING_DIRECTORY ${cdat_CMAKE_BINARY_DIR}
    )

ExternalProject_Add_Step(ParaView InstallVTKPythonModule
    COMMAND ${CMAKE_COMMAND} -P ${cdat_CMAKE_BINARY_DIR}/vtk_install_python_module.cmake
    DEPENDEES install
    WORKING_DIRECTORY ${cdat_CMAKE_BINARY_DIR}
    )


