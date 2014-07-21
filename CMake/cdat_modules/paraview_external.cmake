set(ParaView_source "${CMAKE_CURRENT_BINARY_DIR}/build/ParaView")
set(ParaView_binary "${CMAKE_CURRENT_BINARY_DIR}/build/ParaView-build")
set(ParaView_install "${cdat_EXTERNALS}")

if(QT_QMAKE_EXECUTABLE)
  get_filename_component(QT_BINARY_DIR ${QT_QMAKE_EXECUTABLE} PATH)
  get_filename_component(QT_ROOT ${QT_BINARY_DIR} PATH)
endif()

if(APPLE)
  set(MACOSX_APP_INSTALL_PREFIX "${SB_EXTERNALS_DIR}") 
endif()

# Initialize
set(ParaView_tpl_args)

# VCS needs projections from GeoVis
list(APPEND ParaView_tpl_args
  -DModule_vtkViewsGeovis:BOOL=ON
)
list(APPEND ParaView_tpl_args
  -DModule_vtklibproj4:BOOL=ON
)

# We would like to see CGM as well
list(APPEND ParaView_tpl_args
  -DModule_vtkIOCGM:BOOL=ON
  )


if (CDAT_BUILD_PARALLEL)
  list(APPEND ParaView_tpl_args
    -DPARAVIEW_USE_MPI:BOOL=ON)
  # Mac has issues with MPI4PY of ParaView. Also I don't know if we really need to build it
  # See this bug: paraview.org/bug/view.php?id=13587
  list(APPEND ParaView_tpl_args -DENABLE_MPI4PY:BOOL=OFF)

  if(CDAT_BUILD_MPI)
    if(UNIX)
      set(ENV{LD_LIBRARY_PATH} "${cdat_EXTERNALS}/lib:$ENV{LD_LIBRARY_PATH}")
    elseif(APPLE)
      set(ENV{DYLD_FALLBACK_LIBRARY_PATH} "${cdat_EXTERNALS}/lib:$ENV{DYLD_FALLBACK_LIBRARY_PATH}")
    endif()
    list(APPEND ParaView_tpl_args
      -DMPIEXEC:FILEPATH=${cdat_EXTERNALS}/bin/mpiexec
      -DMPI_CXX_COMPILER:FILEPATH=${cdat_EXTERNALS}/bin/mpicxx
      -DMPI_C_COMPILER:FILEPATH=${cdat_EXTERNALS}/bin/mpicc
      -DMPI_C_INCLUDE_PATH:PATH=${cdat_EXTERNALS}/include
      -DMPI_CXX_INCLUDE_PATH:PATH=${cdat_EXTERNALS}/include
      -DMACOSX_APP_INSTALL_PREFIX:PATH=${MACOSX_APP_INSTALL_PREFIX}
      -DVTK_MPIRUN_EXE:FILEPATH=${cdat_EXTERNALS}/bin/mpiexec)
  endif()
endif()

# Either we use cdat zlib and libxml or system zlib and libxml
list(APPEND ParaView_tpl_args
  -DVTK_USE_SYSTEM_ZLIB:BOOL=ON
  -DVTK_USE_SYSTEM_LIBXML2:BOOL=ON
  -DVTK_USE_SYSTEM_HDF5:BOOL=ON
  -DVTK_USE_SYSTEM_NETCDF:BOOL=ON
  -DVTK_USE_SYSTEM_FREETYPE:BOOL=ON
)

# Turn off testing and other non essential featues
list(APPEND ParaView_tpl_args
  -DBUILD_TESTING:BOOL=OFF
  -DPARAVIEW_BUILD_PLUGIN_MobileRemoteControl:BOOL=OFF
  -DPQWIDGETS_DISABLE_QTWEBKIT:BOOL=ON
  -DModule_vtkIOGeoJSON:BOOL=ON
  -DCMAKE_PREFIX_PATH:PATH=${cdat_EXTERNALS}
)

# Use cdat zlib
if(NOT CDAT_USE_SYSTEM_ZLIB)
  list(APPEND ParaView_tpl_args
    -DZLIB_INCLUDE_DIR:PATH=${cdat_EXTERNALS}/include
    -DZLIB_LIBRARY:FILEPATH=${cdat_EXTERNALS}/lib/libz${_LINK_LIBRARY_SUFFIX}
  )
endif()

# Use cdat libxml
if(NOT CDAT_USE_SYSTEM_LIBXML2)
  list(APPEND ParaView_tpl_args
    -DLIBXML2_INCLUDE_DIR:PATH=${cdat_EXTERNALS}/include/libxml2
    -DLIBXML2_LIBRARIES:FILEPATH=${cdat_EXTERNALS}/lib/libxml2${_LINK_LIBRARY_SUFFIX}
    -DLIBXML2_XMLLINT_EXECUTABLE:FILEPATH=${cdat_EXTERNALS}/bin/xmllint
  )
endif()

# Use cdat hdf5
if(NOT CDAT_USE_SYSTEM_HDF5)
  list(APPEND ParaView_tpl_args
    -DHDF5_DIR:PATH=${cdat_EXTERNALS}/
    -DHDF5_C_INCLUDE_DIR:PATH=${cdat_EXTERNALS}/include
    -DHDF5_INCLUDE_DIR:PATH=${cdat_EXTERNALS}/include
    -DHDF5_LIBRARY:FILEPATH=${cdat_EXTERNALS}/lib/libhdf5${_LINK_LIBRARY_SUFFIX}
    -DHDF5_hdf5_LIBRARY:FILEPATH=${cdat_EXTERNALS}/lib/libhdf5${_LINK_LIBRARY_SUFFIX}
    -DHDF5_hdf5_LIBRARY_RELEASE:FILEPATH=${cdat_EXTERNALS}/lib/libhdf5${_LINK_LIBRARY_SUFFIX}
  )

  if(NOT CDAT_USE_SYSTEM_ZLIB)
    list(APPEND ParaView_tpl_args
      -DHDF5_z_LIBRARY:FILEPATH=${cdat_EXTERNALS}/lib/libz${_LINK_LIBRARY_SUFFIX}
      -DHDF5_z_LIBRARY_RELEASE:FILEPATH=${cdat_EXTERNALS}/lib/libz${_LINK_LIBRARY_SUFFIX}
    )
  endif()
endif()

# Check if should build GUI
if(CDAT_BUILD_GUI)
  list(APPEND ParaView_tpl_args
    -DPARAVIEW_BUILD_QT_GUI:BOOL=ON
    -DVTK_QT_USE_WEBKIT:BOOL=OFF
    -DQT_QMAKE_EXECUTABLE:FILEPATH=${QT_QMAKE_EXECUTABLE}
    -DQT_QTUITOOLS_INCLUDE_DIR:PATH=${QT_ROOT}/include/QtUiTools
    -DQT_BINARY_DIR:FILEPATH=${QT_BINARY_DIR})
else()
  list(APPEND ParaView_tpl_args
    -DPARAVIEW_BUILD_QT_GUI:BOOL=OFF)
endif()

# Check if using R then only enable R support
if (CDAT_BUILD_R OR CDAT_USE_SYSTEM_R)
  list(APPEND ParaView_tpl_args
    -DPARAVIEW_USE_GNU_R:BOOL=ON
    -DR_COMMAND:PATH=${R_install}/bin/R
    -DR_DIR:PATH=${R_install}/lib/R
    -DR_INCLUDE_DIR:PATH=${R_install}/lib/R/include
    -DR_LIBRARY_BASE:PATH=${R_install}/lib/R/lib/libR${_LINK_LIBRARY_SUFFIX}
    -DR_LIBRARY_BLAS:PATH=${R_install}/lib/R/lib/libRblas${_LINK_LIBRARY_SUFFIX}
    -DR_LIBRARY_LAPACK:PATH=${R_install}/lib/R/lib/libRlapack${_LINK_LIBRARY_SUFFIX}
    -DR_LIBRARY_READLINE:PATH=)
endif()

if(UVCDAT_TESTDATA_LOCATION)
  list(APPEND ParaView_tpl_args
    -DUVCDAT_TestData:PATH=${UVCDAT_TESTDATA_LOCATION})
endif()

include(GetGitRevisionDescription)
set(paraview_branch ${PARAVIEW_MD5})

get_git_head_revision(refspec sha)
#if("${refspec}" STREQUAL "refs/heads/devel-master")
#  set(paraview_branch uvcdat-next)
#endif()

string(REPLACE "//" "" GIT_PROTOCOL_PREFIX ${GIT_PROTOCOL})

if (${GIT_PROTOCOL} STREQUAL "git://")
  set(REPLACE_GIT_PROTOCOL_PREFIX "http:")
else()
  set(REPLACE_GIT_PROTOCOL_PREFIX "git:")
endif()

configure_file(${cdat_CMAKE_SOURCE_DIR}/cdat_modules_extra/paraview_download.sh.in
  ${cdat_CMAKE_BINARY_DIR}/paraview_download.sh @ONLY
  )

if (NOT OFFLINE_BUILD)
    set(DOWNLOAD_CMD_STR  DOWNLOAD_COMMAND ${cdat_CMAKE_BINARY_DIR}/paraview_download.sh)
else ()
    set(DOWNLOAD_CMD_STR)
endif()

ExternalProject_Add(ParaView
  DOWNLOAD_DIR ${CDAT_PACKAGE_CACHE_DIR}
  SOURCE_DIR ${ParaView_source}
  BINARY_DIR ${ParaView_binary}
  INSTALL_DIR ${ParaView_install}
  ${DOWNLOAD_CMD_STR}
  GIT_TAG ${paraview_branch}
  UPDATE_COMMAND ""
  PATCH_COMMAND ""
  CMAKE_CACHE_ARGS
    -DBUILD_SHARED_LIBS:BOOL=ON
    -DBUILD_TESTING:BOOL=${BUILD_TESTING}
    -DCMAKE_BUILD_TYPE:STRING=${CMAKE_CFG_INTDIR}
    -DCMAKE_CXX_FLAGS:STRING=${cdat_tpl_cxx_flags}
    -DCMAKE_C_FLAGS:STRING=${cdat_tpl_c_flags}
#    -DPARAVIEW_BUILD_AS_APPLICATION_BUNDLE:BOOL=OFF
#    -DPARAVIEW_DISABLE_VTK_TESTING:BOOL=ON
#    -DPARAVIEW_INSTALL_THIRD_PARTY_LIBRARIES:BOOL=OFF
 #   -DPARAVIEW_TESTING_WITH_PYTHON:BOOL=OFF
    -DINCLUDE_PYTHONHOME_PATHS:BOOL=OFF
    ${cdat_compiler_args}
    ${ParaView_tpl_args}
    # Python
    -DPARAVIEW_ENABLE_PYTHON:BOOL=ON
    -DPYTHON_EXECUTABLE:FILEPATH=${PYTHON_EXECUTABLE}
    -DPYTHON_INCLUDE_DIR:PATH=${PYTHON_INCLUDE}
    -DPYTHON_LIBRARY:FILEPATH=${PYTHON_LIBRARY}
    -DCMAKE_INSTALL_RPATH_USE_LINK_PATH:BOOL=ON
    -DVTK_LEGACY_SILENT:BOOL=ON
    -DPARAVIEW_DO_UNIX_STYLE_INSTALLS:BOOL=ON
  CMAKE_ARGS
    -DCMAKE_INSTALL_PREFIX:PATH=<INSTALL_DIR>
  DEPENDS ${ParaView_deps}
  ${ep_log_options}
)

# Install ParaView and VTK python modules via their setup.py files.

#configure_file(${cdat_CMAKE_SOURCE_DIR}/cdat_modules_extra/vtk_install_python_module.cmake.in
#  ${cdat_CMAKE_BINARY_DIR}/vtk_install_python_module.cmake
#  @ONLY)

#configure_file(${cdat_CMAKE_SOURCE_DIR}/cdat_modules_extra/paraview_install_python_module.cmake.in
#  ${cdat_CMAKE_BINARY_DIR}/paraview_install_python_module.cmake
#  @ONLY)

#ExternalProject_Add_Step(ParaView InstallParaViewPythonModule
#  COMMAND ${CMAKE_COMMAND} -P ${cdat_CMAKE_BINARY_DIR}/paraview_install_python_module.cmake
#  DEPENDEES install
#  WORKING_DIRECTORY ${cdat_CMAKE_BINARY_DIR}
#  )

#ExternalProject_Add_Step(ParaView InstallVTKPythonModule
#  COMMAND ${CMAKE_COMMAND} -P ${cdat_CMAKE_BINARY_DIR}/vtk_install_python_module.cmake
#  DEPENDEES install
#  WORKING_DIRECTORY ${cdat_CMAKE_BINARY_DIR}
#  )

# symlinks of Externals/bin get placed in prefix/bin so we need to symlink paraview
# libs into prefix/lib as well for pvserver to work.
if(NOT EXISTS ${CMAKE_INSTALL_PREFIX}/lib)
  message("making ${ParaView_install}/lib")
  file(MAKE_DIRECTORY ${CMAKE_INSTALL_PREFIX}/lib)
endif()

#ExternalProject_Add_Step(ParaView InstallParaViewLibSymlink
#  COMMAND ${CMAKE_COMMAND} -E create_symlink ${ParaView_install}/lib/paraview-${PARAVIEW_MAJOR}.${PARAVIEW_MINOR} ${CMAKE_INSTALL_PREFIX}/lib/paraview-${PARAVIEW_MAJOR}.${PARAVIEW_MINOR}
#  DEPENDEES install
#  WORKING_DIRECTORY ${cdat_CMAKE_BINARY_DIR}
#)
unset(GIT_CMD_STR)

