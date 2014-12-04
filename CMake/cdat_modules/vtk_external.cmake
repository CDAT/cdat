set(vtk_source "${CMAKE_CURRENT_BINARY_DIR}/build/VTK")
set(vtk_binary "${CMAKE_CURRENT_BINARY_DIR}/build/VTK-build")
set(vtk_install "${cdat_EXTERNALS}")

set(GIT_CMD_STR GIT_REPOSITORY "${VTK_SOURCE}")

set(_vtk_modules "vtkRenderingImage;vtkRenderingVolume;vtkRenderingLabel;vtkRenderingFreeType;vtkRenderingFreeTypeOpenGL;vtkRenderingVolumeOpenGL;vtkRenderingCore;vtkRenderingOpenGL;vtkGeovisCore;vtkViewsCore;vtkViewsGeovis;vtkInteractionImage;vtkInteractionStyle;vtkInteractionWidgets;vtkCommonTransforms;vtkCommonCore;vtkCommonComputationalGeometry;vtkCommonExecutionModel;vtkCommonSystem;vtkCommonMisc;vtkFiltersFlowPaths;vtkFiltersStatistics;vtkFiltersAMR;vtkFiltersGeneric;vtkFiltersSources;vtkFiltersModeling;vtkFiltersExtraction;vtkFiltersSelection;vtkFiltersSMP;vtkFiltersCore;vtkFiltersHybrid;vtkFiltersTexture;vtkFiltersGeneral;vtkFiltersImaging;vtkFiltersGeometry;vtkIOImage;vtkIOCore;vtkIOExport;vtkIOImport;vtkIOGeometry;vtkImagingColor;vtkImagingSources;vtkImagingCore;vtkImagingGeneral;vtkImagingMath")

if(NOT CDAT_BUILD_LEAN)
  list(APPEND _vtk_modules "vtkIOFFMPEG")
endif()

# Either we use cdat zlib and libxml or system zlib and libxml
list(APPEND vtk_build_args
  -DVTK_USE_SYSTEM_ZLIB:BOOL=ON
  -DVTK_USE_SYSTEM_LIBXML2:BOOL=ON
  -DVTK_USE_SYSTEM_HDF5:BOOL=ON
  -DVTK_USE_SYSTEM_NETCDF:BOOL=ON
  -DVTK_USE_SYSTEM_FREETYPE:BOOL=ON
)

# Turn off testing and other non essential featues
list(APPEND vtk_build_args
  -DBUILD_TESTING:BOOL=OFF
  -DCMAKE_PREFIX_PATH:PATH=${cdat_EXTERNALS}
)

# Use cdat zlib
if(NOT CDAT_USE_SYSTEM_ZLIB)
  list(APPEND vtk_build_args
    -DZLIB_INCLUDE_DIR:PATH=${cdat_EXTERNALS}/include
    -DZLIB_LIBRARY:FILEPATH=${cdat_EXTERNALS}/lib/libz${_LINK_LIBRARY_SUFFIX}
  )
endif()

# Use cdat libxml
if(NOT CDAT_USE_SYSTEM_LIBXML2)
  list(APPEND vtk_build_args
    -DLIBXML2_INCLUDE_DIR:PATH=${cdat_EXTERNALS}/include/libxml2
    -DLIBXML2_LIBRARIES:FILEPATH=${cdat_EXTERNALS}/lib/libxml2${_LINK_LIBRARY_SUFFIX}
    -DLIBXML2_XMLLINT_EXECUTABLE:FILEPATH=${cdat_EXTERNALS}/bin/xmllint
  )
endif()

# Use cdat hdf5
if(NOT CDAT_USE_SYSTEM_HDF5)
  list(APPEND vtk_build_args
    -DHDF5_DIR:PATH=${cdat_EXTERNALS}/
    -DHDF5_C_INCLUDE_DIR:PATH=${cdat_EXTERNALS}/include
    -DHDF5_INCLUDE_DIR:PATH=${cdat_EXTERNALS}/include
    -DHDF5_LIBRARY:FILEPATH=${cdat_EXTERNALS}/lib/libhdf5${_LINK_LIBRARY_SUFFIX}
    -DHDF5_hdf5_LIBRARY:FILEPATH=${cdat_EXTERNALS}/lib/libhdf5${_LINK_LIBRARY_SUFFIX}
    -DHDF5_hdf5_LIBRARY_RELEASE:FILEPATH=${cdat_EXTERNALS}/lib/libhdf5${_LINK_LIBRARY_SUFFIX}
  )

  if(NOT CDAT_USE_SYSTEM_ZLIB)
    list(APPEND vtk_build_args
      -DHDF5_z_LIBRARY:FILEPATH=${cdat_EXTERNALS}/lib/libz${_LINK_LIBRARY_SUFFIX}
      -DHDF5_z_LIBRARY_RELEASE:FILEPATH=${cdat_EXTERNALS}/lib/libz${_LINK_LIBRARY_SUFFIX}
    )
  endif()
endif()

if(CDAT_BUILD_OFFSCREEN)
  list(APPEND vtk_build_args
    "-DVTK_USE_X:BOOL=OFF"
    "-DVTK_OPENGL_HAS_OSMESA:BOOL=ON"
    "-DOPENGL_INCLUDE_DIR:PATH=${cdat_EXTERNALS}/include"
    "-DOPENGL_gl_LIBRARY:FILEPATH=${cdat_EXTERNALS}/lib/libOSMesa${_LINK_LIBRARY_SUFFIX}"
    "-DOPENGL_glu_LIBRARY:FILEPATH=${cdat_EXTERNALS}/lib/libGLU${_LINK_LIBRARY_SUFFIX}"
    "-DOSMESA_INCLUDE_DIR:PATH=${cdat_EXTERNALS}/include"
    "-DOSMESA_LIBRARY:FILEPATH=${cdat_EXTERNALS}/lib/libOSMesa${_LINK_LIBRARY_SUFFIX}"
  )
endif()

if(CDAT_BUILD_VTKWEB)
  list(APPEND vtk_build_args
    "-DVTK_Group_Web:BOOL=ON"
  )
endif()

set(_vtk_module_options)
foreach(_module ${_vtk_modules})
  list(APPEND _vtk_module_options "-DModule_${_module}:BOOL=ON")
endforeach()

ExternalProject_Add(VTK
  DOWNLOAD_DIR ${CDAT_PACKAGE_CACHE_DIR}
  SOURCE_DIR ${vtk_source}
  BINARY_DIR ${vtk_binary}
  INSTALL_DIR ${vtk_install}
  ${GIT_CMD_STR}
  GIT_TAG ${VTK_MD5}
  UPDATE_COMMAND ""
  PATCH_COMMAND ""
  CMAKE_CACHE_ARGS
    -DBUILD_SHARED_LIBS:BOOL=ON
    -DBUILD_TESTING:BOOL=OFF
    -DCMAKE_CXX_FLAGS:STRING=${cdat_tpl_cxx_flags}
    -DCMAKE_C_FLAGS:STRING=${cdat_tpl_c_flags}
    -DCMAKE_BUILD_TYPE:STRING=${CMAKE_CFG_INTDIR}
    ${cdat_compiler_args}
    ${vtk_build_args}
    -DVTK_WRAP_PYTHON:BOOL=ON
    -DPYTHON_EXECUTABLE:FILEPATH=${PYTHON_EXECUTABLE}
    -DPYTHON_INCLUDE_DIR:PATH=${PYTHON_INCLUDE}
    -DPYTHON_LIBRARY:FILEPATH=${PYTHON_LIBRARY}
    -DPYTHON_MAJOR_VERSION:STRING=${PYTHON_MAJOR}
    -DPYTHON_MINOR_VERSION:STRING=${PYTHON_MINOR}
    -DVTK_Group_Rendering:BOOL=OFF
    -DVTK_Group_StandAlone:BOOL=OFF
    -DVTK_LEGACY_SILENT:BOOL=ON
    ${_vtk_module_options}
  CMAKE_ARGS
    -DCMAKE_INSTALL_PREFIX:PATH=<INSTALL_DIR>
  DEPENDS ${VTK_deps}
  ${ep_log_options}
)

unset(GIT_CMD_STR)
