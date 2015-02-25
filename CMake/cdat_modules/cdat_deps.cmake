set(CDAT_deps ${wget_pkg} ${python_pkg} ${numpy_pkg}
              ${libcdms_pkg}
              ${jasper_pkg} ${g2clib_pkg} ${tiff_pkg}
              ${libcf_pkg} ${netcdf_pkg} ${myproxyclient_pkg} ${udunits2_pkg})
if (CDAT_BUILD_GRAPHICS)
  if (CDAT_BUILD_PARALLEL)
    list(APPEND CDAT_deps ${paraview_pkg})
  else()
    list(APPEND CDAT_deps ${vtk_pkg})
  endif()
  list(APPEND CDAT_deps ${ffmpeg_pkg})
endif()

if (CDAT_BUILD_ESMF)
    list(APPEND CDAT_deps ${esmf_pkg})
endif()

