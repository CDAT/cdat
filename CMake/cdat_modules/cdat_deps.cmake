set(CDAT_deps ${wget_pkg} ${cmor_pkg} ${python_pkg} ${numpy_pkg}
              ${jasper_pkg} ${g2clib_pkg} ${ffmpeg_pkg}
              ${tiff_pkg} ${libcf_pkg} ${netcdf_pkg}
              ${myproxyclient_pkg} )
if (CDAT_BUILD_GRAPHICS)
  list(APPEND CDAT_deps ${paraview_pkg})
endif()

if (CDAT_BUILD_ESMF)
    list(APPEND CDAT_deps ${esmf_pkg})
endif()

