set(VTK_deps ${pkgconfig_pkg} ${python_pkg} ${hdf5_pkg} ${png_pkg} ${jpeg_pkg} ${libxml2_pkg} ${freetype_pkg} ${netcdfplus_pkg} ${zlib_pkg})

if (NOT CDAT_BUILD_GUI)
  list(APPEND VTK_deps ${qt_pkg})
endif()

if(NOT CDAT_BUILD_LEAN)
  list(APPEND VTK_deps ${ffmpeg_pkg})
endif()

if(CDAT_BUILD_OFFSCREEN)
  list(APPEND VTK_deps ${osmesa_pkg})
endif()
