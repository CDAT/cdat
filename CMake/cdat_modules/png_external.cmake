# If Windows we use CMake otherwise ./configure
if(WIN32)

  set(png_source "${CMAKE_CURRENT_BINARY_DIR}/png")
  set(png_binary "${CMAKE_CURRENT_BINARY_DIR}/png-build")
  set(png_install "${cdat_EXTERNALS}")

  ExternalProject_Add(png
  URL ${PNG_URL}/${PNG_GZ}
  URL_MD5 ${PNG_MD5}
  UPDATE_COMMAND ""
  SOURCE_DIR ${png_source}
  BINARY_DIR ${png_binary}
  CMAKE_CACHE_ARGS
    -DCMAKE_CXX_FLAGS:STRING=${pv_tpl_cxx_flags}
    -DCMAKE_C_FLAGS:STRING=${pv_tpl_c_flags}
    -DCMAKE_BUILD_TYPE:STRING=${CMAKE_CFG_INTDIR}
    ${pv_tpl_compiler_args}
    -DZLIB_INCLUDE_DIR:STRING=${ZLIB_INCLUDE_DIR}
    -DZLIB_LIBRARY:STRING=${ZLIB_LIBRARY}
  CMAKE_ARGS
    -DCMAKE_INSTALL_PREFIX:PATH=<INSTALL_DIR>
  DEPENDS ${png_dependencies}
  ${ep_log_options}
  )

else()

  set(png_source "${CMAKE_CURRENT_BINARY_DIR}/build/png")
  set(png_install "${cdat_EXTERNALS}")

  ExternalProject_Add(png
    DOWNLOAD_DIR ${CDAT_PACKAGE_CACHE_DIR}
    SOURCE_DIR ${png_source}
    INSTALL_DIR ${png_install}
    URL ${PNG_URL}/${PNG_GZ}
    URL_MD5 ${PNG_MD5}
    BUILD_IN_SOURCE 1
    PATCH_COMMAND ${CMAKE_COMMAND} -E copy_if_different ${cdat_external_patch_dir}/src/png/pngconf.h ${png_source}/pngconf.h
    CONFIGURE_COMMAND ${CMAKE_COMMAND} -DINSTALL_DIR=<INSTALL_DIR> -DWORKING_DIR=<SOURCE_DIR> -P ${cdat_CMAKE_BINARY_DIR}/cdat_configure_step.cmake
    DEPENDS ${png_deps}
    ${ep_log_options}
  )

endif()
