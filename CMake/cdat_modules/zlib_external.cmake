
# If Windows we use CMake otherwise ./configure
if(WIN32)

  set(zlib_source "${CMAKE_CURRENT_BINARY_DIR}/zlib")
  set(zlib_binary "${CMAKE_CURRENT_BINARY_DIR}/zlib-build")
  set(zlib_install "${cdat_EXTERNALS}")

  ExternalProject_Add(zlib
    DOWNLOAD_DIR ${CDAT_PACKAGE_CACHE_DIR}
    SOURCE_DIR ${zlib_source}
    BINARY_DIR ${zlib_build}
    INSTALL_DIR ${zlib_install}
    URL ${ZLIB_URL}/${ZLIB_GZ}
    URL_MD5 ${ZLIB_MD5}
    PATCH_COMMAND ${CMAKE_COMMAND} -E remove <SOURCE_DIR>/zconf.h
    CMAKE_CACHE_ARGS
      -DCMAKE_CXX_FLAGS:STRING=${pv_tpl_cxx_flags}
      -DCMAKE_C_FLAGS:STRING=${pv_tpl_c_flags}
      -DCMAKE_BUILD_TYPE:STRING=${CMAKE_CFG_INTDIR}
      ${pv_tpl_compiler_args}
      ${zlib_EXTRA_ARGS}
    CMAKE_ARGS
      -DCMAKE_INSTALL_PREFIX:PATH=<INSTALL_DIR>
    ${ep_log_options}
  )

else()

  set(zlib_source "${CMAKE_CURRENT_BINARY_DIR}/build/zlib")
  set(zlib_install "${cdat_EXTERNALS}")
  set(CONFIGURE_ARGS --shared)

  ExternalProject_Add(zlib
    DOWNLOAD_DIR ${CDAT_PACKAGE_CACHE_DIR}
    SOURCE_DIR ${zlib_source}
    INSTALL_DIR ${zlib_install}
    URL ${ZLIB_URL}/${ZLIB_GZ}
    URL_MD5 ${ZLIB_MD5}
    PATCH_COMMAND ${CMAKE_COMMAND} -E remove <SOURCE_DIR>/zconf.h
    BUILD_IN_SOURCE 1
    CONFIGURE_COMMAND ${CMAKE_COMMAND} -DCONFIGURE_ARGS=${CONFIGURE_ARGS} -DINSTALL_DIR=<INSTALL_DIR> -DWORKING_DIR=<SOURCE_DIR> -P ${cdat_CMAKE_BINARY_DIR}/cleanenv_configure_step.cmake
    DEPENDS ${zlib_deps}
    ${ep_log_options}
  )

endif()

set(ZLIB_INCLUDE_DIR ${zlib_install}/include)

if(WIN32)
  set(ZLIB_LIBRARY optimized ${zlib_install}/lib/zlib${_LINK_LIBRARY_SUFFIX} debug ${zlib_install}/lib/zlibd${_LINK_LIBRARY_SUFFIX})
else()
  set(ZLIB_LIBRARY ${ZLIB_LIBRARY_PATH}/libz${_LINK_LIBRARY_SUFFIX})
endif()
