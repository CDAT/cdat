
set(proj zlib)

ExternalProject_Add(${proj}
  URL ${ZLIB_URL}/${ZLIB_GZ}
  URL_MD5 ${ZLIB_MD5}
  UPDATE_COMMAND ""
  PATCH_COMMAND ${CMAKE_COMMAND} -E remove <SOURCE_DIR>/zconf.h
  SOURCE_DIR ${proj}
  BINARY_DIR ${proj}-build
  INSTALL_DIR ${proj}-install
  CMAKE_GENERATOR ${gen}
  CMAKE_ARGS
    ${ep_common_args}
    -DBUILD_SHARED_LIBS:BOOL=ON
    -DCMAKE_CXX_FLAGS:STRING=${ep_common_cxx_flags}
    -DCMAKE_C_FLAGS:STRING=${ep_common_c_flags}
    -DCMAKE_INSTALL_PREFIX=<INSTALL_DIR>
  INSTALL_COMMAND make install
  DEPENDS
  )
#set(ITK_DIR ${CMAKE_BINARY_DIR}/${proj}-build)

set(ZLIB_INCLUDE_DIR ${proj}-install/include)
set(ZLIB_LIBRARY_DIR ${proj}-install/lib)
set(ZLIB_LIBRARY ${CMAKE_BINARY_DIR}/${proj}-install/lib/libz.so)

