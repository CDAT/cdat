
set(proj png)

ExternalProject_Add(${proj}
  URL ${PNG_URL}/${PNG_GZ}
  URL_MD5 ${PNG_MD5}
  UPDATE_COMMAND ""
#  PATCH_COMMAND ${CMAKE_COMMAND} -E remove <SOURCE_DIR>/zconf.h
  SOURCE_DIR ${proj}
  BINARY_DIR ${proj}-build
  CMAKE_GENERATOR ${gen}
  CMAKE_ARGS
    ${ep_common_args}
    -DBUILD_SHARED_LIBS:BOOL=ON
    -DCMAKE_CXX_FLAGS:STRING=${ep_common_cxx_flags}
    -DCMAKE_C_FLAGS:STRING=${ep_common_c_flags}
    -DZLIB_INCLUDE_DIR:STRING=
    -DZLIB_LIBRARY:STRING=
  INSTALL_COMMAND ""
  DEPENDS ${png_DEPENDENCIES}
  )
#set(ITK_DIR ${CMAKE_BINARY_DIR}/${proj}-build)

