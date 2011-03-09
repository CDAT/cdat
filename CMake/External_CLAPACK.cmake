# The CLAPACK external project for Titan

set(clapack_source "${CMAKE_CURRENT_BINARY_DIR}/CLAPACK")
set(clapack_binary "${CMAKE_CURRENT_BINARY_DIR}/CLAPACK-build")
# turn off the warnings for clapack on windows
string(REPLACE "/W3" "/W0" CMAKE_CXX_FLAGS_CLAPACK "${CMAKE_CXX_FLAGS}")
string(REPLACE "/W4" "/W0" CMAKE_CXX_FLAGS_CLAPACK
  "${CMAKE_CXX_FLAGS_CLAPACK}")
string(REPLACE "/W3" "/W0" CMAKE_C_FLAGS_CLAPACK "${CMAKE_C_FLAGS}")
string(REPLACE "/W4" "/W0" CMAKE_C_FLAGS_CLAPACK
  "${CMAKE_C_FLAGS_CLAPACK}")
ExternalProject_Add(CLAPACK
  DOWNLOAD_DIR ${CMAKE_CURRENT_BINARY_DIR}
  SOURCE_DIR ${clapack_source}
  BINARY_DIR ${clapack_binary}
  URL ${clapack_file}
  URL_MD5 ${clapack_md5}
  CMAKE_GENERATOR ${gen}
  CMAKE_ARGS
    -DCMAKE_CXX_FLAGS:STRING=${CMAKE_CXX_FLAGS_CLAPACK}
    -DCMAKE_C_FLAGS:STRING=${CMAKE_C_FLAGS_CLAPACK}
    -DBUILD_SHARED_LIBS:BOOL=OFF
    -DCMAKE_BUILD_TYPE:STRING=${CMAKE_BUILD_TYPE}
    ${titan_binary_args}
    ${CLAPACK_EXTRA_ARGS}
  INSTALL_COMMAND ""
  )
list(APPEND trilinos_depends CLAPACK)
set(trilinos_blas_args
  -DCLAPACK_DIR=${clapack_binary})

set(Titan_CLAPACK_DIR "${clapack_binary}" CACHE PATH
  "CLAPACK binary directory" FORCE)
