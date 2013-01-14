# The LAPACK external project

set(lapack_source "${CMAKE_CURRENT_BINARY_DIR}/build/LAPACK")
set(lapack_binary "${CMAKE_CURRENT_BINARY_DIR}/build/LAPACK-build")
set(lapack_install "${cdat_EXTERNALS}")
set(NUMPY_LAPACK_binary ${lapack_binary})

ExternalProject_Add(LAPACK
  DOWNLOAD_DIR ${CDAT_PACKAGE_CACHE_DIR}
  SOURCE_DIR ${lapack_source}
  BINARY_DIR ${lapack_binary}
  INSTALL_DIR ${lapack_install}
  URL ${LAPACK_URL}/${LAPACK_GZ}
  URL_MD5 ${LAPACK_MD5}
  CMAKE_ARGS
    -DCMAKE_Fortran_COMPILER:FILEPATH=${CMAKE_Fortran_COMPILER}
    -DCMAKE_CXX_FLAGS:STRING=${cdat_tpl_cxx_flags}
    -DCMAKE_C_FLAGS:STRING=${cdat_tpl_c_flags}
    -DBUILD_SHARED_LIBS:BOOL=ON
    -DENABLE_TESTING:BOOL=OFF
    -DCMAKE_BUILD_TYPE:STRING=${CMAKE_BUILD_TYPE}
  CMAKE_ARGS
    -DCMAKE_INSTALL_PREFIX:PATH=<INSTALL_DIR>
  DEPENDS ${LAPACK_deps}
  ${ep_log_options}
)
