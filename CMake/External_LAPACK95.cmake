# The LAPACK external project for Titan

ExternalProject_Add(LAPACK
  DOWNLOAD_DIR ${CMAKE_CURRENT_BINARY_DIR}
  SOURCE_DIR LAPACK
  BINARY_DIR LAPACK-build
  URL ${LAPACK_URL}/${LAPACK_GZ}
  URL_MD5 ${LAPACK_MD5}

  )
#list(APPEND trilinos_depends LAPACK)
#set(trilinos_blas_args
#  -DTPL_BLAS_LIBRARIES=${lapack_binary}/SRC/liblapack.so
#  -DTPL_LAPACK_LIBRARIES=${lapack_binary}/BLAS/SRC/libblas.so)