# The LAPACK external project for Titan

set(proj BLAS)

#FORTRAN=;export FORTRAN ; cd BLAS; sed 's/FORTRAN  = g77//g' make.inc > crap && mv -f crap make.inc ; sed 's/LOADER   = g77//g' make.inc > crap && mv -f crap make.inc ; FORTRAN=gfortran;export FORTRAN;)
#	cp build/BLAS/blas_LINUX.a /lgm/bew_cdat/Externals/lib/libblas.a

ExternalProject_Add(${proj}
  URL ${BLAS_URL}/${BLAS_GZ}
  URL_MD5 ${BLAS_MD5}
  SOURCE_DIR BLAS
  BINARY_DIR BLAS-build
  INSTALL_DIR BLAS-install
  )

#list(APPEND trilinos_depends LAPACK)
#set(trilinos_blas_args
#  -DTPL_BLAS_LIBRARIES=${lapack_binary}/SRC/liblapack.so
#  -DTPL_LAPACK_LIBRARIES=${lapack_binary}/BLAS/SRC/libblas.so)