################################################################################
# BLAS and LAPACK
# SpanLib, Raynaud 2006
################################################################################

AC_DEFUN([AC_SR_BLASLAPACK],
[dnl

#################################
# Initialisations
#################################
AS_VAR_SET(save_libs,$LIBS)
AS_VAR_SET(save_fcflags,$FCFLAGS)

LIBS=$FLIBS


#################################
# BLAS
#################################

# Library name or LIBS
AC_MSG_CHECKING([for blas library flag])
AC_ARG_VAR(BLAS,[Library name or LIBS flag(s)])
AC_ARG_WITH(blas,
 	AC_HELP_STRING([--with-blas=LIBNAME],
 		[Library name or LIBS flag(s)]),
 	[case AS_VAR_GET(with_blas) in
		no)AC_SR_ERROR([You cant disable blas]);;
		yes)AS_VAR_SET(BLAS,-lblas);;
		*)AS_VAR_SET(BLAS,$with_blas);;
	esac]
)
AS_VAR_SET_IF([BLAS],,[AS_VAR_SET(BLAS,-lblas)])
case AS_VAR_GET(BLAS) in
	-l* | */* | *.a | *.so | *.so.* | *.o):;;
	*)AS_VAR_SET(BLAS,"-l$BLAS");;
esac
AS_VAR_SET(LIBS,"$BLAS $LIBS")
AC_MSG_RESULT(AS_VAR_GET(BLAS))

# Is it a MKL (Intel) library?
AC_MSG_CHECKING([weither it is MKL library from intel])
AS_IF([test "x`echo AS_VAR_GET(BLAS) | grep -i mkl`" = "x"],
	[AS_VAR_SET(USE_MKL,no)],[AS_VAR_SET(USE_MKL,yes)]
)
AC_MSG_RESULT(AS_VAR_GET(USE_MKL))
	

# Directory where to find the library
AC_MSG_CHECKING([for blas library location flag])
AC_ARG_VAR(BLAS_LIB,[Location of the BLAS library (compile-time)])
AC_ARG_WITH(blas-lib,
	AC_HELP_STRING([--with-blas-lib=DIR],
		[Location of the BLAS library (compile-time)]), dnl
	AS_IF([test "$with_blas_lib" != "yes" -a "$with_blas_lib" != "no"],
			AS_VAR_SET(BLAS_LIB,$with_blas_lib))
)
AS_VAR_SET_IF(BLAS_LIB,,[
	AS_IF(test AS_VAR_GET(prefix) != "None/include",
		AS_VAR_SET(BLAS_LIB,AS_VAR_GET(prefix)/lib))
])
AS_VAR_SET_IF([BLAS_LIB],[
	case AS_VAR_GET(BLAS_LIB) in
		-L*):;;
		*)AS_VAR_SET(BLAS_LIB,"-L$BLAS_LIB");;
	esac
	AS_VAR_SET(FCFLAGS,"$BLAS_LIB $FCFLAGS")
])
AC_MSG_RESULT(AS_VAR_GET(BLAS_LIB))

# Try sgemm with blas
AC_CACHE_CHECK([for sgemm of the blas library],ac_cv_blasok,
[AC_TRY_LINK_FUNC(sgemm,
                 [AS_VAR_SET(ac_cv_blasok,yes)],
                 [AS_VAR_SET(ac_cv_blasok,no)])
])


#################################
# LAPACK
#################################

# Default value
AS_IF([test AS_VAR_GET(USE_MKL) = "yes"],
	[AS_VAR_SET(LAPACK_DEFAULT,"-lmkl_lapack")],
	[AS_VAR_SET(LAPACK_DEFAULT,"-llapack")]
)	

# F77 library name or LIBS
AC_MSG_CHECKING([for lapack f77 library flag])
AC_ARG_VAR(LAPACK,F77 library name or LIBS flag(s))
AC_ARG_WITH(lapack,
	AC_HELP_STRING([--with-lapack=LIBNAME],
		[F77 library name or LIBS flag(s)]),
	[case AS_VAR_GET(with_lapack) in
		no)AC_SR_ERROR([You cant disable lapack]);;
		yes)AS_VAR_SET(LAPACK,AS_VAR_GET(LAPACK_DEFAULT));;
		*)AS_VAR_SET(LAPACK,$with_lapack);;
	esac] 
)
AS_VAR_SET_IF([LAPACK],,[AS_VAR_SET(LAPACK,AS_VAR_GET(LAPACK_DEFAULT))])
case AS_VAR_GET(LAPACK) in
	-l* | */* | *.a | *.so | *.so.* | *.o):;;
	*)AS_VAR_SET(LAPACK,"-l$LAPACK");;
esac
AS_VAR_SET(LIBS,"$LAPACK $LIBS")
AC_MSG_RESULT(AS_VAR_GET(LAPACK))

# Library dir name or FCFLAGS for Lapack
AC_MSG_CHECKING([for lapack library location flag])
AC_ARG_VAR(LAPACK_LIB,Location of the LAPACK library (compile-time))
AC_ARG_WITH(lapack-lib,
	AC_HELP_STRING([--with-lapack-lib=DIR],
		[Location of the LAPACK library (compile-time)]),
	AS_IF([test "$with_lapack_lib" != "yes" -a "$with_lapack_lib" != "no"],
		AS_VAR_SET(LAPACK_LIB,$with_lapack_lib))
)
AS_VAR_SET_IF([LAPACK_LIB],[
	case AS_VAR_GET(LAPACK_LIB) in
		-L*):;;
		*)AS_VAR_SET(LAPACK_LIB,"-L$LAPACK_LIB");;
	esac
	AS_VAR_SET(FCFLAGS,"$LAPACK_LIB $FCFLAGS")
])
AC_MSG_RESULT(AS_VAR_GET(LAPACK_LIB))



# Default value
AS_IF([test AS_VAR_GET(USE_MKL) = "yes"],
	[AS_VAR_SET(LAPACK95_DEFAULT,"-lmkl_lapack95")],
	[AS_VAR_SET(LAPACK95_DEFAULT,"-llapack95")]
)	



# F95 library name or LIBS
AC_MSG_CHECKING([for lapack f95 library flag])
AC_ARG_VAR(LAPACK95,LAPACK95 library name or LIBS flag(s))
AC_ARG_WITH(lapack95,
	AC_HELP_STRING([--with-lapack95=LIBNAME],
		[LAPACK95 library name or LIBS flag(s)]),
	[
		case AS_VAR_GET(with_lapack95) in
			no):;;
			yes)AS_VAR_SET(LAPACK95,AS_VAR_GET(LAPACK95_DEFAULT));;
			*)AS_VAR_SET(LAPACK95,$with_lapack95);;
		esac
		AS_VAR_SET_IF([LAPACK95],[
			case AS_VAR_GET(LAPACK95) in
				-l* | */* | *.a | *.so | *.so.* | *.o):;;
				*)AS_VAR_SET(LAPACK95,"-l$LAPACK95");;
			esac
		])
	]
)
AS_VAR_SET_IF([LAPACK95],,[AS_VAR_SET(LAPACK95,AS_VAR_GET(LAPACK95_DEFAULT))])
AC_MSG_RESULT(AS_VAR_GET(LAPACK95))
AS_VAR_SET(LIBS,"$LAPACK95 $LIBS")

# Is it a MKL (Intel) library?
AS_IF([test AS_VAR_GET(USE_MKL) = "yes"],
	[
		AS_VAR_SET(USE_MKL_LAPACK95,no)
		AS_VAR_SET(lapack95_mod,mkl95_lapack)
dnl		AS_VAR_SET(lapack95_pre,mkl95_precision)
		AS_VAR_SET(lapack95_sub,syev)	
	],[
		AS_VAR_SET(USE_MKL_LAPACK95,yes)
		AS_VAR_SET(lapack95_mod,f95_lapack)
dnl		AS_VAR_SET(lapack95_pre,la_precision)
		AS_VAR_SET(lapack95_sub,la_syev)
	]
)

# Library dir name or FCFLAGS for Lapack95 
AC_MSG_CHECKING([for lapack95 library location flag])
AC_ARG_VAR(LAPACK95_LIB,Location of the LAPACK95 library (compile-time))
AC_ARG_WITH(lapack95-lib,
	AC_HELP_STRING([--with-lapack95-lib=DIR],
		[Location of the LAPACK95 library (compile-time)]),
	AS_IF([test "$with_lapack95_lib" != "yes" -a "$with_lapack95_lib" != "no"],
		AS_VAR_SET(LAPACK95_LIB,$with_lapack95_lib))
)
AS_VAR_SET_IF([LAPACK95_LIB],[
	case AS_VAR_GET(LAPACK95_LIB) in
		-L*):;;
		*)AS_VAR_SET(LAPACK95_LIB,"-L$LAPACK95_LIB");;
	esac
	AS_VAR_SET(FCFLAGS,"$LAPACK95_LIB $FCFLAGS")
])
AC_MSG_RESULT(AS_VAR_GET(LAPACK95_LIB))

## F95 module name
#AC_MSG_CHECKING([for lapack f95 module name])
#AC_ARG_VAR(LAPACK95_MOD,LAPACK95 module name)
#AC_ARG_WITH(lapack95_mod,
#	AC_HELP_STRING([--with-lapack95-mod=MODNAME],
#		[LAPACK95 module name]),
#	[case AS_VAR_GET(with_lapack95_mod) in
#		no|yes)AS_VAR_SET(LAPACK95_MOD,f95_lapack);;
#		*)AS_VAR_SET(LAPACK95_MOD,AS_VAR_GET(with_lapack95_mod));;
#	esac] 
#)
#AS_VAR_SET_IF([LAPACK95_MOD],,AS_VAR_SET(LAPACK95_MOD,f95_lapack))
#AC_MSG_RESULT(AS_VAR_GET(LAPACK95_MOD))

# Directory where to find the lapack95 modules or include flag
AC_MSG_CHECKING([for lapack95 include flag])
AC_ARG_VAR(LAPACK95_INC,[Location of the LAPACK f95 modules (compile-time)])
AC_ARG_WITH(lapack95-inc,
	AC_HELP_STRING([--with-lapack95-inc=DIR],
		[Location of the LAPACK95 modules (compile-time)]), dnl
	AS_IF([test "$with_lapack95_inc" != "yes" -a "$with_lapack95_inc" != "no"],
			AS_VAR_SET(LAPACK95_INC,$with_lapack95_inc))
)
AS_VAR_SET_IF(LAPACK95_INC,,[
	AS_IF(test AS_VAR_GET(prefix) != None,
		AS_VAR_SET(LAPACK95_INC,AS_VAR_GET(prefix)/include))
])
AS_VAR_SET_IF(LAPACK95_INC,[
	case AS_VAR_GET(LAPACK95_INC) in
		-I*):;;
		*)AS_VAR_SET(LAPACK95_INC,"-I$LAPACK95_INC");;
	esac
	AS_VAR_SET(FCFLAGS,"$LAPACK95_INC $FCFLAGS")
])
AC_MSG_RESULT(AS_VAR_GET(LAPACK95_INC))

# Try ssyev with lapack95
# AC_CACHE_CHECK([for dsyev of the lapack library],ac_cv_lapackok,
# [AC_TRY_LINK_FUNC([dsyev],
#                  [AS_VAR_SET(ac_cv_lapackok,yes)],
#                  [AS_VAR_SET(ac_cv_lapackok,no)])
# ])
AC_CACHE_CHECK([for la_syev of the lapack95 library],ac_cv_lapackok,
	[
		AC_LINK_IFELSE(
			[program conftest_routine
	use AS_VAR_GET(lapack95_mod), only: AS_VAR_GET(lapack95_sub)
end program conftest_routine],
			AS_VAR_SET(ac_cv_lapackok,yes),
			AS_VAR_SET(ac_cv_lapackok,no))
	])

#################################
# Ending
#################################

# Warning
AS_IF([test "AS_VAR_GET(ac_cv_blasok)" != "yes" -o "AS_VAR_GET(ac_cv_lapackok)" != "yes"],
	[
		AS_VAR_SET(HAS_BLASLAPACK,no)
		AC_SR_WARNING([It seems you have no Blas/Lapack developpment support.
Try with switches --with-blas/lapack/lapack95 and/or --with-blas/lapack-lib
and/or --with-lapack-inc.
Without it, you wont be able to run the example and use the python interface.])
	],
	AS_VAR_SET(HAS_BLASLAPACK,yes)
)

# Variables
AS_VAR_SET(LIBS,$save_libs)
AS_VAR_SET(FCFLAGS,$save_fcflags)
AC_SUBST(BLAS)
AC_SUBST(BLAS_LIB)
AC_SUBST(LAPACK)
AC_SUBST(LAPACK95)
AC_SUBST(USE_MKL)
AC_SUBST(LAPACK_LIB)
AC_SUBST(LAPACK_INC)

])dnl AC_SR_BLASLAPACK
################################################################################
