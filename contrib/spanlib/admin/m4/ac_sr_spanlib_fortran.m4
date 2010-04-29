################################################################################
### BLAS and LAPACK
### Raynaud 2006
################################################################################
AC_DEFUN([AC_SR_SPANLIB_FORTRAN],[

	# Fortran lib
	AC_CONFIG_SRCDIR([src/spanlib.f90])
	AC_SR_FORTRAN()

	# Tools
	AC_CHECK_TOOL(SED, sed, sed)
	AS_IF([test "AS_VAR_GET(SED)" = "no"],
		[AC_SR_ERROR([You need sed to build the library])])
	AC_SR_BLASLAPACK()
	AC_PROG_RANLIB()
	AC_CHECK_TOOL(AR, ar, ar)
	AS_IF([test "AS_VAR_GET(AR)" = "no"],
		[AC_SR_ERROR([You need ar to build the library])])

])
################################################################################
################################################################################
################################################################################
