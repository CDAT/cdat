################################################################################
### SpanLib, Raynaud 2006
################################################################################
AC_DEFUN([AC_SR_SPANLIB_PYTHON],[

	# Tools
# 	AC_CHECK_PROG(PERL,perl,perl,no)
	AC_PROG_CC()

	# Default conditionals
	AM_CONDITIONAL(HAS_F2PY,false)
	AM_CONDITIONAL(LOCAL_PYTHON_INSTALL,false)

	# Basic python
	AX_WITH_PYTHON(2.5,no)

	# We have python
	AS_IF([test "AS_VAR_GET(PYTHON)" != "no"],[

		# Current python
		AS_VAR_SET(MYPYTHONPATH,[`AS_DIRNAME(AS_VAR_GET(PYTHON))`])

		# F2py support
		AC_SR_F2PY()

		# CDAT support
		AC_MSG_CHECKING([for CDAT support with numpy])
		AS_VAR_GET(PYTHON) -c ["import cdms2"] 2> /dev/null
		AS_IF([test "$?" = "0"],
			AS_VAR_SET(HAS_CDAT,"yes"),
			AS_VAR_SET(HAS_CDAT,"no")
		)
		AC_MSG_RESULT(AS_VAR_GET(HAS_CDAT))

		# Vcdat for viewing netcdf
		AC_CHECK_PROG([VCDAT],vcdat,vcdat,no,[AS_VAR_GET(MYPYTHONPATH)])

		# Matplotlib for plotting
		AC_MSG_CHECKING([for Matplotlib support])
		AS_VAR_GET(PYTHON) -c ["from matplotlib import use"] 2> /dev/null
		AS_IF([test "$?" = "0"],
			AS_VAR_SET(HAS_MPL,"yes"),
			AS_VAR_SET(HAS_MPL,"no")
		)
		AC_MSG_RESULT(AS_VAR_GET(HAS_MPL))

	],[
		AS_VAR_SET(F2PY,no)
		AS_VAR_SET(HAS_CDAT,no)
		AS_VAR_SET(HAS_MPL,no)
		AS_VAR_SET(VCDAT,no)
	])


	# So, for python...
	AS_IF([test "AS_VAR_GET(HAS_CDAT)" != "no"  -a "AS_VAR_GET(F2PY)" != "no" -a \
			    "AS_VAR_GET(HAS_BLASLAPACK)" != "no"],
			[AS_VAR_SET(WITH_PYTHON,"yes")],
			[AC_SR_WARNING([You wont be able to build the python module.
You need BLAS/LAPACK, f2py and CDAT.])])
	AM_CONDITIONAL([WITH_PYTHON],AS_VAR_TEST_SET(WITH_PYTHON))
# 	AS_IF(AS_VAR_TEST_SET(WITH_PYTHON),,
# 		[AC_SR_WARNING([You wont be able to build the python module.
# You need perl, BLAS/LAPACK, f2py and CDAT.])]
# 	)



])

################################################################################
################################################################################
################################################################################
