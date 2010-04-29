################################################################################
# F2py with BLAS/LAPACK
# SpanLib, Raynaud 2006
################################################################################
# AC_SR_STRIPFLAGS([FLAGS],[OUTPUT_VARIABLE])
# Convert from "-lblabla -looo" to "['blabla','ooo']" (same for -L)
AC_DEFUN([AC_SR_F2PY_STRIPFLAGS],
[
	dnl AS_VAR_SET($2,`echo $1 | sed -r 's/(^| )-(L|l)/\1/g'`)
	dnl AS_VAR_SET($2,`echo $1 | sed -e 's/^-[[[lL]]]//' -e 's/ -[[[lL]]]/ /g'`)
	AS_VAR_SET($2,`AS_VAR_GET(PYTHON) scripts/strip_fcargs.py $1`)
])
# Setup library dir and name variables F2PY_DIRS and F2PY_LIBS
AC_DEFUN([AC_SR_F2PY],
[
	# Full path to f2py
	AC_ARG_VAR(F2PY,[Absolute path to F2PY executable])
	#AC_MSG_CHECKING([for f2py full path])
	AC_PATH_PROG(F2PY,f2py,no,AS_VAR_GET(MYPYTHONPATH))
	#AC_MSG_RESULT(AS_VAR_GET(F2PY))
	AM_CONDITIONAL([HAS_F2PY],[test "AS_VAR_GET(F2PY)" != "no"])

	# F90 compiler id
	AS_IF([test "AS_VAR_GET(F2PY)" != "no"],[
		AS_VAR_SET(F2PY_FC_VENDOR,[`AS_VAR_GET(PYTHON) scripts/check_fortran.py AS_VAR_GET(FC)`])
		AS_VAR_SET_IF(F2PY_FC_VENDOR,
			AS_VAR_SET(ac_cv_goodfcid,yes),
			[AC_SR_WARNING([Your f90 compiler (AS_VAR_GET(FC)) is not in the available list from scipy and f2py.
You wont be able to build the python package.])
				AM_CONDITIONAL([HAS_F2PY],[`false`])]
		)
	])

	# Blas and Lapack libraries and directories
	AC_MSG_CHECKING([for fortran libraries for F2PY])
		AC_SR_F2PY_STRIPFLAGS("AS_VAR_GET(LAPACK95) AS_VAR_GET(LAPACK) AS_VAR_GET(BLAS)",F2PY_LIBS)
		AC_SUBST(F2PY_LIBS)
	AC_MSG_RESULT(AS_VAR_GET(F2PY_LIBS))
	AC_MSG_CHECKING([for fortran library directories for F2PY])
		AC_SR_F2PY_STRIPFLAGS("AS_VAR_GET(LAPACK95_LIB) AS_VAR_GET(LAPACK_LIB) AS_VAR_GET(BLAS_LIB)",F2PY_LIBDIRS)
		AC_SUBST(F2PY_LIBDIRS)
	AC_MSG_RESULT(AS_VAR_GET(F2PY_LIBDIRS))
	AC_MSG_CHECKING([for fortran include directories for F2PY])
		AC_SR_F2PY_STRIPFLAGS("AS_VAR_GET(LAPACK95_INC)",F2PY_INCDIRS)
		AC_SUBST(F2PY_INCDIRS)
	AC_MSG_RESULT(AS_VAR_GET(F2PY_INCDIRS))

	# Set manual path to install the python module
	AC_ARG_VAR(PYTHONDIR,[Directory where to install the spanlib python module])
	AC_ARG_WITH(pythondir,
		AC_HELP_STRING(--with-pythondir=DIR,
				[Directory where to install the spanlib python module]),
				[case AS_VAR_GET(with_pythondir) in
					no|yes);;
					*)AS_VAR_SET(PYTHONDIR,AS_VAR_GET(with_pythondir))
						AM_CONDITIONAL(LOCAL_PYTHON_INSTALL,true);;
				esac]
	)

	# Build directory
	AS_IF([test "AS_VAR_GET(F2PY)" != "no"],
	[
		AC_MSG_CHECKING([the generic directory name for building python libraries])
		AS_VAR_SET(F2PY_BUILD_DIR,
			[`AS_VAR_GET(PYTHON) -c ["import sys;from distutils.util import get_platform ; print \"lib.\"+get_platform()+\"-\"+sys.version[0:3]"]`])
		AC_MSG_RESULT(AS_VAR_GET(F2PY_BUILD_DIR))
	])

	# Build option and default install path
	AS_VAR_SET_IF(PYTHONDIR,,
		[
			AS_IF([test "AS_VAR_GET(F2PY)" != "no"],[
					AC_MSG_CHECKING([where is the default place for python packages])
					AS_VAR_SET(PYTHONDIR,
						[`AS_VAR_GET(PYTHON) -c ["from distutils import sysconfig; print sysconfig.get_python_lib(1,0)"]`])
					AC_MSG_RESULT(AS_VAR_GET(PYTHONDIR))
				],
				AS_VAR_SET(PYTHONDIR,"")
			)
		]
	)

	AC_SUBST(F2PY_BUILD_DIR)
	AC_SUBST(F2PY_FC_VENDOR)

])
################################################################################


