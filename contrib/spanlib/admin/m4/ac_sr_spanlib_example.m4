################################################################################
### BLAS and LAPACK
### Raynaud 2006
################################################################################
AC_DEFUN([AC_SR_SPANLIB_EXAMPLE],[

	#################################################################
	# We need netcdf
	#################################################################

	AC_SR_NETCDF()
	AS_IF([test "AS_VAR_GET(HAS_F90NETCDF)" = "yes"],[
		AS_VAR_SET(F90_EXAMPLE_TEXT,["To run the first fortran example, type from here '"AS_VAR_GET([NORMAL])"make example"AS_VAR_GET(GREEN)"'."])
	],[
		AC_SR_WARNING([Without f90 netcdf support, you wont be able to run the f90 example])
	])

	#################################################################
	# Blas/Lapack (checked before)
	#################################################################
	AM_CONDITIONAL([WITH_EXAMPLE],
		[test "AS_VAR_GET(HAS_BLASLAPACK)" != "no" -a "AS_VAR_GET(HAS_F90NETCDF)" = "yes"])

	#################################################################
	# Python example
	#################################################################
	AS_VAR_SET_IF(WITH_PYTHON,[
		AS_VAR_SET(PYTHON_EXAMPLE_TEXT,["To run the first python example, type from here '"AS_VAR_GET([NORMAL])"make python1"AS_VAR_GET(GREEN)"'."])
	])


	#################################################################
	# A commandline downloader may be useful to get the data
	#################################################################

	AC_CHECK_PROG(WGET,wget,wget,no)
	AS_IF([test "AS_VAR_GET(WGET)" != "no"],
		AS_VAR_SET(DOWNLOADER,AS_VAR_GET(WGET)))
	AS_VAR_SET_IF(DOWNLOADER,,[
			AC_CHECK_PROG(LINKS,links,links,no)
			AS_IF([test "AS_VAR_GET(LINKS)" != "no"],
				AS_VAR_SET(DOWNLOADER,AS_VAR_GET(LINKS))) ])
	AS_VAR_SET_IF(DOWNLOADER,,
		AC_SR_WARNING([No commandline downloader found:
you will have to download yourself the input data file to run the example]))
	AC_SUBST(DOWNLOADER)
	AM_CONDITIONAL(HAS_DOWNLOADER,AS_VAR_TEST_SET(DOWNLOADER))
	AM_CONDITIONAL(USE_LINKS,
		[test "AS_VAR_GET(DOWNLOADER)" = "AS_VAR_GET(LINKS)"])


	#################################################################
	# A viewver may be useful to visualise the output netcdf file
	#################################################################

	# Fortran examples
	AS_VAR_SET(f90exlist,"1 2 3 4 5 5 6 7 8 9")

	# Ncview...
	AC_CHECK_PROG(NCVIEW,ncview,ncview,no)
	AS_IF([test "AS_VAR_GET(NCVIEW)" != "no"],[
		AS_VAR_SET(NCVIEWER,AS_VAR_GET(NCVIEW))
		for id in AS_VAR_GET(f90exlist) ; do
			AS_VAR_SET(NCVIEWER_ARGS_fortran$id,output_fortran$id.nc)
		done
	])

	# ...or VCDAT...
	AS_VAR_SET_IF(NCVIEWER,,[
		AS_IF([test "AS_VAR_GET(VCDAT)" != "no"], [AS_VAR_SET(NCVIEWER,AS_VAR_GET(VCDAT))])
	])

	# ...or  quick vcs (cdat) or matplotlib viewer
	AS_VAR_SET_IF(NCVIEWER,,[
		AS_IF([test AS_VAR_GET(HAS_CDAT) = "yes" -o AS_VAR_GET(HAS_MPL) = "yes"],[
			AS_VAR_SET(NCVIEWER,[AS_VAR_GET(PYTHON) ../scripts/quickplot.py])
			for id in AS_VAR_GET(f90exlist) ; do
				AS_VAR_SET(NCVIEWER_ARGS_fortran$id,[["]output_fortran$id.nc ['\$(VARIABLE_python]AS_VAR_GET(id)[)']["]])
			done
		])])

	# So...
	AS_VAR_SET_IF(NCVIEWER,,
		AC_SR_WARNING([No netcdf viewer available:
you will have to visualise the output netcdf file by your own]))
	AC_SUBST(NCVIEWER)
	# Shit
	# somebody able to make a loop that works here?
	AC_SUBST(NCVIEWER_ARGS_fortran1)
	AC_SUBST(NCVIEWER_ARGS_fortran2)
	AC_SUBST(NCVIEWER_ARGS_fortran3)
	AC_SUBST(NCVIEWER_ARGS_fortran4)
	AC_SUBST(NCVIEWER_ARGS_fortran5)
	AC_SUBST(NCVIEWER_ARGS_fortran6)
	AC_SUBST(NCVIEWER_ARGS_fortran7)
	AC_SUBST(NCVIEWER_ARGS_fortran8)
	AC_SUBST(NCVIEWER_ARGS_fortran8)
	AC_SUBST(NCVIEWER_ARGS_fortran9)
	AM_CONDITIONAL(HAS_NCVIEWER,AS_VAR_TEST_SET(NCVIEWER))

])
################################################################################
################################################################################
################################################################################

