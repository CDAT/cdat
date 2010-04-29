################################################################################
### BLAS and LAPACK
### Raynaud 2006
################################################################################
AC_DEFUN([AC_SR_SPANLIB_DOC],[

	AC_ARG_ENABLE(
		doc-update,
		AS_HELP_STRING([--enable-doc-update],[Enable documentation update (you need the sphinx doc generator)]),
		,[AS_VAR_SET(enableval,"no")]
	)
	AM_CONDITIONAL([HAS_DOC_SUPPORT],[false])

	AS_IF([test "AS_VAR_GET(enableval)" == "yes"],
	[
		# Sphinx
		AC_CHECK_PROG(SPHINXBUILD,sphinx-build,sphinx-build)
		
		# So doc support?
		AS_VAR_SET_IF(SPHINXBUILD,[
		
			AM_CONDITIONAL([HAS_DOC_SUPPORT],[true])
		
			# Pdflatex
			AC_CHECK_PROG(PDFLATEX,pdflatex,pdflatex)
			AM_CONDITIONAL([HAS_LATEX_SUPPORT],AS_VAR_TEST_SET(PDFLATEX))
			
		])
		
	])

])
################################################################################
################################################################################
################################################################################

