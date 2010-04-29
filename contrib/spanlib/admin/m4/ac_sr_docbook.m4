dnl AC_SR_DOCBOOK([, ACTION-IF-FOUND [, ACTION-IF-NOT-FOUND]])
dnl checks if xsltproc can build docbook documentation
dnl (which is possible if the catalog is set up properly
dnl I also tried checking for a specific version and type of docbook
dnl but xsltproc seemed to happily run anyway, so we can t check for that
dnl and version
dnl this macro takes inspiration from
dnl http://www.movement.uklinux.net/docs/docbook-autotools/configure.html
dnl *** Inspired from ax_check_docbook.m4 *** Raynaud, 2006
AC_DEFUN([AC_SR_DOCBOOK],
[
	XSLTPROC_FLAGS="--xinclude"
	XSL="doc/spanlib.xsl"
	DOCBOOK_VERSION="4.2"

	dnl We need xsltproc to process the test
	AC_CHECK_PROG(XSLTPROC,xsltproc,xsltproc,)

	XSLTPROC_WORKS=no
	if test -n "$XSLTPROC" ; then
		AC_MSG_CHECKING([whether xsltproc docbook processing works])
		$XSLTPROC $XSLTPROC_FLAGS $XSL >/dev/null 2>&1 << END
<?xml version="1.0" encoding='ISO-8859-1'?>
<!DOCTYPE article PUBLIC "-//OASIS//DTD DocBook XML V4.2//EN" "http://www.oasis-open.org/docbook/xml/4.2/docbookx.dtd">
<article id="test">
</article>
END
		if test "$?" = 0; then
			XSLTPROC_WORKS=yes
		fi
		AC_MSG_RESULT($XSLTPROC_WORKS)
	fi

	if test "x$XSLTPROC_WORKS" = "xyes"; then
		dnl execute ACTION-IF-FOUND
		ifelse([$1], , :, [$1])
	else
		dnl execute ACTION-IF-NOT-FOUND
		ifelse([$2], , :, [$2])
	fi

	AC_SUBST(XSLTPROC_FLAGS)
])