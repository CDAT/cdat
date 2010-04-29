dnl autoconf macros
dnl Adapted from Unidata macros, with numerous changes

dnl Set the value of a variable.  Use the environment if possible; otherwise
dnl set it to a default value.  Call the substitute routine.
dnl
AC_DEFUN([PC_DEFAULT], [dnl
$1=${$1-"$2"}
AC_SUBST([$1])
])

dnl Handle a missing value.
dnl
AC_DEFUN([PC_NEED_VALUE], [dnl
echo "$1:$2:$3" >> confdefs.missing
])

dnl Ensure that a variable contains a given string and that it's substituted.
dnl NB: The [)] construction is neccessary with GNU m4 1.4.
dnl
AC_DEFUN([PC_ENSURE], [dnl
ifelse($2, , [dnl
  $1=${$1-}
], [dnl
  for arg in $2; do
    case "$$1" in
      *$arg*[)]
	;;
      *[)]
	$1="${$1-} $arg"
	;;
    esac
  done
])dnl
AC_SUBST($1)dnl
]dnl
)


dnl Determine the operating system.
dnl
AC_DEFUN([PC_OS], [dnl
    AC_MSG_CHECKING(type of operating system) 
    if test -z "$OS"; then
      OS=`uname -s | tr '[[A-Z]]' '[[a-z]]' | sed 's;/;;g'`
      if test -z "$OS"; then
	PC_NEED_VALUE(OS, [operating system], sunos5)dnl
      fi
    fi
    case $OS in
	aix)
	    OS_NAME=`uname -s`
	    OS_MAJOR=`uname -v | sed 's/[[^0-9]]*\([[0-9]]*\)\..*/\1/'`
	    ;;
	hp-ux)
	    OS=hpux`uname -r | sed 's/[[A-Z.0]]*\([[0-9]]*\).*/\1/'`
	    OS_NAME=HPUX
	    OS_MAJOR=`uname -r | sed 's/[[A-Z.0]]*\([[0-9]]*\).*/\1/'`
	    ;;
	irix)
	    OS=${OS}`uname -r | sed 's/\..*//'`
	    OS_NAME=IRIX
	    OS_MAJOR=`uname -r | sed 's/\..*//'`
	    ;;
	osf*)
	    OS_NAME=OSF1
	    OS_MAJOR=`uname -r | sed 's/[[^0-9]]*\([[0-9]]*\)\..*/\1/'`
	    ;;
	sn*)
	    OS=unicos
	    OS_NAME=UNICOS
	    OS_MAJOR=`uname -r | sed 's/[[^0-9]]*\([[0-9]]*\)\..*/\1/'`
	    ;;
	sunos)
	    OS_NAME=SunOS
	    OS_MAJOR=`uname -r | sed 's/\..*//'`
	    OS=$OS$OS_MAJOR
	    ;;
	ultrix)
	    case `uname -m` in
	    VAX)
		OS=vax-ultrix
		;;
	    esac
	    OS_NAME=ULTRIX
	    OS_MAJOR=`uname -r | sed 's/\..*//'`
	    ;;
	*)
	    # On at least one UNICOS system, 'uname -s' returned the
	    # hostname (sigh).
	    if uname -a | grep CRAY >/dev/null; then
		OS=unicos
		OS_NAME=UNICOS
	    else
		OS_NAME=`uname -s | sed 's/[[^A-Za-z0-9_]]//g'`
	    fi
	    OS_MAJOR=`uname -r | sed 's/[[^0-9]]*\([[0-9]]*\)\..*/\1/'`
	    ;;
    esac

    # Adjust OS for CRAY MPP environment.
    #
    case "$OS" in
    unicos)
	AC_REQUIRE([AC_PROG_CC])
	case "$CC$TARGET$CFLAGS" in
	*cray-t3*)
	    OS=unicos-mpp
	    ;;
	esac
	;;
    esac

    AC_SUBST(OS)dnl
    AC_DEFINE_UNQUOTED(OS_NAME, $OS_NAME)
    AC_DEFINE_UNQUOTED(OS_MAJOR, $OS_MAJOR)
    AC_MSG_RESULT($OS) 
])

dnl Check for C compiler.  This macro replaces the ac_prog_cc macro because 
dnl that macro prefers the GNU C compiler.
dnl
AC_DEFUN([PC_PROG_CC],
[dnl
    #
    # Ensure that the CC variable is unset so that it can be
    # set here rather than by the autoconf-generated
    # configure-script preamble.
    #
    # unset CC
    #
    case ${CC-unset} in
	unset)
	    case `uname -s` in
		AIX)
		    AC_CHECK_PROGS(CC, c89 xlc cc gcc) dnl
		    case "$CC" in
			*gcc*)
			    ;;
			*)
			    PC_ENSURE(CPPFLAGS, -D_ALL_SOURCE) dnl
			    ;;
		    esac
		    ;;
		HP-UX)
		    AC_CHECK_PROGS(CC, c89 cc gcc) dnl
		    case "$CC" in
			*gcc*)
			    ;;
			*)
			    PC_ENSURE(CPPFLAGS, -D_HPUX_SOURCE) dnl
			    PC_ENSURE(CPPFLAGS, -D_POSIX_SOURCE) dnl
			    ;;
		    esac
		    ;;
		IRIX*)
		    AC_CHECK_PROGS(CC, cc gcc) dnl
		    ;;
		OSF1|ULTRIX)
		    AC_CHECK_PROGS(CC, cc gcc) dnl
		    case "$CC" in
		    cc)
			case `uname -m` in
			VAX)
			    ;;
			*)
dnl
dnl Don't set -std here, since f77 chokes on it
dnl
#			    PC_ENSURE(CFLAGS, -std) dnl
			    ;;
			esac
			;;
		    esac
		    ;;
		SunOS)
		    case `uname -r` in
			4*)
			    AC_CHECK_PROGS(CC, acc cc gcc) dnl
			    ;;
			5*)
			    AC_CHECK_PROGS(CC, cc gcc) dnl
#
#                           The following is commented-out because
#                           the configure script uses CPPFLAGS when
#                           compiling C++ source and SunOS 5's CC (at
#                           least) emits error messages when given the
#                           -Xa option causing the configure script to
#                           abandon `$CXX -E' in favor of `/lib/cpp'.
#
#			    case "$CC" in
#				*gcc*)
#				    ;;
#				*)
#				    PC_ENSURE(CPPFLAGS, -Xa) dnl
#				    ;;
#			    esac
			    ;;
		    esac
		    ;;
		*)
		    AC_CHECK_PROGS(CC, c89 cc gcc) dnl
		    # Cray 'uname -s' returns the hostname ...
		    if uname -a | grep CRAY >/dev/null; then
		        PC_ENSURE(CPPFLAGS, -DCRAY)
			PC_ENSURE(CPPFLAGS, -Dcray)
		    fi
		    ;;
	    esac
	    ;;
	*)
	    AC_MSG_CHECKING(for C compiler)
	    AC_MSG_RESULT($CC)
	    ;;
    esac
    case "${CC-}" in
    '')
	PC_NEED_VALUE(CC, [C compiler], /bin/cc) dnl
	;;
    *)
	# Find out if we are using GNU C, under whatever name.
	cat <<UD_EOF > conftest.c
#ifdef __GNUC__
	    yes
#endif
UD_EOF
	${CC} -E conftest.c > conftest.out 2>&1
	if egrep yes conftest.out >/dev/null 2>&1; then
	    GCC=1 # For later tests.
	fi
	AC_SUBST(CC) dnl
	;;
    esac
    rm -f conftest*
])

dnl Check for FORTRAN compiler.
dnl
AC_DEFUN([PC_PROG_FC], [dnl
if test -z "${FC+set}"; then
  AC_REQUIRE([PC_OS])dnl
  case "$OS" in
    hpux*)
	AC_PROGRAMS_CHECK(FC, $fc fort77 fortc f77)dnl
	PC_ENSURE(FFLAGS, +U77)dnl
	PC_ENSURE(LD_FORTRAN, -lU77)dnl
	;;
    dgux*)
	AC_PROGRAMS_CHECK(FC, $fc ghf77 f77)dnl
	PC_DEFAULT(LD_FORTRAN)dnl
	;;
    *)
	AC_PROGRAMS_CHECK(FC, $fc f77 f90 cf77)dnl
	PC_DEFAULT(LD_FORTRAN)dnl
	;;
  esac
  if test -z "$FC"; then
    PC_NEED_VALUE(FC, [FORTRAN compiler], /bin/f77)dnl
  fi
else
  AC_MSG_CHECKING(for FORTRAN compiler)
  AC_MSG_RESULT($FC)
fi
AC_SUBST(FC)
])

dnl Form a library reference for the linker/loader
dnl
dnl On a SunOS 5 system, a `-R<dir>' is added in addition to a `-L<dir>'
dnl in order to make the utility independent of LD_LIBRARY_PATH (is this
dnl a good idea?) and to ensure that it'll run regardless of who
dnl executes it.
dnl
dnl PC_LINK_REF(varname, libdir, libname)
dnl    sets varname to library reference (e.g., -L$libdir -l$libname)
dnl
dnl Example: PC_LINK_REF(PC_LD_MATH, /upc/netcdf/lib, netcdf)
dnl
AC_DEFUN([PC_LINK_REF], [dnl
    AC_REQUIRE([PC_OS]) dnl
    case "${OS}$OS_MAJOR" in
	unicos*)
	    case "$2" in
		'') $1="-l $3";;
		*)  $1="-L $2 -l $3";;
	    esac
	    ;;
	sunos5*)
	    case "$2" in
		'') $1="-l$3";;
		*)  $1="-R$2 -L$2 -l$3";;
	    esac
	    ;;
	*)
	    case "$2" in
		'') $1="-l$3";;
		*)  $1="-L$2 -l$3";;
	    esac
	    ;;
    esac
])

dnl Check for a library that contains a function.
dnl
dnl PC_CHECK_LIB(varname, func, dir, lib, flag, libname, other_libs)
dnl
dnl For example: PC_CHECK_LIB(NCOPTS,ncopen,/usr/local/lib,netcdf,HAVE_NETCDF,libnetcdf.a)
dnl
dnl   checks that /usr/local/lib/libnetcdf.a can be linked with 
dnl   function ncopen. If so, NCOPTS is set to the appropriate linker
dnl   options, and CPPFLAGS has -DHAVE_NETCDF. NCOPTS is created as an output variable
dnl
dnl
AC_DEFUN([PC_CHECK_LIB],
[dnl
    AC_REQUIRE([PC_OS])dnl
    AC_MSG_CHECKING(for $6)
    LIBS_save=$LIBS
    CPPFLAGS_save=$CPPFLAGS
    LIBS=
    PC_LINK_REF(LIBS,$3,$4)
    PC_OTHER_LIBS=$7
    PC_ENSURE(LIBS,[$PC_OTHER_LIBS])
dnl
dnl Horrible kluge to circumvent the SunOS 4.1.X missing MAIN_ bug
dnl This depends on AC_TRY_LINK generating a test routine 't()...'
dnl
    if test "$OS" = sunos4; then
       CPPFLAGS=$CPPFLAGS" -Dt=MAIN_"
    fi
    AC_TRY_LINK(, [$2();], 
		  [
			AC_MSG_RESULT(yes)
			$1=$LIBS
			found=yes
		  ],
		  [
			AC_MSG_RESULT(no)
			$1=
			found=no
		  ])dnl
    LIBS=$LIBS_save
    CPPFLAGS=$CPPFLAGS_save
    if test "$found" = yes; then
       PC_ENSURE(CPPFLAGS, -D$5)
    fi
    AC_SUBST($1) dnl
])

dnl
dnl Set default directory for C include file, and check
dnl that the file is present. If so, set VAR to value
dnl set by --with-PACKAGE option, or default if not set
dnl
dnl PC_CHECK_C_HEADER(var, default_dir, package, header, msg)
dnl
dnl Example: PC_CHECK_C_HEADER(NCINC, /usr/local/include, ncinc, netcdf.h, dnl
dnl 		[--with-ncinc=DIR        netcdf.h is in DIR (default [NCINC])])
dnl
AC_DEFUN([PC_CHECK_C_HEADER],
[dnl
    PC_DEFAULT($1,$2)
    AC_ARG_WITH($3,[$5], dnl
        [$1=$withval]) dnl
    echo "checking that $4 is in $$1 (modify with --with-$3=DIR)"
    AC_CHECK_HEADER($$1/$4, dnl
      [PC_ENSURE(CFLAGS$3, -I$$1)], dnl
      [$1=])
])
dnl
dnl Set default directory for FORTRAN include file, and check
dnl that the file is present. If so, set VAR to value
dnl set by --with-PACKAGE option, or default if not set
dnl
dnl PC_CHECK_FORT_HEADER(var, default_dir, package, header, msg)
dnl
dnl Example: PC_CHECK_FORT_HEADER(NCINCF, /usr/local/include, ncincf, netcdf.inc, dnl
dnl 		[--with-ncincf=DIR       netcdf.inc is in DIR (default [NCINCF])])
dnl
AC_DEFUN([PC_CHECK_FORT_HEADER],
[dnl
    PC_DEFAULT($1,$2)
    AC_ARG_WITH($3,[$5], dnl
        [$1=$withval]) dnl
    echo "checking that $4 is in $$1 (modify with --with-$3=DIR)"
    AC_MSG_CHECKING(for $4)
    if test -r "$$1/$4"; then
      PC_ENSURE(FFLAGS, -I$$1)
      AC_MSG_RESULT(yes)
    else
      AC_MSG_RESULT(no)
    fi
])
dnl
dnl Set default directory for a library, and check
dnl that the library is present. If so, set VAR to value
dnl set by --with-PACKAGE option, or default if not set
dnl Sets optvar to appropriate library linker options and
dnl sets -Dflag if library is present.
dnl
dnl PC_CHECK_ARG_LIB(var, default_dir, package, lib, routine, optvar, flag, libname, msg, other_libs)
dnl
dnl Example: PC_CHECK_ARG_LIB(NCLIB, /usr/local/lib, [nclib], netcdf, ncopen, NCOPTS, HAVE_NETCDF, libnetcdf.a, dnl
dnl             [--with-nclib=DIR        libnetcdf.a is in DIR (default [NCLIB])])
dnl
AC_DEFUN([PC_CHECK_ARG_LIB],
[dnl
    PC_DEFAULT($1,$2)
    AC_ARG_WITH($3,[$9], dnl
        [$1=$withval]) dnl
    echo "checking that $8 is in $$1 (modify with --with-$3=DIR)"
    PC_CHECK_LIB($6, $5, $$1, $4, $7, $8, $10)
])
dnl
dnl Append to VAR the link line for Fortran dependencies
dnl
dnl PC_FORT_DEPEND(var)
dnl
AC_DEFUN([PC_FORT_DEPEND],
[dnl
    AC_REQUIRE([PC_OS])
    case "$OS" in
	 aix)
		$1=$$1" -lc -lm"
		;;
	 hpux*)
		$1=$$1" -lU77 -lcl -lm"
		;;
	 irix*)
                $1=$$1" -lF77 -lm -lU77 -lI77 -lisam -lc"
		;;
	 linux*)
dnl                $1=$$1" -lfio -lf77math -lU77 -lm"
                $1=$$1" -L$PGI/linux86/$PGI_VERSION/lib -lpgftnrtl -lpgc -lm"
		;;
	 osf*)
		$1=$$1" -lfor -lc -lm"
		;;
	 sunos4)
		$1=$$1" -lF77 -lc -lm"
		;;
	 sunos5)
		$1=$$1" -lnsl -lF77 -lc -lm -lsunmath"
		;;
	 unicos)
		$1=$$1" -lf -lc -lm"
		;;
	 *)
		;;
    esac
])
dnl
dnl Set default directory for a FORTRAN-dependent library, and check
dnl that the library is present. If so, set VAR to value
dnl set by --with-PACKAGE option, or default if not set
dnl Sets optvar to appropriate library linker options and
dnl sets -Dflag if library is present.
dnl
dnl PC_CHECK_FORT_LIB(var, default_dir, package, lib, routine, optvar, flag, libname, msg, other_libs)
dnl
dnl Example: PC_CHECK_FORT_LIB(NCLIB, /usr/local/lib, [nclib], netcdf, ncopen, NCOPTS, HAVE_NETCDF, libnetcdf.a, dnl
dnl             [--with-nclib=DIR        libnetcdf.a is in DIR (default [NCLIB])])
dnl
AC_DEFUN([PC_CHECK_FORT_LIB],
[dnl
    AC_REQUIRE([PC_OS])
    PC_FORT_EXTRA_LIBS=$10
    PC_FORT_DEPEND(PC_FORT_EXTRA_LIBS)
    PC_DEFAULT($1,$2)
    AC_ARG_WITH($3,[$9], dnl
        [$1=$withval]) dnl
    echo "checking that $8 is in $$1 (modify with --with-$3=DIR)"
dnl
dnl Ugly to link with FORTRAN-dependent libraries using C on certain 
dnl platforms, so just check for library existence.
dnl
    case "$OS" in
	 aix)
		AC_MSG_CHECKING(for $8)
		if test -r "$$1/$8"; then
		  PC_ENSURE(LIBS,[-L$$1 -l$4])
		  PC_ENSURE(LIBS,[$PC_FORT_EXTRA_LIBS])
		  PC_ENSURE(CPPFLAGS, -D$7)
		  AC_MSG_RESULT(yes)
		else
		  AC_MSG_RESULT(no)
		fi
		;;
	 osf*)
		AC_MSG_CHECKING(for $8)
		if test -r "$$1/$8"; then
		  PC_ENSURE(LIBS,[-L$$1 -l$4])
		  PC_ENSURE(LIBS,[$PC_FORT_EXTRA_LIBS])
		  PC_ENSURE(CPPFLAGS, -D$7)
		  AC_MSG_RESULT(yes)
		else
		  AC_MSG_RESULT(no)
		fi
		;;
	 linux*)
		AC_MSG_CHECKING(for $8 on linux)
		if test -r "$$1/$8"; then
		  PC_ENSURE(LIBS,[-L$$1 -l$4])
		  PC_ENSURE(LIBS,[$PC_FORT_EXTRA_LIBS])
		  PC_ENSURE(CPPFLAGS, -D$7)
		  AC_MSG_RESULT(yes)
		else
		  AC_MSG_RESULT(no)
		fi
		;;
	 *)
		PC_CHECK_LIB($6, $5, $$1, $4, $7, $8, $PC_FORT_EXTRA_LIBS)
		;;
    esac
])
