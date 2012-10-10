dnl                                                -*- Autoconf -*-
dnl  Process this file with autoconf to produce a configure script.

dnl AC_PREREQ(2.59)
AC_INIT(cmor, 2.8.0, doutriaux1@llnl.gov)

GIT_TAG=`./get_git_version.sh`

AC_CANONICAL_TARGET
echo "TARGET: "${target_os}
RTAG="none"
case ${target_os} in
 linux-*) 
   RTAG="-Wl,-rpath="
 ;;
 darwin*)
   RTAG="-R"
 ;;
esac

CMOR_VERSION=${PACKAGE_VERSION}" (commit: ${GIT_TAG})"

AC_PREFIX_DEFAULT([/usr/local/cmor])
AC_CONFIG_FILES([Makefile setup.py compile_line.txt include/cmor_locale.h])


INCFILES="include/cmor.h  include/cmor_func_def.h include/cmor_md5.h "`ls -x --width=10000 include/cdTime/*.h include/cdTime/cdunifpp/*.h`
AC_SUBST(PATH)
AC_SUBST(NCCFLAGS)
AC_SUBST(NCLDFLAGS)
AC_SUBST(ZFLAGS)
AC_SUBST(ZLDFLAGS)
AC_SUBST(UDUNITS2FLAGS)
AC_SUBST(UDUNITS2LDFLAGS)
AC_SUBST(UUIDFLAGS)
AC_SUBST(UUIDLDFLAGS)
AC_SUBST(LIBSOURCES)
AC_SUBST(LIBFSOURCES)
AC_SUBST(FC)
AC_SUBST(FFLAGS)
AC_SUBST(FCFLAGS)
AC_SUBST(LDFLAGS)
AC_SUBST(CFLAGS)
AC_SUBST(INCFILES)
AC_SUBST(TEST_FORTRAN)
AC_SUBST(LIBFILES)
AC_SUBST(VERB)
AC_SUBST(MODFILES)
AC_SUBST(MACROS)
AC_SUBST(DEBUG)
AC_SUBST(OK_COLOR)
AC_SUBST(NO_COLOR)

MACROS=""
DEBUG=""

OK_COLOR=""
NO_COLOR=""

AC_ARG_ENABLE([verbose-test],[AS_HELP_STRING([--enable-verbose-test],[enable verbose testing (default off)])])
if test "-"${enable_verbose_test} == "-yes" ; then
  VERB=""
else
  VERB=">/dev/null 2>/dev/null"
fi
if test "-"${enable_verbose_test} == "-yes" ; then
  VERB=""
else
  VERB=">/dev/null 2>/dev/null"
fi

AC_ARG_ENABLE([color],[AS_HELP_STRING([--enable-color],[enable colored output (default on)])],[
if test "x${enableval}" == "xyes" ; then
  MACROS=${MACROS}" -DCOLOREDOUTPUT"
  NO_COLOR="NO_COLOR2"
  OK_COLOR="OK_COLOR2"
fi
],[ 
MACROS=${MACROS}" -DCOLOREDOUTPUT" 
NO_COLOR="NO_COLOR2"
OK_COLOR="OK_COLOR2"
])
AC_ARG_ENABLE([debug],[AS_HELP_STRING([--enable-debug],[enable debug flag while commpiling (default on)])],[enable_debug=yes],[enable_debug=yes])
if test ${enable_debug} == "yes" ; then
  DEBUG="-g"
fi

LIBFILES="cmor.o cmor_axes.o cmor_variables.o cmor_tables.o cdTimeConv.o cdUtil.o timeConv.o timeArith.o cmor_grids.o cmor_md5.o "
LIBFFILES="cmor_cfortran_interface.o cmor_fortran_interface.o"

dnl ##########################################################
dnl  Turn on/off fortran
dnl ##########################################################
AC_ARG_ENABLE([fortran],[AS_HELP_STRING([--enable-fortran],[enable fortran api (default will try)])])
enable_fortran="-"${enable_fortran}
TEST_FORTRAN="test_fortran"
if test ${enable_fortran} == "-no" ; then
  FC=""
  F77=""
  FFLAGS=""
  FCFLAGS=""
  TEST_FORTRAN=" "
  MODFILES=" "
else
  LIBFILES=${LIBFILES}" "${LIBFFILES}
  MODFILES="cmor_users_functions.*"
fi

CDATPREFIX=" "
MAKETESTPYTHON=" "
dnl turn on/off python
AC_ARG_WITH([cdat],[AS_HELP_STRING([--with-cdat],[enable support for cdat put path root (w/o bin/cdat)])],[],[with_cdat="no"])
with_cdatb="-"${with_cdat}
if [ test ${with_cdatb} != "-yes"  ] ; then
  if [ test ${with_cdatb} != "-no" ] ; then
    PYTHONEXEC=${with_cdat}/bin/cdat
    MAKEDEPPYTHON="python"
    MAKETESTPYTHON="test_python"
    CDATPREFIX="--prefix="${with_cdat}
  else
    PYTHONEXEC=" "
    MAKEDEPPYTHON=" "
  fi
else
  PYTHONEXEC="cdat"
  MAKEDEPPYTHON="python"
  MAKETESTPYTHON="test_python"
fi
AC_ARG_WITH([python],[AS_HELP_STRING([--with-python],[enable support for python put path root (w/o bin/python)])],[],[with_python="no"])
with_pythonb="-"${with_python}
if [ test ${with_pythonb} != "-yes"  ] ; then
  if [ test ${with_pythonb} != "-no" ] ; then
    PYTHONEXEC=${with_python}/bin/python
    MAKEDEPPYTHON="python"
  fi
else
  PYTHONEXEC="python"
  MAKEDEPPYTHON="python"
fi

AC_SUBST(CDATPREFIX)

AC_SUBST(PYTHONEXEC)
AC_SUBST(MAKEDEPPYTHON)
AC_SUBST(MAKETESTPYTHON)

dnl  basic test
AC_PROG_CC
AC_PROG_CPP
AC_PROG_LN_S
AC_PROG_SED
AC_PROG_MKDIR_P
AC_PROG_RANLIB
if test ${enable_fortran} != "-no" ; then
  AC_PROG_FC
  AC_FC_LIBRARY_LDFLAGS
fi

LDFLAGS=${LDFLAGS}" -lm "


AC_ARG_WITH([uuid],[AS_HELP_STRING([--with-uuid],[enable support for uuid in none standard location])],[],[with_uuid="no"])
with_uuidb="-"${with_uuid}
if [ test ${with_uuidb} != "-yes" ] ; then
  if [ test ${with_uuidb} != "-no" ] ; then
   UUIDFLAGS=" -I${with_uuid}/include"
   if [ test ${RTAG} != "none" ] ; then
     UUIDLDFLAGS=" -L${with_uuid}/lib  ${RTAG}${with_uuid}/lib -luuid"
   else
     UUIDLDFLAGS=" -L${with_uuid}/lib -luuid"
   fi
  else
    UUIDLDFLAGS="-luuid"
    UUIDFLAGS=""
  fi
else
  UUIDLDFLAGS="-luuid"
  UUIDFLAGS=""
fi
AC_CHECK_LIB([uuid],[uuid_create],[],[AC_ERROR([Could not get a working uuid])],[ ${UUIDFLAGS} ${UUIDLDFLAGS}  ])

LIBS=""
AC_ARG_WITH([udunits2],[AS_HELP_STRING([--with-udunits2],[enable support for udunits2 in none standard location])],[],[with_udunits2="no"])
with_udunits2b="-"${with_udunits2}
if [ test ${with_udunits2b} != "-yes" ] ; then
  if [ test ${with_udunits2b} != "-no" ] ; then
   UDUNITS2FLAGS=" -I${with_udunits2}/include"
   if [ test ${RTAG} != "none" ] ; then
     UDUNITS2LDFLAGS=" -L${with_udunits2}/lib  ${RTAG}${with_udunits2}/lib -ludunits2 -lexpat"
   else
     UDUNITS2LDFLAGS=" -L${with_udunits2}/lib -ludunits2 -lexpat"
   fi
  else
    UDUNITS2FLAGS="-ludunits2"
    UDUNITS2LDFLAGS=""
  fi
else
  UDUNITS2FLAGS="-ludunits2"
  UDUNITS2LDFLAGS=""
fi
AC_CHECK_LIB([udunits2],[ut_parse],[],[AC_ERROR([Could not get a working udunits2])],[ ${UDUNITS2FLAGS} ${UDUNITS2LDFLAGS} -lm  ])
LIBS=""


AC_ARG_WITH([netcdf],[AS_HELP_STRING([--with-netcdf],[enable support for NetCDF in none standard location])],[],[with_netcdf="no"])
with_netcdfb="-"${with_netcdf}
if [ test ${with_netcdfb} != "-yes" ]; then
  if [ test ${with_netcdfb} != "-no" ] ; then
    AC_CHECK_FILE([${with_netcdf}/bin/nc-config],[HAS_NCCONFIG=yes],[HAS_NCCONFIG=no])
    if [ test ${HAS_NCCONFIG} == "yes" ] ; then 
      NCCFLAGS=`${with_netcdf}/bin/nc-config --cflags`
      NCLDFLAGS=`${with_netcdf}/bin/nc-config --libs`
    else 
#echo "CRAP!"
     NCCFLAGS=" -I${with_netcdf}/include"
     if [ test ${RTAG} != "none" ] ; then
      NCLDFLAGS=" -L${with_netcdf}/lib  ${RTAG}${with_netcdf}/lib -lnetcdf"
     else
      NCLDFLAGS=" -L${with_netcdf}/lib -lnetcdf"
     fi
    fi
  else
    NCCFLAGS="-lnetcdf"
    NCLDFLAGS=""
  fi
else
  AC_PATH_PROG(NCCONFIG,nc-config,"no")
  if [ test NCCONFIG != "no" ] ; then
   NCCFLAGS=`${NCCONFIG} --cflags`
   NCLDFLAGS=`${NCCONFIG} --libs`
  else
   NCCFLAGS="-lnetcdf"
   NCLDFLAGS=""
  fi
fi

CFLAGS=${NCCFLAGS}
LDFLAGS=${NCLDFLAGS}

dnl ok that this point we need to see if it is NC3 or NC4
AC_CHECK_LIB([netcdf],[nc_inq_var_deflate],[HAS_NC4="yes"],[HAS_NC4="no"],[${NCFLAGS} ${NCLDFLAGS} ])
BUILD_NC3="no"
HAS_NC3="no"
OLIBS=${LIBS}
LIBS="-lnetcdf "${NCCFLAGS}" "${NCLDFLAGS}
if [ test ${HAS_NC4} == "no" ] ; then
  dnl ok couldn't get nc4, lets see if that is a nc3
  AC_CHECK_LIB([netcdf],[nc_open],[HAS_NC="yes"],[HAS_NC="no"],[${NCFLAGS} ${NCLDFLAGS} ])
  if [ test ${HAS_NCCONFIG} == "yes" ] ; then
    HAS_NC3="yes"
  else
    dnl need to test if it is version 3.6.3
    AC_MSG_CHECKING([if you have netcdf 3.6.3)])
    AC_RUN_IFELSE(
        [AC_LANG_PROGRAM([
        #include <stdio.h>
        #include <string.h>
	#include <stdlib.h>
        #include <netcdf.h>
        ],
        [ [

     char version[50];
     int major,minor,patch;
     strncpy(version,nc_inq_libvers(),50);
     sscanf(version,"%*c%1d%*c%1d%*c%1d%*s",&major,&minor,&patch);
     if ((major!=3) && (minor!=6) && (patch!=3)) return 1;
     return 0;
        ]])],[BUILD_NC3="no"],[BUILD_NC3="yes"])
    if [test ${BUILD_NC3} == "yes" ] ; then
       AC_MSG_RESULT(["no"])
       AC_ERROR([Could not get a working NetCDF])
    else
       AC_MSG_RESULT(["yes"])
    fi
    HAS_NC3="yes"
  fi
fi
LIBS=${OLIBS}
CFLAGS=${CFLAGS}" -Iinclude -Iinclude/cdTime"
MAINDIR=`pwd`

# Library sources
LIBSOURCES="Src/cmor.c Src/cmor_variables.c Src/cmor_axes.c Src/cmor_tables.c Src/cmor_grids.c Src/cdTime/cdTimeConv.c Src/cdTime/cdUtil.c Src/cdTime/timeConv.c Src/cdTime/timeArith.c Src/cmor_cfortran_interface.c Src/cmor_md5.c"
LIBFSOURCES="Src/cmor_fortran_interface.f90"


AC_OUTPUT

AC_MSG_NOTICE([])
AC_MSG_NOTICE([************************************************************************])
AC_MSG_NOTICE([                              SUMMARY])
AC_MSG_NOTICE([                            VERSION: ${CMOR_VERSION}])
AC_MSG_NOTICE([************************************************************************])
AC_MSG_NOTICE([])
AC_MSG_NOTICE([........................................................................])
AC_MSG_NOTICE([...                             COMPILERS])
AC_MSG_NOTICE([........................................................................])
AC_MSG_NOTICE([])
AC_MSG_NOTICE([            FORTRAN])
if test ${enable_fortran} == "-no" ; then
  AC_MSG_NOTICE([        DISABLED])
else
  AC_MSG_NOTICE([ FC=${FC}])
  AC_MSG_NOTICE([ F77=${F77}])
  AC_MSG_NOTICE([ FFLAGS=${FFLAGS}])
  AC_MSG_NOTICE([ FCFLAGS=${FCFLAGS}])
fi
AC_MSG_NOTICE([........................................................................])
AC_MSG_NOTICE([...                             EXTERNALS                            ...])
AC_MSG_NOTICE([........................................................................])
if [test ${SZLIBFLAGS}!="nosz" ]; then
  AC_MSG_NOTICE([])
  AC_MSG_NOTICE([SZLIB])
  AC_MSG_NOTICE([ CFLAGS  : ${SZLIBFLAGS}])
  AC_MSG_NOTICE([ LDFLAGS : ${SZLIBLDFLAGS}])
fi
AC_MSG_NOTICE([])
AC_MSG_NOTICE([UUID])
AC_MSG_NOTICE([ CFLAGS  : ${UUIDFLAGS}])
AC_MSG_NOTICE([ LDFLAGS : ${UUIDLDFLAGS}])
AC_MSG_NOTICE([])
AC_MSG_NOTICE([UDUNITS2])
AC_MSG_NOTICE([ CFLAGS  : ${UDUNITS2FLAGS}])
AC_MSG_NOTICE([ LDFLAGS : ${UDUNITS2LDFLAGS}])
AC_MSG_NOTICE([])
if [test ${BUILD_NC3} == "no" ] ; then 
  if [test ${HAS_NC4} == "yes" ] ; then
     AC_MSG_NOTICE([NetCDF4])
     AC_MSG_NOTICE([ CFLAGS  : ${NCCFLAGS}])
     AC_MSG_NOTICE([ LDFLAGS : ${NCLDFLAGS}])
  else
     AC_MSG_NOTICE([NetCDF3.6.3])
     AC_MSG_NOTICE([ CFLAGS  : ${NCCFLAGS}])
     AC_MSG_NOTICE([ LDFLAGS : ${NCLDFLAGS}])
  fi
else
     AC_MSG_NOTICE([No valid NetCDF found will build NetCDF3.6.3 for you])
fi
AC_MSG_NOTICE([])
AC_MSG_NOTICE([ PYTHON])
if test "-"${PYTHONEXEC} == "-" ; then
  AC_MSG_NOTICE([ Not built, not used it tests])
else
  AC_MSG_NOTICE([ Built using python: ${PYTHONEXEC}])
fi
AC_MSG_NOTICE([])
AC_MSG_NOTICE([Type "make" to build cmor])
AC_MSG_NOTICE([Type "make install" to build and install cmor to ${prefix}])
AC_MSG_NOTICE([])
AC_MSG_NOTICE([Type "make test" to build and test cmor code only])
AC_MSG_NOTICE([Type "make test_C" to build cmor and test C only])
if test "-"${MAKETESTPYTHON} != "-" ; then
  AC_MSG_NOTICE([Will also test for python since your are using CDAT])
AC_MSG_NOTICE([Type "make test_python" to build and test cmor python only])
fi
if test ${enable_fortran} != "-no" ; then
  AC_MSG_NOTICE([Type "make test_fortran" to build cmor and test fortran only])
else
  AC_MSG_NOTICE([will also test for fortran])
fi
AC_MSG_NOTICE([])
AC_MSG_NOTICE([You can compile C or FORTRAN code using the compilation lines shown in file: compile_line.txt and printed bellow:])
AC_MSG_NOTICE([])
AC_MSG_NOTICE([])


cat compile_line.txt
