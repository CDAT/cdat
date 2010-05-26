dnl                                                -*- Autoconf -*-
dnl  Process this file with autoconf to produce a configure script.

dnl AC_PREREQ(2.59)

AC_INIT(cdat, 6.0.alpha, doutriaux1@llnl.gov)
SVN_TAG="rev"`./get_svn_version.sh`
if test ${SVN_TAG} == "rev" ; then
  SVN_TAG=""
fi
CDAT_VERSION=${PACKAGE_VERSION}${SVN_TAG}
AC_PREFIX_DEFAULT([/opt/cdat])
AC_CONFIG_FILES([Makefile site.cfg README.txt Scripts/cdat Scripts/vcdat exsrc/CMakeCache.txt checked_get.sh],[chmod +x Scripts/cdat Scripts/vcdat checked_get.sh])


dnl  Lib VERSION NUMBERS TO CHECK AGAINST
PYTHON_MAJOR=2
PYTHON_MINOR=6
PYTHON_PATCH=0
dnl  Lib VERSION NUMBERS WE PROVIDE
PYTHON_MAJOR_SRC=2
PYTHON_MINOR_SRC=6
PYTHON_PATCH_SRC=5
PYTHON_URL=http://python.org/ftp/python/${PYTHON_MAJOR_SRC}.${PYTHON_MINOR_SRC}.${PYTHON_PATCH_SRC}
PYTHON_GZ=Python-${PYTHON_MAJOR_SRC}.${PYTHON_MINOR_SRC}.${PYTHON_PATCH_SRC}.tgz
AC_SUBST(PREFIX_PATH)
AC_SUBST(PYTHON_MAJOR)
AC_SUBST(PYTHON_MINOR)
AC_SUBST(PYTHON_PATCH)
AC_SUBST(PYTHON_MAJOR_SRC)
AC_SUBST(PYTHON_MINOR_SRC)
AC_SUBST(PYTHON_PATCH_SRC)
AC_SUBST(PYTHON_URL)
AC_SUBST(PYTHON_GZ)
PYVER=${PYTHON_MAJOR_SRC}.${PYTHON_MINOR_SRC}
AC_SUBST(PYVER)
XML_MAJOR=2
XML_MINOR=6
XML_PATCH=27
XML_MAJOR_SRC=2
XML_MINOR_SRC=7
XML_PATCH_SRC=6
XML_URL=ftp://xmlsoft.org/libxml2
XML_GZ=libxml2-${XML_MAJOR_SRC}.${XML_MINOR_SRC}.${XML_PATCH_SRC}.tar.gz
AC_SUBST(XML_URL)
AC_SUBST(XML_GZ)
AC_SUBST(XML_MAJOR)
AC_SUBST(XML_MINOR)
AC_SUBST(XML_PATCH)
AC_SUBST(XML_MAJOR_SRC)
AC_SUBST(XML_MINOR_SRC)
AC_SUBST(XML_PATCH_SRC)
XSLT_MAJOR=1
XSLT_MINOR=1
XSLT_PATCH=22
XSLT_MAJOR_SRC=1
XSLT_MINOR_SRC=1
XSLT_PATCH_SRC=26
XSLT_URL=ftp://xmlsoft.org/libxml2
XSLT_GZ=libxslt-${XSLT_MAJOR_SRC}.${XSLT_MINOR_SRC}.${XSLT_PATCH_SRC}.tar.gz
AC_SUBST(XSLT_URL)
AC_SUBST(XSLT_GZ)
AC_SUBST(XSLT_MAJOR)
AC_SUBST(XSLT_MINOR)
AC_SUBST(XSLT_PATCH)
AC_SUBST(XSLT_MAJOR_SRC)
AC_SUBST(XSLT_MINOR_SRC)
AC_SUBST(XSLT_PATCH_SRC)
PKG_MAJOR=0
PKG_MINOR=9
PKG_PATCH=0
PKG_MAJOR_SRC=0
PKG_MINOR_SRC=23
PKG_PATCH_SRC=0
PKG_URL=http://pkgconfig.freedesktop.org/releases/
PKG_GZ=pkg-config-${PKG_MAJOR_SRC}.${PKG_MINOR_SRC}.tar.gz
AC_SUBST(PKG_URL)
AC_SUBST(PKG_GZ)
AC_SUBST(PKG_MAJOR)
AC_SUBST(PKG_MINOR)
AC_SUBST(PKG_PATCH)
AC_SUBST(PKG_MAJOR_SRC)
AC_SUBST(PKG_MINOR_SRC)
AC_SUBST(PKG_PATCH_SRC)
CAIRO_MAJOR=1
CAIRO_MINOR=8
CAIRO_PATCH=8
CAIRO_MAJOR_SRC=1
CAIRO_MINOR_SRC=8
CAIRO_PATCH_SRC=10
CAIRO_URL=http://cairographics.org/releases
CAIRO_GZ=cairo-${CAIRO_MAJOR_SRC}.${CAIRO_MINOR_SRC}.${CAIRO_PATCH_SRC}.tar.gz
AC_SUBST(CAIRO_URL)
AC_SUBST(CAIRO_GZ)
AC_SUBST(CAIRO_MAJOR)
AC_SUBST(CAIRO_MINOR)
AC_SUBST(CAIRO_PATCH)
AC_SUBST(CAIRO_MAJOR_SRC)
AC_SUBST(CAIRO_MINOR_SRC)
AC_SUBST(CAIRO_PATCH_SRC)
FT_MAJOR=9
FT_MINOR=7
FT_PATCH=3
FT_MAJOR_SRC=2
FT_MINOR_SRC=3
FT_PATCH_SRC=12
FT_URL=http://download.savannah.gnu.org/releases-noredirect/freetype
FT_GZ=freetype-${FT_MAJOR_SRC}.${FT_MINOR_SRC}.${FT_PATCH_SRC}.tar.gz
AC_SUBST(FT_URL)
AC_SUBST(FT_GZ)
AC_SUBST(FT_MAJOR)
AC_SUBST(FT_MINOR)
AC_SUBST(FT_PATCH)
AC_SUBST(FT_MAJOR_SRC)
AC_SUBST(FT_MINOR_SRC)
AC_SUBST(FT_PATCH_SRC)
FTCFG_MAJOR=2
FTCFG_MINOR=4
FTCFG_PATCH=2
FTCFG_MAJOR_SRC=2
FTCFG_MINOR_SRC=8
FTCFG_PATCH_SRC=0
FTCFG_URL=http://fontconfig.org/release
FTCFG_GZ=fontconfig-${FTCFG_MAJOR_SRC}.${FTCFG_MINOR_SRC}.${FTCFG_PATCH_SRC}.tar.gz
AC_SUBST(FTCFG_URL)
AC_SUBST(FTCFG_GZ)
AC_SUBST(FTCFG_MAJOR)
AC_SUBST(FTCFG_MINOR)
AC_SUBST(FTCFG_PATCH)
AC_SUBST(FTCFG_MAJOR_SRC)
AC_SUBST(FTCFG_MINOR_SRC)
AC_SUBST(FTCFG_PATCH_SRC)
PIX_MAJOR=0
PIX_MINOR=16
PIX_PATCH=2
PIX_MAJOR_SRC=0
PIX_MINOR_SRC=17
PIX_PATCH_SRC=10
PIX_URL=http://cairographics.org/releases
PIX_GZ=pixman-${PIX_MAJOR_SRC}.${PIX_MINOR_SRC}.${PIX_PATCH_SRC}.tar.gz
AC_SUBST(PIX_URL)
AC_SUBST(PIX_GZ)
AC_SUBST(PIX_MAJOR)
AC_SUBST(PIX_MINOR)
AC_SUBST(PIX_PATCH)
AC_SUBST(PIX_MAJOR_SRC)
AC_SUBST(PIX_MINOR_SRC)
AC_SUBST(PIX_PATCH_SRC)
CURL_MAJOR=7
CURL_MINOR=19
CURL_PATCH=7
CURL_MAJOR_SRC=7
CURL_MINOR_SRC=20
CURL_PATCH_SRC=0
CURL_URL=http://curl.haxx.se/download
CURL_GZ=curl-${CURL_MAJOR_SRC}.${CURL_MINOR_SRC}.${CURL_PATCH_SRC}.tar.gz
AC_SUBST(CURL_URL)
AC_SUBST(CURL_GZ)
AC_SUBST(CURL_MAJOR)
AC_SUBST(CURL_MINOR)
AC_SUBST(CURL_PATCH)
AC_SUBST(CURL_MAJOR_SRC)
AC_SUBST(CURL_MINOR_SRC)
AC_SUBST(CURL_PATCH_SRC)
PNG_MAJOR=1
PNG_MINOR=4
PNG_PATCH=1
PNG_MAJOR_SRC=1
PNG_MINOR_SRC=4
PNG_PATCH_SRC=2
PNG_URL=ftp://ftp.simplesystems.org/pub/libpng/png/src
PNG_GZ=libpng-${PNG_MAJOR_SRC}.${PNG_MINOR_SRC}.${PNG_PATCH_SRC}.tar.gz
AC_SUBST(PNG_URL)
AC_SUBST(PNG_GZ)
AC_SUBST(PNG_MAJOR)
AC_SUBST(PNG_MINOR)
AC_SUBST(PNG_PATCH)
AC_SUBST(PNG_MAJOR_SRC)
AC_SUBST(PNG_MINOR_SRC)
AC_SUBST(PNG_PATCH_SRC)
PMW_MAJOR=1
PMW_MINOR=3
PMW_MAJOR_SRC=1
PMW_MINOR_SRC=3
PMW_PATCH_SRC=2
PMW_URL=http://downloads.sourceforge.net/project/pmw/Pmw/Pmw.${PMW_MAJOR_SRC}.${PMW_MINOR_SRC}.${PMW_PATCH_SRC}
PMW_GZ=Pmw.${PMW_MAJOR_SRC}.${PMW_MINOR_SRC}.${PMW_PATCH_SRC}.tar.gz
AC_SUBST(PMW_URL)
AC_SUBST(PMW_GZ)
AC_SUBST(PMW_MAJOR)
AC_SUBST(PMW_MINOR)
AC_SUBST(PMW_MAJOR_SRC)
AC_SUBST(PMW_MINOR_SRC)
NUMPY_MAJOR=1
NUMPY_MINOR=2
NUMPY_PATCH=1
NUMPY_MAJOR_SRC=1
NUMPY_MINOR_SRC=4
NUMPY_PATCH_SRC=1
NUMPY_URL=http://downloads.sourceforge.net/project/numpy/NumPy/${NUMPY_MAJOR_SRC}.${NUMPY_MINOR_SRC}.${NUMPY_PATCH_SRC}
NUMPY_GZ=numpy-${NUMPY_MAJOR_SRC}.${NUMPY_MINOR_SRC}.${NUMPY_PATCH_SRC}.tar.gz
AC_SUBST(NUMPY_URL)
AC_SUBST(NUMPY_GZ)
AC_SUBST(NUMPY_MAJOR)
AC_SUBST(NUMPY_MINOR)
AC_SUBST(NUMPY_PATCH)
AC_SUBST(NUMPY_MAJOR_SRC)
AC_SUBST(NUMPY_MINOR_SRC)
AC_SUBST(NUMPY_PATCH_SRC)
TCLTK_MAJOR=8
TCLTK_MINOR=5
TCLTK_PATCH=4
TCLTK_MAJOR_SRC=8
TCLTK_MINOR_SRC=5
TCLTK_PATCH_SRC=8
TCLTK_VERSION=${TCLTK_MAJOR_SRC}.${TCLTK_MINOR_SRC}
TCLTK_URL=http://prdownloads.sourceforge.net/tcl
TCL_GZ=tcl${TCLTK_MAJOR_SRC}.${TCLTK_MINOR_SRC}.${TCLTK_PATCH}-src.tar.gz
TK_GZ=tk${TCLTK_MAJOR_SRC}.${TCLTK_MINOR_SRC}.${TCLTK_PATCH}-src.tar.gz
AC_SUBST(TCLTK_URL)
AC_SUBST(TCL_GZ)
AC_SUBST(TK_GZ)
AC_SUBST(TCLTK_MAJOR)
AC_SUBST(TCLTK_MINOR)
AC_SUBST(TCLTK_PATCH)
AC_SUBST(TCLTK_MAJOR_SRC)
AC_SUBST(TCLTK_MINOR_SRC)
AC_SUBST(TCLTK_PATCH_SRC)
AC_SUBST(TCLTK_VERSION)
RL_MAJOR=5
RL_MINOR=2
RL_MAJOR_SRC=6
RL_MINOR_SRC=1
READLINE_URL=ftp://ftp.gnu.org/gnu/readline
READLINE_GZ=readline-${RL_MAJOR_SRC}.${RL_MINOR_SRC}.tar.gz
AC_SUBST(READLINE_URL)
AC_SUBST(READLINE_GZ)
AC_SUBST(RL_MAJOR)
AC_SUBST(RL_MINOR)
AC_SUBST(RL_MAJOR_SRC)
AC_SUBST(RL_MINOR_SRC)
CMAKE_MAJOR=2
CMAKE_MINOR=4
CMAKE_MAJOR_SRC=2
CMAKE_MINOR_SRC=8
CMAKE_PATCH_SRC=0
CMAKE_URL=http://www.cmake.org/files/v${CMAKE_MAJOR_SRC}.${CMAKE_MINOR_SRC}
CMAKE_GZ=cmake-${CMAKE_MAJOR_SRC}.${CMAKE_MINOR_SRC}.${CMAKE_PATCH_SRC}.tar.gz
AC_SUBST(CMAKE_URL)
AC_SUBST(CMAKE_GZ)
AC_SUBST(CMAKE_MAJOR)
AC_SUBST(CMAKE_MINOR)
AC_SUBST(CMAKE_MAJOR_SRC)
AC_SUBST(CMAKE_MINOR_SRC)
AC_SUBST(CMAKE_PATCH_SRC)
FLTK2_MAJOR=2
FLTK2_MINOR=0
FLTK2_PATCH=0
FLTK2_MAJOR_SRC=2
FLTK2_MINOR_SRC=0
FLTK2_PATCH_SRC=0
AC_SUBST(FLTK2_MAJOR)
AC_SUBST(FLTK2_MINOR)
AC_SUBST(FLTK2_PATCH)
AC_SUBST(FLTK2_MAJOR_SRC)
AC_SUBST(FLTK2_MINOR_SRC)
AC_SUBST(FLTK2_PATCH_SRC)
FTGL_MAJOR=2
FTGL_MINOR=1
FTGL_PATCH=2
FTGL_MAJOR_SRC=2
FTGL_MINOR_SRC=1
FTGL_PATCH_SRC=2
AC_SUBST(FTGL_MAJOR)
AC_SUBST(FTGL_MINOR)
AC_SUBST(FTGL_PATCH)
AC_SUBST(FTGL_MAJOR_SRC)
AC_SUBST(FTGL_MINOR_SRC)
AC_SUBST(FTGL_PATCH_SRC)
SETUPTOOLS_MAJOR=0
SETUPTOOLS_MINOR=6
SETUPTOOLS_MAJOR_SRC=0
SETUPTOOLS_MINOR_SRC=6c11
SETUPTOOLS_URL=http://pypi.python.org/packages/source/s/setuptools
SETUPTOOLS_GZ=setuptools-${SETUPTOOLS_MAJOR_SRC}.${SETUPTOOLS_MINOR_SRC}.tar.gz
AC_SUBST(SETUPTOOLS_URL)
AC_SUBST(SETUPTOOLS_GZ)
AC_SUBST(SETUPTOOLS_MAJOR)
AC_SUBST(SETUPTOOLS_MINOR)
AC_SUBST(SETUPTOOLS_MAJOR_SRC)
AC_SUBST(SETUPTOOLS_MINOR_SRC)
MATPLOTLIB_MAJOR=0
MATPLOTLIB_MINOR=98
MATPLOTLIB_MAJOR_SRC=0
MATPLOTLIB_MINOR_SRC=98
MATPLOTLIB_PATCH_SRC=3
AC_SUBST(MATPLOTLIB_MAJOR)
AC_SUBST(MATPLOTLIB_MINOR)
AC_SUBST(MATPLOTLIB_MAJOR_SRC)
AC_SUBST(MATPLOTLIB_MINOR_SRC)
AC_SUBST(MATPLOTLIB_PATCH_SRC)
PyOpenGL_MAJOR=3
PyOpenGL_MINOR=0
PyOpenGL_PATCH=0
PyOpenGL_MAJOR_SRC=3
PyOpenGL_MINOR_SRC=0
PyOpenGL_PATCH_SRC=1
PyOpenGL_URL=http://pypi.python.org/packages/source/P/PyOpenGL
PyOpenGL_GZ=PyOpenGL-${PyOpenGL_MAJOR_SRC}.${PyOpenGL_MINOR_SRC}.${PyOpenGL_PATCH_SRC}.tar.gz
AC_SUBST(PyOpenGL_URL)
AC_SUBST(PyOpenGL_GZ)
AC_SUBST(PyOpenGL_MAJOR)
AC_SUBST(PyOpenGL_MINOOR)
AC_SUBST(PyOpenGL_PATCH)
AC_SUBST(PyOpenGL_MAJOR_SRC)
AC_SUBST(PyOpenGL_MINOR_SRC)
AC_SUBST(PyOpenGL_PATCH_SRC)
HDF4_MAJOR_SRC=4
HDF4_MINOR_SRC=2
HDF4_PATCH_SRC=5
HDF4_URL=http://www.hdfgroup.org/ftp/HDF/HDF_Current/src
HDF4_GZ=hdf-${HDF4_MAJOR_SRC}.${HDF4_MINOR_SRC}.${HDF4_PATCH_SRC}.tar.gz
AC_SUBST(HDF4_URL)
AC_SUBST(HDF4_GZ)
AC_SUBST(HDF4_MAJOR_SRC)
AC_SUBST(HDF4_MINOR_SRC)
AC_SUBST(HDF4_PATCH_SRC)
HDF5_MAJOR_SRC=1
HDF5_MINOR_SRC=8
HDF5_PATCH_SRC=4-patch1
HDF5_URL=http://www.hdfgroup.org/ftp/HDF5/current/src
HDF5_GZ=hdf5-${HDF5_MAJOR_SRC}.${HDF5_MINOR_SRC}.${HDF5_PATCH_SRC}.tar.gz
AC_SUBST(HDF5_URL)
AC_SUBST(HDF5_GZ)
AC_SUBST(HDF5_MAJOR_SRC)
AC_SUBST(HDF5_MINOR_SRC)
AC_SUBST(HDF5_PATCH_SRC)
UUID_MAJOR_SRC=1
UUID_MINOR_SRC=6
UUID_PATCH_SRC=2
UUID_URL=ftp://ftp.ossp.org/pkg/lib/uuid
UUID_GZ=uuid-${UUID_MAJOR_SRC}.${UUID_MINOR_SRC}.${UUID_PATCH_SRC}.tar.gz
AC_SUBST(UUID_URL)
AC_SUBST(UUID_GZ)
AC_SUBST(UUID_MAJOR_SRC)
AC_SUBST(UUID_MINOR_SRC)
AC_SUBST(UUID_PATCH_SRC)
UDUNITS2_MAJOR_SRC=2
UDUNITS2_MINOR_SRC=1
UDUNITS2_PATCH_SRC=14
UDUNITS2_URL=ftp://ftp.unidata.ucar.edu/pub/udunits
UDUNITS2_GZ=udunits-${UDUNITS2_MAJOR_SRC}.${UDUNITS2_MINOR_SRC}.${UDUNITS2_PATCH_SRC}.tar.gz
AC_SUBST(UDUNITS2_URL)
AC_SUBST(UDUNITS2_GZ)
AC_SUBST(UDUNITS2_MAJOR_SRC)
AC_SUBST(UDUNITS2_MINOR_SRC)
AC_SUBST(UDUNITS2_PATCH_SRC)
NC4_MAJOR_SRC=4
NC4_MINOR_SRC=1
NC4_PATCH_SRC=1
NC4_URL=ftp://ftp.unidata.ucar.edu/pub/netcdf
NC4_GZ=netcdf-${NC4_MAJOR_SRC}.${NC4_MINOR_SRC}.${NC4_PATCH_SRC}.tar.gz
AC_SUBST(NC4_URL)
AC_SUBST(NC4_GZ)
AC_SUBST(NC4_MAJOR_SRC)
AC_SUBST(NC4_MINOR_SRC)
AC_SUBST(NC4_PATCH_SRC)
NC4_MAJOR=4
NC4_MINOR=1
NC4_PATCH=0
AC_SUBST(NC4_MAJOR)
AC_SUBST(NC4_MINOR)
AC_SUBST(NC4_PATCH)
ZLIB_MAJOR_SRC=1
ZLIB_MINOR_SRC=2
ZLIB_PATCH_SRC=5
ZLIB_URL=http://zlib.net
ZLIB_GZ=zlib-${ZLIB_MAJOR_SRC}.${ZLIB_MINOR_SRC}.${ZLIB_PATCH_SRC}.tar.gz
AC_SUBST(ZLIB_MAJOR_SRC)
AC_SUBST(ZLIB_MINOR_SRC)
AC_SUBST(ZLIB_PATCH_SRC)
AC_SUBST(ZLIB_URL)
AC_SUBST(ZLIB_GZ)
TCAP_MAJOR_SRC=1
TCAP_MINOR_SRC=3
TCAP_PATCH_SRC=1
AC_SUBST(TCAP_MAJOR_SRC)
AC_SUBST(TCAP_MINOR_SRC)
AC_SUBST(TCAP_PATCH_SRC)
GIFSICLE_MAJOR_SRC=1
GIFSICLE_MINOR_SRC=58
GIFSICLE_URL=http://www.lcdf.org/gifsicle
GIFSICLE_GZ=gifsicle-${GIFSICLE_MAJOR_SRC}.${GIFSICLE_MINOR_SRC}.tar.gz
AC_SUBST(GIFSICLE_URL)
AC_SUBST(GIFSICLE_GZ)
AC_SUBST(GIFSICLE_MAJOR_SRC)
AC_SUBST(GIFSICLE_MINOR_SRC)

XGKS_URL=http://rainbow.llnl.gov/dist/cdat
XGKS_GZ=xgks.tar.gz
AC_SUBST(XGKS_URL)
AC_SUBST(XGKS_GZ)
JPEG_URL=http://www.ijg.org/files
JPEG_GZ=jpegsrc.v8a.tar.gz
AC_SUBST(JPEG_URL)
AC_SUBST(JPEG_GZ)
BLAS_URL=http://www.netlib.org/blas
BLAS_GZ=blas.tgz
AC_SUBST(BLAS_URL)
AC_SUBST(BLAS_GZ)
LAPACK_URL=http://www.netlib.org/lapack
LAPACK_GZ=lapack.tgz
AC_SUBST(LAPACK_URL)
AC_SUBST(LAPACK_GZ)
LAPACK95_URL=http://www.netlib.org/lapack95
LAPACK95_GZ=lapack95.tgz
AC_SUBST(LAPACK95_URL)
AC_SUBST(LAPACK95_GZ)
PBMPLUS_URL=http://acme.com/software/pbmplus
PBMPLUS_GZ=pbmplus_10dec1991.tar.gz
AC_SUBST(PBMPLUS_URL)
AC_SUBST(PBMPLUS_GZ)
GS_MAJOR_SRC=8
GS_MINOR_SRC=64
GS_URL=http://ghostscript.com/releases
GS_GZ=ghostscript-8.64.tar.gz
AC_SUBST(GS_URL)
AC_SUBST(GS_GZ)
AC_SUBST(GS_MAJOR_SRC)
AC_SUBST(GS_MINOR_SRC)
FFMPEG_MAJOR_SRC=0
FFMPEG_MINOR_SRC=5
FFMPEG_PATCH_SRC=1
FFMPEG_URL=http://ffmpeg.org/releases
FFMPEG_GZ=ffmpeg-${FFMPEG_MAJOR_SRC}.${FFMPEG_MINOR_SRC}.${FFMPEG_PATCH_SRC}.tar.gz
AC_SUBST(FFMPEG_MAJOR_SRC)
AC_SUBST(FFMPEG_MINOR_SRC)
AC_SUBST(FFMPEG_PATCH_SRC)
AC_SUBST(FFMPEG_URL)
AC_SUBST(FFMPEG_GZ)
VTK_MAJOR_SRC=5
VTK_MINOR_SRC=4
VTK_PATCH_SRC=2
VTK_URL=http://www.vtk.org/files/release/${VTK_MAJOR_SRC}.${VTK_MINOR_SRC}
VTK_GZ=vtk-${VTK_MAJOR_SRC}.${VTK_MINOR_SRC}.${VTK_PATCH_SRC}.tar.gz
AC_SUBST(VTK_URL)
AC_SUBST(VTK_GZ)
dnl AC_SUBST(COMPILE_IOAPI)
AC_SUBST(ESG)
AC_SUBST(PGCONFIG)
AC_SUBST(SETUPTOOLS)
AC_SUBST(PyOpenGL)
AC_SUBST(NCPUS)


dnl md5 checker program
MD5PRG="md5sum"
MD5CNT=1
AC_SUBST(MD5PRG)
AC_SUBST(MD5CNT)

AC_SUBST(DOCMOR)
DOCMOR="yes"

PYTHON_INSTALL_OPTIONS=""

PREFIX_PATH=${prefix}
export PKG_CONFIG_PATH
AC_ARG_WITH([externals],
	[AS_HELP_STRING([--with-externals],[path where to build/look for externals programs/libraries [PREFIX/Externals]])],
        [],
	[with_externals=no])
if test ${with_externals} == "no" ; then
   EXTERNALS=${prefix}/Externals
else
   EXTERNALS=${with_externals}
fi
NCPUS=1
AC_ARG_WITH([cpus],
	[AS_HELP_STRING([--with-cpus],[How many CPUs to use for make -j ])],
        [],
	[with_cpus=1])
if test ${with_cpus} == "no" ; then
   NCPUS=1
else
   NCPUS=${with_cpus}
fi

PATH=${EXTERNALS}/bin:${prefix}/bin:${EXTERNALS}/NetCDF/bin:${EXTERNALS}/HDF5/bin:$PATH
PKG_CONFIG_PATH=${EXTERNALS}/lib/pkgconfig:${PKG_CONFIG_PATH}

BUILD_DIR=`pwd`
BUILDDIRNOSPC=`pwd | sed s/\ /XXX/`
if test "${BUILD_DIR}" != "${BUILDDIRNOSPC}" ; then
        AC_MSG_ERROR([Your current directory contains spaces this can cause some build to fail please relocate your CDAT sources],1)
fi
BUILD_DIR=${BUILD_DIR}"/build"
dnl SUBSTs
AC_SUBST(MAINDIR)
AC_SUBST(BUILD_DIR)
AC_SUBST(AQUA_PYTHON)
AC_SUBST(COMPILE_HDF)
AC_SUBST(HDF)
AC_SUBST(COMPILE_NC)
AC_SUBST(NC4LOC)
AC_SUBST(COMPILE_HDF5)
AC_SUBST(HDF5LOC)
AC_SUBST(COMPILE_UDUNITS2)
AC_SUBST(UDUNITS2LOC)
AC_SUBST(COMPILE_CURL)
AC_SUBST(NC_FORTRAN)
AC_SUBST(COMPILE_CAIRO)
AC_SUBST(COMPILE_FREETYPE)
AC_SUBST(COMPILE_PIXMAN)
AC_SUBST(COMPILE_FONTCONFIG)
AC_SUBST(COMPILE_XML2)
AC_SUBST(COMPILE_XSLT)
AC_SUBST(COMPILE_PKGCONFIG)
AC_SUBST(COMPILE_LIBPNG)
AC_SUBST(COMPILE_FTGL)
AC_SUBST(COMPILE_FLTK2)
AC_SUBST(Pmw)
AC_SUBST(NUMPY)
AC_SUBST(COMPILE_XGKS)
AC_SUBST(COMPILE_LIBJPEG)
AC_SUBST(COMPILE_READLINE)
AC_SUBST(COMPILE_Z)
AC_SUBST(COMPILE_TCLTK)
AC_SUBST(PYOPT)
AC_SUBST(PYADD)
AC_SUBST(COMPILE_PYTHON)
AC_SUBST(PYTHON)
AC_SUBST(DOGS)
AC_SUBST(DOPBMPLUS)
AC_SUBST(HAVE_LIBBLAS)
AC_SUBST(HAVE_LIBLAPACK)
AC_SUBST(HAVE_LIBLAPACK95)
AC_SUBST(PYTHON_INSTALL_OPTIONS)
AC_SUBST(PKG_CONFIG_PATH)
AC_SUBST(EXTERNALS)
AC_SUBST(COMPILE_TERMCAP)
TERMCAP_URL=http://ftp.wayne.edu/pub/gnu/termcap
TERMCAP_GZ=termcap-1.3.1.tar.gz
AC_SUBST(TERMCAP_URL)
AC_SUBST(TERMCAP_GZ)
AC_SUBST(KEEPLOG)
AC_SUBST(KEEPLOGa)
AC_SUBST(KEEPLOGroot)
AC_SUBST(PATH)
AC_SUBST(COMPILE_CMAKE)
AC_SUBST(CDAT_VERSION)
AC_SUBST(PACKAGE_VERSION)
AC_SUBST(COMPILE_UUID)
AC_SUBST(UUIDLOC)
AC_SUBST(CMAKE_PATH)
AC_SUBST(DOWGET)
AC_SUBST(HASWGET)
AC_SUBST(WGET_URL)
AC_SUBST(WGET_GZ)
AC_SUBST(HASCURL)

WGET_URL=http://ftp.gnu.org/gnu/wget
WGET_GZ=wget-latest.tar.gz
DOWGET="no"

AC_PATH_PROG(HASWGET,wget,no)

if test ${HASWGET} == "no" ; then
   AC_PATH_PROG(HASCURL,curl,no)
   if test ${HASCURL} == "no" ; then
    AC_MSG_ERROR([you need either wget or curl installed])
   else
    DOWGET="yes"
    HASWGET=${EXTERNALS}/bin/wget
   fi
fi
dnl #######################################################
dnl Turning off compilation by default
dnl #######################################################
COMPILE_CAIRO=no
COMPILE_PIXMAN=no
COMPILE_LIBPNG=no
COMPILE_FREETYPE=no
COMPILE_FONTCONFIG=no
COMPILE_CMAKE=no
COMPILE_XGKS=no
COMPILE_UDUNITS2=no
COMPILE_UUID=no
DOPBMPLUS=no
FFMPEG=yes
GIFSICLE=yes
DOGS=no
COMPILE_LIBJPEG=no
HAVE_LIBBLAS=yes
HAVE_LIBLAPACK=yes
HAVE_LIBLAPACK95=yes
dnl COMPILE_IOAPI=no
COMPILE_PKGCONFIG=no
COMPILE_XSLT=no

PYOPT="--enable-shared "
dnl ##########################################################
dnl  Turn off fortran
dnl ##########################################################
AC_ARG_ENABLE([fortran],[AS_HELP_STRING([--enable-fortran],[enable pur fortran codes (default will try)])])
enable_fortran="-"${enable_fortran}
if test ${enable_fortran} == "-no" ; then
  FC=""
  F77=""
  FFLAGS=""
  FCFLAGS=""
fi

dnl ###########################################################
dnl Option to build esg only
dnl ###########################################################
AC_ARG_ENABLE([esg],[AS_HELP_STRING([--enable-esg],[enable build of esg])])
ESG=no
enable_esg="-"${enable_esg}

dnl ###########################################################
dnl Option to bypass X11 stuff
dnl ###########################################################
AC_ARG_ENABLE([X11],[AS_HELP_STRING([--enable-x],[turn on/off build of things depending on X11 (default on)])])
enable_X11="-"${enable_X11}

dnl  basic test
AC_PROG_CXX
AC_PROG_CC
AC_PROG_CPP
AC_PROG_LN_S
AC_PROG_SED
AC_PROG_MKDIR_P
AC_PROG_RANLIB
if test ${enable_fortran} != "-no" ; then
  AC_PROG_FC
  AC_PROG_F77
  AC_FC_LIBRARY_LDFLAGS
  AC_F77_LIBRARY_LDFLAGS
fi
AC_CANONICAL_TARGET
echo "TARGET: "${target_os}
DO_X="yes"
PYADD=""
case ${target_os} in
 darwin*)
    case ${target_cpu} in
      i686*)
         dnl do nothing
         DO_X="no"
         echo "MAC skipping X11 testing" ;;
      power*)
         dnl ok need to do it
	 echo "Powerpc Mac will test for X11";;
    esac ;;
  *)
    echo "Testing for X11";;
esac
if test ${DO_X} == "yes" ; then
  AC_PATH_XTRA
  dnl here we add the result of XTRA and make sure /usr/X11 is in anyway
  long_no_x="_"${no_x}
  if  test ${long_no_x} == "_yes"   ; then
    if test ${enable_esg} == "-yes" ; then
      enable_X11="-no"
      AC_MSG_WARN([no X found turn off X11 dependencies, that means no tcl/tk or Tkinter suport in your python])
    else
      AC_MSG_WARN([no X Found no, we will add /usr/X11 to the path but it will probably fail])
      CFLAGS="-I/usr/X11/include "${CFLAGS}
      LDFLAGS="-L/usr/X11/lib "${LDFLAGS}
    fi
  dnl   echo "XDISPLAY MISSING"${X_DISPLAY_MISSING}"---"
  else
    echo "Got X11 adding:"${X_CFLAGS}" and "${X_LIBS}" and "${X_EXTRA_LIBS}" that is it"
    CFLAGS=${X_CFLAGS}" "${CFLAGS}
    LDFLAGS=${X_LIBS}" "${X_EXTRA_LIBS}" "${LDFLAGS}
  fi
fi
dnl # Preps the version file
cat >version.in <<\_ACEOF
version
_ACEOF
rm version
sed -e 's/version/'`echo ${CDAT_VERSION}`'/g' version.in > version
rm version.in

AC_CHECK_LIB(m,atan)

MAINDIR=`pwd`

PYTHONPATH="${prefix}/lib/python${PYTHON_MAJOR}.${PYTHON_MINOR}/site-packages:${PYTHONPATH}"
export PYTHONPATH

AC_ARG_ENABLE([vtk],[AS_HELP_STRING([--enable-vtk],[enable/disable vtk build])]) 
enable_vtk="-"${enable_vtk}
if test ${enable_vtk} == "-yes" ; then
   COMPILE_VTK="yes"
else
  COMPILE_VTK="no"
fi
AC_SUBST(COMPILE_VTK)
EXTRAENV=""

dnl #General compilation flags
AC_MSG_NOTICE([add ${EXTERNALS} to LD and CPP FLAGS])
LDFLAGS="-L${EXTERNALS}/lib  -L${prefix}/lib -L${EXTERNALS}/NetCDF/lib  -L${EXTERNALS}/HDF5/lib "$LDFLAGS
CPPFLAGS="-I${EXTERNALS}/include  "$CPPFLAGS
CFLAGS="-I${EXTERNALS}/include  -L${EXTERNALS}/lib  "${CFLAGS}
COMPILE_TCLTK=no
case ${target_os} in
 linux-*) 
  cp exsrc/src/CMakeCache.txt.linux.in exsrc/CMakeCache.txt.in
  AC_MSG_NOTICE([Linux: adding -Wl,--rpath,${prefix}/lib -Wl,--rpath,${EXTERNALS}/lib  -Wl,--rpath,${EXTERNALS}/NetCDF/lib  -Wl,--rpath,${EXTERNALS}/HDF5/lib to LDFLAGS])
  LDFLAGS=${LDFLAGS}" -Wl,--rpath,${prefix}/lib -Wl,--rpath,${EXTERNALS}/lib  -Wl,--rpath,${EXTERNALS}/NetCDF/lib  -Wl,--rpath,${EXTERNALS}/HDF5/lib" 
  AC_MSG_CHECKING([if we are using icc])
  if test ${CC} == "icc" ; then
    AC_MSG_RESULT([yes])
    AC_MSG_CHECKING([if IC env variable is set])
    if test "-"${IC} == "-" ; then
       AC_MSG_RESULT([no])
       AC_MSG_ERROR([set environment variable IC to the location of libimf.a, libirc.a])
    else
       AC_MSG_RESULT([yes])
       AC_MSG_NOTICE([adding -L${IC}/lib -Wl,--rpath,${IC}/lib -limf -lirc to LDFLAGS])
       LDFLAGS=${LDFLAGS}" -L${IC}/lib -Wl,--rpath,${IC}/lib -limf -lirc"
    fi
  else
    AC_MSG_RESULT([no])
  fi ;;
 darwin*)
  MD5PRG="md5"
  MD5CNT=4
  LDFLAGS=${LDFLAGS}" -R${prefix}/lib -R${EXTERNALS}/lib  -R${EXTERNALS}/NetCDF/lib  -R${EXTERNALS}/HDF5/lib " 
  dnl CFLAGS=${CFLAGS}" -R${EXTERNALS}/lib   -R${EXTERNALS}/NetCDF/lib   -R${EXTERNALS}/HDF5/lib -R${EXTERNALS}/OpenDAP/lib" 
  dnl AC_MSG_NOTICE([Darwin removing -Wno-long-double when building Python for gcc 4.2 and higher. Replacing configure])
  dnl PYADD=${PYADD}" cp -f ../../pysrc/src/Python-2.5.2-configure-mac10.6-replacement configure ; "
  AC_MSG_NOTICE([Darwin: Cannot use the aqua enabled tk, will build it if not already in Externals])
  AC_CHECK_FILE([${EXTERNALS}/lib/libtclstub${TCLTK_MAJOR}.${TCLTK_MINOR}.a],[COMPILE_TCLTK=no],[COMPILE_TCLTK=yes]) 
  dnl ##########################################################
  dnl  Turn on AQUA on Mac
  dnl ##########################################################
  AC_ARG_ENABLE([aqua],[AS_HELP_STRING([--enable-aqua],[enables aqua support for tk (if tk with aqua is on your system)])])
  dnl #################
  dnl add ppc flags
  dnl ##############
  dnl CFLAGS=${CFLAGS}" -arch i386 -arch x86_64"
  dnl CXXFLAGS=${CXXFLAGS}" -arch i386 -arch x86_64"
  cp exsrc/src/CMakeCache.txt.mac.in exsrc/CMakeCache.txt.in
  enable_aqua="-"${enable_aqua}
  if test ${enable_aqua} == "-yes" ; then
     AQUA_PYTHON="export AQUA_CDAT=yes ; "
  else
     AQUA_PYTHON="export AQUA_CDAT=no ; "
  fi
  dnl ##########################################################
  dnl  Turn on framework on Mac
  dnl ##########################################################
  AC_ARG_ENABLE([framework],[AS_HELP_STRING([--enable-framework],[Build (MacOSX|Darwin) framework])])
  enable_fw="-"${enable_framework}
  if test ${enable_fw} == "-yes" ; then
    PYOPT=" --enable-framework"
    cp exsrc/src/CMakeCache.txt.mac.Framework.in exsrc/CMakeCache.txt.in
  else
     if test ${enable_fw} != "-" ; then
         PYOPT=" --enable-framework=${enable_framework}"
         cp exsrc/src/CMakeCache.txt.mac.Framework.in exsrc/CMakeCache.txt.in
     fi
  fi
  dnl ##########################################################
  dnl  Turn on tool on Mac
  dnl ##########################################################
  AC_ARG_ENABLE([toolbox-glue],[AS_HELP_STRING([--enable-toolbox-glue],[disable/enable MacOSX glue code for extensions])])
  enable_tbg="-"${enable_toolbox_glue}
  if test ${enable_tbg} == "-yes" ; then
    PYOPT=${PYOPT}" --enable-toolbox-glue"
  fi
  case ${target_os} in
    darwin10*)
dnl    AC_MSG_NOTICE([Darwin 10.6 or higher need to remove -Wno-long-double when building Python. Replacing configure])
dnl    PYADD=${PYADD}" cp -f ../../pysrc/src/Python-2.5.2-configure-mac10.6-replacement configure ; cp ../../pysrc/src/Python-2.5.2-pyconfig.in-replacement pyconfig.h.in ; " 
    EXTRAENV="MACOSX_DEPLOYMENT_TARGET=10.6"
    AC_SUBST(EXTRAENV)
    PYOPT=${PYOPT}" --disable-toolbox-glue --disable-framework " 
    CFLAGS=${CFLAGS}" -fPIC -arch x86_64 -pipe -Xarch_x86_64 -mmacosx-version-min=10.5 "
    LDFLAGS=${LDFLAGS}" -fPIC  -bind_at_load  -mmacosx-version-min=10.5 "
    CXXFLAGS=${CXXFLAGS}" -fPIC -arch x86_64 -pipe -Xarch_x86_64 -mmacosx-version-min=10.5 ";;
  esac ;;
 cygwin*)
   dnl  the following do not work under cygwin... no need to detext and build
   FFMPEG=yes
   HAVE_LIBLAPACK=yes
   HAVE_LIBLAPACK95=yes ;;
esac

AC_MSG_CHECKING([for 64bit machine])
if test ${target_cpu} == "x86_64" -o ${target_cpu} == "ia64" ; then
  AC_MSG_RESULT([yes])
  AC_MSG_NOTICE([adding -fPIC to C and CXX FLAGS])
  CFLAGS=${CFLAGS}" -fPIC"
  CXXFLAGS=${CXXFLAGS}" -fPIC"
else
      AC_MSG_RESULT([no])
fi
dnl  some fortran flags... yerk
dnl TESTFFLAGS=""
dnl AC_MSG_CHECKING([for gfortran compiler])
dnl if test ${FC} == "gfortran" ; then
dnl   AC_MSG_RESULT([yes])
dnl   AC_MSG_NOTICE([adding -DpgiFortran to CPPFLAGS])
dnl   CPPFLAGS=${CPPFLAGS}" -DpgiFortran"
dnl   TESTFFLAGS="-lgfortran"
dnl else
dnl    AC_MSG_RESULT([no])
dnl fi
dnl  something for hpux?
if (test `uname` = "HP-UX") then
    AC_MSG_NOTICE([addinf HPUX flags to CPP])
    CPPFLAGS=${CPPFLAGS}" +z -D_HPUX_SOURCE"
fi


dnl  
dnl  Python related checks
COMPILE_PYTHON=no
dnl enable_logfiles=crap
AC_ARG_ENABLE([logfiles],[AS_HELP_STRING([--enable-logfiles],[enable log files (default)])])
enable_logfiles="-"${enable_logfiles}
if test  ${enable_logfiles} == "-no" ; then
   KEEPLOG=""
   KEEPLOGa=""
   KEEPLOGroot="/dev/null"
else
   KEEPLOG="> \"${MAINDIR}/logs/\$(2).LOG\" 2>\"${MAINDIR}/logs/\$(2).LOG\""
   KEEPLOGa=">> \"${MAINDIR}/logs/\$(2).LOG\" 2>>\"${MAINDIR}/logs/\$(2).LOG\""
   KEEPLOGroot="\"${MAINDIR}/logs\""
fi

if test ${enable_esg} == "-yes" ; then
  enable_cdms_only="yes"
  ESG=yes
  DOCMOR="no"
  AC_PATH_PROG(PGCONFIG,pg_config,no)
  if test ${PGCONFIG} == "no" ; then
    AC_ARG_WITH([pg_config],
		[AS_HELP_STRING([--with-pg_config],[specify where to find pg_config program (esg only)])],
		[],
		[with_pg_config='no'])
    if test ${with_pg_config} != "no" ; then
        AC_CHECK_FILE([${with_pg_config}],[PGCONFIG=${with_pg_config}],[PGCONFIG='no'])
    fi
  fi
  dnl if test ${PGCONFIG} == "no" ; then
  dnl   AC_MSG_ERROR([You need to have pg_config in your path or secify with --with-pg_config=])
  dnl fi
  PGCONFIG=`dirname ${PGCONFIG}`
fi
AC_ARG_ENABLE([cdms-only],[AS_HELP_STRING([--enable-cdms-only],[enable build of cdms only (default off)])])
enable_cdms_only="-"${enable_cdms_only}
if test ${enable_cdms_only} == "-yes" ; then
  PYTHON_INSTALL_OPTIONS=${PYTHON_INSTALL_OPTIONS}" --enable-cdms-only"
fi

AC_ARG_ENABLE([debug],[AS_HELP_STRING([--enable-debug],[enable build with debug flags (default off)])])
enable_debug="-"${enable_debug}
if test ${enable_debug} == "-yes" ; then
  PYTHON_INSTALL_OPTIONS=${PYTHON_INSTALL_OPTIONS}" --debug"
  CFLAGS="-g "${CFLAGS}
fi


AC_ARG_WITH([python],
		[AS_HELP_STRING([--with-python],[use your own python])],
		[],
		[with_python=no])
if test ${with_python} != "no" ; then
  AC_CHECK_FILE([${with_python}],[PYTHON=${with_python}],[AC_MSG_ERROR([your python is not here])])
else
  AC_PATH_PROG(PYTHON,python,"no")
  if test ${PYTHON} != "${prefix}/bin/python" ; then
    AC_MSG_RESULT([  ... got ${PYTHON} for your python, not the one we want, we will build our own])
    COMPILE_PYTHON=yes
  fi
  AC_CHECK_FILE([${prefix}/bin/python],[PYTHON=${prefix}/bin/python ; COMPILE_PYTHON=no ],[])
fi
dnl  at this point we have a python, but we need to test for some features
if test ${COMPILE_PYTHON} == "no"; then
dnl  First testing for version of python
  AC_MSG_CHECKING([if python ${PYTHON} is recent enough])
  if [test `${PYTHON} -c "import sys;print sys.version_info[0]"` -lt "${PYTHON_MAJOR}"] ; then
    AC_MSG_RESULT([got a very old python])
    COMPILE_PYTHON=yes
  else
    if [test `${PYTHON} -c "import sys;print sys.version_info[0]"` == "${PYTHON_MAJOR}"] ; then
      if [test `${PYTHON} -c "import sys;print sys.version_info[1]"` -lt "${PYTHON_MINOR}"] ; then
        AC_MSG_RESULT([got a slightly old python])
        COMPILE_PYTHON=yes
      else
        AC_MSG_RESULT([yes])
      fi
    fi
  fi
  if test ${COMPILE_PYTHON} == "yes" ; then
    if test ${with_python} != "no" ; then
      AC_MSG_ERROR([your python is too old])
    else
      AC_MSG_WARN([we will build a new python for you])
    fi
  fi
fi
if test ${COMPILE_PYTHON} == "no" ; then
 if [ test ${enable_cdms_only} != "-yes" -o ${enable_esg} == "-yes"  ] ; then
  dnl  Now testing for must have modules
  for module in Tkinter ; do
    AC_MSG_CHECKING([if python ${PYTHON} has Tkinter support])
    if [test `${PYTHON} -c "cmd = 'try:\n  import Tkinter\n  print 0\nexcept:\n  print 1'; exec(cmd)"` == "1"] ; then
      AC_MSG_RESULT([no])
      COMPILE_PYTHON=yes
      if test ${with_python} != "no" ; then
        AC_MSG_ERROR([your python does not have Tkinter support])
      else
        AC_MSG_WARN([we will build a new python for you])
      fi
    else
      AC_MSG_RESULT([yes])
    fi
  done
 fi
fi
if test ${COMPILE_PYTHON} == "no"; then
  dnl figures out which version of python we have
  PYVER=`${PYTHON} -c "import sys;print '.'.join(sys.version.split('.').__getslice__(0,2))"`
  dnl figures out which version of tcl/tk we have
  TCLTK_VERSION=`${PYTHON} -c "import Tkinter;print Tkinter.TkVersion"`
  dnl  since we will not build python no need to build these
  COMPILE_READLINE=no
  COMPILE_Z=no
  COMPILE_TCLTK=no
  dnl  Now testing for useful modules
  for module in rlcompleter readline ; do
    AC_MSG_CHECKING([if python ${PYTHON} has ${module} support])
    if [test `${PYTHON} -c "cmd = 'try:\n  import ${module}\n  print 0\nexcept:\n  print 1'; exec(cmd)"` == "1"] ; then
      AC_MSG_RESULT([no])
      AC_MSG_WARN([your python does not have support for ${module}, you should consider adding support for it in your python])
    else
      AC_MSG_RESULT([yes])
    fi
  done
else
  dnl  Ok now platform specific flags
  dnl PYADD=${PYADD}" cp ../../pysrc/src/setup.py . ; patch Lib/idlelib/WidgetRedirector.py ../../pysrc/src/Python-2.5.2-idlelib.patch ; "
  PYADD="cp ../../pysrc/src/setup.py . ; "
  if test ${target_vendor} = "apple" ; then  dnl  MacIntosh OSX
      PYADD=${PYADD}' OPT=""; export OPT ; '
  fi
  if test ${target_os} = "OSF1" ;  then
    PYOPT=${PYOPT}" --disable-ipv6"
  fi
  if test ${target_os} == "AIX" ; then
    PYOPT=${PYOPT}" --disable-ipv6"
  fi
  PYTHON=${prefix}/bin/python



  dnl  VERY first check for lbtermcap
  COMPILE_TERMCAP=no
  AC_CHECK_LIB([termcap],[tgetstr],[],[COMPILE_TERMCAP=yes])
  AC_CHECK_HEADER([termcap.h],[],[COMPILE_TERMCAP=yes])
  COMPILE_READLINE=no
  dnl Check for readline 5.2
  AC_CHECK_HEADER([readline/readline.h],[COMPILE_READLINE=no],[COMPILE_READLINE=yes])
  AC_CHECK_LIB([readline],[readline])
  AC_MSG_CHECKING([if you have readline > ${RL_MAJOR}.${RL_MINOR}])
  AC_LANG([C])
  AC_RUN_IFELSE(
  [AC_LANG_PROGRAM([
  #include <stdio.h>
  #include <readline/readline.h>
  ],
  [
  if (RL_VERSION_MAJOR<${RL_MAJOR}) return 1;
  if (RL_VERSION_MAJOR>${RL_MAJOR}) return 0;
  if (RL_VERSION_MINOR<${RL_MINOR}) return 1;
  ])],[COMPILE_READLINE=no],[COMPILE_READLINE=yes])
  if test $COMPILE_READLINE == "no" ; then
    AC_MSG_RESULT([yes])
  else
    AC_MSG_RESULT([no])
  fi
  dnl  Now checking for zlib
  COMPILE_Z=no
  AC_CHECK_LIB([z],[compress],[],[COMPILE_Z=yes])
  AC_CHECK_HEADER([zlib.h],[],[COMPILE_Z=yes])

  COMPILE_TCLTK=yes
  AC_MSG_NOTICE([tcl/tk always built. This avoids conflicts with systems with multiple tcl/tk versions])
  if [ test ${COMPILE_TCLTK} != "yes" ] ; then 
   if [ test ${enable_cdms_only} != "-yes" -o ${enable_esg} == "-yes"  ] ; then
    dnl Now checking for tcl/tk
    AC_MSG_CHECKING([if you have a recent tcl (> ${TCLTK_MAJOR}.${TCLTK_MINOR})])
    AC_RUN_IFELSE(
    [AC_LANG_PROGRAM([
    #include <stdio.h>
    #include <tcl.h>
    ],
    [
  
    if (TCL_MAJOR_VERSION<${TCLTK_MAJOR}) return 1;
    if (TCL_MAJOR_VERSION>${TCLTK_MAJOR}) return 0;
    if (TCL_MINOR_VERSION<${TCLTK_MINOR}) return 1;
    if (TCL_MINOR_VERSION>${TCLTK_MINOR}) return 0;
    /*if (TCL_RELEASE_SERIAL<${TCLTK_PATCH}) return 1;*/
    ])],[COMPILE_TCLTK=no],[COMPILE_TCLTK=yes])
    if test $COMPILE_TCLTK == "no" ; then
      AC_MSG_RESULT([yes])
      AC_MSG_CHECKING([if you have a recent tk])
      AC_RUN_IFELSE(
      [AC_LANG_PROGRAM([
      #include <stdio.h>
      #include <tcl.h>
      #include <tk.h>
      ],
      [
      if (TK_MAJOR_VERSION<${TCLTK_MAJOR}) return 1;
      if (TK_MAJOR_VERSION>${TCLTK_MAJOR}) return 0;
      if (TK_MINOR_VERSION<${TCLTK_MINOR}) return 1;
      if (TK_MINOR_VERSION>${TCLTK_MINOR}) return 0;
      /*if (TK_RELEASE_SERIAL<${TCLTK_PATCH}) return 1;*/
      ])],[COMPILE_TCLTK=no],[COMPILE_TCLTK=yes])
      if test $COMPILE_TCLTK == "no" ; then
        AC_MSG_RESULT([yes])
      else
        AC_MSG_RESULT([no])
      fi
    else
      AC_MSG_RESULT([no])
    fi
    if test ${COMPILE_TCLTK} == "yes" ; then 
      AC_MSG_CHECKING([if you have a recent tcl  (> ${TCLTK_MAJOR}.${TCLTK_MINOR}) (with subdir for tcl include)])
      AC_RUN_IFELSE(
      [AC_LANG_PROGRAM([
      #include <stdio.h>
      #include <tcl${TCLTK_MAJOR}.${TCLTK_MINOR}/tcl.h>
      ],
      [
      
      if (TCL_MAJOR_VERSION<${TCLTK_MAJOR}) return 1;
      if (TCL_MAJOR_VERSION>${TCLTK_MAJOR}) return 0;
      if (TCL_MINOR_VERSION<${TCLTK_MINOR}) return 1;
      if (TCL_MINOR_VERSION>${TCLTK_MINOR}) return 0;
      /*if (TCL_RELEASE_SERIAL<${TCLTK_PATCH}) return 1;*/
      ])],[COMPILE_TCLTK=no],[COMPILE_TCLTK=yes])
      if test $COMPILE_TCLTK == "no" ; then
        AC_MSG_RESULT([yes])
        AC_MSG_CHECKING([if you have a recent tk])
        AC_RUN_IFELSE(
        [AC_LANG_PROGRAM([
        #include <stdio.h>
        #include <tcl${TCLTK_MAJOR}.${TCLTK_MINOR}/tcl.h>
        #include <tcl${TCLTK_MAJOR}.${TCLTK_MINOR}/tk.h>
        ],
        [
        if (TK_MAJOR_VERSION<${TCLTK_MAJOR}) return 1;
        if (TK_MAJOR_VERSION>${TCLTK_MAJOR}) return 0;
        if (TK_MINOR_VERSION<${TCLTK_MINOR}) return 1;
        if (TK_MINOR_VERSION>${TCLTK_MINOR}) return 0;
        /*if (TK_RELEASE_SERIAL<${TCLTK_PATCH}) return 1;*/
        ])],[COMPILE_TCLTK=no],[COMPILE_TCLTK=yes])
        if test $COMPILE_TCLTK == "no" ; then
          AC_MSG_RESULT([yes])
        else
          AC_MSG_RESULT([no])
        fi
      else
        AC_MSG_RESULT([no])
      fi
    fi
   fi
  fi
fi

DOGS=no

CMAKE_PATH=${EXTERNALS}/bin/cmake

if test  ${enable_cdms_only} != "-yes" ; then
  dnl ##########################################################
  dnl  Programs
  dnl ##########################################################
  dnl
  dnl cmake tests
  dnl
  COMPILE_CMAKE=yes
  AC_PATH_PROG(HAVE_CMAKE,cmake,no)
  if test ${HAVE_CMAKE} != "no" ; then
    AC_MSG_CHECKING([for cmake version, must be at least ${CMAKE_MAJOR}.${CMAKE_MINOR}])
    AC_RUN_IFELSE(
     [AC_LANG_PROGRAM([
      #include <stdio.h>
      ],
      [ [
      FILE *fp;
      char s1[250],s2[250],s3[250];
      int M,m;
      fp = popen("cmake --version","r");
      fscanf(fp,"%s %s %i.%i%s",&s1,&s2,&M,&m,&s3);
      if (M<${CMAKE_MAJOR}) return 1; 
      if (M>${CMAKE_MAJOR}) return 0;
      if (m<${CMAKE_MINOR}) return 1;
      printf("got : %i.%i ... ",M,m);
      return 0;
     ]])],[COMPILE_CMAKE=no],[COMPILE_CMAKE=yes])
     if test ${COMPILE_CMAKE} == "yes" ; then
        AC_MSG_RESULT([no])
	CMAKE_PATH=${EXTERNALS}/bin/cmake
     else
        AC_MSG_RESULT([yes])
     fi
  else
    COMPILE_CMAKE=yes
    CMAKE_PATH=${EXTERNALS}/bin/cmake
  fi

  AC_CHECK_PROG(FFMPEG,ffmpeg,[yes],[no])
  AC_CHECK_PROG(GS,gs,[yes],[no])
  AC_CHECK_PROG(PS2EPSI,ps2epsi,[yes],[no])
  if test ${GS} == "no" -o ${PS2EPSI} == "no" ; then
   DOGS=yes
  fi
  AC_CHECK_PROG(GIFSICLE,gifsicle,[yes],[no])
  AC_CHECK_PROG(PNMFLIP,pnmflip,[yes],[no])
  AC_CHECK_PROG(PNMCROP,pnmcrop,[yes],[no])
  AC_CHECK_PROG(PPMTOGIF,ppmtogif,[yes],[no])
  DOPBMPLUS=no
  if test ${PNMFLIP} == "no" ; then
    DOPBMPLUS=yes
  fi
  if test ${PPMTOGIF} == "no"; then
    DOPBMPLUS=yes
  fi
  if test ${PNMCROP} == "no" ; then
    DOPBMPLUS=yes
  fi
  AC_ARG_ENABLE([spanlib],[AS_HELP_STRING([--enable-spanlib],[enable spanlib, and therefore checks for blas/lapack/lapack95])])
  enable_spanlib="-"${enable_spanlib}
  if test ${enable_spanlib} == "-yes" ; then
    PYTHON_INSTALL_OPTIONS=${PYTHON_INSTALL_OPTIONS}" --enable-spanlib"
    dnl ########################################################
    dnl  BLAS, LAPACK and LAPACK95
    dnl ########################################################
    AC_ARG_ENABLE([blasbuild],[AS_HELP_STRING([--enable-blasbuild],[enable blas build (default off)])])
    enable_blasbuild="-"${enable_blasbuild}
    if test  ${enable_blasbuild} == "-no" ; then
      HAVE_LIBBLAS=yes
    fi
    if test  ${enable_blasbuild} == "-yes" ; then
      HAVE_LIBBLAS=no
    fi
    AC_ARG_ENABLE([lapackbuild],[AS_HELP_STRING([--enable-lapackbuild],[enable lapack build (default off)])])
    enable_lapackbuild="-"${enable_lapackbuild}
    if test  ${enable_lapackbuild} == "-no" ; then
      HAVE_LIBLAPACK=yes
    fi
    if test  ${enable_lapackbuild} == "-yes" ; then
      HAVE_LIBLAPACK=no
    fi
    AC_ARG_ENABLE([lapack95build],[AS_HELP_STRING([--enable-lapack95build],[enable lapack95 build (default off)])])
    enable_lapack95build="-"${enable_lapack95build}
    if test  ${enable_lapack95build} == "-no" ; then
      HAVE_LIBLAPACK95=yes
    fi
    if test  ${enable_lapack95build} == "-yes" ; then
      HAVE_LIBLAPACK95=no
    fi

    if test ${enable_fortran} != "-no" ; then
      if test [ ${FC} != "" -o ${F77} != "" ]  ; then
        if test ${enable_blasbuild} == "-" ; then
          AC_CHECK_LIB([blas],[srotmg_],[HAVE_LIBBLAS=yes],[HAVE_LIBBLAS=no],[${FCLIBS}])
        fi
        if test ${enable_lapackbuild} == "-" ; then
          AC_CHECK_LIB([lapack],[slags2_],[HAVE_LIBLAPACK=yes],[HAVE_LIBLAPACK=no],[${FCLIBS}])
        fi
        if test ${enable_lapack95build} == "-" ; then
          AC_CHECK_LIB([lapack95],[erinfo_],[HAVE_LIBLAPACK95=yes],[HAVE_LIBLAPACK95=no],[${FCLIBS}])
        fi
      fi
    fi
  else
    HAVE_LIBBLAS="yes"
    HAVE_LIBLAPACK="yes"
    HAVE_LIBLAPACK95="yes"
  fi
  dnl ########################################################
  dnl  LIBJPEG
  dnl ########################################################
  AC_CHECK_LIB([jpeg],[jpeg_start_decompress],[COMPILE_LIBJPEG=no],[COMPILE_LIBJPEG=yes])
  AC_CHECK_HEADER([jpeglib.h],[COMPILE_LIBJPEG=no],[COMPILE_LIBJPEG=yes])

  dnl ##########################################################
  dnl  UDPOSIX
  dnl ##########################################################
  AC_CHECK_HEADER([udposix.h],[COMPILE_XGKS=no],[COMPILE_XGKS=yes])
fi

dnl ########################################################
dnl  PYTHON Dependent parts
dnl  no check if we build the python, obviously it won't be in!
dnl ########################################################
if [ test ${COMPILE_PYTHON} == "no" ] ; then 
  dnl ########################################################
  dnl  NUMPY
  dnl ########################################################
  NUMPY=no
  AC_MSG_NOTICE([your python path: ${PYTHONPATH}])
  AC_MSG_CHECKING([if python ${PYTHON} has numpy support]) 
  if [test `${PYTHON} -c "cmd = 'try:\n  import numpy\n  print 0\nexcept:\n  print 1'; exec(cmd)"` == "1"] ; then
    AC_MSG_RESULT([no])
    AC_MSG_NOTICE([your python does not have numpy, we will build it])
    NUMPY=yes
  else
    AC_MSG_RESULT([yes])
    AC_MSG_CHECKING([if numpy is the right version ( > ${NUMPY_MAJOR}.${NUMPY_MINOR}.${NUMPY_PATCH})])
    if test [`${PYTHON} -c "import numpy ; print numpy.version.version.split('.')[0]"` -lt "${NUMPY_MAJOR}" ] ; then
      AC_MSG_RESULT([no])
      AC_MSG_NOTICE([your python does not have a recent enough numpy (MAJOR release is wrong), we will rebuild it])
      NUMPY=yes
    else
      if test [`${PYTHON} -c "import numpy ; print numpy.version.version.split('.')[0]"` -eq "${NUMPY_MAJOR}" ] ; then
        if test [`${PYTHON} -c "import numpy ; print numpy.version.version.split('.')[1]"` -lt "${NUMPY_MINOR}" ] ; then
          AC_MSG_RESULT([no])
          AC_MSG_NOTICE([your python does not have a recent enough numpy (MINOR release is wrong), we will rebuild it])
          NUMPY=yes
        else
          if test [`${PYTHON} -c "import numpy ; print numpy.version.version.split('.')[1]"` -eq "${NUMPY_MINOR}" ] ; then
            if test [`${PYTHON} -c "import numpy ; print numpy.version.version.split('.')[2]"` -lt "${NUMPY_PATCH}" ] ; then
              AC_MSG_RESULT([no])
              AC_MSG_NOTICE([your python does not have a recent enough numpy (PATCH NUMBER is WRONG), we will rebuild it])
              NUMPY=yes
            else
              AC_MSG_RESULT([yes])
            fi
          else
            AC_MSG_RESULT([yes])
          fi
        fi
      else
        AC_MSG_RESULT([yes])
      fi
    fi
  fi
  dnl ########################################################
  dnl  setuptools
  dnl ########################################################
  SETUPTOOLS=no
  AC_MSG_CHECKING([if python ${PYTHON} has setuptools support]) 
  if [test `${PYTHON} -c "cmd = 'try:\n  import setuptools\n  print 0\nexcept:\n  print 1'; exec(cmd)"` == "1"] ; then
    AC_MSG_RESULT([no])
    AC_MSG_NOTICE([your python does not have setuptools, we will build it])
    SETUPTOOLS=yes
  else
    AC_MSG_RESULT([yes])
    AC_MSG_CHECKING([if setuptools is the right version ( > ${SETUPTOOLS_MAJOR}.${SETUPTOOLS_MINOR})])
    if test [`${PYTHON} -c "import setuptools ; print setuptools.__version__.split('.')[0]"` -lt "${SETUPTOOLS_MAJOR}" ] ; then
      AC_MSG_RESULT([no])
      AC_MSG_NOTICE([your python does not have a recent enough setuptools (MAJOR release is wrong), we will rebuild it])
      SETUPTOOLS=yes
    else
      if test [`${PYTHON} -c "import setuptools ; print setuptools.__version__.split('.')[0]"` -eq "${SETUPTOOLS_MAJOR}" ] ; then
        if test [`${PYTHON} -c "import setuptools ; vmin = setuptools.__version__.split('.')[1]; cont = True ; cmd = 'while cont:\n try:\n  vmin=int(vmin)\n  cont=False\n except:\n  vmin=vmin[:-1]';exec(cmd);print vmin"` -lt "${SETUPTOOLS_MINOR}" ] ; then
          AC_MSG_RESULT([no])
          AC_MSG_NOTICE([your python does not have a recent enough setuptools (MINOR release is wrong), we will rebuild it])
          SETUPTOOLS=yes
	else
          AC_MSG_RESULT([yes])
        fi
      else
        AC_MSG_RESULT([yes])
      fi
    fi
  fi
  PyOpenGL=no
  if test ${enable_cdms_only} != "yes" ; then
   dnl ########################################################
   dnl  PyOpenGL
   dnl ########################################################
   AC_MSG_CHECKING([if python ${PYTHON} has PyOpenGL support]) 
   if [test `${PYTHON} -c "cmd = 'try:\n  import OpenGL\n  print 0\nexcept:\n  print 1'; exec(cmd)"` == "1"] ; then
     AC_MSG_RESULT([no])
     AC_MSG_NOTICE([your python does not have PyOpenGL, we will build it])
     PyOpenGL=yes
   else
     AC_MSG_RESULT([yes])
     AC_MSG_CHECKING([if PyOpenGL is the right version ( > ${PyOpenGL_MAJOR}.${PyOpenGL_MINOR})])
     if test [`${PYTHON} -c "import OpenGL ; print OpenGL.__version__.split('.')[0]"` -lt "${PyOpenGL_MAJOR}" ] ; then
       AC_MSG_RESULT([no])
       AC_MSG_NOTICE([your python does not have a recent enough PyOpenGL (MAJOR release is wrong), we will rebuild it])
       PyOpenGL=yes
     else
       if test [`${PYTHON} -c "import OpenGL ; print OpenGL.__version__.split('.')[0]"` -eq "${PyOpenGL_MAJOR}" ] ; then
         if test [`${PYTHON} -c "import OpenGL ; vmin = OpenGL.__version__.split('.')[1]; cont = True ; cmd = 'while cont:\n try:\n  vmin=int(vmin)\n  cont=False\n except:\n  vmin=vmin[:-1]';exec(cmd);print vmin"` -lt "${PyOpenGL_MINOR}" ] ; then
           AC_MSG_RESULT([no])
           AC_MSG_NOTICE([your python does not have a recent enough PyOpenGL (MINOR release is wrong), we will rebuild it])
           PyOpenGL=yes
 	else
           AC_MSG_RESULT([yes])
         fi
       else
         AC_MSG_RESULT([yes])
       fi
     fi
   fi
  fi
  dnl  no need to go further is cdms-only
  if test  ${enable_cdms_only} != "-yes" -o ${enable_esg} == "-yes" ; then
    dnl ##########################################################
    dnl  Pmw
    dnl ##########################################################
    Pmw=no
    AC_MSG_CHECKING([if python ${PYTHON} has Pmw support]) 
    if [test `${PYTHON} -c "cmd = 'try:\n  import Pmw\n  print 0\nexcept:\n  print 1'; exec(cmd)"` == "1"] ; then
      AC_MSG_RESULT([no])
      AC_MSG_NOTICE([your python does not have Pmw, we will build it])
      Pmw=yes
    else
      AC_MSG_RESULT([yes])
      AC_MSG_CHECKING([if Pmw is the right version ( > ${PMW_MAJOR}.${PMW_MINOR})])
      if test [`${PYTHON} -c "import Pmw ; print Pmw.version().split('.')[0]"` -lt "${PMW_MAJOR}" ] ; then
        AC_MSG_RESULT([no])
        AC_MSG_NOTICE([your python does not have a recent enough Pmw, we will rebuild it])
        Pmw=yes
      else
        if test [`${PYTHON} -c "import Pmw ; print Pmw.version().split('.')[1]"` -lt "${PMW_MINOR}" ] ; then
          AC_MSG_RESULT([no])
          AC_MSG_NOTICE([your python does not have a recent enough Pmw, we will rebuild it])
          Pmw=yes
        else
          AC_MSG_RESULT([yes])
        fi
      fi
    fi
  fi
else
  NUMPY=yes
  SETUPTOOLS=yes
  PyOpenGL=no
  Pmw=no
dnl   SCIPY=yes
  if test  ${enable_cdms_only} != "-yes" ; then
    Pmw=yes
    PyOpenGL=yes
  fi
  if test  ${enable_esg} == "-yes" ; then
    Pmw="yes"
  fi
fi

dnl ###########################################################
dnl  FLTK2
dnl ###########################################################
COMPILE_FLTK2=yes
AC_PATH_PROG(FLTK2_CONF,fltk2-config,"no")
if test ${FLTK2_CONF} != "no" ; then
   FLFLAGS=`${FLTK2_CONF} --ldflags`
   AC_CHECK_LIB([fltk2],[_ZN4fltk4beepEi],[COMPILE_FLTK2=no],[COMPILE_FLTK2=yes],[ ${FLFLAGS}  ])
fi



dnl ###########################################################
dnl  PKG_CONFIG
dnl ###########################################################
AC_PATH_PROG(PKG_CONFIG,pkg-config,"no")
AC_MSG_CHECKING([for a usable pkg-config > ${PKG_MAJOR}.${PKG_MINOR}.${PKG_PATCH}])
if AC_RUN_LOG([ ${PKG_CONFIG} --atleast-pkgconfig-version ${PKG_MAJOR}.${PKG_MINOR}.${PKG_PATCH} --print-errors ]); then
  COMPILE_PKGCONFIG=no  
  AC_MSG_RESULT([yes])
else
  COMPILE_PKGCONFIG=yes  
  AC_MSG_RESULT([no])
  AC_MSG_WARN([no pkgconfig working found therefore we will probably fail the following pkgconfig tests and might lead to unnecessary  build])
fi

dnl ###########################################################
dnl ## LIBXML 
dnl ###########################################################
AC_MSG_CHECKING([for a usable xml2 > ${XML_MAJOR}.${XML_MINOR}.${XML_PATCH}])
if AC_RUN_LOG([${PKG_CONFIG} --atleast-version ${XML_MAJOR}.${XML_MINOR}.${XML_PATCH} --print-errors libxml-2.0 ]); then
  COMPILE_XML2=no
  AC_MSG_RESULT([yes])
else
  COMPILE_XML2=yes  
  AC_MSG_RESULT([no])
fi
dnl ###########################################################
dnl ## LIBXSLT 
dnl ###########################################################
if test ${enable_esg} == "-yes" ; then
 AC_MSG_CHECKING([for a usable libxslt > ${XSLT_MAJOR}.${XSLT_MINOR}.${XSLT_PATCH}])
 if AC_RUN_LOG([${PKG_CONFIG} --atleast-version ${XSLT_MAJOR}.${XSLT_MINOR}.${XSLT_PATCH} --print-errors libxslt ]); then
  COMPILE_XSLT=no
  AC_MSG_RESULT([yes])
 else
  COMPILE_XSLT=yes  
  AC_MSG_RESULT([no])
 fi
fi
if test  ${enable_cdms_only} != "-yes" ; then
  dnl ###########################################################
  dnl LIBPNG
  dnl ###########################################################
  AC_MSG_CHECKING([for a usable libpng > ${PNG_MAJOR}.${PNG_MINOR}.${PNG_PATCH}])
  if AC_RUN_LOG([${PKG_CONFIG} --atleast-version ${PNG_MAJOR}.${PNG_MINOR}.${PNG_PATCH} --print-errors libpng ]); then
    COMPILE_LIBPNG=no
    AC_MSG_RESULT([yes])
  else
    COMPILE_LIBPNG=yes  
    AC_MSG_RESULT([no])
  fi

  AC_MSG_CHECKING([for a usable freetype > ${FT_MAJOR}.${FT_MINOR}.${FT_PATCH}])
  if AC_RUN_LOG([${PKG_CONFIG} --atleast-version ${FT_MAJOR}.${FT_MINOR}.${FT_PATCH} --print-errors freetype2]); then
    COMPILE_FREETYPE=no  
    AC_MSG_RESULT([yes])
  else
    COMPILE_FREETYPE=yes  
    AC_MSG_RESULT([no])
  fi
  dnl ###########################################################
  dnl  FONTCONFIG
  dnl ###########################################################
  AC_MSG_CHECKING([for fontconfig > ${FTCFG_MAJOR}.${FTCFG_MINOR}.${FTCFG_PATCH}])
  if AC_RUN_LOG([ ${PKG_CONFIG} --atleast-version ${FTCFG_MAJOR}.${FTCFG_MINOR}.${FTCFG_PATCH}  --print-errors fontconfig]); then
    COMPILE_FONTCONFIG=no  
    AC_MSG_RESULT([yes])
  else
    COMPILE_FONTCONFIG=yes  
    AC_MSG_RESULT([no])
  fi

  dnl ###########################################################
  dnl  LIBPIXMAN-1
  dnl ###########################################################
  AC_MSG_CHECKING([for a usable pixman-1 > ${PIX_MAJOR}.${PIX_MINOR}.${PIX_PATCH}])
  if AC_RUN_LOG([ ${PKG_CONFIG} --atleast-version ${PIX_MAJOR}.${PIX_MINOR}.${PIX_PATCH} --print-errors pixman-1]); then
     COMPILE_PIXMAN=no  
     AC_MSG_RESULT([yes])
  else
     COMPILE_PIXMAN=yes  
     AC_MSG_RESULT([no])
  fi
  dnl ###########################################################
  dnl  CAIRO
  dnl ###########################################################
  AC_MSG_CHECKING([for a usable cairo > ${CAIRO_MAJOR}.${CAIRO_MINOR}.${CAIRO_PATCH}])
  if AC_RUN_LOG([${PKG_CONFIG} --atleast-version ${CAIRO_MAJOR}.${CAIRO_MINOR}.${CAIRO_PATCH}  --print-errors cairo-xlib]); then
    COMPILE_CAIRO=no
    AC_MSG_RESULT([yes])
  else
    COMPILE_CAIRO=yes  
    AC_MSG_RESULT([no])
  fi
  dnl ###########################################################
  dnl  UDUNITS2
  dnl ###########################################################
  COMPILE_UDUNITS2=no
  AC_ARG_WITH([udunits2],[AS_HELP_STRING([--with-udunits2],[enable location of udunits2])],[],[with_udunits2=yes])
  if test ${with_udunits2} == "yes" ; then
       UDUNITS2LOC="${EXTERNALS}"
  else 
      if test ${with_udunits2} != "no" ; then
        UDUNITS2LOC=${with_udunits2}
dnl      CPPFLAGS="-I${with_udunits2}/include "${CPPFLAGS}
dnl      LDFLAGS="-L${with_udunits2}/lib "${LDFLAGS}
dnl      CFLAGS="-I${with_udunits2}/include -L${with_udunits2}/lib "${CFLAGS}
      fi
  fi
  if test ${with_udunits2} != "no" ; then
      dnl ###########################################################
      dnl  LIBUDUNITS2
      dnl ###########################################################
      AC_CHECK_LIB([udunits2],[ut_parse],[COMPILE_UDUNITS2=no],[COMPILE_UDUNITS2=yes],[""])
  fi
  dnl ###########################################################
  dnl  UUID
  dnl ###########################################################
  AC_ARG_WITH([uuid],[AS_HELP_STRING([--with-uuid],[enable support for uuid in none standard location])],[],[with_uuid="no"])
  with_uuidb="-"${with_uuid}
  if [ test ${with_uuidb} != "-yes" ] ; then
   if [ test ${with_uuidb} != "-no" ] ; then
    UUIDFLAGS=" -I${with_uuid}/include"
    UUIDLDFLAGS=" -L${with_uuid}/lib -luuid"
    UUIDLOC=${with_uuid}
   else
     UUIDFLAGS="-luuid"
     UUIDLDFLAGS=""
     UUIDLOC="${EXTERNALS}"
   fi
  else
   UUIDFLAGS="-luuid"
   UUIDLDFLAGS=""
   UUIDLOC="${EXTERNALS}"
  fi
  AC_CHECK_LIB([uuid],[uuid_create],[COMPILE_UUID=no],[COMPILE_UUID=yes],[] )
fi

COMPILE_NC=no
COMPILE_CURL=no

AC_ARG_WITH([netcdf],[AS_HELP_STRING([--with-netcdf],[enable support for NetCDF in some none standard location])],[],[with_netcdf=no])
AC_ARG_ENABLE([netcdf-fortran],[AS_HELP_STRING([--enable-netcdf-fortran],[enables netcdf-fortran (default is no)])])
enable_netcdf_fortran="-"${enable_netcdf_fortran}
if test  ${enable_netcdf_fortran} == "-yes" ; then
   NC_FORTRAN=""
else
   NC_FORTRAN=" export FC=''; export F77='' ;  export FCFLAGS='' ; export FCLIBS='' ; export FFLAGS='' ; export FLIBS='' ;"
fi

dnl ###########################################################
dnl  LIBCURL
dnl ###########################################################
AC_CHECK_LIB([curl],[curl_version],[COMPILE_CURL=no],[COMPILE_CURL=yes])
AC_MSG_CHECKING([if you have curl > ${CURL_MAJOR}.${CURL_MINOR}.${CURL_PATCH}])
AC_LANG([C])
AC_RUN_IFELSE(
	[AC_LANG_PROGRAM([
		#include <stdio.h>
		#include <curl/curlver.h>
		],
		[
		 if (LIBCURL_VERSION_MAJOR<${CURL_MAJOR}) return 1;
		 if (LIBCURL_VERSION_MAJOR>${CURL_MAJOR}) return 0;
		 if (LIBCURL_VERSION_MINOR<${CURL_MINOR}) return 1;
		 if (LIBCURL_VERSION_MINOR>${CURL_MINOR}) return 0;
		 if (LIBCURL_VERSION_PATCH<${CURL_PATCH}) return 1;
		])],
	[COMPILE_CURL=no],
	[COMPILE_CURL=yes]
	)
if test ${COMPILE_CURL} == "no" ; then
  AC_MSG_RESULT([yes])
else
  AC_MSG_RESULT([no])
fi

dnl ###########################################################
dnl  HDF 5
dnl ###########################################################
COMPILE_HDF5=no
AC_ARG_WITH([hdf5],[AS_HELP_STRING([--with-hdf5],[enable location of hdf5 libs])],[],[with_hdf5=yes])
if test ${with_hdf5} == "yes" ; then
    HDF5LOC="${EXTERNALS}/HDF5"
dnl    CPPFLAGS="-I${EXTERNALS}/HDF5/include "${CPPFLAGS}
dnl    LDFLAGS="-L${EXTERNALS}/HDF5/lib "${LDFLAGS}
dnl    CFLAGS="-I${EXTERNALS}/HDF5/include -L${EXTERNALS}/HDF5/lib "${CFLAGS}
else 
    if test ${with_hdf5} != "no" ; then
      HDF5LOC=${with_hdf5}
dnl      CPPFLAGS="-I${with_hdf5}/include "${CPPFLAGS}
dnl      LDFLAGS="-L${with_hdf5}/lib "${LDFLAGS}
dnl      CFLAGS="-I${with_hdf5}/include -L${with_hdf5}/lib "${CFLAGS}
    fi
fi
if test ${with_hdf5} != "no" ; then
    dnl ###########################################################
    dnl  LIBHDF5
    dnl ###########################################################
    AC_CHECK_LIB([hdf5],[H5open],[COMPILE_HDF5=no],[COMPILE_HDF5=yes],[ -lz  ])
    AC_CHECK_LIB([hdf5_hl],[H5PTopen],[COMPILE_HDF5=no],[COMPILE_HDF5=yes],[-lhdf5 -lz ])
dnl    CPPFLAGS=${CPPFLAGS}" -I${EXTERNALS}/HDF5/include"
dnl    CFLAGS="-I${EXTERNALS}/HDF5/include -L${EXTERNALS}/HDF5/lib "${CFLAGS}
    dnl #AC_CHECK_HEADER([hdf5.h],[],[COMPILE_HDF5=yes])
fi

dnl ###########################################################
dnl  LIBNETCDF4
dnl ###########################################################
if test ${with_netcdf} != "yes" -a ${with_netcdf} != "no"; then
    AC_CHECK_FILE([${with_netcdf}/bin/nc-config],[HAS_NCCONFIG=yes],[HAS_NCCONFIG=no])
    if  test ${HAS_NCCONFIG} == "yes"  ; then 
      NCCFLAGS=`${with_netcdf}/bin/nc-config --cflags`
      NCLDFLAGS=`${with_netcdf}/bin/nc-config --libs`
    else 
     NCCFLAGS=" -I${with_netcdf}/include"
     NCLDFLAGS=" -L${with_netcdf}/lib "
    fi
    NC4LOC=${with_netcdf}
    CPPFLAGS=${NCCFLAGS}" "${CPPFLAGS}
    LDFLAGS=${NCLDFLAGS}" "${LDFLAGS}
    CFLAGS=${NCCFLAGS}" "${CFLAGS}
else
    AC_PATH_PROG(LOCNCCONFIG,nc-config,"no")
    if  test ${LOCNCCONFIG} != "no" ; then 
      NCCFLAGS=`nc-config --cflags`
      NCLDFLAGS=`nc-config --libs`
      NC4LOC=`nc-config --prefix`
      CPPFLAGS=${NCCFLAGS}" "${CPPFLAGS}
      LDFLAGS=${NCLDFLAGS}" "${LDFLAGS}
      CFLAGS=${NCCFLAGS}" "${CFLAGS}
    else
      NC4LOC=${EXTERNALS}/NetCDF
      CPPFLAGS="-I${EXTERNALS}/NetCDF/include "${CPPFLAGS}
      LDFLAGS="-L${EXTERNALS}/NetCDF/lib "${LDFLAGS}
      CFLAGS="-I${EXTERNALS}/NetCDF/include -L${EXTERNALS}/NetCDF/lib "${CFLAGS}
      NCLDFLAGS="-L${EXTERNALS}/NetCDF/lib "
      NCCFLAGS="-I${EXTERNALS}/NetCDF/include"
    fi
fi
AC_CHECK_LIB([netcdf],[nc_open],[COMPILE_NC=no],[COMPILE_NC=yes],[ ${NCCFLAGS} ${NCLDFLAGS} ])
if test ${COMPILE_NC} == "no" ; then
    AC_CHECK_FILE([${with_netcdf}/bin/nc-config],[HAS_NCCONFIG=yes],[HAS_NCCONFIG=no])
    if  test ${HAS_NCCONFIG} == "yes"  ; then 
      NCCFLAGS=`${with_netcdf}/bin/nc-config --cflags`
      NCLDFLAGS=`${with_netcdf}/bin/nc-config --libs`
    else 
     NCCFLAGS=" -I${with_netcdf}/include"
     NCLDFLAGS=" -L${with_netcdf}/lib "
    fi
    LIBS=${LIBS}" "${NCLDFLAGS}
    AC_MSG_CHECKING([if you have version 4 of NetCDF])
    AC_RUN_IFELSE(
    [AC_LANG_PROGRAM([
    #include <stdio.h>
    #include <stdlib.h>
    #include <netcdf.h>
    ],
    [[
      char version[50];
        int num;
	strcpy(version,nc_inq_libvers());
	num=atoi(&version[0]);
	if (num>=${NC4_MAJOR}) return 0;
	num=atoi(&version[1]); /*sometimes there is a " before */
	if (num>${NC4_MAJOR}) return 0;
	return 1;
    ]])],[COMPILE_NC=no],[COMPILE_NC=yes])
    if test ${COMPILE_NC} == "no" ; then
        AC_MSG_RESULT([yes])
    else
        AC_MSG_RESULT([no])
    fi
    dnl LDFLAGS=${LDFLAGS_OLD}
fi  


dnl ###########################################################
dnl  HDF 4
dnl ###########################################################
COMPILE_HDF=no
AC_ARG_WITH([hdf4],[AS_HELP_STRING([--with-hdf4],[enable support for HDF4])],[],[with_hdf4=no])
if test ${with_hdf4} == "yes" ; then
dnl  CPPFLAGS=${CPPFLAGS}" -I${EXTERNALS}/HDF/include"
  PYTHON_INSTALL_OPTIONS=${PYTHON_INSTALL_OPTIONS}" --with-hdf=${EXTERNALS}/HDF"
else 
  if test ${with_hdf4} != "no" ; then
dnl    CPPFLAGS=${CPPFLAGS}" -I${with_hdf4}/include"
dnl    LDFLAGS=${LDFLAGS}" -L${with_hdf4}/lib"
    PYTHON_INSTALL_OPTIONS=${PYTHON_INSTALL_OPTIONS}" --with-hdf=${with_hdf4}"
  fi
fi

dnl ###########################################################
dnl Ghostscript stuff
dnl ###########################################################

if test ${DOGS} == "no" -a ${COMPILE_HDF} == "no" ; then
  COMPILE_LIBJPEG=no
fi

dnl AC_ARG_ENABLE([ioapi],[AS_HELP_STRING([--enable-ioapi],[enable ioapi (default is off)])])
dnl enable_ioapi="-"${enable_ioapi}
dnl if test ${enable_ioapi} == "-yes" ; then
dnl    COMPILE_IOAPI=yes
dnl    PYTHON_INSTALL_OPTIONS=${PYTHON_INSTALL_OPTIONS}" --enable-ioapi"
dnl    NC_FORTRAN=""
dnl else
dnl    COMPILE_IOAPI=no
dnl fi

if test ${enable_esg} == "-yes" ; then
  if test ${enable_X11} == "-no" ; then
     COMPILE_TCLTK=no
     COMPILE_XGKS=no
  fi
fi

AC_ARG_ENABLE([all],[AS_HELP_STRING([--enable-all],[build all externals])])
enall="-"${enable_all}
if test ${enall} == "-yes" ; then
COMPILE_TERMCAP="yes"
COMPILE_READLINE="yes"
COMPILE_Z="yes"
COMPILE_PKGCONFIG="yes"
COMPILE_XML2="yes"
COMPILE_CURL="yes"
COMPILE_XSLT="yes"
COMPILE_PNG="yes"
COMPILE_FREETYPE="yes"
COMPILE_PIXMAN="yes"
COMPILE_FONTCONFIG="yes"
COMPILE_TCLTK="yes"
COMPILE_XGKS="yes"
COMPILE_LIBJPEG="yes"
DOPBMPLUS="yes"
GIFSICLE="yes"
DOGS="yes"
FFMPEG="no"
COMPILE_CAIRO="yes"
COMPILE_HDF5="yes"
COMPILE_UDUNITS2="yes"
COMPILE_NC="yes"
COMPILE_LIBPNG="yes"
AC_MSG_NOTICE([ALL EXTERNALS TURN ON])

fi

dnl ###########################################################
dnl QT
dnl ###########################################################

AC_ARG_WITH([qt],
	    [AS_HELP_STRING([--with-qt],[specify where to find qt default: /usr/local/Trolltech/Qt-4.5.2])],
	    [],
	    [with_qt='no'])
if test ${with_qt} != "no" ; then
   PYTHON_INSTALL_OPTIONS=${PYTHON_INSTALL_OPTIONS}" --with-qt="${with_qt}
fi;
AC_ARG_WITH([qt-lib],
	    [AS_HELP_STRING([--with-qt-lib],[specify where to find qt libraries],)],
	    [],
	    [with_qt_lib='no'])
if test ${with_qt} != "no" ; then
   PYTHON_INSTALL_OPTIONS=${PYTHON_INSTALL_OPTIONS}" --with-qt-lib="${with_qt_lib}
fi;
AC_ARG_WITH([qt-inc],
	    [AS_HELP_STRING([--with-qt-inc],[specify where to find qt includes],)],
	    [],
	    [with_qt_inc='no'])
if test ${with_qt-inc} != "no" ; then
   PYTHON_INSTALL_OPTIONS=${PYTHON_INSTALL_OPTIONS}" --with-qt-inc="${with_qt_inc}
fi;
AC_ARG_WITH([qt-bin],
	    [AS_HELP_STRING([--with-qt-bin],[specify where to find qt binary (namely the moc bin)],)],
	    [],
	    [with_qt_bin='no'])
if test ${with_qt_bin} != "no" ; then
   PYTHON_INSTALL_OPTIONS=${PYTHON_INSTALL_OPTIONS}" --with-qt-bin="${with_qt_bin}
fi;
AC_ARG_ENABLE([qt-debug],
	    [AS_HELP_STRING([--enable-qt-debug],[adds _debug to Qt libs])],
	    [],
	    [enable_qt_debug='no'])
enble_qtdbg="-"${enable_qt_debug}
if test ${enble_qtdbg} == "-yes" ; then
     PYTHON_INSTALL_OPTIONS=${PYTHON_INSTALL_OPTIONS}" --qt-debug"
fi;
AC_ARG_ENABLE([qt],
	    [AS_HELP_STRING([--enable-qt],[use qt instead of tcl/tk default: /usr/local/Trolltech/Qt-4.5.2/ to change these use --with-qt/--with-qt-inc/--with-qt-lib/--with-qt-bin instead])],
	    [],
	    [enable_qt='no'])
enble_qt="-"${enable_qt}
if test ${enble_qt} == "-yes" ; then
   if test ${enable_fw} == "-yes" ; then
     PYTHON_INSTALL_OPTIONS=${PYTHON_INSTALL_OPTIONS}" --enable-qt-framework"
   else
     PYTHON_INSTALL_OPTIONS=${PYTHON_INSTALL_OPTIONS}" --enable-qt"
   fi;
fi;
AC_ARG_ENABLE([qtframework],
	    [AS_HELP_STRING([--enable-qtframework],[use qt framework instead of tcl/tk],)],
	    [],
	    [enable_qtframework='no'])
enble_qtfw="-"${enable_qtframework}
if test ${enble_qtfw} == "-yes" ; then
     PYTHON_INSTALL_OPTIONS=${PYTHON_INSTALL_OPTIONS}" --enable-qt-framework"
fi;
AC_OUTPUT

AC_MSG_NOTICE([])
AC_MSG_NOTICE([************************************************************************])
AC_MSG_NOTICE([                              SUMMARY])
AC_MSG_NOTICE([                            VERSION: ${CDAT_VERSION}])
AC_MSG_NOTICE([************************************************************************])
AC_MSG_NOTICE([])
AC_MSG_NOTICE([........................................................................])
AC_MSG_NOTICE([...                             EXTERNALS                            ...])
if test ${NCPUS} != "1" ; then
AC_MSG_NOTICE([...                     Taking advantage of ${NCPUS} CPUS                  ...])
fi
AC_MSG_NOTICE([........................................................................])
AC_MSG_NOTICE([])
if test ${COMPILE_CURL} == "yes" ; then
  AC_MSG_NOTICE([curl >= ${CURL_MAJOR}.${CURL_MINOR}.${CURL_PATCH} not found... building library ${CURL_MAJOR_SRC}.${CURL_MINOR_SRC}.${CURL_PATCH_SRC}])
else
  AC_MSG_NOTICE([curl >= ${CURL_MAJOR}.${CURL_MINOR}.${CURL_PATCH} found or not necessary for this configuration])
fi
if test ${COMPILE_XML2} == "yes" ; then
  AC_MSG_NOTICE([libxml2 >= ${XML_MAJOR}.${XML_MINOR}.${XML_PATCH} not found... building library ${XML_MAJOR_SRC}.${XML_MINOR_SRC}.${XML_PATCH_SRC}])
else
  AC_MSG_NOTICE([libxml2 >= ${XML_MAJOR}.${XML_MINOR}.${XML_PATCH} found])
fi
if test ${enable_esg} == "-yes" ; then
 if test ${COMPILE_XSLT} == "yes" ; then
  AC_MSG_NOTICE([libxslt >= ${XSLT_MAJOR}.${XSLT_MINOR}.${XSLT_PATCH} not found... building library ${XSLT_MAJOR_SRC}.${XSLT_MINOR_SRC}.${XSLT_PATCH_SRC}])
 else
  AC_MSG_NOTICE([libxslt >= ${XSLT_MAJOR}.${XSLT_MINOR}.${XSLT_PATCH} found])
 fi
fi
if test ${enable_cdms_only} != "-yes" ; then
 if test ${COMPILE_CMAKE} == "yes" ; then
  AC_MSG_NOTICE([cmake >= ${CMAKE_MAJOR}.${CMAKE_MINOR} not found... building software ${CMAKE_MAJOR_SRC}.${CMAKE_MINOR_SRC}.${CMAKE_PATCH_SRC}])
 else
  AC_MSG_NOTICE([cmake >= ${CMAKE_MAJOR}.${CMAKE_MINOR} found])
 fi
 if test ${COMPILE_XGKS} == "yes" ; then
   AC_MSG_NOTICE([xgks not found ... building library])
 else
   AC_MSG_NOTICE([xgks found])
 fi
 if test ${DOPBMPLUS} == "yes" ; then
   AC_MSG_NOTICE([pbmplus not found ... building library])
 else
   AC_MSG_NOTICE([pbmplus found])
 fi
 if test ${FFMPEG} == "no" ; then
   AC_MSG_NOTICE([ffmpeg not found ... building software])
 else
   AC_MSG_NOTICE([ffmpeg found])
 fi
 if test ${GIFSICLE} == "no" ; then
   AC_MSG_NOTICE([gifsicle not found ... building software ${GIFSICLE_MAJOR_SRC}.${GIFSICLE_MINOR_SRC}])
 else
   AC_MSG_NOTICE([gifsicle found])
 fi
 if test ${DOGS} == "yes" ; then
   AC_MSG_NOTICE([gs not found ... building software])
 else
   AC_MSG_NOTICE([gs found])
 fi
 if test ${COMPILE_LIBJPEG} == "yes" ; then
   AC_MSG_NOTICE([libjpeg not found ... building library])
 else
   AC_MSG_NOTICE([libjpeg found or not necessary for this configuration])
 fi
 if test ${HAVE_LIBBLAS} == "no" ; then
   AC_MSG_NOTICE([libblas not found ... building library])
 else
   AC_MSG_NOTICE([libblas found or not necessary for this configuration])
 fi
 if test ${HAVE_LIBLAPACK} == "no" ; then
   AC_MSG_NOTICE([liblapack not found ... building library])
 else
   AC_MSG_NOTICE([liblapack found or not necessary for this configuration])
 fi
 if test ${HAVE_LIBLAPACK95} == "no" ; then
   AC_MSG_NOTICE([liblapack95 not found ... building library])
 else
   AC_MSG_NOTICE([liblapack95 found or not necessary for this configuration])
 fi
dnl  if test ${COMPILE_IOAPI} == "yes" ; then
dnl   AC_MSG_NOTICE([ioapi enabled... building ioapi, proj and gdal])
dnl  else
dnl   AC_MSG_NOTICE([ioapi not enabled])
dnl  fi
 if test ${COMPILE_LIBPNG} == "yes" ; then
  AC_MSG_NOTICE([libpng >= ${PNG_MAJOR}.${PNG_MINOR}.${PNG_PATCH} not found... building library ${PNG_MAJOR_SRC}.${PNG_MINOR_SRC}.${PNG_PATCH_SRC}])
 else
  AC_MSG_NOTICE([libpng >= ${PNG_MAJOR}.${PNG_MINOR}.${PNG_PATCH} found])
 fi
 if test ${COMPILE_PKGCONFIG} == "yes" ; then
  AC_MSG_NOTICE([pkg-config >= ${PKG_MAJOR}.${PKG_MINOR}.${PKG_PATCH} not found... building software ${PKG_MAJOR_SRC}.${PKG_MINOR_SRC}.${PKG_PATCH_SRC}])
 else
  AC_MSG_NOTICE([pkg-config >= ${PKG_MAJOR}.${PKG_MINOR}.${PKG_PATCH} found])
 fi
 if test ${COMPILE_FREETYPE} == "yes" ; then
  AC_MSG_NOTICE([libfreetype >= ${FT_MAJOR}.${FT_MINOR}.${FT_PATCH} not found... building library ${FT_MAJOR_SRC}.${FT_MINOR_SRC}.${FT_PATCH_SRC} ])
 else
  AC_MSG_NOTICE([libfreetype >= ${FT_MAJOR}.${FT_MINOR}.${FT_PATCH} found])
 fi
 if test ${COMPILE_PIXMAN} == "yes" ; then
  AC_MSG_NOTICE([libpixman >= ${PIX_MAJOR}.${PIX_MINOR}.${PIX_PATCH} not found... building  library ${PIX_MAJOR_SRC}.${PIX_MINOR_SRC}.${PIX_PATCH_SRC}])
 else
  AC_MSG_NOTICE([libpixman >= ${PIX_MAJOR}.${PIX_MINOR}.${PIX_PATCH} found])
 fi
 if test ${COMPILE_FONTCONFIG} == "yes" ; then
  AC_MSG_NOTICE([fontconfig >= ${FTCFG_MAJOR}.${FTCFG_MINOR}.${FTCFG_PATCH} not found... building library ${FTCFG_MAJOR_SRC}.${FTCFG_MINOR_SRC}.${FTCFG_PATCH_SRC}])
 else
  AC_MSG_NOTICE([fontconfig >= ${FTCFG_MAJOR}.${FTCFG_MINOR}.${FTCFG_PATCH} found])
 fi
 if test ${COMPILE_CAIRO} == "yes" ; then
  AC_MSG_NOTICE([libcairo >= ${CAIRO_MAJOR}.${CAIRO_MINOR}.${CAIRO_PATCH} not found... building library ${CAIRO_MAJOR_SRC}.${CAIRO_MINOR_SRC}.${CAIRO_PATCH_SRC}])
 else
  AC_MSG_NOTICE([libcairo >= ${CAIRO_MAJOR}.${CAIRO_MINOR}.${CAIRO_PATCH} found])
 fi
fi
if test ${enable_esg} != "-yes" ; then
  if test ${COMPILE_UUID} == "yes" ; then
    AC_MSG_NOTICE([libuuid not found... building library ${UUID_MAJOR_SRC}.${UUID_MINOR_SRC}.${UUID_PATCH_SRC}])
  else
    AC_MSG_NOTICE([libuuid found])
  fi
  if test ${COMPILE_UDUNITS2} == "yes" ; then
    AC_MSG_NOTICE([libudunits2 not found... building library ${UDUNITS2_MAJOR_SRC}.${UDUNITS2_MINOR_SRC}.${UDUNITS2_PATCH_SRC}])
  else
    AC_MSG_NOTICE([libudunits2 found])
  fi
fi
if test ${COMPILE_HDF5} == "yes" ; then
    AC_MSG_NOTICE([libhdf5 not found... building library ${HDF5_MAJOR_SRC}.${HDF5_MINOR_SRC}.${HDF5_PATCH_SRC}])
else
    AC_MSG_NOTICE([libhdf5 found])
fi
if test ${COMPILE_NC} == "yes" ; then
    AC_MSG_NOTICE([libnetcdf >=4 not found... building library ${NC4_MAJOR_SRC}.${NC4_MINOR_SRC}.${NC4_PATCH_SRC}])
else
    AC_MSG_NOTICE([libnetcdf >=4 found at ${NC4LOC}])
fi
AC_MSG_NOTICE([])
AC_MSG_NOTICE([........................................................................])
AC_MSG_NOTICE([...                               PYTHON                             ...])
AC_MSG_NOTICE([........................................................................])
AC_MSG_NOTICE([])
if test ${COMPILE_PYTHON} == "yes" ; then
  AC_MSG_NOTICE([python >= ${PYTHON_MAJOR}.${PYTHON_MINOR} with tcl/tk support not found... building software ${PYTHON_MAJOR_SRC}.${PYTHON_MINOR_SRC}.${PYTHON_PATCH_SRC}])
  if test ${COMPILE_Z} == "yes" ; then
     AC_MSG_NOTICE([zlib not found ... building library ${ZLIB_MAJOR_SRC}.${ZLIB_MINOR_SRC}.${ZLIB_PATCH_SRC}])
  else
     AC_MSG_NOTICE([zlib found])
  fi
  if test ${COMPILE_TERMCAP} == "yes" ; then
     AC_MSG_NOTICE([termcap not found ... building library ${TCAP_MAJOR_SRC}.${TCAP_MINOR_SRC}.${TCAP_PATCH_SRC}])
  else
     AC_MSG_NOTICE([termcap found])
  fi
  if test ${COMPILE_READLINE} == "yes" ; then
    AC_MSG_NOTICE([libreadline >= ${RL_MAJOR}.${RL_MINOR} not found... building library ${RL_MAJOR_SRC}.${RL_MINOR_SRC}])
  else
    AC_MSG_NOTICE([libreadline >= ${RL_MAJOR}.${RL_MINOR} found])
  fi
  if test ${COMPILE_TCLTK} == "yes" ; then
    AC_MSG_NOTICE([tcl/tk >= ${TCLTK_MAJOR}.${TCLTK_MINOR}.${TCLTK_PATCH} not found... building library ${TCLTK_MAJOR_SRC}.${TCLTK_MINOR_SRC}.${TCLTK_PATCH_SRC}])
  else
    AC_MSG_NOTICE([tcl/tk >= ${TCLTK_MAJOR}.${TCLTK_MINOR}.${TCLTK_PATCH} found])
  fi
else
  AC_MSG_NOTICE([python >= ${PYTHON_MAJOR}.${PYTHON_MINOR}.${PYTHON_PATCH} with tcl/tk support found])
fi
if test ${SETUPTOOLS} == "yes" ; then
  AC_MSG_NOTICE([setuptools >= ${SETUPTOOLS_MAJOR}.${SETUPTOOLS_MINOR} not found... building package ${SETUPTOOLS_MAJOR_SRC}.${SETUPTOOLS_MINOR_SRC}])
else
  AC_MSG_NOTICE([setuptools >= ${SETUPTOOLS_MAJOR}.${SETUPTOOLS_MINOR} found])
fi
if test ${PyOpenGL} == "yes" ; then
  AC_MSG_NOTICE([PyOpenGL >= ${PyOpenGL_MAJOR}.${PyOpenGL_MINOR} not found... building package ${PyOpenGL_MAJOR_SRC}.${PyOpenGL_MINOR_SRC}.${PyOpenGL_PATCH_SRC}])
else
  AC_MSG_NOTICE([PyOpenGL >= ${PyOpenGL_MAJOR}.${PyOpenGL_MINOR} found or not necessary in this configuration])
fi
if test ${NUMPY} == "yes" ; then
  AC_MSG_NOTICE([numpy >= ${NUMPY_MAJOR}.${NUMPY_MINOR}.${NUMPY_PATCH} not found... building package ${NUMPY_MAJOR_SRC}.${NUMPY_MINOR_SRC}.${NUMPY_PATCH_SRC}])
else
  AC_MSG_NOTICE([numpy >= ${NUMPY_MAJOR}.${NUMPY_MINOR}.${NUMPY_PATCH} found])
fi
if test ${Pmw} == "yes" ; then
  AC_MSG_NOTICE([Pmw >= ${PMW_MAJOR}.${PMW_MINOR} not found... building package ${PMW_MAJOR_SRC}.${PMW_MINOR_SRC}])
else
  AC_MSG_NOTICE([Pmw >= ${PMW_MAJOR}.${PMW_MINOR} found])
fi
if test ${with_qt} != "no" ; then
  AC_MSG_NOTICE([Will build with QT support])
fi
if test ${with_qt_lib} != "no" ; then
  AC_MSG_NOTICE([Will build with QT support])
fi
if test ${enble_qt} == "-yes" ; then
   if test ${enable_fw} == "-yes" ; then
     AC_MSG_NOTICE([Will build with Framework QT support])
   else
     AC_MSG_NOTICE([Will build with QT support])
   fi
else
  if test ${enble_qtfw} == "-yes" ; then
     AC_MSG_NOTICE([Will build with Framework QT support])
  fi
fi
AC_MSG_NOTICE([])
AC_MSG_NOTICE([Now type "make" to install CDAT-${CDAT_VERSION} to: ${prefix} and its dependencies to: ${EXTERNALS}])
dnl R check
dnl ioapi
dnl debug
