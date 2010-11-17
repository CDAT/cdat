
#-----------------------------------------------------------------------------
cmake_minimum_required(VERSION 2.8.3)
#-----------------------------------------------------------------------------

project(cdat)

set(cdat_CMAKE_SOURCE_DIR ${cdat_SOURCE_DIR}/CMake)
set(cdat_CMAKE_BINARY_DIR ${cdat_BINARY_DIR}/CMake)

#-----------------------------------------------------------------------------
# Update CMake module path
#------------------------------------------------------------------------------

set(CMAKE_MODULE_PATH
  ${cdat_CMAKE_SOURCE_DIR}
  ${cdat_CMAKE_BINARY_DIR}
  ${CMAKE_MODULE_PATH}
  )

# cdat version
set(cdat_VERSION_MAJOR 6)
set(cdat_VERSION_MINOR 0)
set(cdat_VERSION_PATCH "alpha")
set(cdat_VERSION ${cdat_VERSION_MAJOR}.${cdat_VERSION_MINOR}.${cdat_VERSION_PATCH})

# Attempt to detect if we have a Fortran Compiler available.
include(CheckFortran)

if(CMAKE_Fortran_COMPILER)
  enable_language(Fortran)
endif(CMAKE_Fortran_COMPILER)

include(PackageVersions)

macro (add_cdat_package package)
  message("Adding package: ${package}")
  string(TOUPPER ${package} UC_PACKAGE)
  string(TOLOWER ${package} LC_PACKAGE)
  option(CDAT_USE_SYSTEM_${UC_PACKAGE} "Use system installed ${LC_PACKAGE}" ON)
  if(NOT ${UC_PACKAGE}_FOUND AND CDAT_USE_SYSTEM_${UC_PACKAGE})
    message("${package} not found.\n  ->Building ${UC_PACKAGE} ${${UC_PACKAGE}_MAJOR_SRC}.${${UC_PACKAGE}_MINOR_SRC}.${${UC_PACKAGE}_PATCH_SRC} from source.")
    #include(External_${package})
    set(CDAT_USE_SYSTEM_${UC_PACKAGE} OFF CACHE BOOL "Use system installed ${LC_PACKAGE}" FORCE)
  else()
    set(CDAT_USE_SYSTEM_${UC_PACKAGE} ON CACHE BOOL "Use system installed ${LC_PACKAGE}" FORCE)
  endif()
endmacro (add_cdat_package)

find_package(CURL)
find_package(LibXml2)
#find_package(XGKS)
#find_package(PBMPLUS)
find_package(FFMPEG)
#find_package(gifsicle)
#find_package(GS)
find_package(JPEG)
find_package(BLAS)
find_package(LAPACK)
find_package(ZLIB)
find_package(PNG)
find_package(PkgConfig)
find_package(Freetype)
#find_package(pixman)
#find_package(fontconfig)
find_package(Cairo)
#find_package(uuid)
#find_package(uduinits2)
find_package(HDF5)
#find_package(NetCDF)
find_package(Qt4)
find_package(PythonInterp)
find_package(PythonLibs)
# unix tools
#find_package(termcap)
#find_package(readline)
find_package(TCL)
# python packages
#find_package(setuptools)
#find_package(PyOpenGL)
#find_package(NUMPY)
# python mega widgets
#find_package(PMW)



add_cdat_package(CURL)
add_cdat_package(LibXml2)
#add_cdat_package(xgks)
#add_cdat_package(pbmplus)
add_cdat_package(ffmpeg)
#add_cdat_package(gifsicle)
#add_cdat_package(gs)

add_cdat_package(jpeg)
add_cdat_package(BLAS)
add_cdat_package(LAPACK)
add_cdat_package(zlib)
add_cdat_package(png)
add_cdat_package(pkgconfig)
add_cdat_package(Freetype)
#add_cdat_package(pixman)
#add_cdat_package(fontconfig)
add_cdat_package(Cairo)
#add_cdat_package(uuid)
#add_cdat_package(uduinits2)
add_cdat_package(HDF5)
#add_cdat_package(NetCDF)
add_cdat_package(Qt)
add_cdat_package(Python)
#add_cdat_package(termcap)
#add_cdat_package(readline)
add_cdat_package(TclTk)
#add_cdat_package(setuptools)
#add_cdat_package(sip)
#add_cdat_package(pyqt)
#add_cdat_package(pyopengl)
#add_cdat_package(numpy)
#add_cdat_package(pmw)
