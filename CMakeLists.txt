
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
  #option(Slicer3_BUILD_NUMPY "Build Slicer with NumPy (builds Blas and Lapack)." ON)

  #if(Slicer3_BUILD_NUMPY)
  #  option(Slicer3_BUILD_SCIPY "Build Slicer with SciPy (requires NumPy)" ON)
  #endif()
  enable_language(Fortran)
endif(CMAKE_Fortran_COMPILER)

macro (add_cdat_package package)
  string(TOUPPER package UC_PACKAGE)
  string(TOLOWER package LC_PACKAGE)
  option(cdat_USE_SYSTEM_${UC_PACKAGE} "Use system installed ${LC_PACKAGE}")
  if(NOT ${UC_PACKAGE}_FOUND AND NOT cdat_USE_SYSTEM_${UC_PACKAGE})
    message("${package} not found.\n\t->Building ${UC_PACKAGE} ${${UC_PACKAGE}_MAJOR_SRC}.${${UC_PACKAGE}_MINOR_SRC} from source.")
    include(External_${package})
  endif()
endmacro (add_cdat_package)

find_package(LibXml2)
#find_package(XGKS)
#find_package(PBMPLUS)
find_package(FFMPEG)
#  find_package(gifsicle)
#  find_package(GS)
find_package(ZLIB)
find_package(JPEG)
find_package(PNG)
find_package(BLAS)
find_package(LAPACK)
find_package(TCL)
find_package(PythonInterp)
find_package(PythonLibs)

add_cdat_package(CURL)
add_cdat_package(LibXml2)
add_cdat_package(xgks)
add_cdat_package(pbmplus)
add_cdat_package(ffmpeg)
add_cdat_package(gifsicle)
add_cdat_package(gs)
add_cdat_package(zlib)
add_cdat_package(jpeg)
add_cdat_package(png)
add_cdat_package(BLAS)
add_cdat_package(LAPACK)
add_cdat_package(TclTk)
