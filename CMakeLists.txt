
#-----------------------------------------------------------------------------
cmake_minimum_required(VERSION 2.8.3)
#-----------------------------------------------------------------------------

project(cdat)

# Set the Titan version
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

option(cdat_USE_SYSTEM_PYTHON
  "Wrap Titan and VTK in the Python language" TRUE)

if(cdat_USE_SYSTEM_PYTHON)
  find_package(PythonInterp REQUIRED)
  find_package(PythonLibs REQUIRED)
else()
  # download and build python.
endif()