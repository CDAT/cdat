
#-----------------------------------------------------------------------------
cmake_minimum_required(VERSION 2.8.3)
#-----------------------------------------------------------------------------

project(cdat)

set(cdat_CMAKE_SOURCE_DIR ${cdat_SOURCE_DIR}/CMake)
set(cdat_CMAKE_BINARY_DIR ${cdat_BINARY_DIR}/CMake)

set(CMAKE_MODULE_PATH
  ${cdat_CMAKE_SOURCE_DIR}
  ${cdat_CMAKE_BINARY_DIR}
  ${CMAKE_MODULE_PATH}
  )

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


option(cdat_USE_SYSTEM_CURL "CURL ${CURL_MAJOR_SRC}.${CURL_MINOR_SRC}" TRUE)

if(cdat_USE_SYSTEM_CURL)
  find_package(CURL REQUIRED)
else()
  include(External_CURL)
endif()

option(cdat_USE_SYSTEM_LIBXML2 "libxml2 ${XML_MAJOR_SRC}.${XML_MINOR_SRC}" TRUE)

if(cdat_USE_SYSTEM_LIBXML2)
  find_package(LibXml2 REQUIRED)
else()
  include(External_LibXml2)
endif()

option(cdat_USE_SYSTEM_XGKS "xgks" TRUE)

if(cdat_USE_SYSTEM_XGKS)
  #find_package(PythonInterp REQUIRED)
else()
  include(External_XGKS)
endif()

option(cdat_USE_SYSTEM_PBMPLUS "pbmplus" TRUE)

if(cdat_USE_SYSTEM_PBMPLUS)
  #find_package(PythonInterp REQUIRED)
else()
  include(External_PBMPlus)
endif()

option(cdat_USE_SYSTEM_FFMPEG "FFMPEG ${FFMPEG_MAJOR_SRC}.${FFMPEG_MINOR_SRC}" TRUE)

if(cdat_USE_SYSTEM_FFMPEG)
  find_package(FFMPEG REQUIRED)
else()
  include(External_FFMPEG)
endif()

option(cdat_USE_SYSTEM_GIFSICLE "gifsicle ${GIFSICLE_MAJOR_SRC}.${GIFSICLE_MINOR_SRC}" TRUE)

if(cdat_USE_SYSTEM_GIFSICLE)
#  find_package(gifsicle REQUIRED)
else()
  include(External_gifsicle)
endif()

option(cdat_USE_SYSTEM_GHOSTSCRIPT "Ghost Script -${GS_MAJOR_SRC}.${GS_MINOR_SRC}" TRUE)

if(cdat_USE_SYSTEM_GHOSTSCRIPT)
#  find_package(GS REQUIRED)
else()
  include(External_GS)
endif()

option(cdat_USE_SYSTEM_JPEG "libjpeg" TRUE)

if(cdat_USE_SYSTEM_JPEG)
  find_package(JPEG REQUIRED)
else() 
  include(External_jpeg)
endif()

option(cdat_USE_SYSTEM_PNG "libpng ${PNG_MAJOR_SRC}.${PNG_MINOR_SRC}" TRUE)

if(cdat_USE_SYSTEM_PNG)
  find_package(PNG REQUIRED)
else()
  include(External_png)
endif()

option(cdat_USE_SYSTEM_BLAS "BLAS" TRUE)

if(cdat_USE_SYSTEM_BLAS)
  find_package(BLAS REQUIRED)
else()
  include(External_BLAS)
endif()

option(cdat_USE_SYSTEM_LAPACK "LAPACK" TRUE)

if(cdat_USE_SYSTEM_LAPACK)
  find_package(LAPACK REQUIRED)
else()
  include(External_LAPACK)
endif()

option(cdat_USE_SYSTEM_PYTHON "Python 2.7" TRUE)

if(cdat_USE_SYSTEM_PYTHON)
  find_package(PythonInterp REQUIRED)
  find_package(PythonLibs REQUIRED)
else()
  include(External_Python)
endif()