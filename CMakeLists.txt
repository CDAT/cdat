
#-----------------------------------------------------------------------------
cmake_minimum_required(VERSION 2.8.4)
#-----------------------------------------------------------------------------

project(cdat)

set(cdat_CMAKE_SOURCE_DIR ${cdat_SOURCE_DIR}/CMake)
set(cdat_CMAKE_BINARY_DIR ${cdat_BINARY_DIR}/CMake)
set(cdat_external_patch_dir ${cdat_SOURCE_DIR}/exsrc)

#------------------------------------------------------------------------------
# Update CMake module path
#------------------------------------------------------------------------------

set(CMAKE_MODULE_PATH
  ${cdat_CMAKE_SOURCE_DIR}
  ${cdat_CMAKE_BINARY_DIR}
  ${CMAKE_MODULE_PATH}
  )

#-----------------------------------------------------------------------------
# CDat version
#------------------------------------------------------------------------------
set(cdat_VERSION_MAJOR 6)
set(cdat_VERSION_MINOR 0)
set(cdat_VERSION_PATCH "alpha")
set(cdat_VERSION ${cdat_VERSION_MAJOR}.${cdat_VERSION_MINOR}.${cdat_VERSION_PATCH})

# configure version file
configure_file(${cdat_CMAKE_SOURCE_DIR}/version.in
  ${cdat_BINARY_DIR}/version
  @ONLY)

#-----------------------------------------------------------------------------
# Git protocole option
#-----------------------------------------------------------------------------

option(GIT_USE_GIT_PROTOCOL "If behind a firewall turn this off to use http instead." ON)

set(git_protocol "git")
if(NOT Slicer_USE_GIT_PROTOCOL)
  set(git_protocol "http")
endif()


# Attempt to detect if we have a Fortran Compiler available.
include(CheckFortran)

if(CMAKE_Fortran_COMPILER)
  enable_language(Fortran)
endif(CMAKE_Fortran_COMPILER)

#-----------------------------------------------------------------------------
# Enable and setup External project global properties
#-----------------------------------------------------------------------------

INCLUDE(ExternalProject)

#set(ep_common_args
  #-DCMAKE_INSTALL_PREFIX:PATH=${ep_install_dir}
#  -DCMAKE_BUILD_TYPE:STRING=${CMAKE_BUILD_TYPE}
#  -DBUILD_TESTING:BOOL=OFF
#  )

#set(ep_common_c_flags "${CMAKE_C_FLAGS}")
#set(ep_common_cxx_flags "${CMAKE_CXX_FLAGS}")

# ----------------------------------------------------------------------------
# generic compiler args (compiler and cflags) to pass to subprojects
#-----------------------------------------------------------------------------

set(cdat_compiler_args
  -DCMAKE_CXX_COMPILER:FILEPATH=${CMAKE_CXX_COMPILER}
  -DCMAKE_C_COMPILER:FILEPATH=${CMAKE_C_COMPILER}
  -DCMAKE_CXX_FLAGS_DEBUG:STRING=${CMAKE_CXX_FLAGS_DEBUG}
  -DCMAKE_CXX_FLAGS_MINSIZEREL:STRING=${CMAKE_CXX_FLAGS_MINSIZEREL}
  -DCMAKE_CXX_FLAGS_RELEASE:STRING=${CMAKE_CXX_FLAGS_RELEASE}
  -DCMAKE_CXX_FLAGS_RELWITHDEBINFO:STRING=${CMAKE_CXX_FLAGS_RELWITHDEBINFO}
  -DCMAKE_C_FLAGS_DEBUG:STRING=${CMAKE_C_FLAGS_DEBUG}
  -DCMAKE_C_FLAGS_MINSIZEREL:STRING=${CMAKE_C_FLAGS_MINSIZEREL}
  -DCMAKE_C_FLAGS_RELEASE:STRING=${CMAKE_C_FLAGS_RELEASE}
  -DCMAKE_C_FLAGS_RELWITHDEBINFO:STRING=${CMAKE_C_FLAGS_RELWITHDEBINFO}
  -DCMAKE_EXE_LINKER_FLAGS:STRING=${CMAKE_EXE_LINKER_FLAGS}
  -DCMAKE_SHARED_LINKER_FLAGS:STRING=${CMAKE_SHARED_LINKER_FLAGS}
)

set(cdat_tpl_cxx_flags ${CMAKE_CXX_FLAGS})
set(cdat_tpl_c_flags ${CMAKE_C_FLAGS})
set(cdat_cxx_flags ${CMAKE_CXX_FLAGS})
set(cdat_c_flags ${CMAKE_C_FLAGS})

if(WIN32)
  list(APPEND cdat_compiler_args -DCMAKE_BUILD_TYPE:STRING=${CMAKE_CFG_INTDIR})
else()
  list(APPEND cdat_compiler_args -DCMAKE_BUILD_TYPE:STRING=${CMAKE_BUILD_TYPE})
endif()

if(APPLE)
  list(APPEND cdat_compiler_args ${pv_apple_compiler_args})
endif()

# We aren't interested in seeing third party compiler warnings.
set(pv_tpl_compiler_args ${pv_compiler_args})
string(REPLACE "/W3" "/W0" pv_tpl_cxx_flags "${pv_tpl_cxx_flags}")
string(REPLACE "/W4" "/W0" pv_tpl_cxx_flags "${pv_tpl_cxx_flags}")
string(REPLACE "/W3" "/W0" pv_tpl_c_flags "${pv_tpl_c_flags}")
string(REPLACE "/W4" "/W0" pv_tpl_c_flags "${pv_tpl_c_flags}")

if(WIN32)
  set(LIBRARY_PATH PATH)
  set(CMAKE_PLATFORM WIN32)
  set(_LINK_LIBRARY_SUFFIX .lib)
elseif(APPLE)
  set(LIBRARY_PATH DYLD_LIBRARY_PATH)
  set(CMAKE_PLATFORM APPLE)
  set(_LINK_LIBRARY_SUFFIX .dylib)
else()
  set(LIBRARY_PATH LD_LIBRARY_PATH)
  set(CMAKE_PLATFORM UNIX)
  set(_LINK_LIBRARY_SUFFIX .so)
endif()

configure_file(${cdat_CMAKE_SOURCE_DIR}/cdat_configure_step.cmake.in
    ${cdat_CMAKE_BINARY_DIR}/cdat_configure_step.cmake
    @ONLY)
  
configure_file(${cdat_CMAKE_SOURCE_DIR}/cdat_make_step.cmake.in
  ${cdat_CMAKE_BINARY_DIR}/cdat_make_step.cmake
  @ONLY)
  
configure_file(${cdat_CMAKE_SOURCE_DIR}/cdat_install_step.cmake.in
  ${cdat_CMAKE_BINARY_DIR}/cdat_install_step.cmake
  @ONLY)

set(cdat_configure_command ${CMAKE_COMMAND} -P ${cdat_CMAKE_BINARY_DIR}/cdat_configure_step.cmake)
set(cdat_make_command ${CMAKE_COMMAND} -P ${cdat_CMAKE_BINARY_DIR}/cdat_make_step.cmake)
set(cdat_install_command ${CMAKE_COMMAND} -P ${cdat_CMAKE_BINARY_DIR}/cdat_install_step.cmake)

#-----------------------------------------------------------------------------
#-----------------------------------------------------------------------------

include(PackageVersions)

macro (add_cdat_package package)
  message("Adding package: ${package}")
  string(TOUPPER ${package} UC_PACKAGE)
  string(TOLOWER ${package} LC_PACKAGE)
  option(CDAT_USE_SYSTEM_${UC_PACKAGE} "Use system installed ${LC_PACKAGE}" ON)
  if(NOT ${UC_PACKAGE}_FOUND) 
    if(NOT CDAT_USE_SYSTEM_${UC_PACKAGE})
      message("${package} not found.\n  ->Building ${UC_PACKAGE} ${${UC_PACKAGE}_MAJOR_SRC}.${${UC_PACKAGE}_MINOR_SRC}.${${UC_PACKAGE}_PATCH_SRC} from source.")
      #set(CDAT_USE_SYSTEM_${UC_PACKAGE} OFF CACHE BOOL "Use system installed ${LC_PACKAGE}" FORCE)
      include(External_${package})
    endif()
  else()
    if(NOT CDAT_USE_SYSTEM_${UC_PACKAGE})
      include(External_${package})
    endif()
  endif()
endmacro (add_cdat_package)

find_package(MD5)
find_package(Wget)
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
##find_package(ZLIB)
##find_package(PNG)
##find_package(PkgConfig)
find_package(Freetype)
#find_package(pixman)
#find_package(fontconfig)
find_package(Cairo)
#find_package(uuid)
#find_package(udunits2)
find_package(HDF5)
#find_package(NetCDF)
find_package(Qt4)
find_package(PythonInterp)
find_package(PythonLibs)
# unix tools
#find_package(termcap)
#find_package(readline)
##find_package(TCL)
# python packages
#find_package(setuptools)
#find_package(PyOpenGL)
#find_package(NUMPY)
# python mega widgets
#find_package(PMW)

#------------------------------------------------------------------------------
# Establish Target Dependencies based on Selected Options and find results
#------------------------------------------------------------------------------

set(zlib_DEPENDENCIES pkgconfig)
set(png_DEPENDENCIES zlib)
set(tcl_DEPENDENCIES pkgconfig)
set(python_DEPENDENCIES TclTk)
set(sip_DEPENDENCIES python)
set(PyQt_DEPENDENCIES Qt sip)
set(PyOpenGL_DEPENDENCIES python)
set(Pmw_DEPENDENCIES python)
set(pixman_DEPENDENCIES pkgconfig)
set(fontconfig_DEPENDENCIES libXML2)
set(freetype_DEPENDENCIES png)
set(ghostscript_DEPENDENCIES zlib jpeg png fontconfig freetype pixman xslt libXML2)
set(netcdf_DEPENDENCIES HDF5)

set(python_DEPENDENCIES tcltk)

set(CLAPACK_DEPENDENCIES)
set(setuptools_DEPENDENCIES python)
set(NUMPY_DEPENDENCIES python)
if(NOT APPLE)
  if(CMAKE_Fortran_COMPILER)
    set(NUMPY_DEPENDENCIES LAPACK)
  else()
    set(NUMPY_DEPENDENCIES CLAPACK)
  endif()
endif()

set(VTK_DEPENDENCIES zlib png jpeg xml)
set(cmcurl_DEPENDENCIES)
set(cdat_DEPENDENCIES VTK Insight BatchMake OpenIGTLink teem cmcurl)
set(Cairo_DEPENDENCIES pkgconfig png fontconfig freetype pixman)
set(HDF4_DEPENDENCIES zlib jpeg)
set(HDF5_DEPENDENCIES zlib)
set(cmor_DEPENDENCIES HDF5 netcdf zlib uuid udunits2)
set(CDAT_DEPENDENCIES cmor Cairo)

#-----------------------------------------------------------------------------
#-----------------------------------------------------------------------------
add_cdat_package(wget)

#-----------------------------------------------------------------------------
# checked_get
#-----------------------------------------------------------------------------

set(HASWGET ${WGET_EXECUTABLE})
set(MD5PRG ${MD5_EXECUTABLE})
set(MD5CNT 1)
configure_file(${cdat_SOURCE_DIR}/checked_get.sh.in
  ${cdat_BINARY_DIR}/checked_get.sh
  @ONLY)

#-----------------------------------------------------------------------------
#-----------------------------------------------------------------------------

add_cdat_package(pkgconfig)
#add_cdat_package(termcap)
#add_cdat_package(CURL)
add_cdat_package(readline)
add_cdat_package(libXML2)
if(NOT APPLE)
  add_cdat_package(xgks)
endif()
add_cdat_package(xslt)
add_cdat_package(pbmplus)
add_cdat_package(FFMPEG)
add_cdat_package(gifsicle)
add_cdat_package(jpeg)
add_cdat_package(TclTk)

# On OSX use vecLib framework
if(NOT APPLE)
  if(CMAKE_Fortran_COMPILER)
    add_cdat_package(LAPACK)
  else()
    add_cdat_package(CLAPACK)
  endif()
endif()

add_cdat_package(zlib)
add_cdat_package(png)
add_cdat_package(freetype)
add_cdat_package(pixman)
add_cdat_package(fontconfig)
add_cdat_package(Cairo)
add_cdat_package(uuid)
add_cdat_package(udunits2)
#add_cdat_package(ghostscript)
#add_cdat_package(HDF4) 
add_cdat_package(HDF5)
add_cdat_package(NetCDF)
add_cdat_package(Qt)
add_cdat_package(CMOR)

# Python Packages
add_cdat_package(Python)
add_cdat_package(setuptools)
add_cdat_package(SIP)
add_cdat_package(PyQt)
add_cdat_package(PyOpenGL)
add_cdat_package(NUMPY)
add_cdat_package(Pmw)
add_cdat_package(CDAT)

