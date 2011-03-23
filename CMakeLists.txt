
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
  set(cdat_apple_compiler_args
    -DCMAKE_OSX_ARCHITECTURES:STRING=${CMAKE_OSX_ARCHITECTURES}
    -DCMAKE_OSX_DEPLOYMENT_TARGET:STRING=${CMAKE_OSX_DEPLOYMENT_TARGET}
    -DCMAKE_OSX_SYSROOT:PATH=${CMAKE_OSX_SYSROOT})

  list(APPEND cdat_compiler_args ${cdat_apple_compiler_args})
  set(cdat_osx_arch_flag "-arch ${CMAKE_OSX_ARCHITECTURES}")
  set(cdat_osx_version_flag "-mmacosx-version-min=${CMAKE_OSX_DEPLOYMENT_TARGET}")
  set(cdat_osx_sysroot "-isysroot ${CMAKE_OSX_SYSROOT}")
endif()

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
  string(TOUPPER ${package} UC_PACKAGE)
  string(TOLOWER ${package} LC_PACKAGE)
  if(NOT DEFINED CDAT_USE_SYSTEM_${UC_PACKAGE})
    if(NOT ${UC_PACKAGE}_FOUND)
      option(CDAT_USE_SYSTEM_${UC_PACKAGE} "Use system installed ${LC_PACKAGE}" OFF)
    else()
      option(CDAT_USE_SYSTEM_${UC_PACKAGE} "Use system installed ${LC_PACKAGE}" ON)
    endif()
  endif()

  if(NOT ${UC_PACKAGE}_FOUND)
    list(APPEND external_packages External_${package})
    set(${package}_dep ${package})
  else()
    unset(${package}_dep)
  endif()
endmacro (add_cdat_package)

find_package(PkgConfig)
set(PKGCONFIG_FOUND ${PKG_CONFIG_FOUND})

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
find_package(ZLIB)
find_package(PNG)
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

#------------------------------------------------------------------------------
# Establish Target Dependencies based on Selected Options and find results
#------------------------------------------------------------------------------

set(zlib_DEPENDENCIES ${pkgconfig_dep})
set(png_DEPENDENCIES ${zlib_dep})
set(tcl_DEPENDENCIES ${pkgconfig_dep})
set(python_DEPENDENCIES ${TclTk_dep})
set(SIP_DEPENDENCIES ${Python_dep})
set(PyQt_DEPENDENCIES ${SIP_dep})

if(NOT QT_FOUND)
  list(APPEND PyQt_DEPENDENCIES ${Qt_dep})
endif()

set(PyOpenGL_DEPENDENCIES ${Python_dep})
set(Pmw_DEPENDENCIES ${Python_dep})
set(pixman_DEPENDENCIES ${pkgconfig_dep})
set(fontconfig_DEPENDENCIES ${libXML2_dep})
set(freetype_DEPENDENCIES ${png_dep})
set(ghostscript_DEPENDENCIES ${zlib_dep} ${jpeg_dep} ${png_dep} ${fontconfig_dep} ${freetype_dep} ${pixman_dep} ${xslt_dep} ${libXML2_dep})
set(NetCDF_DEPENDENCIES ${HDF5_dep})

set(TclTk_DEPENDENCIES ${pkgconfig_dep})
set(Python_DEPENDENCIES ${TclTk_dep})

set(setuptools_DEPENDENCIES ${Python_dep})
set(NUMPY_DEPENDENCIES ${Python_dep})
if(NOT APPLE)
  if(CMAKE_Fortran_COMPILER)
    set(LAPACK_DEPENDENCIES ${pkgconfig_dep})
    set(NUMPY_DEPENDENCIES ${LAPACK_dep})
  else()
    set(CLAPACK_DEPENDENCIES ${pkgconfig_dep})
    set(NUMPY_DEPENDENCIES ${CLAPACK_dep})
  endif()
endif()

set(VTK_DEPENDENCIES ${zlib_deb} ${png_dep} ${jpeg_dep} ${xml_dep})
set(cmcurl_DEPENDENCIES)
set(Cairo_DEPENDENCIES ${pkgconfig_dep} ${png_dep} ${fontconfig_dep} ${freetype_dep} ${pixman_dep})
set(HDF4_DEPENDENCIES ${zlib_dep} ${jpeg_dep})
set(HDF5_DEPENDENCIES ${zlib_dep})
set(cmor_DEPENDENCIES ${HDF5_dep} ${NetCDF_dep} ${zlib_dep} ${uuid_dep} ${udunits2_dep})
set(CDAT_DEPENDENCIES ${CMOR_dep} ${Cairo_dep} ${PyQt_dep} ${PyOpenGL_dep} ${Pmw_dep})

message("Pmw_DEPENDENCIES ${Pmw_DEPENDENCIES}")
message("ghostscript_DEPENDENCIES ${ghostscript_DEPENDENCIES}")
message("VTK_DEPENDENCIES ${VTK_DEPENDENCIES}")
message("Cairo_DEPENDENCIES ${Cairo_DEPENDENCIES}")
message("cmor_DEPENDENCIES ${cmor_DEPENDENCIES}")
message("CDAT_DEPENDENCIES ${CDAT_DEPENDENCIES}")


foreach(package ${external_packages})
  message("Building ${package}")
  include(${package})
endforeach()

