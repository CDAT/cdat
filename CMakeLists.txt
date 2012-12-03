#-----------------------------------------------------------------------------
cmake_minimum_required(VERSION 2.8.8)
#-----------------------------------------------------------------------------

project(cdat)

set(cdat_CMAKE_SOURCE_DIR ${cdat_SOURCE_DIR}/CMake)
set(cdat_CMAKE_BINARY_DIR ${cdat_BINARY_DIR}/CMake)
set(cdat_external_patch_dir ${cdat_SOURCE_DIR}/exsrc)
option(CDAT_USE_SYSTEM_INTROSPECTION "Detect System installed libraries." OFF)

if("${CMAKE_INSTALL_PREFIX}" STREQUAL "/usr/local")
  get_filename_component(cdat_ROOT_DIR ${cdat_BINARY_DIR} PATH)
  set(CMAKE_INSTALL_PREFIX ${cdat_ROOT_DIR}/install CACHE STRING "" FORCE)
endif()

# Disable in source build of any kind
STRING(REGEX MATCH "${cdat_SOURCE_DIR}/"  BUILDINSOURCE  "${cdat_BINARY_DIR}")
if (BUILDINSOURCE)
   message(FATAL_ERROR "[ERROR] CDAT requires an out of source Build")
endif()

set(cdat_EXTERNALS ${CMAKE_INSTALL_PREFIX}/Externals)
set(ENV{PATH} "${cdat_EXTERNALS}/bin:$ENV{PATH}")
message("We reset your path to: " $ENV{PATH})

#------------------------------------------------------------------------------
# Update CMake module path
#------------------------------------------------------------------------------

set(CMAKE_MODULE_PATH
  ${cdat_CMAKE_SOURCE_DIR}
  ${cdat_CMAKE_BINARY_DIR}
  ${CMAKE_MODULE_PATH}
  )

#allow cmake to help create regression tests
include(CTest)

#-----------------------------------------------------------------------------
# CDAT version
#------------------------------------------------------------------------------
set(cdat_VERSION_MAJOR 6)
set(cdat_VERSION_MINOR 1)
set(cdat_VERSION_PATCH 0)
set(cdat_VERSION ${cdat_VERSION_MAJOR}.${cdat_VERSION_MINOR}.${cdat_VERSION_PATCH})

# configure version file
configure_file(${cdat_CMAKE_SOURCE_DIR}/version.in
  ${cdat_SOURCE_DIR}/version
  @ONLY)
# Cleans all CDAT builds
execute_process(
    COMMAND ./scripts/clean_script all
    WORKING_DIRECTORY ${cdat_SOURCE_DIR}
  )

##############################################################################
# Important: change architecture *before* any enable_language() or project()
# calls so that it's set properly to detect 64-bit-ness...
#
if(APPLE)

  string(REGEX REPLACE "^([0-9]+)\\.([0-9]+).*$" "\\1" OSX_VER_MAJOR "${CURRENT_OSX_VERSION}")
  string(REGEX REPLACE "^([0-9]+)\\.([0-9]+).*$" "\\2" OSX_VER_MINOR "${CURRENT_OSX_VERSION}")

  if("${CMAKE_OSX_DEPLOYMENT_TARGET}" STREQUAL "")
    set(CMAKE_OSX_ARCHITECTURES "x86_64" CACHE STRING "" FORCE)
    set(CMAKE_OSX_ARCHITECTURES_M "64" CACHE STRING "" FORCE)
    set(CMAKE_OSX_DEPLOYMENT_TARGET "${OSX_VER_MAJOR}.${OSX_VER_MINOR}" CACHE STRING "" FORCE)
  endif()

  ## Added by C. Doutriaux, cmake 2.8.10 on make sets OSX_SYSROOT to "" leadoing to error, this fixes it
  if ("${CMAKE_OSX_SYSROOT}" STREQUAL "")
    set(CMAKE_OSX_SYSROOT "/Developer/SDKs/MacOSX${OSX_VER_MAJOR}.${OSX_VER_MINOR}.sdk" CACHE STRING "" FORCE)
  endif()

  if(NOT "${CMAKE_OSX_SYSROOT}" STREQUAL "")
    if(NOT EXISTS "${CMAKE_OSX_SYSROOT}")
      message(FATAL_ERROR "error: CMAKE_OSX_SYSROOT='${CMAKE_OSX_SYSROOT}' does not exist")
    endif()
  endif()
endif()

#-----------------------------------------------------------------------------
# Git protocol option
#-----------------------------------------------------------------------------

find_package(Git)

set(ZLIB_PATCH_SRC 5 CACHE INTEGER "Version of ZLIB to use")
MARK_AS_ADVANCED(ZLIB_PATCH_SRC)
if (ZLIB_PATCH_SRC EQUAL 3)
  set(ZLIB_MD5 debc62758716a169df9f62e6ab2bc634)
elseif (ZLIB_PATCH_SRC EQUAL 5)
  set(ZLIB_MD5 c735eab2d659a96e5a594c9e8541ad63)
elseif (ZLIB_PATCH_SRC EQUAL 7)
  set(ZLIB_MD5 60df6a37c56e7c1366cca812414f7b85)
else ()
  message(FATAL_ERROR "error: invalid zlib patch number: '${ZLIB_PATCH_SRC}' valid: 3, 5 or 7")
endif()

if(NOT GIT_PROTOCOL)
  message("Select Either git:// or http:// for checking out git submodules.
  If you are behind a firewall then likely you need to choose http:// instead.")
  set(GIT_PROTOCOL "git://" CACHE STRING "Choose protocol to be used by git for submodules" FORCE)
  set_property(CACHE GIT_PROTOCOL PROPERTY STRINGS "git://" "http://")
  return()
endif()

if(GIT_PROTOCOL MATCHES "http://")

  execute_process(
    COMMAND ${GIT_EXECUTABLE} config --unset url.git://uv-cdat.llnl.gov/eof2.git.insteadof http://uv-cdat.llnl.gov/git/eof2.git
    WORKING_DIRECTORY ${cdat_SOURCE_DIR}
  )

  execute_process(
    COMMAND ${GIT_EXECUTABLE} config --unset url.git://uv-cdat.llnl.gov/windfield.git.insteadof http://uv-cdat.llnl.gov/git/windfield.git
    WORKING_DIRECTORY ${cdat_SOURCE_DIR}
  )

else()

  execute_process(
    COMMAND ${GIT_EXECUTABLE} config url.git://uv-cdat.llnl.gov/eof2.git.insteadOf http://uv-cdat.llnl.gov/git/eof2.git
    WORKING_DIRECTORY ${cdat_SOURCE_DIR}
  )

  execute_process(
    COMMAND ${GIT_EXECUTABLE} config url.git://uv-cdat.llnl.gov/windfield.git.insteadOf http://uv-cdat.llnl.gov/git/windfield.git
    WORKING_DIRECTORY ${cdat_SOURCE_DIR}
  )

endif()

#-----------------------------------------------------------------------------
# Initialize git submodules.
#-----------------------------------------------------------------------------
if(GIT_FOUND)
  message("Updating git submodules")
  execute_process(
    COMMAND ${GIT_EXECUTABLE} submodule init
    OUTPUT_VARIABLE subinit_out
    ERROR_VARIABLE subinit_err
    WORKING_DIRECTORY ${cdat_SOURCE_DIR}
  )
  execute_process(
    COMMAND ${GIT_EXECUTABLE} submodule update
    OUTPUT_VARIABLE subup_out
    ERROR_VARIABLE subup_err
    WORKING_DIRECTORY ${cdat_SOURCE_DIR}
  )
endif()

#-----------------------------------------------------------------------------
# Attempt to detect if we have a Fortran Compiler available.
#-----------------------------------------------------------------------------

include(CheckFortran)
#set(CMAKE_Fortran_COMPILER gfortran)

if(CMAKE_Fortran_COMPILER)
  enable_language(Fortran)
  set(CONFIG_SUMMARY ${cdat_SOURCE_DIR}/logs/config.summary)
  include(${CMAKE_ROOT}/Modules/CMakeDetermineFortranCompiler.cmake)
endif(CMAKE_Fortran_COMPILER)

#-----------------------------------------------------------------------------
# Enable and setup External project global properties
#-----------------------------------------------------------------------------

set(EP_PREFIX ${cdat_BINARY_DIR}/build/prefix)

set_property(DIRECTORY PROPERTY EP_LOG_DIR  ${cdat_BINARY_DIR}/logs)

set(EP_LOG_OPTIONS
  LOG_DOWNLOAD 0
  LOG_UPDATE 1
  LOG_CONFIGURE 1
  LOG_BUILD 1
  LOG_TEST 1
  LOG_INSTALL 1
)
if (CDASH_EP_LOG_OPTIONS)
  #override on dashboards so build results are visible
  set(EP_LOG_OPTIONS ${CDASH_EP_LOG_OPTIONS})
endif()

INCLUDE(ExternalProject)

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

if(APPLE)
  set(cdat_rpath_flag -R)
else()
  set(cdat_rpath_flag -Wl,-rpath,)
endif()

if(WIN32)
  list(APPEND cdat_compiler_args -DCMAKE_BUILD_TYPE:STRING=${CMAKE_CFG_INTDIR})
else()
  list(APPEND cdat_compiler_args -DCMAKE_BUILD_TYPE:STRING=${CMAKE_BUILD_TYPE})
endif()

if(APPLE)
  set(cdat_apple_compiler_args
    -DCMAKE_OSX_ARCHITECTURES:STRING=${CMAKE_OSX_ARCHITECTURES}
    -DCMAKE_OSX_ARCHITECTURES_M:STRING=${CMAKE_OSX_ARCHITECTURES_M}
    -DCMAKE_OSX_DEPLOYMENT_TARGET:STRING=${CMAKE_OSX_DEPLOYMENT_TARGET}
    -DCMAKE_OSX_SYSROOT:PATH=${CMAKE_OSX_SYSROOT})

  set(cdat_osx_arch_flag "-m${CMAKE_OSX_ARCHITECTURES_M}")
  set(cdat_osx_arch_flag_fortran "-m${CMAKE_OSX_ARCHITECTURES_M}")
  set(cdat_osx_version_flag "-mmacosx-version-min=${CMAKE_OSX_DEPLOYMENT_TARGET}")
  set(cdat_osx_sysroot "-isysroot ${CMAKE_OSX_SYSROOT}")
  set(cdat_osx_flags "${cdat_osx_arch_flag} ${cdat_osx_version_flag} ${cdat_osx_sysroot} -pipe")
  set(cdat_osx_flags_fortran "${cdat_osx_arch_flag_fortran} ${cdat_osx_version_flag} ${cdat_osx_sysroot} -pipe")
  set(cdat_osx_ld_flags "-bind_at_load -mmacosx-version-min=${CMAKE_OSX_DEPLOYMENT_TARGET}")
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

#-----------------------------------------------------------------------------
# Create binary subdirectories
#-----------------------------------------------------------------------------

# Here we create several of binary directory subdirectories as often
# third party install systems do bad things when left to create them.

file(MAKE_DIRECTORY ${cdat_EXTERNALS} ${cdat_EXTERNALS}/lib ${cdat_EXTERNALS}/bin ${cdat_EXTERNALS}/include)

file(MAKE_DIRECTORY ${cdat_BINARY_DIR}/logs)
file(MAKE_DIRECTORY ${cdat_SOURCE_DIR}/logs)
file(MAKE_DIRECTORY ${cdat_SOURCE_DIR}/libcdms)
file(MAKE_DIRECTORY ${cdat_BINARY_DIR}/build)
file(MAKE_DIRECTORY ${cdat_BINARY_DIR}/sources)

#-----------------------------------------------------------------------------
#-----------------------------------------------------------------------------

include(PackageVersions)

set(external_packages)
set(found_system_include_dirs)
set(found_system_libraries)

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

  if(NOT ${UC_PACKAGE}_FOUND AND NOT ${CDAT_USE_SYSTEM_${UC_PACKAGE}})
    list(APPEND external_packages External_${package})
    set(${package}_dep ${package})
  else()
    unset(${package}_dep)
    if(external_packages)
      list(REMOVE_ITEM external_packages External_${package})
    endif()

    if(${UC_PACKAGE}_INCLUDE_DIR)
      list(APPEND found_system_include_dirs ${${UC_PACKAGE}_INCLUDE_DIR})
      message("  Including: ${UC_PACKAGE}_INCLUDE_DIR: ${${UC_PACKAGE}_INCLUDE_DIR}")
    endif()

    if(${UC_PACKAGE}_LIBRARY)
      get_filename_component(lib_path ${${UC_PACKAGE}_LIBRARY} PATH)
      list(APPEND found_system_libraries ${lib_path})
      message("  Linking: ${UC_PACKAGE}_LIBRARY: ${lib_path}")
    endif()
  endif()
endmacro (add_cdat_package)

find_package(Qt4 REQUIRED)
# Do we download the data ?
option(CDAT_DOWNLOADS_SAMPLE_DATA "Download sample data" ON)
if (CDAT_DOWNLOADS_SAMPLE_DATA)
  set(SAMPLE_DATA "")
else()
  set(SAMPLE_DATA --disable-sampledata)
endif()

# Added by Charles Doutriaux to build with libdrs support
option(CDAT_BUILD_WITH_LIBDRS "Build with DRS file format support (needs to be in /usr/local/lib)" OFF)
MARK_AS_ADVANCED(CDAT_BUILD_WITH_LIBDRS)

# Added by Dave Kindig Tech-X Corp. kindig@txcorp.com
option(CDAT_BUILD_ESMF_ESMP "Build python version Earth System Modeling Framework" ON)
option(CDAT_BUILD_ESMF_PARALLEL "Build parallel version of Earth System Modeling Framework library" ON)

# Attempt to find each package on the system

find_package(MD5)
find_package(Wget)

# By default lets build everything but make an exception for Qt.
if(CDAT_USE_SYSTEM_INTROSPECTION)

  find_package(PkgConfig)
  set(PKGCONFIG_FOUND ${PKG_CONFIG_FOUND})

  find_package(CURL)
  find_package(LibXml2)
  find_package(LibXslt)
  find_package(FFMPEG)
  #find_package(gifsicle)
  #find_package(GS)
  find_package(TIFF)
  find_package(JPEG)
  find_package(BLAS)
  find_package(LAPACK)
  find_package(ZLIB)
  # This will find the png framework which Cairo won't find.
  if(NOT APPLE)
    find_package(PNG)
    find_package(Freetype)
  endif()
  #find_package(pixman)
  #find_package(fontconfig)
  find_package(Cairo)

  # CMOR doesn't know how to configure with the System frameworks version of uuid
  if(NOT APPLE)
    find_package(UUID)
  endif()
  #find_package(udunits2)
  find_package(HDF5)
  #find_package(NetCDF)
  #find_package(PythonInterp)
  #find_package(PythonLibs)
  ## unix tools
  find_package(TermCap)
  find_package(Readline)
  #find_package(TCL)
  ## python packages
  #find_package(setuptools)
  #find_package(PyOpenGL)
  #find_package(NUMPY)
  #find_package(PMW)
endif(CDAT_USE_SYSTEM_INTROSPECTION)
find_package(PKG_CONFIG)
#message("PKGCONFIG SAYS:"${PKG_CONFIG_FOUND})
#if(NOT PKG_CONFIG_FOUND)
  add_cdat_package(pkgconfig)
  set(cdat_PKG_CONFIG_EXECUTABLE ${cdat_EXTERNALS}/bin/pkg-config)
  set(ENV{PKG_CONFIG} "${cdat_PKG_CONFIG_EXECUTABLE}")
#  message("DID NOT FIND pkg-config")
#else()
#  set(cdat_PKG_CONFIG_EXECUTABLE ${PKG_CONFIG_EXECUTABLE})
#endif()
set(ENV{PKG_CONFIG_PATH} "${cdat_EXTERNALS}/lib/pkgconfig:$ENV{PKG_CONFIG_PATH}")
set(ENV{PKG_CONFIG} ${cdat_PKG_CONFIG_EXECUTABLE})

set(cdat_configure_command ${CMAKE_COMMAND} -P ${cdat_CMAKE_BINARY_DIR}/cdat_configure_step.cmake)
set(cdat_make_command ${CMAKE_COMMAND} -P ${cdat_CMAKE_BINARY_DIR}/cdat_make_step.cmake)
set(cdat_install_command ${CMAKE_COMMAND} -P ${cdat_CMAKE_BINARY_DIR}/cdat_install_step.cmake)

#-----------------------------------------------------------------------------
#-----------------------------------------------------------------------------
add_cdat_package(wget)
add_cdat_package(pkgconfig)
add_cdat_package(termcap)
add_cdat_package(readline)
if (NOT CDAT_USE_SYSTEM_LIBXML2)
   add_cdat_package(libXML2)
endif()

add_cdat_package(libXSLT)
add_cdat_package(zlib)
add_cdat_package(CURL)

add_cdat_package(YASM)
add_cdat_package(FFMPEG)
#add_cdat_package(gifsicle)
if (NOT CDAT_USE_SYSTEM_LIBPNG)
   add_cdat_package(png)
endif()
add_cdat_package(jpeg)
add_cdat_package(tiff)


add_cdat_package(TclTk)

# On OSX use vecLib framework
if(NOT APPLE)
  if(CMAKE_Fortran_COMPILER)
    add_cdat_package(LAPACK)
  else()
    add_cdat_package(CLAPACK)
  endif()
  set(CDAT_OS_XTRA_PATH  "")
else()
  set(CDAT_OS_XTRA_PATH  ${CMAKE_INSTALL_PREFIX}/Library/Frameworks/Python.framework/Versions/${PYVER}/bin)
endif()

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
add_cdat_package(NetCDFPLUS)
add_cdat_package(Qt)
add_cdat_package(jasper)
add_cdat_package(g2clib)

# Python Packages
add_cdat_package(Python)
if (CDAT_USE_SYSTEM_PYTHON)
   include(FindPythonInterp)
   set(PYVER ${PYTHON_VERSION_MAJOR}.${PYTHON_VERSION_MINOR})
   # \NOTE This is required or else FindPythonLibs may find whatever version is
   # listed first internally and if that version exists on the system. For example
   # a system might have python version 2.6 and 2.7 both installed.
   set(Python_ADDITIONAL_VERSIONS ${PYVER})
   include(FindPythonLibs)
   message("Using system python ${PYTHON_EXECUTABLE}")
   set(PYTHON_SITE_PACKAGES ${CMAKE_INSTALL_PREFIX}/lib/python${PYVER}/site-packages)
   message("Putting packages in directory ${PYTHON_SITE_PACKAGES}")
   set(PYTHON_EXTRA_PREFIX "--prefix=${CMAKE_INSTALL_PREFIX}")
   message("Setting up prefix for instaling python packages into: ${PYTHON_EXTRA_PREFIX}")
   set(ENV{LD_LIBRARY_PATH} $ENV{LD_LIBRARY_PATH})
   set(PYTHONPATH "${PYTHON_SITE_PACKAGES}:$ENV{PYTHONPATH}")
   message("Set PYTHONPATH to $ENV{PYTHONPATH}")
   get_filename_component(PYTHON_EXECUTABLE_PATH ${PYTHON_EXECUTABLE} PATH)
   set(PYTHON_LIBRARY ${PYTHON_LIBRARIES})
   message("set PYTHON_LIBRARY TO" ${PYTHON_LIBRARY})
   set(PYTHON_INCLUDE ${PYTHON_INCLUDE_DIRS})
else ()
   set(PYTHON_EXECUTABLE ${CMAKE_INSTALL_PREFIX}/bin/python)
   message("Building you a python at ${PYTHON_EXECUTABLE}")
   set(PYTHON_EXTRA_PREFIX "")
   message("Building packages into this python")
   set(PYVER 2.7)
   if (NOT APPLE)
     set(EASY_INSTALL_BINARY ${CMAKE_INSTALL_PREFIX}/bin/easy_install)
     set(PYTHONPATH ${CMAKE_INSTALL_PREFIX}/lib/python${PYVER}/site-packages)
   else ()
     set(EASY_INSTALL_BINARY ${CMAKE_INSTALL_PREFIX}/Library/Frameworks/Python.framework/Versions/${PYVER}/bin/easy_install)
     set(PYTHONPATH ${CMAKE_INSTALL_PREFIX}/Library/Frameworks/Python.framework/Versions/${PYVER}/lib/python${PYVER}/site-packages)
   endif()
endif()
set(ENV{PYTHONPATH} "${PYTHONPATH}")
add_cdat_package(setuptools)
add_cdat_package(SIP)
add_cdat_package(PyQt)
#add_cdat_package(PyOpenGL)

add_cdat_package(NUMPY)
add_cdat_package(SCIPY)
#add_cdat_package(ZMQ)
add_cdat_package(IPYTHON)
add_cdat_package(OPENMPI)
add_cdat_package(Mpi4py)

add_cdat_package(Pmw)
add_cdat_package(libcf)
add_cdat_package(Cython)
add_cdat_package(Numexpr)
add_cdat_package(PyTables)

add_cdat_package(pyspharm)

find_package(Matplotlib)
if (Matplotlib_FOUND)
  message("We found Matplotlib")
else (Matplotlib_FOUND)
  message("We will build Matplotlib for you")
  add_cdat_package(Matplotlib)
endif(Matplotlib_FOUND)

add_cdat_package(GEOS)
add_cdat_package(basemap)

option(CDAT_BUILD_SHAPELY "Build Shapely python module to represent geometric oiobjects and apply predicates and operations on these objects" OFF)

if (CDAT_BUILD_SHAPELY)
  find_package(Shapely)
  if (Shapely_FOUND)
    message("We found Shapely")
  else (Shapely_FOUND)
    message("We will build Shapely for you")
    add_cdat_package(Shapely)
  endif(Shapely_FOUND)
endif()

option(CDAT_BUILD_GDAL "Build the Geospatial Data Abstraction Library (GDAL) and python osgeo module " OFF)

if (CDAT_BUILD_GDAL)
  find_package(Gdal)
  if (Gdal_FOUND)
    message("We found GDAL")
  else (Gdal_FOUND)
    message("We will build GDAL for you")
    add_cdat_package(gdal)
  endif(Gdal_FOUND)
endif()

add_cdat_package(CMOR)
add_cdat_package(CDAT)
add_cdat_package(gui_support)
add_cdat_package(R)

add_cdat_package(scikits)

# Earth System Modelling Framework and the Python wrapper.
# Added by Dave Kindig Tech-X Corp. kindig@txcorp.com
message(crap ${CMAKE_Fortran_COMPILER_ID} and ${Fortran_MAJOR_VERSION})
if(CDAT_BUILD_ESMF_ESMP)

 # the following may need to be adjusted on Crays, otherwise the defaults will likely apply
 set(CDAT_BUILD_ESMF_OS "${CMAKE_SYSTEM_NAME}" CACHE STRING "ESMF_OS env variable, may need to change to Unicos on Crays")
 set(CDAT_BUILD_ESMF_COMPILER "gfortran" CACHE STRING "ESMF_COMPILER env variable, choices are gfortran, intel, pgi, g95, or nag")
 set(CDAT_BUILD_ESMF_COMM "openmpi" CACHE STRING "ESMF_COMM env variable, choices are openmpi, mpiuni, mpi, mpich2, or mvapich2")
 set(CDAT_BUILD_ESMF_ABI "64" CACHE STRING "ESMF_ABI env variable, choices are 32 or 64")
 
 set(TXCMAKE_DIR ${cdat_SOURCE_DIR}/contrib/sciMake)
 include(${TXCMAKE_DIR}/sciFuncsMacros.cmake)
 include(${TXCMAKE_DIR}/sciFortranChecks.cmake)
 if("${CMAKE_Fortran_COMPILER_ID}" STREQUAL GNU)
   string(REGEX MATCHALL "[0-9]+\\." test_version_list ${Fortran_VERSION})
   string(SUBSTRING ${Fortran_VERSION} 0 3 Fortran_MAJOR_VERSION)
   LIST(GET test_version_list 0 Fortran_MAJOR_VERSION)
   LIST(GET test_version_list 1 Fortran_MINOR_VERSION)
 else()
   set(Fortran_MINOR_VERSION "")
 endif()

 if("${CMAKE_Fortran_COMPILER_ID}" STREQUAL GNU)
   # GNU gfortran must be >= 4.3
   if(${Fortran_MAJOR_VERSION} GREATER 3 AND ${Fortran_MINOR_VERSION} GREATER 2)
     ## On APPLE need to test for -arch as well!
     add_cdat_package(ESMF)
   else()
     message("    Skipping ESMF")
     message("    gfortran version needs to be at least 4.3 to install ESMF")
     message("    You have ${Fortran_VERSION}")
   endif()
 else()
   add_cdat_package(ESMF)
   message("Fortran Compiler is: ${CMAKE_Fortran_COMPILER}")
 endif()
endif()


# Optional Packages


option(CDAT_BUILD_PARAVIEW "Build ParaView" ON)
if(CDAT_BUILD_PARAVIEW)
  add_cdat_package(ParaView)
  add_cdat_package(PVFileDialog)
endif()

option(CDAT_BUILD_VISIT "Build VisIt" ON)
if(CDAT_BUILD_VISIT)
  add_cdat_package(VisIt)
endif()

add_cdat_package(vistrails)
add_cdat_package(lepl)
add_cdat_package(Sphinx)
#Comemnting out because file cant seem to download!
#add_cdat_package(lxml)
add_cdat_package(spyder)
add_cdat_package(MyProxyClient)


# C. Doutriaux Turning off Vistrails build from within CDAT
#option(CDAT_BUILD_VISTRAILS "Build VisTrails" OFF)
#if(CDAT_BUILD_VISTRAILS)
#  add_cdat_package(VisTrails)
#endif()

#-----------------------------------------------------------------------------
# Construct Include and Link variables
#-----------------------------------------------------------------------------

#set(system_include_dirs)
if(found_system_include_dirs)
  list(REMOVE_DUPLICATES found_system_include_dirs)
  list(REMOVE_ITEM found_system_include_dirs ${CMAKE_CXX_IMPLICIT_INCLUDE_DIRECTORIES} ${CMAKE_C_IMPLICIT_INCLUDE_DIRECTORIES})
  set(cdat_external_include_directories)
  foreach(include_dir ${found_system_include_dirs})
    set(cdat_external_include_directories "-I${include_dir} ${cdat_external_include_directories}")
  endforeach()
endif()
message("cdat_external_include_directories: ${cdat_external_include_directories}")


#set(system_libraries)
message("found_system_libraries ${found_system_libraries}")
if(found_system_libraries)
  list(REMOVE_DUPLICATES found_system_libraries)
  list(REMOVE_ITEM found_system_libraries ${CMAKE_PLATFORM_IMPLICIT_LINK_DIRECTORIES})
  set(cdat_external_link_directories)
  foreach(library_dir ${found_system_libraries})
    set(cdat_external_link_directories "-L${library_dir} ${cdat_external_link_directories}")
  endforeach()
endif()
message("cdat_external_link_directories: ${cdat_external_link_directories}")

#-----------------------------------------------------------------------------
# configure custom configure/build/install step files
#-----------------------------------------------------------------------------

configure_file(${cdat_CMAKE_SOURCE_DIR}/cdat_common_environment.cmake.in
    ${cdat_CMAKE_BINARY_DIR}/cdat_common_environment.cmake
    @ONLY)

configure_file(${cdat_CMAKE_SOURCE_DIR}/cdat_configure_step.cmake.in
    ${cdat_CMAKE_BINARY_DIR}/cdat_configure_step.cmake
    @ONLY)

configure_file(${cdat_CMAKE_SOURCE_DIR}/cdat_make_step.cmake.in
  ${cdat_CMAKE_BINARY_DIR}/cdat_make_step.cmake
  @ONLY)

configure_file(${cdat_CMAKE_SOURCE_DIR}/cdat_cmake_make_step.cmake.in
  ${cdat_CMAKE_BINARY_DIR}/cdat_cmake_make_step.cmake
  @ONLY)

configure_file(${cdat_CMAKE_SOURCE_DIR}/cdat_install_step.cmake.in
  ${cdat_CMAKE_BINARY_DIR}/cdat_install_step.cmake
  @ONLY)

configure_file(${cdat_CMAKE_SOURCE_DIR}/cleanenv_configure_step.cmake.in
    ${cdat_CMAKE_BINARY_DIR}/cleanenv_configure_step.cmake
    @ONLY)

configure_file(${cdat_CMAKE_SOURCE_DIR}/cdat_configure_step.cmake.in
    ${cdat_CMAKE_BINARY_DIR}/cdat_configure_step.cmake
    @ONLY)

#configure_file(${cdat_CMAKE_SOURCE_DIR}/cdat_python_install_step.cmake.in
#  ${cdat_CMAKE_BINARY_DIR}/cdat_python_install_step.cmake
#  @ONLY)

#------------------------------------------------------------------------------
# Establish Target Dependencies based on Selected Options and find results
#------------------------------------------------------------------------------

set(wget_DEPENDENCIES)
set(pkgconfig_DEPDENCIES ${wget_dep})
set(termcap_DEPENDENCIES ${pkgconfig_dep})
set(readline_DEPENDENCIES ${pkgconfig_dep} ${termcap_dep})
set(uuid_DEPENDENCIES ${pkgconfig_dep})
set(udunits2_DEPENDENCIES ${pkgconfig_dep})
set(libXML2_DEPENDENCIES ${pkgconfig_dep} ${readline_dep})
set(libXSLT_DEPENDENCIES ${pkgconfig_dep} ${readline_dep} ${libXML2_dep})
set(jpeg_DEPENDENCIES ${pkgconfig_dep})
set(zlib_DEPENDENCIES ${pkgconfig_dep})
set(CURL_DEPENDENCIES ${pkgconfig_dep} ${libXML2_dep} ${zlib_dep})
set(png_DEPENDENCIES ${pkgconfig_dep} ${zlib_dep})
set(tiff_DEPENDENCIES ${pkgconfig_dep} ${jpeg_dep} ${zlib_dep})
set(jasper_DEPENDENCIES ${pkgconfig_dep} ${jpeg_dep} ${zlib_dep})
#set(gifsicle_DEPENDENCIES ${pkgconfig_dep})
set(freetype_DEPENDENCIES ${pkgconfig_dep} ${png_dep})
set(fontconfig_DEPENDENCIES ${pkgconfig_dep} ${libXML2_dep} ${freetype_dep})
set(g2clib_DEPENDENCIES ${pkgconfig_dep} ${png_dep} ${zlib_dep} ${jasper_dep})
set(Python_DEPENDENCIES ${pkgconfig_dep} ${termcap_dep} ${readline_dep} ${TclTk_dep} ${libXML2_dep} ${CURL_dep} ${zlib_dep} ${libXSLT_dep} ${png_dep} ${jpeg_dep} ${tiff_dep})
set(SIP_DEPENDENCIES ${pkgconfig_dep} ${Python_dep})
set(Qt_DEPENDENCIES ${pkgconfig_dep} ${readline_dep} ${libXML2_dep} ${jpeg_dep} ${zlib_dep} ${png_dep} ${tiff_dep} ${fontconfig_dep} ${fontconfig_dep})
set(PyQt_DEPENDENCIES ${pkgconfig_dep} ${Qt_dep} ${SIP_dep})
#set(PyOpenGL_DEPENDENCIES ${pkgconfig_dep} ${Python_dep})
set(Pmw_DEPENDENCIES ${pkgconfig_dep} ${Python_dep})
set(libcf_DEPENDENCIES ${pkgconfig_dep} ${Python_dep} ${NetCDF_dep} ${HDF5_dep} ${CURL_dep} ${zlib_dep})
# Added by Dave Kindig Tech-X Corp. kindig@txcorp.com
set(OPENMPI_DEPENDENCIES ${pkgconfig_dep})
set(ESMF_DEPENDENCIES ${pkgconfig_dep} ${OPENMPI_dep})
set(ESMP_DEPENDENCIES ${pkgconfig_dep} ${NUMPY_dep} ${Python_dep} ${ESMF_dep})
set(Cython_DEPENDENCIES ${pkgconfig_dep} ${Python_dep})
set(Numexpr_DEPENDENCIES ${pkgconfig_dep} ${NUMPY_dep} ${setuptools_dep})
set(PyTables_DEPENDENCIES ${pkgconfig_dep} ${Python_dep} ${Cython_dep} ${Numexpr_dep} ${HDF5_dep} ${libXML2_dep} ${libXSLT_dep} ${zlib_dep})
set(pixman_DEPENDENCIES ${pkgconfig_dep})
set(YASM_DEPENDENCIES ${pkgconfig_dep})
set(FFMPEG_DEPENDENCIES ${pkgconfig_dep} ${YASM_dep})
set(ghostscript_DEPENDENCIES ${pkgconfig_dep} ${zlib_dep} ${jpeg_dep} ${png_dep} ${fontconfig_dep} ${freetype_dep} ${pixman_dep} ${libXSLT_dep} ${libXML2_dep})
set(NetCDF_DEPENDENCIES ${pkgconfig_dep} ${HDF5_dep} ${CURL_dep} ${zlib_dep})
set(NetCDFPLUS_DEPENDENCIES ${NetCDF_dep})

set(TclTk_DEPENDENCIES ${pkgconfig_dep} ${libXML2_dep} ${libXSLT_dep} ${jpeg_dep} ${png_dep} ${tiff_dep})

set(setuptools_DEPENDENCIES ${pkgconfig_dep} ${Python_dep})
set(LAPACK_DEPENDENCIES ${pkgconfig_dep})
set(CLAPACK_DEPENDENCIES ${pkgconfig_dep})
set(NUMPY_DEPENDENCIES ${pkgconfig_dep} ${Python_dep} ${CLAPACK_dep} ${LAPACK_dep})
set(SCIPY_DEPENDENCIES ${NUMPY_dep})
set(ZMQ_DEPENDENCIES)
set(IPYTHON_DEPENDENCIES ${setuptools_dep} ${ZMQ_dep} ${NUMPY_dep})

set(VTK_DEPENDENCIES ${pkgconfig_dep} ${zlib_deb} ${png_dep} ${jpeg_dep} ${xml_dep})
set(Cairo_DEPENDENCIES ${pkgconfig_dep} ${png_dep} ${fontconfig_dep} ${freetype_dep} ${pixman_dep} ${libXML2_dep})
set(HDF4_DEPENDENCIES ${pkgconfig_dep} ${zlib_dep} ${jpeg_dep})
set(HDF5_DEPENDENCIES ${pkgconfig_dep} ${libXML2_dep} ${libXSLT_dep} ${zlib_dep})

set(R_DEPENDENCIES)
set(ParaView_DEPENDENCIES ${pkgconfig_dep} ${Python_dep} ${HDF5_dep} ${png_dep} ${jpeg_dep} ${libXML2_dep} ${Qt_dep} ${freetype_dep} ${NetCDFPLUS_dep} ${zlib_dep} ${R_dep})
set(VisIt_DEPENDENCIES  ${pkgconfig_dep} ${SIP_dep} ${PyQt_dep} ${ParaView_dep})

set(CMOR_DEPENDENCIES ${pkgconfig_dep} ${Python_dep} ${HDF5_dep} ${Cairo_dep} ${png_dep} ${fontconfig_dep} ${freetype_dep} ${pixman_dep} ${NetCDF_dep} ${zlib_dep} ${uuid_dep} ${udunits2_dep})
set(CDAT_DEPENDENCIES ${wget_dep} ${CMOR_dep} ${Python_dep} ${PyQt_dep} ${NUMPY_dep} ${Pmw_dep} ${jasper_dep} ${g2clib_dep} ${FFMPEG_dep} ${Qt_dep} ${pixman_dep} ${tiff_dep})
set(gui_support_DEPENDENCIES ${CDAT_dep})
# C.Doutriaux Turning off vistrails build from within CDAT not needed
#set(VisTrails_DEPENDENCIES ${ParaView_dep} ${CDAT_dep})
set(Matplotlib_DEPENDENCIES ${PyQt_dep} ${Cairo_dep} ${NUMPY_dep} )

set(Mpi4py_DEPENDENCIES ${NUMPY_dep} ${OPENMPI_dep})

set(GEOS_DEPENDENCIES ${Matplotlib_dep})
set(Shapely_DEPENDENCIES ${pkgconfig_dep} ${Python_dep} ${GEOS_dep})
set(gdal_DEPENDENCIES ${pkgconfig_dep} ${Python_dep} ${NetCDF_dep} ${HDF5_dep} ${CURL_dep} ${zlib_dep} ${jasper_dep} ${setuptools_dep})
set(basemap_DEPENDENCIES ${GEOS_dep})
set(pyspharm_DEPENDENCIES ${NUMPY_dep})
set(lepl_DEPENDENCIES ${CDAT_dep} ${Numexpr_dep})
set(Sphinx_DEPENDENCIES ${lepl_dep})
set(spyder_DEPENDENCIES ${Sphinx_dep} ${PyQt_dep} )
set(MyProxyClient_DEPENDENCIES ${spyder_dep})
#set(lxml_DEPENDENCIES ${Cython_dep} ${setuptools_dep})
set(vistrails_DEPENDENCIES ${Python_dep})
set(scikits_DEPENDENCIES ${setuptools_dep} ${SCIPY_dep})
# Added by Aashish @ Kitware Inc
set(PVFileDialog_DEPENDENCIES ${ParaView_dep} ${SIP_dep})
list(REMOVE_DUPLICATES external_packages)
foreach(package ${external_packages})
  message("Building ${package}")
  include(${package})
endforeach()

#-----------------------------------------------------------------------------
# checked_get
#-----------------------------------------------------------------------------

set(HASWGET ${WGET_EXECUTABLE})
set(MD5PRG ${MD5_EXECUTABLE})
set(MD5CNT 1)

configure_file(${cdat_CMAKE_SOURCE_DIR}/checked_get.sh.in
  ${cdat_SOURCE_DIR}/checked_get.sh
  @ONLY)

#include(CDATLogger)

configure_file(${cdat_CMAKE_SOURCE_DIR}/setup_cdat.sh.in
  ${CMAKE_INSTALL_PREFIX}/bin/setup_cdat.sh
  @ONLY)
configure_file(${cdat_CMAKE_SOURCE_DIR}/setup_cdat.csh.in
  ${CMAKE_INSTALL_PREFIX}/bin/setup_cdat.csh
  @ONLY)
configure_file(${cdat_CMAKE_SOURCE_DIR}/uvcdat.in
  ${CMAKE_INSTALL_PREFIX}/bin/uvcdat
  @ONLY)
configure_file(${cdat_CMAKE_SOURCE_DIR}/runpytest.in
  ${CMAKE_INSTALL_PREFIX}/bin/runpytest
  @ONLY)

if (BUILD_TESTING)
  add_subdirectory(testing)
endif()
