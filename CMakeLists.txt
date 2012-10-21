#-----------------------------------------------------------------------------
# Top level description and settings
#-----------------------------------------------------------------------------

cmake_minimum_required(VERSION 2.8.8)

# Project name
project(cdat)

set(cdat_CMAKE_SOURCE_DIR ${cdat_SOURCE_DIR}/CMake)
set(cdat_CMAKE_BINARY_DIR ${cdat_BINARY_DIR}/CMake)
set(cdat_external_patch_dir ${cdat_SOURCE_DIR}/exsrc)

if("${CMAKE_INSTALL_PREFIX}" STREQUAL "/usr/local")
  get_filename_component(cdat_ROOT_DIR ${cdat_BINARY_DIR} PATH)
  set(CMAKE_INSTALL_PREFIX ${cdat_ROOT_DIR}/install CACHE STRING "" FORCE)
endif()

set(cdat_EXTERNALS ${CMAKE_INSTALL_PREFIX}/Externals)
set(ENV{PATH} "${cdat_EXTERNALS}/bin:$ENV{PATH}")
message("[INFO] We reset your path to: " $ENV{PATH})

#------------------------------------------------------------------------------
# Update CMake module path
#------------------------------------------------------------------------------

set(CMAKE_MODULE_PATH
  ${cdat_CMAKE_SOURCE_DIR}
  ${cdat_CMAKE_BINARY_DIR}
  ${CMAKE_MODULE_PATH}
  )

include(cmake_utils)

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

  if(NOT "${CMAKE_OSX_SYSROOT}" STREQUAL "")
    if(NOT EXISTS "${CMAKE_OSX_SYSROOT}")
      message(FATAL_ERROR "[ERROR] CMAKE_OSX_SYSROOT='${CMAKE_OSX_SYSROOT}' does not exist")
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
  message(FATAL_ERROR "[ERROR] invalid zlib patch number: '${ZLIB_PATCH_SRC}' valid: 3, 5 or 7")
endif()

if(NOT GIT_PROTOCOL)
  message("[INFO] Select Either git:// or http:// for checking out git submodules.

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
  message("[INFO] Updating git submodules")
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
include(PackageVersions)

set(external_packages)
set(sorted_external_packages)
set(found_system_include_dirs)
set(found_system_libraries)

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

# FIXME: This is a hack put together from previous versions.
find_package(MD5)
find_package(Wget)
find_package(PKG_CONFIG)

add_cdat_package(pkgconfig "")
set(cdat_PKG_CONFIG_EXECUTABLE ${cdat_EXTERNALS}/bin/pkg-config)
set(ENV{PKG_CONFIG} "${cdat_PKG_CONFIG_EXECUTABLE}")

set(ENV{PKG_CONFIG_PATH} "${cdat_EXTERNALS}/lib/pkgconfig:$ENV{PKG_CONFIG_PATH}")
set(ENV{PKG_CONFIG} ${cdat_PKG_CONFIG_EXECUTABLE})

set(cdat_configure_command ${CMAKE_COMMAND} -P ${cdat_CMAKE_BINARY_DIR}/cdat_configure_step.cmake)
set(cdat_make_command ${CMAKE_COMMAND} -P ${cdat_CMAKE_BINARY_DIR}/cdat_make_step.cmake)
set(cdat_install_command ${CMAKE_COMMAND} -P ${cdat_CMAKE_BINARY_DIR}/cdat_install_step.cmake)

add_cdat_package(CDAT "")
add_cdat_package(CMOR "")
add_cdat_package(CURL "")
add_cdat_package(Cairo "")
add_cdat_package(Cython "")
add_cdat_package(FFMPEG "")
add_cdat_package(GEOS "")
add_cdat_package(HDF5 "")
add_cdat_package(IPYTHON "")
add_cdat_package(Matplotlib "")
add_cdat_package(Mpi4py "")
add_cdat_package(MyProxyClient "")
add_cdat_package(NUMPY "")
add_cdat_package(NetCDF "")
add_cdat_package(NetCDFPLUS "")
add_cdat_package(Numexpr "")
add_cdat_package(OPENMPI "")
add_cdat_package(Pmw "")
add_cdat_package(PyQt "")
add_cdat_package(PyTables "")
add_cdat_package(R "")
add_cdat_package(SCIPY "")
add_cdat_package(SIP "")
add_cdat_package(Shapely "")
add_cdat_package(Sphinx "")
add_cdat_package(TclTk "")
add_cdat_package(YASM "")
add_cdat_package(basemap "")
add_cdat_package(fontconfig "")
add_cdat_package(freetype "")
add_cdat_package(g2clib "")
add_cdat_package(gdal "")
add_cdat_package(gui_support "")
add_cdat_package(jasper "")
add_cdat_package(jpeg "")
add_cdat_package(lepl "")
add_cdat_package(libXML2 "")
add_cdat_package(libXSLT "")
add_cdat_package(libcf "")
add_cdat_package(lxml "")
add_cdat_package(pixman "")
add_cdat_package(pkgconfig "")
add_cdat_package(png "")
add_cdat_package(pyspharm "")
add_cdat_package(readline "")
add_cdat_package(setuptools "")
add_cdat_package(spyder "")
add_cdat_package(termcap "")
add_cdat_package(tiff "")
add_cdat_package(udunits2 "")
add_cdat_package(uuid "")
add_cdat_package(vistrails "")
add_cdat_package(wget "")
add_cdat_package(zlib "")
# FIXME: Why this?
# On OSX use vecLib framework
if(NOT APPLE)
  if(CMAKE_Fortran_COMPILER)
    add_cdat_package(LAPACK "")
  else()
    add_cdat_package(CLAPACK "")
  endif()
  set(CDAT_OS_XTRA_PATH  "")
else()
  set(CDAT_OS_XTRA_PATH  ${CMAKE_INSTALL_PREFIX}/Library/Frameworks/Python.framework/Versions/${PYVER}/bin)
endif()
# Python
add_cdat_package(Python "")
if (CDAT_USE_SYSTEM_PYTHON)
   include(FindPythonInterp)
   set(PYVER ${PYTHON_VERSION_MAJOR}.${PYTHON_VERSION_MINOR})
   # \NOTE This is required or else FindPythonLibs may find whatever version is
   # listed first internally and if that version exists on the system. For example
   # a system might have python version 2.6 and 2.7 both installed.
   set(Python_ADDITIONAL_VERSIONS ${PYVER})
   include(FindPythonLibs)
   message("[INFO] Using system python ${PYTHON_EXECUTABLE}")
   set(PYTHON_SITE_PACKAGES ${CMAKE_INSTALL_PREFIX}/lib/python${PYVER}/site-packages)
   message("[INFO] Putting packages in directory ${PYTHON_SITE_PACKAGES}")
   set(PYTHON_EXTRA_PREFIX "--prefix=${CMAKE_INSTALL_PREFIX}")
   message("[INFO] Setting up prefix for instaling python packages into: ${PYTHON_EXTRA_PREFIX}")
   set(ENV{LD_LIBRARY_PATH} $ENV{LD_LIBRARY_PATH})
   set(PYTHONPATH "${PYTHON_SITE_PACKAGES}:$ENV{PYTHONPATH}")
   message("[INFO] Set PYTHONPATH to $ENV{PYTHONPATH}")
   get_filename_component(PYTHON_EXECUTABLE_PATH ${PYTHON_EXECUTABLE} PATH)
   set(PYTHON_LIBRARY ${PYTHON_LIBRARIES})
   message("[INFO] set PYTHON_LIBRARY TO" ${PYTHON_LIBRARY})
   set(PYTHON_INCLUDE ${PYTHON_INCLUDE_DIRS})
else ()
   set(PYTHON_EXECUTABLE ${CMAKE_INSTALL_PREFIX}/bin/python)
   message("[INFO] Building you a python at ${PYTHON_EXECUTABLE}")
   set(PYTHON_EXTRA_PREFIX "")
   message("[INFO] Building packages into this python")
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

# Earth System Modelling Framework and the Python wrapper.
# Added by Dave Kindig Tech-X Corp. kindig@txcorp.com
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
     add_cdat_package(ESMF "")
   else()
     message("[INFO] Skipping ESMF")
     message("[INFO] gfortran version needs to be at least 4.3 to install ESMF")
     message("[INFO] You have ${Fortran_VERSION}")
   endif()
 else()
   add_cdat_package(ESMF "")
   message("[INFO] Fortran Compiler is: ${CMAKE_Fortran_COMPILER}")
 endif()
endif()

# Optional Packages
option(CDAT_BUILD_PARAVIEW "Build ParaView" ON)
if(CDAT_BUILD_PARAVIEW)
  add_cdat_package(ParaView "")
  add_cdat_package(PVFileDialog "")
endif()

option(CDAT_BUILD_VISIT "Build VisIt" ON)
if(CDAT_BUILD_VISIT)
  add_cdat_package(VisIt "")
endif()

#------------------------------------------------------------------------------
# Establish Target Dependencies based on Selected Options and find results
#------------------------------------------------------------------------------
set(CDAT_deps ${wget_pkg} ${CMOR_pkg} ${Python_pkg} ${PyQt_pkg} ${NUMPY_pkg} ${Pmw_pkg} ${jasper_pkg} ${g2clib_pkg} ${FFMPEG_pkg} ${Qt_pkg} ${pixman_pkg} ${tiff_pkg})
set(CLAPACK_deps ${pkgconfig_pkg})
set(CMOR_deps ${pkgconfig_pkg} ${Python_pkg} ${HDF5_pkg} ${Cairo_pkg} ${png_pkg} ${fontconfig_pkg} ${freetype_pkg} ${pixman_pkg} ${NetCDF_pkg} ${zlib_pkg} ${uuid_pkg} ${udunits2_pkg})
set(CURL_deps ${pkgconfig_pkg} ${libXML2_pkg} ${zlib_pkg})
set(Cairo_deps ${pkgconfig_pkg} ${png_pkg} ${fontconfig_pkg} ${freetype_pkg} ${pixman_pkg} ${libXML2_pkg})
set(Cython_deps ${pkgconfig_pkg} ${Python_pkg})
set(ESMF_deps ${pkgconfig_pkg} ${OPENMPI_pkg})
set(ESMP_deps ${pkgconfig_pkg} ${NUMPY_pkg} ${Python_pkg} ${ESMF_pkg})
set(FFMPEG_deps ${pkgconfig_pkg} ${YASM_pkg})
set(GEOS_deps ${Matplotlib_pkg})
set(HDF4_deps ${pkgconfig_pkg} ${zlib_pkg} ${jpeg_pkg})
set(HDF5_deps ${pkgconfig_pkg} ${libXML2_pkg} ${libXSLT_pkg} ${zlib_pkg})
set(IPYTHON_deps ${setuptools_pkg} ${ZMQ_pkg} ${NUMPY_pkg})
set(LAPACK_deps ${pkgconfig_pkg})
set(Matplotlib_deps ${PyQt_pkg} ${Cairo_pkg} ${NUMPY_pkg} )
set(Mpi4py_deps ${NUMPY_pkg} ${OPENMPI_pkg})
set(MyProxyClient_deps ${spyder_pkg})
set(NUMPY_deps ${pkgconfig_pkg} ${Python_pkg} ${CLAPACK_pkg} ${LAPACK_pkg})
set(NetCDFPLUS_deps ${NetCDF_pkg})
set(NetCDF_deps ${pkgconfig_pkg} ${HDF5_pkg} ${CURL_pkg} ${zlib_pkg})
set(Numexpr_deps ${pkgconfig_pkg} ${NUMPY_pkg} ${setuptools_pkg})
set(OPENMPI_deps ${pkgconfig_pkg})
set(PVFileDialog_deps ${ParaView_pkg} ${SIP_pkg})
set(ParaView_deps ${pkgconfig_pkg} ${Python_pkg} ${HDF5_pkg} ${png_pkg} ${jpeg_pkg} ${libXML2_pkg} ${Qt_pkg} ${freetype_pkg} ${NetCDFPLUS_pkg} ${zlib_pkg} ${R_pkg})
set(Pmw_deps ${pkgconfig_pkg} ${Python_pkg})
set(PyQt_deps ${pkgconfig_pkg} ${Qt_pkg} ${SIP_pkg})
set(PyTables_deps ${pkgconfig_pkg} ${Python_pkg} ${Cython_pkg} ${Numexpr_pkg} ${HDF5_pkg} ${libXML2_pkg} ${libXSLT_pkg} ${zlib_pkg})
set(Python_deps ${pkgconfig_pkg} ${termcap_pkg} ${readline_pkg} ${TclTk_pkg} ${libXML2_pkg} ${CURL_pkg} ${zlib_pkg} ${libXSLT_pkg} ${png_pkg} ${jpeg_pkg} ${tiff_pkg})
set(Qt_deps ${pkgconfig_pkg} ${readline_pkg} ${libXML2_pkg} ${jpeg_pkg} ${zlib_pkg} ${png_pkg} ${tiff_pkg} ${fontconfig_pkg} ${fontconfig_pkg})
set(R_deps)
set(SCIPY_deps ${NUMPY_pkg})
set(SIP_deps ${pkgconfig_pkg} ${Python_pkg})
set(Shapely_deps ${pkgconfig_pkg} ${Python_pkg} ${GEOS_pkg})
set(Sphinx_deps ${lepl_pkg})
set(TclTk_deps ${pkgconfig_pkg} ${libXML2_pkg} ${libXSLT_pkg} ${jpeg_pkg} ${png_pkg} ${tiff_pkg})
set(VTK_deps ${pkgconfig_pkg} ${zlib_deb} ${png_pkg} ${jpeg_pkg} ${xml_pkg})
set(VisIt_deps  ${pkgconfig_pkg} ${SIP_pkg} ${PyQt_pkg} ${ParaView_pkg})
set(YASM_deps ${pkgconfig_pkg})
set(ZMQ_deps)
set(basemap_deps ${GEOS_pkg})
set(fontconfig_deps ${pkgconfig_pkg} ${libXML2_pkg} ${freetype_pkg})
set(freetype_deps ${pkgconfig_pkg} ${png_pkg})
set(g2clib_deps ${pkgconfig_pkg} ${png_pkg} ${zlib_pkg} ${jasper_pkg})
set(gdal_deps ${pkgconfig_pkg} ${Python_pkg} ${NetCDF_pkg} ${HDF5_pkg} ${CURL_pkg} ${zlib_pkg} ${jasper_pkg} ${setuptools_pkg})
set(ghostscript_deps ${pkgconfig_pkg} ${zlib_pkg} ${jpeg_pkg} ${png_pkg} ${fontconfig_pkg} ${freetype_pkg} ${pixman_pkg} ${libXSLT_pkg} ${libXML2_pkg})
set(gui_support_deps ${CDAT_pkg})
set(jasper_deps ${pkgconfig_pkg} ${jpeg_pkg} ${zlib_pkg})
set(jpeg_deps ${pkgconfig_pkg})
set(lepl_deps ${CDAT_pkg} ${Numexpr_pkg})
set(libXML2_deps ${pkgconfig_pkg} ${readline_pkg})
set(libXSLT_deps ${pkgconfig_pkg} ${readline_pkg} ${libXML2_pkg})
set(libcf_deps ${pkgconfig_pkg} ${Python_pkg} ${NetCDF_pkg} ${HDF5_pkg} ${CURL_pkg} ${zlib_pkg})
set(lxml_deps ${Cython_pkg} ${setuptools_pkg})
set(pixman_deps ${pkgconfig_pkg})
set(pkgconfig_DEPDENCIES ${wget_pkg})
set(png_deps ${pkgconfig_pkg} ${zlib_pkg})
set(pyspharm_deps ${NUMPY_pkg})
set(readline_deps ${pkgconfig_pkg} ${termcap_pkg})
set(scikits_deps ${setuptools_pkg} ${SCIPY_pkg})
set(setuptools_deps ${pkgconfig_pkg} ${Python_pkg})
set(spyder_deps ${Sphinx_pkg} ${PyQt_pkg} )
set(termcap_deps ${pkgconfig_pkg})
set(tiff_deps ${pkgconfig_pkg} ${jpeg_pkg} ${zlib_pkg})
set(udunits2_deps ${pkgconfig_pkg})
set(uuid_deps ${pkgconfig_pkg})
set(vistrails_deps ${Python_pkg})
set(wget_deps)
set(zlib_deps ${pkgconfig_pkg})
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

#-----------------------------------------------------------------------------
# Sort and then include external packages (less dependant to more)
#-----------------------------------------------------------------------------
list(REMOVE_DUPLICATES external_packages)
sort_external_packages("${external_packages}")
foreach(package ${sorted_external_packages})
  message("[INFO] Building ${package}")
  set(external_package_cmake "External_${package}")
  include(${external_package_cmake})
endforeach()

#-----------------------------------------------------------------------------
# Construct Include and Link variables
#-----------------------------------------------------------------------------
if(found_system_include_dirs)
  list(REMOVE_DUPLICATES found_system_include_dirs)
  list(REMOVE_ITEM found_system_include_dirs ${CMAKE_CXX_IMPLICIT_INCLUDE_DIRECTORIES} ${CMAKE_C_IMPLICIT_INCLUDE_DIRECTORIES})
  set(cdat_external_include_directories)
  foreach(include_dir ${found_system_include_dirs})
    set(cdat_external_include_directories "-I${include_dir} ${cdat_external_include_directories}")
  endforeach()
endif()
message("[INFO] cdat_external_include_directories: ${cdat_external_include_directories}")

message("[INFO] found_system_libraries ${found_system_libraries}")
if(found_system_libraries)
  list(REMOVE_DUPLICATES found_system_libraries)
  list(REMOVE_ITEM found_system_libraries ${CMAKE_PLATFORM_IMPLICIT_LINK_DIRECTORIES})
  set(cdat_external_link_directories)
  foreach(library_dir ${found_system_libraries})
    set(cdat_external_link_directories "-L${library_dir} ${cdat_external_link_directories}")
  endforeach()
endif()
message("[INFO] cdat_external_link_directories: ${cdat_external_link_directories}")

#-----------------------------------------------------------------------------
# checked_get
#-----------------------------------------------------------------------------
set(HASWGET ${WGET_EXECUTABLE})
set(MD5PRG ${MD5_EXECUTABLE})
set(MD5CNT 1)

configure_file(${cdat_CMAKE_SOURCE_DIR}/checked_get.sh.in
  ${cdat_SOURCE_DIR}/checked_get.sh
  @ONLY)
configure_file(${cdat_CMAKE_SOURCE_DIR}/setup_cdat.sh.in
  ${CMAKE_INSTALL_PREFIX}/bin/setup_cdat.sh
  @ONLY)
configure_file(${cdat_CMAKE_SOURCE_DIR}/setup_cdat.csh.in
  ${CMAKE_INSTALL_PREFIX}/bin/setup_cdat.csh
  @ONLY)
configure_file(${cdat_CMAKE_SOURCE_DIR}/uvcdat.in
  ${CMAKE_INSTALL_PREFIX}/bin/uvcdat
  @ONLY)
