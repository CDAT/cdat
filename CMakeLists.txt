cmake_minimum_required(VERSION 2.8.8 FATAL_ERROR)

# Project name and initial checks
#-----------------------------------------------------------------------------
project(cdat)

# Disable in source build of any kind
STRING(REGEX MATCH "${cdat_SOURCE_DIR}/" BUILDINSOURCE "${cdat_BINARY_DIR}/")
if (BUILDINSOURCE)
   message(FATAL_ERROR "[ERROR] CDAT requires an out of source Build")
endif()

# Set cdat and cmake variables
#-----------------------------------------------------------------------------
set(LLNL_URL http://uv-cdat.llnl.gov/cdat/resources)

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

set(CMAKE_MODULE_PATH
  ${cdat_CMAKE_SOURCE_DIR}
  ${cdat_CMAKE_SOURCE_DIR}/cmake_modules
  ${cdat_CMAKE_SOURCE_DIR}/cdat_modules
  ${cdat_CMAKE_BINARY_DIR}
  ${CMAKE_MODULE_PATH}
)

# Include useful cmake scripts
#-----------------------------------------------------------------------------
include(cmake_utils)
include(check_fortran)
include(CTest)

# Set up the test data. If UVCDAT_USE_SYSTEM_TESTDATA is ON and UVCDAT_TESTDATA
# is not set then we won't use it for testing. Otherwise we'll test either
# with the system test data or download it ourselves.
#-----------------------------------------------------------------------------
if (BUILD_TESTING)
  set(UVCDAT_USE_SYSTEM_TESTDATA ON CACHE BOOL "Use UV-CDAT's test data from the system")
  if(UVCDAT_USE_SYSTEM_TESTDATA)
    set(UVCDAT_TESTDATA "" CACHE PATH "Location of UV-CDAT test data")
    set(UVCDAT_TESTDATA_LOCATION ${UVCDAT_TESTDATA})
  else()
    set(UVCDAT_TestData_GZ boonth-1-22-2013.p94m.tar.gz)
    set(UVCDAT_TestData_MD5 cf47adb0b6164997fb122ccbc3bd6f92)
    file(DOWNLOAD ${LLNL_URL}/${UVCDAT_TestData_GZ} ${CMAKE_BINARY_DIR}/${UVCDAT_TestData_GZ}
      STATUS testdatastatus SHOW_PROGRESS EXPECTED_MD5 ${UVCDAT_TestData_MD5})
    list(GET testdatastatus 0 actualtestdatastatus)
    if(actualtestdatastatus)
      message("[WARNING] Unable to automatically download test data ${testdatastatus}")
    else()
      set(UVCDAT_TESTDATA_DIR ${CMAKE_BINARY_DIR}/UVCDAT_TestData)
      file(MAKE_DIRECTORY ${UVCDAT_TESTDATA_DIR})
      execute_process(
        COMMAND ${CMAKE_COMMAND} -E tar xzf ${CMAKE_BINARY_DIR}/${UVCDAT_TestData_GZ}
        WORKING_DIRECTORY ${UVCDAT_TESTDATA_DIR})
      set(UVCDAT_TESTDATA_LOCATION ${UVCDAT_TESTDATA_DIR})
    endif()
  endif()
endif()

# Change architecture *before* any enable_language() or project()
# calls so that it's set properly to detect 64-bit-ness...
#-----------------------------------------------------------------------------
if(APPLE)

  string(REGEX REPLACE "^([0-9]+)\\.([0-9]+).*$" "\\1" OSX_VER_MAJOR "${CURRENT_OSX_VERSION}")
  string(REGEX REPLACE "^([0-9]+)\\.([0-9]+).*$" "\\2" OSX_VER_MINOR "${CURRENT_OSX_VERSION}")

  if("${CMAKE_OSX_DEPLOYMENT_TARGET}" STREQUAL "")
    set(CMAKE_OSX_ARCHITECTURES "x86_64" CACHE STRING "" FORCE)
    set(CMAKE_OSX_ARCHITECTURES_M "64" CACHE STRING "" FORCE)
    set(CMAKE_OSX_DEPLOYMENT_TARGET "${OSX_VER_MAJOR}.${OSX_VER_MINOR}" CACHE STRING "" FORCE)
  endif()
  
  # Support only 10.6 or higher for now
  if(IS_DIRECTORY "/Developer/SDKs/MacOSX10.6.sdk")
    set(HAVE_10_6_SDK TRUE)
  endif()
  if(IS_DIRECTORY "/Developer/SDKs/MacOSX10.7.sdk")
    set(HAVE_10_7_SDK TRUE)
  endif()
  if(IS_DIRECTORY "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX10.8.sdk")
    set(HAVE_10_8_SDK TRUE)
  endif()

  # Try for the lowest version SDK we can
  if(HAVE_10_6_SDK)
    set(CMAKE_OSX_DEPLOYMENT_TARGET 10.6 CACHE STRING "" FORCE)
    set(CMAKE_OSX_SYSROOT "/Developer/SDKs/MacOSX10.6.sdk" CACHE PATH "" FORCE)
  elseif(HAVE_10_7_SDK)
    set(CMAKE_OSX_DEPLOYMENT_TARGET 10.7 CACHE STRING "" FORCE)
    set(CMAKE_OSX_SYSROOT "/Developer/SDKs/MacOSX10.7.sdk" CACHE PATH "" FORCE)
  elseif(HAVE_10_8_SDK)
    if(NOT IS_DIRECTORY /opt/X11/include/X11)
      message(FATAL_ERROR "[ERROR] X11 headers are not found")
    endif()
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -I/opt/X11/include/X11")
    set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -I/opt/X11/include/X11")
    set(CMAKE_SHARED_LINKER_FLAGS "${CMAKE_SHARED_LINKER_FLAGS} -headerpad_max_install_names")
    set(CMAKE_OSX_DEPLOYMENT_TARGET 10.8 CACHE STRING "" FORCE)
    set(CMAKE_OSX_SYSROOT "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX10.8.sdk" CACHE PATH "" FORCE)
  else()
    message(FATAL_ERROR "[ERROR] CMAKE_OSX_SYSROOT='${CMAKE_OSX_SYSROOT}' not found. Requires MacOS X 10.6 or higher")
  endif()

endif()

# Setup git and related options
#-----------------------------------------------------------------------------
find_package(Git)

if(NOT GIT_PROTOCOL)
  message("[INFO] Select Either git:// or http:// for checking out git submodules.
           If you are behind a firewall then likely you need to choose http:// instead.")
  set(GIT_PROTOCOL "git://" CACHE STRING "Choose protocol to be used by git" FORCE)
  set_property(CACHE GIT_PROTOCOL PROPERTY STRINGS "git://" "http://")
  return()
endif()

if(GIT_PROTOCOL MATCHES "http://")
  execute_process(
    COMMAND ${GIT_EXECUTABLE} config --global url.http://github.com/ajdawson/eof2.git.insteadof git://github.com/ajdawson/eof2.git
    WORKING_DIRECTORY ${cdat_SOURCE_DIR}
  )
  execute_process(
    COMMAND ${GIT_EXECUTABLE} config --global url.http://github.com/ajdawson/eofs.git.insteadof git://github.com/ajdawson/eofs.git
    WORKING_DIRECTORY ${cdat_SOURCE_DIR}
 )
  execute_process(
    COMMAND ${GIT_EXECUTABLE} config --global --unset url.git://uv-cdat.llnl.gov/windfield.git.insteadof http://uv-cdat.llnl.gov/git/windfield.git
    WORKING_DIRECTORY ${cdat_SOURCE_DIR}
  )
  execute_process(
      COMMAND ${GIT_EXECUTABLE} config --global url.http://github.com/UV-CDAT/scimake.git.insteadof git://github.com/UV-CDAT/scimake.git
    WORKING_DIRECTORY ${cdat_SOURCE_DIR}
    )
  execute_process(
    COMMAND ${GIT_EXECUTABLE} config --global url.http://github.com/ajdawson/windspharm.git.insteadof git://github.com/ajdawson/windspharm.git
    WORKING_DIRECTORY ${cdat_SOURCE_DIR}
 )
else()
  execute_process(
    COMMAND ${GIT_EXECUTABLE} config --global --unset url.http://github.com/ajdawson/eof2.git.insteadof git://github.com/ajdawson/eof2.git
    WORKING_DIRECTORY ${cdat_SOURCE_DIR}
  )
  execute_process(
    COMMAND ${GIT_EXECUTABLE} config --global --unset url.http://github.com/ajdawson/eofs.git.insteadof git://github.com/ajdawson/eofs.git
    WORKING_DIRECTORY ${cdat_SOURCE_DIR}
 )
  execute_process(
    COMMAND ${GIT_EXECUTABLE} config --global url.git://uv-cdat.llnl.gov/windfield.git.insteadof http://uv-cdat.llnl.gov/git/windfield.git
    WORKING_DIRECTORY ${cdat_SOURCE_DIR}
  )
  execute_process(
      COMMAND ${GIT_EXECUTABLE} config --global --unset url.http://github.com/UV-CDAT/scimake.git.insteadof git://github.com/UV-CDAT/scimake.git
    WORKING_DIRECTORY ${cdat_SOURCE_DIR}
    )
  execute_process(
    COMMAND ${GIT_EXECUTABLE} config --global --unset url.http://github.com/ajdawson/windspharm.git.insteadof git://github.com/ajdawson/windspharm.git
    WORKING_DIRECTORY ${cdat_SOURCE_DIR}
 )
endif()

# Initialize submodules
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

# Check if system has fortran compiler and act accordingly
#-----------------------------------------------------------------------------
if(CMAKE_Fortran_COMPILER)
  enable_language(Fortran)
  set(CONFIG_SUMMARY ${cdat_BINARY_DIR}/logs/config.summary)
  include(${CMAKE_ROOT}/Modules/CMakeDetermineFortranCompiler.cmake)
endif(CMAKE_Fortran_COMPILER)


# Set build related properties
#-----------------------------------------------------------------------------
set(PARTS_BUILT_INFO "${cdat_BINARY_DIR}/build_info.txt" CACHE STRING "File where to store build infos" FORCE)

## The following is to know if we are prepring for an offline installation
## ON is the default means we can download as we build
## PREP means with are using this machine as a proxy to download all needed files in order to move them (somehow) to the OFFLINE machine where build will happen
## OFF the machine has no internet access all files are suppposed to be here, pre-downloaded
option(OFFLINE_BUILD "Is there internet access, are we preping for it?" OFF)

option(CDAT_BUILD_PARALLEL "Build parallel components of CDAT" ON)
#OPtion to choose between easy_install and pip (crunchy ssl/man in the middle prevents us to use pip here...
set(EGG_INSTALLER "PIP" CACHE STRING "Which package installer to use") 
set_property(CACHE EGG_INSTALLER PROPERTY STRINGS "PIP" "EASY_INSTALL")
set(PIP_CERTIFICATE "" CACHE STRING "Certificate to use for PIP (LLNL issue really)")
#Options for various types of builds
option(CDAT_BUILD_GUI "Builds GUI-based dependencies (Vistrails, ParaView, VisIt, R, etc...) " ON)
option(CDAT_BUILD_GRAPHICS "Build graphics-based dependencies (vcs, pyqt, Vistrails, ParaView, VisIt, R, etc...) " ON)
option(CDAT_BUILD_ESGF "Builds a customized version for ESGF (Tkinter, less packages etc..)" OFF)

#Flipping the option, Aashish add_cdat_external_dependent seems to need a negative of that
if (CDAT_BUILD_ESGF)
    set(CDAT_BUILD_WO_ESGF OFF)
else()
    set(CDAT_BUILD_WO_ESGF ON)
endif()

#First of all if ESGF then turn OFF GRAPHICS
if (NOT CDAT_BUILD_WO_ESGF)
    set(CDAT_BUILD_GRAPHICS OFF)
    set(CDAT_BUILD_PARALLEL OFF)
    set(CDMS_ONLY --enable-cdms-only)
else()
    set(CDMS_ONLY "")
endif()

# if no graphics then no gui as well
if (NOT CDAT_BUILD_GRAPHICS)
  set(CDAT_BUILD_GUI OFF)
endif()
set(ep_prefix ${cdat_BINARY_DIR}/build/prefix)
set_property(DIRECTORY PROPERTY ep_log_dir ${cdat_BINARY_DIR}/logs)
set(ep_log_options
  LOG_DOWNLOAD 0
  LOG_UPDATE 1
  LOG_CONFIGURE 1
  LOG_BUILD 1
  LOG_TEST 1
  LOG_INSTALL 1
)

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
  set(cdat_apple_compiler_args
    -DCMAKE_OSX_ARCHITECTURES:STRING=${CMAKE_OSX_ARCHITECTURES}
    -DCMAKE_OSX_ARCHITECTURES_M:STRING=${CMAKE_OSX_ARCHITECTURES_M}
    -DCMAKE_OSX_DEPLOYMENT_TARGET:STRING=${CMAKE_OSX_DEPLOYMENT_TARGET}
    -DCMAKE_OSX_SYSROOT:PATH=${CMAKE_OSX_SYSROOT}
  )

  set(cdat_osx_arch_flag "-m${CMAKE_OSX_ARCHITECTURES_M}")
  set(cdat_osx_arch_flag_fortran "-m${CMAKE_OSX_ARCHITECTURES_M}")
  set(cdat_osx_version_flag "-mmacosx-version-min=${CMAKE_OSX_DEPLOYMENT_TARGET}")
  set(cdat_osx_sysroot "-isysroot ${CMAKE_OSX_SYSROOT}")
  set(cdat_osx_flags "${cdat_osx_arch_flag} ${cdat_osx_version_flag} ${cdat_osx_sysroot} -pipe")
  set(cdat_osx_flags_fortran "${cdat_osx_arch_flag_fortran} ${cdat_osx_version_flag} ${cdat_osx_sysroot} -pipe")
  set(cdat_osx_ld_flags "-bind_at_load -mmacosx-version-min=${CMAKE_OSX_DEPLOYMENT_TARGET}")

  set(LIBRARY_PATH DYLD_LIBRARY_PATH)
  set(CMAKE_PLATFORM APPLE)
  set(_LINK_LIBRARY_SUFFIX .dylib)
  list(APPEND cdat_compiler_args -DCMAKE_BUILD_TYPE:STRING=${CMAKE_BUILD_TYPE})
elseif(UNIX)
  set(cdat_rpath_flag -Wl,-rpath,)
  list(APPEND cdat_compiler_args -DCMAKE_BUILD_TYPE:STRING=${CMAKE_BUILD_TYPE})
  set(LIBRARY_PATH LD_LIBRARY_PATH)
  set(CMAKE_PLATFORM UNIX)
  set(_LINK_LIBRARY_SUFFIX .so)
elseif(WIN32)
  list(APPEND cdat_compiler_args -DCMAKE_BUILD_TYPE:STRING=${CMAKE_CFG_INTDIR})
  set(LIBRARY_PATH PATH)
  set(CMAKE_PLATFORM WIN32)
  set(_LINK_LIBRARY_SUFFIX .lib)
endif()

# Include external project now (we have our own modifications) and create
# required directories as often third party install systems do bad things
# when left to create them.
#-----------------------------------------------------------------------------
set(CDAT_PACKAGE_CACHE_DIR
#  ${CMAKE_CURRENT_BINARY_DIR}/../cdat_dependencies"
  "${CMAKE_CURRENT_BINARY_DIR}"
  CACHE PATH
  "Directory where source tar balls of external dependencies are kept"
)

include(ExternalProject)

file(MAKE_DIRECTORY ${cdat_EXTERNALS})
file(MAKE_DIRECTORY ${cdat_EXTERNALS}/lib)
file(MAKE_DIRECTORY ${cdat_EXTERNALS}/bin)
file(MAKE_DIRECTORY ${cdat_EXTERNALS}/include)
file(MAKE_DIRECTORY ${cdat_BINARY_DIR}/logs)
file(MAKE_DIRECTORY ${cdat_BINARY_DIR}/build)
file(MAKE_DIRECTORY ${cdat_BINARY_DIR}/sources)

# Configure cdat command files
#-----------------------------------------------------------------------------
set(cdat_configure_command ${CMAKE_COMMAND} -P ${cdat_CMAKE_BINARY_DIR}/cdat_configure_step.cmake)
set(cdat_make_command ${CMAKE_COMMAND} -P ${cdat_CMAKE_BINARY_DIR}/cdat_make_step.cmake)
set(cdat_install_command ${CMAKE_COMMAND} -P ${cdat_CMAKE_BINARY_DIR}/cdat_install_step.cmake)

# Include essential packages
#-----------------------------------------------------------------------------
set(external_packages)
set(found_system_include_dirs)
set(found_system_libraries)

include(basemap_pkg)
include(cairo_pkg)
include(cdat_pkg)
include(clapack_pkg)
include(cmor_pkg)
include(curl_pkg)
include(cython_pkg)
include(data_pkg)
include(esmf_pkg)
include(ffmpeg_pkg)
include(fontconfig_pkg)
include(freetype_pkg)
include(g2clib_pkg)
include(g2clib_pkg)
include(gdal_pkg)
include(geos_pkg)
include(gui_support_pkg)
include(hdf5_pkg)
include(zmq_pkg)
include(pyzmq_pkg)
include(tornado_pkg)
include(ipython_pkg)
include(jasper_pkg)
include(jpeg_pkg)
include(lapack_pkg)
include(lepl_pkg)
include(libcf_pkg)
include(libdrs_pkg)
include(libxml2_pkg)
include(libxslt_pkg)
include(matplotlib_pkg)
include(md5_pkg)
include(mpi4py_pkg)
include(pyopenssl_pkg)
include(myproxyclient_pkg)
include(netcdf_pkg)
include(netcdfplus_pkg)
include(numexpr_pkg)
include(numpy_pkg)
include(mpi_pkg)
include(paraview_pkg)
include(pixman_pkg)
include(pkgconfig_pkg)
include(pmw_pkg)
include(png_pkg)
include(pyqt_pkg)
include(pyspharm_pkg)
include(pytables_pkg)
include(python_pkg)
include(qt4_pkg)
include(readline_pkg)
include(r_pkg)
include(scikits_pkg)
include(scipy_pkg)
include(setuptools_pkg)
include(distribute_pkg)
include(pip_pkg)
include(shapely_pkg)
include(sip_pkg)
include(pygments_pkg)
include(jinja2_pkg)
include(docutils_pkg)
include(sphinx_pkg)
include(spyder_pkg)
include(tcltk_pkg)
include(termcap_pkg)
include(tiff_pkg)
include(udunits2_pkg)
include(uuid_pkg)
include(visit_pkg)
include(vistrails_pkg)
include(wget_pkg)
include(yasm_pkg)
include(zlib_pkg)
include(cdatlogger_pkg)

# Configure custom configure/build/install step files
#-----------------------------------------------------------------------------
configure_file(${cdat_CMAKE_SOURCE_DIR}/cdat_modules_extra/cdat_common_environment.cmake.in
    ${cdat_CMAKE_BINARY_DIR}/cdat_common_environment.cmake
    @ONLY
)

configure_file(${cdat_CMAKE_SOURCE_DIR}/cdat_modules_extra/cdat_configure_step.cmake.in
    ${cdat_CMAKE_BINARY_DIR}/cdat_configure_step.cmake
    @ONLY
)

configure_file(${cdat_CMAKE_SOURCE_DIR}/cdat_modules_extra/cdat_make_step.cmake.in
  ${cdat_CMAKE_BINARY_DIR}/cdat_make_step.cmake
  @ONLY
)

configure_file(${cdat_CMAKE_SOURCE_DIR}/cdat_modules_extra/cdat_cmake_make_step.cmake.in
  ${cdat_CMAKE_BINARY_DIR}/cdat_cmake_make_step.cmake
  @ONLY
)

configure_file(${cdat_CMAKE_SOURCE_DIR}/cdat_modules_extra/cdat_install_step.cmake.in
  ${cdat_CMAKE_BINARY_DIR}/cdat_install_step.cmake
  @ONLY
)

configure_file(${cdat_CMAKE_SOURCE_DIR}/cdat_modules_extra/cleanenv_configure_step.cmake.in
    ${cdat_CMAKE_BINARY_DIR}/cleanenv_configure_step.cmake
    @ONLY
)

configure_file(${cdat_CMAKE_SOURCE_DIR}/cdat_modules_extra/cdat_configure_step.cmake.in
    ${cdat_CMAKE_BINARY_DIR}/cdat_configure_step.cmake
    @ONLY
)

# Now sort and include external packages
#-----------------------------------------------------------------------------
include(TopologicalSort)
message("[DEBUG] packages ${external_packages}")
foreach(package ${external_packages})
  string(TOLOWER ${package} lc_package)
  include("${lc_package}_deps")
endforeach()

topological_sort(external_packages "" "_deps")

foreach(package ${external_packages})
  enable_cdat_package_deps(${package})
endforeach()

foreach(package ${external_packages})
  string(TOLOWER ${package} lc_package)
  string(TOUPPER ${package} uc_package)

  if(CDAT_BUILD_${uc_package})
      message("[INFO] Package --- ${package}/${uc_package}/${${uc_package}_VERSION} --- will be built")
    list(APPEND packages_info "${package} ${${uc_package}_VERSION} ${${uc_package}_SOURCE} ${${uc_package}_MD5}\n")
    include("${lc_package}_external")
  endif()
endforeach()
file(WRITE ${PARTS_BUILT_INFO} ${packages_info})
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
message("[INFO] CDAT external include directories: ${cdat_external_include_directories}")

message("[INFO] System libraries: ${found_system_libraries}")
if(found_system_libraries)
  list(REMOVE_DUPLICATES found_system_libraries)
  list(REMOVE_ITEM found_system_libraries ${CMAKE_PLATFORM_IMPLICIT_LINK_DIRECTORIES})
  set(cdat_external_link_directories)
  foreach(library_dir ${found_system_libraries})
    set(cdat_external_link_directories "-L${library_dir} ${cdat_external_link_directories}")
  endforeach()
endif()
message("[INFO] CDAT external link directories: ${cdat_external_link_directories}")

# Configure remaining files
#-----------------------------------------------------------------------------
configure_file(${cdat_CMAKE_SOURCE_DIR}/cdat_modules_extra/checked_get.sh.in
  ${cdat_BINARY_DIR}/checked_get.sh
  @ONLY
)

configure_file(${cdat_CMAKE_SOURCE_DIR}/cdat_modules_extra/predownload.py.in
  ${cdat_BINARY_DIR}/predownload.py
  @ONLY
)

configure_file(${cdat_CMAKE_SOURCE_DIR}/cdat_modules_extra/setup_cdat.sh.in
  ${CMAKE_INSTALL_PREFIX}/bin/setup_cdat.sh
  @ONLY
)

configure_file(${cdat_CMAKE_SOURCE_DIR}/cdat_modules_extra/preofflinebuild.sh.in
  ${cdat_BINARY_DIR}/preofflinebuild.sh
  @ONLY
)

configure_file(${cdat_CMAKE_SOURCE_DIR}/cdat_modules_extra/setup_cdat.csh.in
  ${CMAKE_INSTALL_PREFIX}/bin/setup_cdat.csh
  @ONLY
)

configure_file(${cdat_CMAKE_SOURCE_DIR}/cdat_modules_extra/uvcdat.in
  ${CMAKE_INSTALL_PREFIX}/bin/uvcdat
  @ONLY
)


if (BUILD_TESTING)
  configure_file(${cdat_CMAKE_SOURCE_DIR}/cdat_modules_extra/runpytest.in
    ${CMAKE_INSTALL_PREFIX}/bin/runpytest
    @ONLY
  )
  add_subdirectory(testing)
endif()
