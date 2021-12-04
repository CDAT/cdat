cmake_minimum_required(VERSION 2.8.8 FATAL_ERROR)
#=============================================================================
project(cdat)

# Set various variables
#=============================================================================
set(LLNL_URL http://uv-cdat.llnl.gov/cdat/resources)
set(cdat_CMAKE_SOURCE_DIR ${cdat_SOURCE_DIR}/CMake)
set(cdat_CMAKE_BINARY_DIR ${cdat_BINARY_DIR}/CMake)
set(cdat_external_patch_dir ${cdat_SOURCE_DIR}/exsrc)



set(CMAKE_MODULE_PATH
  ${cdat_CMAKE_SOURCE_DIR}
  ${cdat_CMAKE_SOURCE_DIR}/cmake_modules
  ${cdat_CMAKE_SOURCE_DIR}/cdat_modules
  ${cdat_CMAKE_BINARY_DIR}
  ${CMAKE_MODULE_PATH}
)

find_program(CONDA NAMES conda)

if ( ${CONDA} STREQUAL "CONDA-NOTFOUND" ) 
    message(FATAL_ERROR "Could not locate conda, please make sure conda is installed and in your PATH")
endif()

find_program(ACTIVATE NAMES activate)
if ( ${ACTIVATE} STREQUAL "ACTIVATE-NOTFOUND" ) 
    message(FATAL_ERROR "Could not locate activate, please make sure conda is installed and in your PATH")
endif()
message("[INFO] Found conda at: ${CONDA}")

if (DEFINED ENV{UVCDAT_ANONYMOUS_LOG})
  if (($ENV{UVCDAT_ANONYMOUS_LOG} STREQUAL "yes") OR
      ($ENV{UVCDAT_ANONYMOUS_LOG} STREQUAL "no"))
    message("[INFO] Using UVCDAT_ANONYMOUS_LOG found in environment which is set to: '$ENV{UVCDAT_ANONYMOUS_LOG}'")
  else()
    message("[INFO] Found UVCDAT_ANONYMOUS_LOG in environment which is set to: '$ENV{UVCDAT_ANONYMOUS_LOG}'")
    message("[INFO] Valid values are 'True' or 'False' resetting to: 'True'")
    set(ENV{UVCDAT_ANONYMOUS_LOG} "yes")
  endif()
else()
  set(ENV{UVCDAT_ANONYMOUS_LOG} "no")
endif()

# Include useful cmake scripts
#=============================================================================
include(cmake_utils)
include(check_fortran)
include(CTest)

# Change architecture *before* any enable_language() or project()
# calls so that it's set properly to detect 64-bit-ness...
#-----------------------------------------------------------------------------
if(APPLE)
 include (AppleSDKUtils)
endif()

# Setup git and related options
#=============================================================================
find_package(Git)

if(NOT GIT_PROTOCOL)
   set(temp_git_protocol "git://")
   execute_process(COMMAND "${GIT_EXECUTABLE}" ls-remote "git://github.com/UV-CDAT/uvcdat.git"
    WORKING_DIRECTORY "${CMAKE_SOURCE_DIR}"
    RESULT_VARIABLE res
    OUTPUT_VARIABLE out
    ERROR_QUIET
    OUTPUT_STRIP_TRAILING_WHITESPACE)
  if(NOT res EQUAL 0)
    set(temp_git_protocol "http://")
    execute_process(COMMAND "${GIT_EXECUTABLE}" ls-remote "http://github.com/UV-CDAT/uvcdat.git"
      WORKING_DIRECTORY "${CMAKE_SOURCE_DIR}"
      RESULT_VARIABLE res
      OUTPUT_VARIABLE out
      ERROR_QUIET
      OUTPUT_STRIP_TRAILING_WHITESPACE)
      if (NOT res EQUAL 0)
        set(temp_git_protocol "https://")
      endif()
  endif()

  set(GIT_PROTOCOL ${temp_git_protocol} CACHE STRING "Choose protocol to be used by git" FORCE)
  set_property(CACHE GIT_PROTOCOL PROPERTY STRINGS "git://" "http://" "https://")
endif()

# Checkout the baseline repository.
#=============================================================================
if(BUILD_TESTING)
  # bots will turn this off
  option(CDAT_CHECKOUT_BASELINE_MATCHING_BRANCH "Do magic to find matching baseline branch" ON)
  set(UVCDAT_GIT_TESTDATA_DIR "${CMAKE_BINARY_DIR}/uvcdat-testdata/"
    CACHE PATH "Directory to store supplemental testing data (Baselines, etc).")
  set(REPO_URL "")
  if(GIT_PROTOCOL MATCHES "http://")
    set(REPO_URL "http://github.com/UV-CDAT/uvcdat-testdata.git")
  else()
    set(REPO_URL "git://github.com/UV-CDAT/uvcdat-testdata.git")
  endif()
  find_package(Git QUIET REQUIRED)
  add_custom_target(update-testdata ALL
    COMMAND "${CMAKE_COMMAND}"
      "-DGIT_EXECUTABLE=${GIT_EXECUTABLE}"
      "-DTESTDATA_URL=${REPO_URL}"
      "-DTESTDATA_DIR=${UVCDAT_GIT_TESTDATA_DIR}"
      "-DSOURCE_DIR=${cdat_SOURCE_DIR}"
      "-DCDAT_CHECKOUT_BASELINE_MATCHING_BRANCH=${CDAT_CHECKOUT_BASELINE_MATCHING_BRANCH}"
      -P "${cdat_CMAKE_SOURCE_DIR}/cdat_modules_extra/checkout_testdata.cmake"
    COMMENT "Updating uvcdat-testdata repo."
  )
endif()

# Check if system has fortran compiler and act accordingly
#=============================================================================
if(CMAKE_Fortran_COMPILER)
  enable_language(Fortran)
  set(CONFIG_SUMMARY ${cdat_BINARY_DIR}/logs/config.summary)
  include(${CMAKE_ROOT}/Modules/CMakeDetermineFortranCompiler.cmake)
endif(CMAKE_Fortran_COMPILER)

# Set build related properties
#=============================================================================
set(PARTS_BUILT_INFO "${cdat_BINARY_DIR}/build_info.txt" CACHE STRING "File where to store build infos" FORCE)

# The following is to know if we are prepring for an offline installation
# ON is the default means we can download as we build
# PREP means with are using this machine as a proxy to download all needed
# files in order to move them (somehow) to the OFFLINE machine where build will happen
# OFF the machine has no internet access all files are suppposed to be here, pre-downloaded

# option(OFFLINE_BUILD "Is there internet access, are we preping for it?" OFF)
option(CDAT_BUILD_PARALLEL "Build parallel components of CDAT" OFF)

# OSMesa/VTK aren't playing nicely on macs. Disabling for now.
include(CMakeDependentOption)
cmake_dependent_option(CDAT_BUILD_OFFSCREEN "Use OSMesa for offscreen rendering." OFF
  "NOT APPLE" OFF)
if (CDAT_BUILD_OFFSCREEN)
    message("[INFO] will build against -nox version of conda packages")
    set(CONDA_NOX "-nox")
else()
    message("[INFO] will build against regular X version of conda packages")
    set(CONDA_NOX "")
endif()

# Option to enable vtkweb for cdatweb
option(CDAT_BUILD_WEB "Build in Web support (VTKWeb, etc.)" OFF)


set(ep_prefix ${cdat_BINARY_DIR}/build/prefix)
set_property(DIRECTORY PROPERTY ep_log_dir ${cdat_BINARY_DIR}/logs)

option(CDAT_DISPLAY_LOG "Show logging information on the console" OFF)

if (CDAT_DISPLAY_LOG)
    set(ep_log_options
      LOG_DOWNLOAD 1
      LOG_UPDATE 0
      LOG_CONFIGURE 1
      LOG_BUILD 0
      LOG_TEST 1
      LOG_INSTALL 1
    )
else()
set(ep_log_options
  LOG_DOWNLOAD 0
  LOG_UPDATE 1
  LOG_CONFIGURE 1
  LOG_BUILD 1
  LOG_TEST 1
  LOG_INSTALL 1
)
endif()

# Set compiler arguments
#=============================================================================
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
  if("${CMAKE_C_COMPILER_ID}" STREQUAL "Clang" AND ${CMAKE_C_COMPILER_VERSION} VERSION_GREATER 4.2)
    set(cdat_rpath_flag -Wl,-rpath,)
  else()
    set(cdat_rpath_flag -R)
  endif()
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
if(APPLE)
  set(cdat_osx_cppflags "-I${CMAKE_OSX_SYSROOT}/usr/include -I/usr/X11R6/include ${cdat_osx_arch_flag} ${cdat_osx_version_flag} ${cdat_osx_sysroot} -pipe ")
else()
  set(cdat_osx_cppflags "-I${CMAKE_OSX_SYSROOT}/usr/include  ${cdat_osx_arch_flag} ${cdat_osx_version_flag} ${cdat_osx_sysroot} -pipe ")
endif()
  set(cdat_osx_flags_fortran "-I${CMAKE_OSX_SYSROOT}/usr/include ${cdat_osx_arch_flag_fortran} ${cdat_osx_version_flag} ${cdat_osx_sysroot} -pipe")
  set(cdat_osx_ld_flags "-L${CMAKE_OSX_SYSROT}/usr/X11R6/lib -L/System/Library/Frameworks/ImageIO.framework/Versions/A/Resources -bind_at_load -mmacosx-version-min=${CMAKE_OSX_DEPLOYMENT_TARGET}")
  set(cdat_osx_cxxflags "-I${CMAKE_OSX_SYSROOT}/usr/include ${cdat_osx_cppflag} ${cdat_osx_ld_flags}")
  set(cdat_osx_flags "${cdat_osx_cxxflags}")

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
#=============================================================================
set(CDAT_PACKAGE_CACHE_DIR
  "${CMAKE_CURRENT_BINARY_DIR}"
  CACHE PATH
  "Directory where source tar balls of external dependencies are kept"
)

include(ExternalProject)
include(cdat_pkg)
# CONDA Options
set(CONDA_ENVIRONMENT_NAME ${cdat_VERSION} CACHE STRING "Name of conda environment we want to build CDAT in")
set(CONDA_CHANNEL_UVCDAT "conda-forge -c uvcdat" CACHE STRING "channels to use (if more than one use '-c' between channels e.g. uvcdat/label/nightly -c uvcdat)")

execute_process(COMMAND
    "${GIT_EXECUTABLE}" symbolic-ref --short HEAD
    WORKING_DIRECTORY "${cdat_SOURCE_DIR}"
    RESULT_VARIABLE RESULT
    ERROR_VARIABLE OUTPUT
    OUTPUT_VARIABLE CURRENT_BRANCH)
message("[INFO] CURRENT BRANCH ${CURRENT_BRANCH}")
set(CHECKOUT_BRANCH ${CURRENT_BRANCH} CACHE STRING "branch to try to check out on each repo")
# Now sort and include external packages
#=============================================================================
include(TopologicalSort)
message("[INFO] These are the packages that we will try to build: ${external_packages}")
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
    list(APPEND packages_info "${package} ${${uc_package}_VERSION} ${${uc_package}_SOURCE} ")
    if (${uc_package}_MD5)
      list(APPEND packages_info "${${uc_package}_MD5}\n")
    else()
      list(APPEND packages_info "${${uc_package}_BRANCH}\n")
    endif()
    include("${lc_package}_external")
  endif()
endforeach()

configure_file(${cdat_CMAKE_SOURCE_DIR}/cdat_modules_extra/runtest.in
    ${cdat_CMAKE_BINARY_DIR}/runtest
    @ONLY
    )
configure_file(${cdat_CMAKE_SOURCE_DIR}/cdat_modules_extra/install_cdat_from_conda.bash.in
    ${cdat_CMAKE_BINARY_DIR}/install_cdat_from_conda.bash
    @ONLY
    )

configure_file(${cdat_CMAKE_SOURCE_DIR}/cdat_modules_extra/clean_cdat_from_conda.bash.in
    ${cdat_CMAKE_BINARY_DIR}/clean_cdat_from_conda.bash
    @ONLY
    )

configure_file(${cdat_CMAKE_SOURCE_DIR}/cdat_modules_extra/clean_local_build.bash.in
    ${cdat_CMAKE_BINARY_DIR}/clean_local_build.bash
    @ONLY
    )

# Clean conda env
add_custom_target(clean-conda
    COMMAND ${cdat_CMAKE_BINARY_DIR}/clean_cdat_from_conda.bash
    )

# Clean local env
add_custom_target(clean-local
    COMMAND ${cdat_CMAKE_BINARY_DIR}/clean_local_build.bash
    )

# Let's clone the repos so that we can add test suites...
set(GIT_REPOS "cdat_info;cdtime;cdms;esg;dv3d;vcs;vcsaddons;cdutil;unidata;xmgrace;genutil;thermo;wk;distarray")
foreach(gpkg ${GIT_REPOS})
    message("[INFO] cloning repo: ${gpkg}")
    execute_process(COMMAND
        "${GIT_EXECUTABLE}" clone git://github.com/UV-CDAT/${gpkg}
        WORKING_DIRECTORY "${cdat_BINARY_DIR}"
        RESULT_VARIABLE RESULT
        ERROR_VARIABLE OUTPUT
        OUTPUT_VARIABLE OUTPUT)
    message("[INFO] checking out branch ${CHECKOUT_BRANCH}")
    execute_process(COMMAND
        "${GIT_EXECUTABLE}" pull
        WORKING_DIRECTORY "${cdat_BINARY_DIR}/${gpkg}"
        RESULT_VARIABLE RESULT
        ERROR_VARIABLE OUTPUT
        OUTPUT_VARIABLE OUTPUT)
    execute_process(COMMAND
        "${GIT_EXECUTABLE}" checkout ${CHECKOUT_BRANCH}
        WORKING_DIRECTORY "${cdat_BINARY_DIR}/${gpkg}"
        RESULT_VARIABLE RESULT
        ERROR_VARIABLE OUTPUT
        OUTPUT_VARIABLE OUTPUT)
endforeach()
# Now we can add testing directory
add_subdirectory(testing)
