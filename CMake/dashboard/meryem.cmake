## Provide information about this project
## This is for my mac 10.6.8 with 16 CPU to be used, everything is ON except for sample download
## env DASHTYPE=Nightly DASHROOT=/lgm/dashboard/BUILDS ctest -S /lgm/dashboard/uvcdat/CMake/dashboard/dashboard.cmake,"next;meryem_mac_106;meryem.cmake" -VVenv DASHTYPE=Nightly DASHROOT=/lgm/dashboard/BUILDS ctest -S /lgm/dashboard/uvcdat/CMake/dashboard/dashboard.cmake,"next;meryem_mac_106;meryem.cmake" -VV
cmake_minimum_required(VERSION 2.8.8)

set(CTEST_BUILD_COMMAND "/usr/bin/make -j1")

## Populate CMakeCache with block of initial data
file(
  # CMake settings
  WRITE "${CTEST_BINARY_DIRECTORY}/CMakeCache.txt" "
  CMAKE_BUILD_TYPE:STRING=${CTEST_BUILD_CONFIGURATION}
  DART_TESTING_TIMEOUT:STRING=1500
  UPDATE_TYPE:STRING=git
  ${PLATFORM_SPECIFIC_CACHE_DATA}

  # Hostname specific library paths
  CDAT_BUILD_ESMF_ESMP:BOOL=ON
  CDAT_BUILD_ESMF_PARALLEL:BOOL=ON
  CDAT_BUILD_PARAVIEW:BOOL=ON
  CDAT_BUILD_VISIT:BOOL=ON
  CDAT_DOWNLOAD_SAMPLE_DATA:BOOL=ON
  GIT_PROTOCOL:STRING="http://"
  CDAT_PACKAGE_CACHE_DIR:PATH=${CTEST_BINARY_DIRECTORY}/../cdat_dependencies
")
