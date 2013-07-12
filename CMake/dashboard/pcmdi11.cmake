## Provide information about this project
## This is for a RH6 with locally installed QT 4.8.0 (in /usr/local/Qt/4.8.0)
## 16 CPUs
## Building a zlib version that matches what's on the system
## Uses local png/xml2/xslt

## env DASHTYPE=Nightly DASHROOT=/export/doutriaux1/dashboard/BUILDS ctest -S /export/doutriaux1/dashboard/uvcdat/CMake/dashboard/dashboard.cmake,"next;crunchy_RedHat_6;crunchy.cmake" -VV

cmake_minimum_required(VERSION 2.8.8)

set(CTEST_BUILD_COMMAND "/usr/bin/make -j15")

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
  CDAT_ANONYMOUS_LOG:BOOL=OFF
  CDAT_BUILD_ESMF_PARALLEL:BOOL=ON
  CDAT_BUILD_PARAVIEW:BOOL=ON
  CDAT_BUILD_VISIT:BOOL=ON
  CDAT_DOWNLOAD_SAMPLE_DATA:BOOL=ON
  CDAT_USE_SYSTEM_LIBXML2:BOOL=ON
  CDAT_USE_SYSTEM_LIBXSLT:BOOL=ON
  CDAT_USE_SYSTEM_LIBPNG:BOOL=ON
  CDAT_USE_SYSTEM_ZLIB_PATCH_SRC:STRING=3
  QT_QMAKE_EXECUTABLE:STRING=/usr/local/Qt/4.8.0/bin/qmake
  CDAT_PACKAGE_CACHE_DIR:PATH=${CTEST_BINARY_DIRECTORY}/../cdat_dependencies
  PIP_CERTIFICATE:STRING=/export/doutriaux1/ca.llnl.gov.pem.cer
")
