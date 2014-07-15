set(cdat_VERSION_MAJOR 1)
set(cdat_VERSION_MINOR 5)
set(cdat_VERSION_PATCH 0)
set(cdat_VERSION ${cdat_VERSION_MAJOR}.${cdat_VERSION_MINOR}.${cdat_VERSION_PATCH})
option(CDAT_BUILD_VCS_LEGACY "Build the legacy (xgks/cairo/qt based vcs in addition to the newer VTK-based vcs" OFF)

execute_process(
    COMMAND ${GIT_EXECUTABLE} describe --tags
    WORKING_DIRECTORY ${cdat_SOURCE_DIR}
    RESULT_VARIABLE res
    OUTPUT_VARIABLE ver
    )

if (NOT ${res} EQUAL 0) 
    set(cdat_VERSION ${cdat_VERSION_MAJOR}.${cdat_VERSION_MINOR}.${cdat_VERSION_PATCH})
    message("Couldn't get version from git setting from values in cdat_pkg.cmake ${cdat_VERSION}")
else ()
    STRING(REGEX REPLACE "(\r?\n)+$" "" ver "${ver}")
    set(cdat_VERSION ${ver})
endif()

set(CDAT_SOURCE N/A)

# configure version file
set (nm cdat_VERSION)
string(TOUPPER ${nm} uc_nm)
set(${uc_nm}_VERSION ${cdat_VERSION})
set(CDAT_VERSION ${CDAT_VERSION_VERSION})

configure_file(${cdat_CMAKE_SOURCE_DIR}/cdat_modules_extra/version.in
  ${cdat_BINARY_DIR}/version
  @ONLY
)

add_cdat_package(CDAT "" "" "")

