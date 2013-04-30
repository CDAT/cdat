set(cdat_VERSION_MAJOR 1)
set(cdat_VERSION_MINOR 3)
set(cdat_VERSION_PATCH 1)
set(cdat_VERSION ${cdat_VERSION_MAJOR}.${cdat_VERSION_MINOR}.${cdat_VERSION_PATCH})

# configure version file
set (nm cdat_VERSION)
string(TOUPPER ${nm} uc_nm)
set(${uc_nm}_VERSION ${${nm}_MAJOR}.${${nm}_MINOR}.${${nm}_PATCH})
set(CDAT_VERSION ${CDAT_VERSION_VERSION})

configure_file(${cdat_CMAKE_SOURCE_DIR}/cdat_modules_extra/version.in
  ${cdat_BINARY_DIR}/version
  @ONLY
)

add_cdat_package(CDAT "" "" "")

