set(cdat_VERSION_MAJOR 6)
set(cdat_VERSION_MINOR 1)
set(cdat_VERSION_PATCH 0)
set(cdat_VERSION ${cdat_VERSION_MAJOR}.${cdat_VERSION_MINOR}.${cdat_VERSION_PATCH})

# configure version file
configure_file(${cdat_CMAKE_SOURCE_DIR}/cdat_modules_extra/version.in
  ${cdat_BINARY_DIR}/version
  @ONLY
)

add_cdat_package(CDAT "" "" "")

