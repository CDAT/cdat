set(cdat_VERSION_MAJOR 6)
set(cdat_VERSION_MINOR 1)
set(cdat_VERSION_PATCH 0)
set(cdat_VERSION ${cdat_VERSION_MAJOR}.${cdat_VERSION_MINOR}.${cdat_VERSION_PATCH})

# configure version file
configure_file(${cdat_CMAKE_SOURCE_DIR}/version.in
  ${cdat_SOURCE_DIR}/version
  @ONLY
)

# Cleans all CDAT builds
execute_process(
  COMMAND ./scripts/clean_script all
  WORKING_DIRECTORY ${cdat_SOURCE_DIR}
)

add_cdat_package(CDAT "" "" "")

