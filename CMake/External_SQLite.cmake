set(name "sqlite")
set(version "3.6.22")
set(latest 1)
set(release 1)
set(prefix ${CSE_RELEASE_INSTALL_DIR}/${name}-${version})


# OpenMPI-3.6.22
#
set(proj "${name}-${version}")
set(tgz  "${CMAKE_CURRENT_SOURCE_DIR}/${name}-${version}.tar.gz")
include("${CMAKE_CURRENT_SOURCE_DIR}/../../../CMakeModules/cse_macros.cmake")

ExternalProject_Add(${proj}
  URL ${tgz}
  CONFIGURE_COMMAND <SOURCE_DIR>/configure --prefix=${prefix} --enable-mpi-threads
  BUILD_COMMAND ${build_cmd_smp} 
)

  cse_module_install(
    "${name}" "${version}"
    "${CSE_MODRELTOOLS_DIR}" "${extras}"
    ${release} ${latest}
  )

set(CSE_SQLITE_HOME "${prefix}" PARENT_SCOPE)
split_version(${version} version_major version_minor version_patch)
set(CSE_SQLITE_PACKAGE "${name}-${version}" PARENT_SCOPE)
set(CSE_SQLITE_NAME "${name}" PARENT_SCOPE)
set(CSE_SQLITE_VERSIONXX "${version_major}.${version_minor}" PARENT_SCOPE)
