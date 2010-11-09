set(name "libxml2")
set(version "2.7.4")
set(latest 1)
set(release 1)
set(prefix ${CSE_RELEASE_INSTALL_DIR}/${name}-${version})


# libxml2
#
set(proj "${name}-${version}")
set(tgz "${CMAKE_CURRENT_SOURCE_DIR}/${name}-${version}.tar.gz")
include("${CMAKE_CURRENT_SOURCE_DIR}/../../../CMakeModules/cse_macros.cmake")

ExternalProject_Add(${proj}
  URL ${tgz}
  CONFIGURE_COMMAND <SOURCE_DIR>/configure
                    --prefix=${prefix}
                    --with-python=${CSE_PYTHON_HOME}
  BUILD_COMMAND ${build_cmd_smp}
  DEPENDS ${CSE_PYTHON_PACKAGE} 
)


cse_module_install(
  "${name}" "${version}"
  "${CSE_MODRELTOOLS_DIR}" ""
  ${release} ${latest}
)


  split_version(${version} version_major version_minor version_patch)
  set(CSE_LIBXML2_HOME "${prefix}" PARENT_SCOPE)
  set(CSE_LIBXML2_PACKAGE "${name}-${version}" PARENT_SCOPE)
  set(CSE_LIBXML2_NAME "${name}" PARENT_SCOPE)
  set(CSE_LIBXML2_VERSIONXX "${version_major}.${version_minor}" PARENT_SCOPE)

add_test(SmokeTest-${name}-${version}
  "${prefix}/bin/xml2-config" "--version")
