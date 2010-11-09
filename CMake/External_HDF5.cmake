set(name "hdf5")
set(version "1.8.5")
set(latest 1)
set(release 1)
set(prefix ${CSE_RELEASE_INSTALL_DIR}/${name}-${version})


# Hdf5
#
set(proj "${name}-${version}")
set(tgz "${CMAKE_CURRENT_SOURCE_DIR}/${proj}-patch1.tar.gz")
set(cfg "${CMAKE_CURRENT_SOURCE_DIR}/configure.cmake")
include("${CMAKE_CURRENT_SOURCE_DIR}/../../../../CMakeModules/cse_macros.cmake")

ExternalProject_Add(${proj}
  URL ${tgz}
  CONFIGURE_COMMAND ${CMAKE_COMMAND}
                    -Dsrc=<SOURCE_DIR>
                    -Dprefix=${prefix}
                    -DCFLAGS=-fno-strict-aliasing
                    -P ${CMAKE_CURRENT_SOURCE_DIR}/configure.cmake
  BUILD_COMMAND ${build_cmd_smp}
  DEPENDS patch-2.5.4
)

ExternalProject_Add_Step(${proj} "check-configure"
   COMMENT "Check ${cfg}"
   DEPENDERS configure
   DEPENDS ${cfg}
) 

set(extras
   "setenv HDF5_INCLUDE_DIR /usr/cta/CSE/packages/hdf5-1.8.5/include"
   "prepend-path CMAKE_INCLUDES /usr/cta/CSE/packages/hdf5-1.8.5/include"
   "setenv          CSE_HDF5_INCLUDE_DIR  /usr/cta/CSE/packages/hdf5-1.8.5/include"
   "setenv          CSE_HDF5_LIB_DIR  /usr/cta/CSE/packages/hdf5-1.8.5/lib"
)

set(CSE_HDF5185_HOME ${prefix} PARENT_SCOPE)
split_version(${version} version_major version_minor version_patch)
set(CSE_HDF5185_PACKAGE "${name}-${version}" PARENT_SCOPE)
set(CSE_HDF5185_NAME "${name}" PARENT_SCOPE)
set(CSE_HDF5185_VERSIONXX "${version_major}.${version_minor}" PARENT_SCOPE)

cse_module_install(
  "${name}" "${version}"
  "${CSE_MODRELTOOLS_DIR}" "${extras}"
  ${release} ${latest}
)

add_test(SmokeTest-${name}-${version}
  "${prefix}/bin/h5ls" "--version")
