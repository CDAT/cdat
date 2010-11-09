set(name "sip")
set(version "4.9.1")
set(latest 1)
set(release 1)
set(prefix ${CSE_RELEASE_INSTALL_DIR}/${name}-${version})


# SIP
set(proj "${name}-${version}")
set(tgz "${CMAKE_CURRENT_SOURCE_DIR}/${name}-${version}.tar.gz")
include("${CMAKE_CURRENT_SOURCE_DIR}/../../../../CMakeModules/cse_macros.cmake")

ExternalProject_Add(${proj}
  URL ${tgz}
  BUILD_IN_SOURCE 1
  CONFIGURE_COMMAND ${CSE_PYTHON_HOME}/bin/python configure.py
    -b ${prefix}/bin
    -d ${prefix}/lib/${CSE_PYTHON_NAME}${CSE_PYTHON_VERSIONXX}/site-packages
    -e ${prefix}/include
    -v ${prefix}/share
    CC=${CMAKE_C_COMPILER}
    CXX=${CMAKE_CXX_COMPILER}
  BUILD_COMMAND ${build_cmd_smp}
  DEPENDS ${CSE_PYTHON_PACKAGE}
  )

set(CSE_SIP_HOME ${prefix} PARENT_SCOPE)
split_version(${version} version_major version_minor version_patch)
set(CSE_SIP_PACKAGE "${name}-${version}" PARENT_SCOPE)
set(CSE_SIP_NAME "${name}" PARENT_SCOPE)
set(CSE_SIP_VERSIONXX "${version_major}.${version_minor}" PARENT_SCOPE)

add_test(SmokeTest-${name}-${version}
  "${prefix}/bin/sip" "-V")

