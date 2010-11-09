set(name "tcltk")
set(version "8.4.19")
string(REGEX MATCH "^[0-9]+[.][0-9]+" version_short ${version})
set(latest 1)
set(release 1)
set(prefix ${CSE_RELEASE_INSTALL_DIR}/${name}-${version})


# tcl
#
set(proj "tcl-${version}")
set(tgz "${CMAKE_CURRENT_SOURCE_DIR}/tcl${version}-src.tar.gz")
include("${CMAKE_CURRENT_SOURCE_DIR}/../../../CMakeModules/cse_macros.cmake")
split_version(${version} version_major version_minor version_patch)

ExternalProject_Add(${proj}
  URL ${tgz}
  INSTALL_DIR ${prefix}
  CONFIGURE_COMMAND <SOURCE_DIR>/unix/configure --prefix=<INSTALL_DIR>
  BUILD_COMMAND ${build_cmd_smp}
)

set(CSE_TCL_NAME "tcl" PARENT_SCOPE)
set(CSE_TCL_VERSIONXX "${version_major}.${version_minor}" PARENT_SCOPE)

# tk
#
set(proj "tk-${version}")
set(tgz "${CMAKE_CURRENT_SOURCE_DIR}/tk${version}-src.tar.gz")

ExternalProject_Add(${proj}
  URL ${tgz}
  INSTALL_DIR ${prefix}
  CONFIGURE_COMMAND <SOURCE_DIR>/unix/configure --prefix=<INSTALL_DIR>
  BUILD_COMMAND ${build_cmd_smp}
  DEPENDS tcl-${version}
)

ExternalProject_Add_Step(${proj} symlink
  COMMAND ${CMAKE_COMMAND} -E create_symlink "wish${version_short}" wish
  WORKING_DIRECTORY ${prefix}/bin
  COMMENT "Linking wish${version_short} to wish"
  DEPENDEES install
)

set(CSE_TK_NAME "tk" PARENT_SCOPE)
set(CSE_TK_VERSIONXX "${version_major}.${version_minor}" PARENT_SCOPE)

# tcltk
#
set(proj "${name}-${version}")

ExternalProject_Add(${proj}
  DOWNLOAD_COMMAND ""
  CONFIGURE_COMMAND ""
  BUILD_COMMAND ""
  INSTALL_COMMAND ""
  DEPENDS tk-${version}
)

# other projects need to know where tcltk gets installed:
#
set(CSE_TCLTK_HOME ${prefix} PARENT_SCOPE)
set(CSE_TCLTK_PACKAGE "${name}-${version}" PARENT_SCOPE)
set(CSE_TCLTK_NAME "${name}" PARENT_SCOPE)
set(CSE_TCLTK_VERSIONXX "${version_major}.${version_minor}" PARENT_SCOPE)

set(extras
  "setenv TCLTK_INCLUDE_DIR  $prefix/include"
  "setenv TCLTK_LIB_DIR $prefix/lib"
  "setenv TCL_LIBRARY $prefix/lib/tcl8.4"
)

cse_module_install(
  "${name}" "${version}"
  "${CSE_MODRELTOOLS_DIR}" "${extras}"
  ${release} ${latest}
)

# tests
#
add_test(SmokeTest-${name}-${version}-tclsh8.4
  "${prefix}/bin/tclsh8.4" "${CSE_SOURCE_DIR}/Tests/exit.tcl")
