set(name "numpy")
set(version "1.3.0")
set(latest 1)
set(release 1)
set(prefix ${CSE_RELEASE_INSTALL_DIR}/${name}-${version})


# numpy
#
set(proj "${name}-${version}")
set(tgz "${CMAKE_CURRENT_SOURCE_DIR}/${name}-${version}.tar.gz")
include("${CMAKE_CURRENT_SOURCE_DIR}/../../../../CMakeModules/cse_macros.cmake")

ExternalProject_Add(${proj}
  URL ${tgz}
  CONFIGURE_COMMAND ""
  BUILD_COMMAND ""
  INSTALL_COMMAND ${CSE_PYTHON_HOME}/bin/python <SOURCE_DIR>/setup.py install
    --prefix=${prefix}
  DEPENDS patch-2.5.4
          ${CSE_PYTHON_PACKAGE}
)

cse_add_runtime_depends(${proj}
  ${CSE_PYTHON_PACKAGE}
)

set(extras
  "prepend-path    PYTHONPATH      $prefix/lib/${CSE_PYTHON_NAME}${CSE_PYTHON_VERSIONXX}/site-packages"
)

cse_module_install(
  "${name}" "${version}"
  "${CSE_MODRELTOOLS_DIR}" "${extras}"
  ${release} ${latest}
)

set(CSE_NUMPY_HOME ${prefix} PARENT_SCOPE)
split_version(${version} version_major version_minor version_patch)
set(CSE_NUMPY_HOME "${prefix}" PARENT_SCOPE)
set(CSE_NUMPY_PACKAGE "${name}-${version}" PARENT_SCOPE)
set(CSE_NUMPY_NAME "${name}" PARENT_SCOPE)
set(CSE_NUMPY_VERSIONXX "${version_major}.${version_minor}" PARENT_SCOPE)

add_test(SmokeTest-${name}-${version}
  "${CSE_PYTHON_HOME}/bin/python"
  "${prefix}/lib/${CSE_PYTHON_NAME}${CSE_PYTHON_VERSIONXX}/site-packages/numpy/testing/numpytest.py")

set_property(TEST SmokeTest-${name}-${version} PROPERTY
  ENVIRONMENT LD_LIBRARY_PATH=${prefix}/lib:${CSE_PYTHON_HOME}/lib
              PYTHONPATH=${prefix}/lib/${CSE_PYTHON_NAME}${CSE_PYTHON_VERSIONXX}/site-packages
  )
