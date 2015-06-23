set(CDAT_source "${cdat_SOURCE_DIR}")

set(RUNTIME_FLAGS ${cdat_EXTERNALS}/lib)
set(LDFLAGS -L${cdat_EXTERNALS}/lib)

if (CDAT_BUILD_WITH_LIBDRS)
 set(cdat_xtra_flags "${cdat_xtra_flags} -c pcmdi.py")
endif()

set(cdat_build_dir ${CMAKE_CURRENT_BINARY_DIR}/cdat-build)

set(WORKING_DIR "${cdat_CMAKE_BINARY_DIR}")
configure_file(${cdat_CMAKE_SOURCE_DIR}/cdat_modules_extra/cdat_python_install_step.cmake.in
  ${cdat_CMAKE_BINARY_DIR}/cdat_python_install_step.cmake
  @ONLY)

ExternalProject_Add(CDAT
  DOWNLOAD_DIR ""
  SOURCE_DIR ${cdat_SOURCE_DIR}
  BINARY_DIR ${cdat_build_dir}
  BUILD_IN_SOURCE 0
  PATCH_COMMAND ""
  CONFIGURE_COMMAND ""
  BUILD_COMMAND ""
  INSTALL_COMMAND env "PYTHONPATH=$ENV{PYTHONPATH}" ${CMAKE_COMMAND} -DPYTHON_INSTALL_ARGS=${cdat_xtra_flags} -P ${cdat_CMAKE_BINARY_DIR}/cdat_python_install_step.cmake
  DEPENDS ${CDAT_deps}
  ${ep_log_options}
)
