# The CLAPACK external project

set(clapack_source "${CMAKE_CURRENT_BINARY_DIR}/LAPACK")
set(clapack_binary "${CMAKE_CURRENT_BINARY_DIR}/LAPACK-build")
set(clapack_install "${cdat_EXTERNALS}")
set(NUMPY_LAPACK_binary ${clapack_binary})

#
# To fix compilation problem: relocation R_X86_64_32 against `a local symbol' can not be
# used when making a shared object; recompile with -fPIC
# See http://www.cmake.org/pipermail/cmake/2007-May/014350.html
#
if(UNIX AND CMAKE_SYSTEM_PROCESSOR STREQUAL "x86_64")
  set(cdat_tpl_c_flags_LAPACK "-fPIC ${cdat_tpl_c_flags}")
endif()

configure_file(${cdat_CMAKE_SOURCE_DIR}/cdat_modules_extra/CLAPACK_install_step.cmake.in
    ${cdat_CMAKE_BINARY_DIR}/CLAPACK_install_step.cmake
    @ONLY)

set(CLAPACK_INSTALL_COMMAND ${CMAKE_COMMAND} -P ${cdat_CMAKE_BINARY_DIR}/CLAPACK_install_step.cmake)

ExternalProject_Add(CLAPACK
  DOWNLOAD_DIR ${CDAT_PACKAGE_CACHE_DIR}
  SOURCE_DIR ${clapack_source}
  BINARY_DIR ${clapack_binary}
  INSTALL_DIR ${clapack_install}
  URL ${CLAPACK_URL}/${CLAPACK_GZ}
  URL_MD5 ${CLAPACK_MD5}
  CMAKE_CACHE_ARGS
    -DCMAKE_CXX_FLAGS:STRING=${cdat_tpl_cxx_flags}
    -DCMAKE_C_FLAGS:STRING=${cdat_tpl_c_flags}
    -DBUILD_SHARED_LIBS:BOOL=ON
    -DCMAKE_BUILD_TYPE:STRING=${CMAKE_BUILD_TYPE}
    ${CLAPACK_EXTRA_ARGS}
  INSTALL_COMMAND ${CLAPACK_INSTALL_COMMAND}
  DEPENDS ${CLAPACK_deps}
  ${ep_log_options}
  )
