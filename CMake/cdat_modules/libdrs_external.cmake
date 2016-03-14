set(libdrs_source "${CMAKE_CURRENT_BINARY_DIR}/build/libdrs")
set(libdrs_install "${cdat_EXTERNALS}")

if (APPLE)
    set(libdrs_make_file libdrs_Makefile.Mac.gfortran.in)
else ()
    set(libdrs_make_file libdrs_Makefile.Linux.gfortran.in)
endif ()

configure_file(
    ${cdat_CMAKE_SOURCE_DIR}/cdat_modules_extra/${libdrs_make_file}
    ${CMAKE_CURRENT_BINARY_DIR}/CMake/libdrs_Makefile
    )

if(DEFINED GIT_CMD_STR_LIBDRS )
    message("[INFO] [libdrs] Installing ${nm} from ${GIT_CMD_STR_LIBDRS}")
    include(GetGitRevisionDescription)
    set(URL_STR )
    set(URL_MD5_STR )
else()
    message("[INFO] [libdrs] Installed ${nm} from tarball ${LIBDRS_GZ}")
    set(URL_STR URL ${LIBDRS_URL}/${LIBDRS_GZ})
    set(URL_MD5_STR URL_MD5 ${LIBDRS_MD5})
    set(GIT_CMD_STR_LIBDRS )
    set(GIT_TAG )
endif()
set(LIBDRS_MAKE_ARGS -f^^${CMAKE_CURRENT_BINARY_DIR}/CMake/libdrs_Makefile)
set(LIBDRS_MAKE_INSTALL_ARGS -f^^${CMAKE_CURRENT_BINARY_DIR}/CMake/libdrs_Makefile^^install)
set(LIBDRS_BUILD_ARGS -fPIC)

ExternalProject_Add(libdrs
  LIST_SEPARATOR ^^
  DOWNLOAD_DIR ${CDAT_PACKAGE_CACHE_DIR}
  SOURCE_DIR ${libdrs_source}
  INSTALL_DIR ${libdrs_install}
  ${URL_STR}
  ${URL_MD5_STR}
  ${GIT_CMD_STR_LIBDRS}
  ${GIT_TAG}
  BUILD_IN_SOURCE 1
  CONFIGURE_COMMAND ""
  BUILD_COMMAND ${CMAKE_COMMAND} -DADDITIONAL_CFLAGS=${LIBDRS_BUILD_ARGS} -Dmake=$(MAKE) -DBUILD_ARGS=${LIBDRS_MAKE_ARGS} -DWORKING_DIR=<SOURCE_DIR>/lib -P ${cdat_CMAKE_BINARY_DIR}/cdat_make_step.cmake
  INSTALL_COMMAND ${CMAKE_COMMAND} -DADDITIONAL_CFLAGS=${LIBDRS_BUILD_ARGS} -Dmake=$(MAKE) -DBUILD_ARGS=${LIBDRS_MAKE_INSTALL_ARGS} -DWORKING_DIR=<SOURCE_DIR>/lib -P ${cdat_CMAKE_BINARY_DIR}/cdat_make_step.cmake
  DEPENDS ${libdrs_deps}
  ${ep_log_options}
)
if (DEFINED GIT_CMD_STR_LIBDRS)
  unset(GIT_CMD_STR_LIBDRS)
endif()
