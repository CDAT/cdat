set(ezget_source "${CMAKE_CURRENT_BINARY_DIR}/build/ezget")
set(ezget_install "${cdat_EXTERNALS}")
if (APPLE)
    set(ezget_make_file ezget_Makefile.Mac.gfortran.in)
else ()
    set(ezget_make_file ezget_Makefile.Linux.gfortran.in)
endif ()

configure_file(
    ${cdat_CMAKE_SOURCE_DIR}/cdat_modules_extra/${ezget_make_file}
    ${CMAKE_CURRENT_BINARY_DIR}/CMake/ezget_Makefile
    )

if(DEFINED GIT_CMD_STR_EZGET )
    message("[INFO] [ezget] Installing ${nm} from ${GIT_CMD_STR_EZGET}")
    include(GetGitRevisionDescription)
    set(URL_STR )
    set(URL_MD5_STR )
else()
    message("[INFO] [ezget] Installed ${nm} from tarball ${EZGET_GZ}")
    set(URL_STR URL ${EZGET_URL}/${EZGET_GZ})
    set(URL_MD5_STR URL_MD5 ${EZGET_MD5})
    set(GIT_CMD_STR_EZGET )
    set(GIT_TAG )
endif()
set(EZGET_MAKE_ARGS -f^^${CMAKE_CURRENT_BINARY_DIR}/CMake/ezget_Makefile)
set(EZGET_MAKE_INSTALL_ARGS -f^^${CMAKE_CURRENT_BINARY_DIR}/CMake/ezget_Makefile^^install)
set(EZGET_BUILD_ARGS -fPIC)

ExternalProject_Add(ezget
  LIST_SEPARATOR ^^
  DOWNLOAD_DIR ${CDAT_PACKAGE_CACHE_DIR}
  SOURCE_DIR ${ezget_source}
  INSTALL_DIR ${ezget_install}
  ${URL_STR}
  ${URL_MD5_STR}
  ${GIT_CMD_STR_EZGET}
  ${GIT_TAG}
  BUILD_IN_SOURCE 1
  CONFIGURE_COMMAND ""
  BUILD_COMMAND ${CMAKE_COMMAND} -DADDITIONAL_CFLAGS=${EZGET_BUILD_ARGS} -Dmake=$(MAKE) -DBUILD_ARGS=${EZGET_MAKE_ARGS} -DWORKING_DIR=<SOURCE_DIR> -P ${cdat_CMAKE_BINARY_DIR}/cdat_make_step.cmake
  INSTALL_COMMAND ${CMAKE_COMMAND} -DADDITIONAL_CFLAGS=${EZGET_BUILD_ARGS} -Dmake=$(MAKE) -DBUILD_ARGS=${EZGET_MAKE_INSTALL_ARGS} -DWORKING_DIR=<SOURCE_DIR> -P ${cdat_CMAKE_BINARY_DIR}/cdat_make_step.cmake
  DEPENDS ${ezget_deps}
  ${ep_log_options}
)
if (DEFINED GIT_CMD_STR_EZGET)
  unset(GIT_CMD_STR_EZGET)
endif()
