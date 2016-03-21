
set(lats_source "${CMAKE_CURRENT_BINARY_DIR}/build/lats")
set(lats_install "${cdat_EXTERNALS}")

configure_file(
    ${cdat_CMAKE_SOURCE_DIR}/cdat_modules_extra/lats_Makefile.gfortran.in
    ${CMAKE_CURRENT_BINARY_DIR}/CMake/lats_Makefile
    )

if(DEFINED GIT_CMD_STR_LATS )
    message("[INFO] [lats] Installing ${nm} from ${GIT_CMD_STR_LATS}")
    include(GetGitRevisionDescription)
    set(URL_STR )
    set(URL_MD5_STR )
else()
    message("[INFO] [lats] Installed ${nm} from tarball ${LATS_GZ}")
    set(URL_STR URL ${LATS_URL}/${LATS_GZ})
    set(URL_MD5_STR URL_MD5 ${LATS_MD5})
    set(GIT_CMD_STR_LATS )
    set(GIT_TAG )
endif()
set(LATS_MAKE_ARGS -f^^${CMAKE_CURRENT_BINARY_DIR}/CMake/lats_Makefile)
set(LATS_MAKE_INSTALL_ARGS -f^^${CMAKE_CURRENT_BINARY_DIR}/CMake/lats_Makefile^^install)
set(LATS_BUILD_ARGS -fPIC)

ExternalProject_Add(lats
  LIST_SEPARATOR ^^
  DOWNLOAD_DIR ${CDAT_PACKAGE_CACHE_DIR}
  SOURCE_DIR ${lats_source}
  INSTALL_DIR ${lats_install}
  ${URL_STR}
  ${URL_MD5_STR}
  ${GIT_CMD_STR_LATS}
  ${GIT_TAG}
  BUILD_IN_SOURCE 1
  CONFIGURE_COMMAND ""
  BUILD_COMMAND ${CMAKE_COMMAND} -DADDITIONAL_CFLAGS=${LATS_BUILD_ARGS} -Dmake=$(MAKE) -DBUILD_ARGS=${LATS_MAKE_ARGS} -DWORKING_DIR=<SOURCE_DIR> -P ${cdat_CMAKE_BINARY_DIR}/cdat_make_step.cmake
  INSTALL_COMMAND ${CMAKE_COMMAND} -DADDITIONAL_CFLAGS=${LATS_BUILD_ARGS} -Dmake=$(MAKE) -DBUILD_ARGS=${LATS_MAKE_INSTALL_ARGS} -DWORKING_DIR=<SOURCE_DIR> -P ${cdat_CMAKE_BINARY_DIR}/cdat_make_step.cmake
  DEPENDS ${lats_deps}
  ${ep_log_options}
)
if (DEFINED GIT_CMD_STR_LATS)
  unset(GIT_CMD_STR_LATS)
endif()
