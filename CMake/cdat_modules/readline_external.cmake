set(readline_source "${CMAKE_CURRENT_BINARY_DIR}/build/readline")
set(readline_install "${cdat_EXTERNALS}")
set(readline_conf_args)

# with -fPIC
IF(UNIX AND NOT WIN32)
  FIND_PROGRAM(CMAKE_UNAME uname /bin /usr/bin /usr/local/bin )
  IF(CMAKE_UNAME)
    EXEC_PROGRAM(uname ARGS -m OUTPUT_VARIABLE CMAKE_SYSTEM_PROCESSOR)
    SET(CMAKE_SYSTEM_PROCESSOR ${CMAKE_SYSTEM_PROCESSOR} CACHE INTERNAL
"processor type (i386 and x86_64)")
    IF(CMAKE_SYSTEM_PROCESSOR MATCHES "x86_64")
      set(readline_conf_args "CFLAGS=-fPIC")
    ENDIF(CMAKE_SYSTEM_PROCESSOR MATCHES "x86_64")
  ENDIF(CMAKE_UNAME)
ENDIF(UNIX AND NOT WIN32)

ExternalProject_Add(readline
  DOWNLOAD_DIR ${CDAT_PACKAGE_CACHE_DIR}
  SOURCE_DIR ${readline_source}
  INSTALL_DIR ${readline_install}
  URL ${READLINE_URL}/${READLINE_GZ}
  URL_MD5 ${READLINE_MD5}
  BUILD_IN_SOURCE 1
  PATCH_COMMAND ${CMAKE_COMMAND} -E copy_if_different ${cdat_external_patch_dir}/src/readline/shobj-conf ${readline_source}/support/shobj-conf
  CONFIGURE_COMMAND ${CMAKE_COMMAND} -DCONFIGURE_ARGS=${readline_conf_args} -DINSTALL_DIR=<INSTALL_DIR> -DWORKING_DIR=<SOURCE_DIR> -P ${cdat_CMAKE_BINARY_DIR}/cdat_configure_step.cmake
  DEPENDS ${readline_deps}
  ${ep_log_options}
)

