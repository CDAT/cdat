set(wget_source "${CMAKE_CURRENT_BINARY_DIR}/build/wget")
set(wget_install "${cdat_EXTERNALS}")

ExternalProject_Add(Wget
  DOWNLOAD_DIR ${CMAKE_CURRENT_BINARY_DIR}
  SOURCE_DIR ${wget_source}
  INSTALL_DIR ${wget_install}
  URL ${WGET_URL}/${WGET_GZ}
  URL_MD5 ${WGET_MD5}
  BUILD_IN_SOURCE 1
  PATCH_COMMAND ""
  CONFIGURE_COMMAND ${CMAKE_COMMAND} -DINSTALL_DIR=<INSTALL_DIR> -DWORKING_DIR=<SOURCE_DIR> -P ${cdat_CMAKE_BINARY_DIR}/cdat_configure_step.cmake
  DEPENDS ${wget_deps}
  ${ep_log_options}
)

if(NOT cdat_Wget_FOUND)
  if(WIN32)
    set(WGET_EXECUTABLE ${wget_install}/bin/wget.exe)
  else()
    set(WGET_EXECUTABLE ${wget_install}/bin/wget)
  endif()
endif()

set(HASWGET ${WGET_EXECUTABLE})

set(wget_DIR "${wget_binary}" CACHE PATH "wget binary directory" FORCE)
mark_as_advanced(wget_DIR)
