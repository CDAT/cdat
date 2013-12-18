
set(pixman_source "${CMAKE_CURRENT_BINARY_DIR}/build/pixman")
set(pixman_install "${cdat_EXTERNALS}")
set(pixman_configure_args "--disable-gtk")

ExternalProject_Add(pixman
  LIST_SEPARATOR ^^
  DOWNLOAD_DIR ${CDAT_PACKAGE_CACHE_DIR}
  SOURCE_DIR ${pixman_source}
  INSTALL_DIR ${pixman_install}
  URL ${PIX_URL}/${PIX_GZ}
  URL_MD5 ${PIX_MD5}
  BUILD_IN_SOURCE 1
  PATCH_COMMAND ""
  CONFIGURE_COMMAND ${CMAKE_COMMAND} -DINSTALL_DIR=${pixman_install} -DWORKING_DIR=${pixman_source} -DCONFIGURE_ARGS=${pixman_configure_args} -P ${cdat_CMAKE_BINARY_DIR}/cdat_configure_step.cmake
  DEPENDS ${pixman_deps}
  ${ep_log_options}
)

set(pixman_DIR "${pixman_binary}" CACHE PATH "pixman binary directory" FORCE)
mark_as_advanced(pixman_DIR)
