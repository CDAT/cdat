
set(ffi_source "${CMAKE_CURRENT_BINARY_DIR}/build/ffi-${FFI_MAJOR}.${FFI_MINOR}.${FFI_PATCH}")
set(ffi_install "${cdat_EXTERNALS}")

ExternalProject_Add(FFI
  DOWNLOAD_DIR ${CDAT_PACKAGE_CACHE_DIR}
  SOURCE_DIR ${ffi_source}
  INSTALL_DIR ${ffi_install}
  URL ${FFI_URL}/${FFI_BZ2}
  URL_MD5 ${FFI_MD5}
  BUILD_IN_SOURCE 1
  PATCH_COMMAND ""
  CONFIGURE_COMMAND ${CMAKE_COMMAND} -DINSTALL_DIR=<INSTALL_DIR> -DWORKING_DIR=<SOURCE_DIR> -P ${cdat_CMAKE_BINARY_DIR}/cdat_configure_step.cmake
  DEPENDS ${FFI_deps}
  ${ep_log_options}
)
