option(CDAT_USE_SYSTEM_OPENSSL "Use system OpenSSL, if found." ON)
mark_as_advanced(CDAT_USE_SYSTEM_OPENSSL)
if(CDAT_USE_SYSTEM_OPENSSL)
  find_package(OpenSSL QUIET)
  if(OPENSSL_FOUND)
    get_filename_component(OPENSSL_LIBRARY_DIR
      "${OPENSSL_SSL_LIBRARY}" DIRECTORY)
    message(STATUS "System OpenSSL found. "
      "OpenSSL library directory: ${OPENSSL_LIBRARY_DIR}. "
      "OpenSSL Version: ${OPENSSL_VERSION}")
  endif(OPENSSL_FOUND)
endif(CDAT_USE_SYSTEM_OPENSSL)

if(NOT CDAT_USE_SYSTEM_OPENSSL OR NOT OPENSSL_FOUND OR NOT OPENSSL_INCLUDE_DIR)
  set(OPENSSL_MAJOR_SRC 1)
  set(OPENSSL_MINOR_SRC 0)
  set(OPENSSL_PATCH_SRC 2e)
  set(OPENSSL_VERSION
    ${OPENSSL_MAJOR_SRC}.${OPENSSL_MINOR_SRC}.${OPENSSL_PATCH_SRC})
  
  message(STATUS "Compiling OpenSSL from source. Version: ${OPENSSL_VERSION}")

  set(OPENSSL_URL ${LLNL_URL})
  set(OPENSSL_GZ "openssl-${OPENSSL_VERSION}.tar.gz")
  set(OPENSSL_MD5 5262bfa25b60ed9de9f28d5d52d77fc5)
  set(OPENSSL_SOURCE_URL ${OPENSSL_URL}/${OPENSSL_GZ})
  
  # We've reached here because we need OpenSSL.
  # Hence, defaulting to ON
  add_cdat_package(openssl "" "" ON)
endif(NOT CDAT_USE_SYSTEM_OPENSSL OR NOT OPENSSL_FOUND OR NOT OPENSSL_INCLUDE_DIR)
