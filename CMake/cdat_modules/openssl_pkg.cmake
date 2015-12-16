find_package (OpenSSL QUIET)
if (OPENSSL_FOUND)
  message(STATUS "System OpenSSL found. OpenSSL Version: ${OPENSSL_VERSION}")
  get_filename_component (OPENSSL_LIBRARY_DIR
    "${OPENSSL_SSL_LIBRARY}" DIRECTORY)
else (OPENSSL_FOUND)

  set(OPENSSL_MAJOR_SRC 1)
  set(OPENSSL_MINOR_SRC 0)
  set(OPENSSL_PATCH_SRC 2e)
  set(OPENSSL_VERSION
    ${OPENSSL_MAJOR_SRC}.${OPENSSL_MINOR_SRC}.${OPENSSL_PATCH_SRC})
  
  message(STATUS "System OpenSSL not found. "
  "Compiling from source. Version: ${OPENSSL_VERSION}")

  set(OPENSSL_URL "https://www.openssl.org/source")
  set(OPENSSL_GZ "openssl-${OPENSSL_VERSION}.tar.gz")
  set(OPENSSL_MD5 5262bfa25b60ed9de9f28d5d52d77fc5)
  set(OPENSSL_SOURCE_URL ${OPENSSL_URL}/${OPENSSL_GZ})
  
  # We've reached here because we need OpenSSL.
  # Hence, defaulting to ON
  add_cdat_package(openssl "" "" ON)
endif (OPENSSL_FOUND)
