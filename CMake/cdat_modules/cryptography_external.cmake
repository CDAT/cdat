
# create an external project to install MyProxyClient,
# and configure and build it
set(nm CRYPTOGRAPHY)

# Set LDFlags and CFlags to make it easier to find OpenSSL
list(APPEND USR_ENVS
  "LDFLAGS=-L${OPENSSL_LIBRARY_DIR} $ENV{LDFLAGS}"
  "CFLAGS=-I${OPENSSL_INCLUDE_DIR} $ENV{CFLAGS}"
  )

include(pipinstaller)
