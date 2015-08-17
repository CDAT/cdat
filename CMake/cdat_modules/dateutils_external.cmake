
# create an external project to install MyProxyClient,
# and configure and build it
set(nm DATEUTILS)
set(USER_INSTALL_OPTIONS --old-and-unmanageable)
include(pipinstaller)
unset(USER_INSTALL_OPTIONS)
