
# create an external project to install MyProxyClient,
# and configure and build it
# --old-and-unmangeable solution avoids the use of eggs
# and  forces to create a directory.
# this seems to fix issues of the type encountered in 
# bug #1192 and #1486

set(nm DATEUTILS)
set(USER_INSTALL_OPTIONS --old-and-unmanageable)
include(pipinstaller)
unset(USER_INSTALL_OPTIONS)
