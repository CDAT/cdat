################################################################################
### SpanLib
### Raynaud 2006
################################################################################
# Configure footer
################################################################################
dnl AC_DEFUN([AC_SR_SPANLIB_FOOTER],
dnl [
dnl AC_SR_SET_GREENINK
dnl echo "################################################################################"
dnl echo -e "Now type '"AS_VAR_GET([NORMAL])"make"AS_VAR_GET([GREEN])"' to build the library.\nThen, to install it, type '"AS_VAR_GET([NORMAL])"make install"AS_VAR_GET([GREEN])"'.\nyou may need to do it as "AS_VAR_GET([NORMAL])"root"AS_VAR_GET([GREEN])" if you didnt use "AS_VAR_GET([NORMAL])"--prefix"AS_VAR_GET([GREEN])"."
dnl AS_VAR_SET_IF(F90_EXAMPLE_TEXT,echo -e "AS_VAR_GET(F90_EXAMPLE_TEXT)")
dnl AS_VAR_SET_IF(PYTHON_EXAMPLE_TEXT,echo -e "AS_VAR_GET(PYTHON_EXAMPLE_TEXT)")
dnl echo "################################################################################"
dnl AC_SR_SET_NORMALINK
dnl ])

AC_DEFUN([AC_SR_SPANLIB_FOOTER],
[
echo "################################################################################"
echo -e "Now type 'make' to build the library.\nThen, to install it, type 'make install'.\nyou may need to do it as root if you didnt use --prefix."
AS_VAR_SET_IF(F90_EXAMPLE_TEXT,echo -e "AS_VAR_GET(F90_EXAMPLE_TEXT)")
AS_VAR_SET_IF(PYTHON_EXAMPLE_TEXT,echo -e "AS_VAR_GET(PYTHON_EXAMPLE_TEXT)")
echo "################################################################################"
])


