##############################################################################
# 
# sciSquishTestScript.cmake: Commands to be executed by Squish ctests
#
# $Id: sciSquishTestScript.cmake 1220 2012-01-17 23:40:28Z jdelamere $
#
# This script launches a GUI test using Squish.  You should not call
# the script directly; instead, you should access it via the
# macro that is defined in sciSquishMacros.cmake
#
##############################################################################

set(ENV{SQUISH_LIBQTDIR} ${Squish_LIBQTDIR})
set(ENV{SQUISH_USER_SETTINGS_DIR} ${Squish_USER_SETTINGS_DIR})
set(ENV{SQUISH_LICENSEKEY_DIR} ${Squish_LICENSEDIR})

if (WIN32)
  set(Bash_START "C:/cygwin/bin/bash.exe")
  set(Bash_ARG "--login")
else ()
  set(Bash_START "")
  set(Bash_ARG "")
endif ()

execute_process(
  COMMAND ${Bash_START} ${Bash_ARG} ${Squish_SCRIPT} ${Squish_SERVER_EXECUTABLE} ${Squish_CLIENT_EXECUTABLE} ${Squish_AUT} ${Squish_AUTPATH} ${Ctests_TESTSUITE} ${Ctests_TEST} ${Ctests_LOGFILE}   
  RESULT_VARIABLE ERRTOT
)

# check for an error with running the test
if(NOT "${ERRTOT}" STREQUAL "0")
  message(FATAL_ERROR "Squish Errors: ${ERRTOT}")
endif(NOT "${ERRTOT}" STREQUAL "0")



