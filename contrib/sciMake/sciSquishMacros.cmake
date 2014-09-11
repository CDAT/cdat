######################################################################
#
# sciSquishMacros: Various functions and macros used by Tech-X CMake in
# adding Squish ctests.
#
# $Id: sciSquishMacros.cmake 1250 2012-02-03 20:16:19Z jdelamere $
# Copyright 2010-2012 Tech-X Corporation.
# Arbitrary redistribution allowed provided this copyright remains.
#
######################################################################
macro(sciSquishAddTest testName)
 
  if (LINUX)
    find_program(XVFB Xvfb)
    if (XVFB)
      set(CTEST_RUN_EXEC ${CMAKE_SOURCE_DIR}/CMake/xvfb-run.sh -a)
      message(STATUS "Found Xvfb as ${XVFB}.  Will run tests with ${CTEST_RUN_EXEC}.")
    else ()
      message(WARNING "Cannot find Xvfb.  Guaranteed failure of some tests.")
   endif ()
  endif ()

# Define what cmake test file to invoke
  set(Squish_CTESTFILE "${PROJECT_SOURCE_DIR}/CMake/sciSquishTestScript.cmake")
# Include variables from CPackConfig.cmake
  include(${CPACK_OUTPUT_CONFIG_FILE})
  set(Squish_LICENSEDIR ${Squish_DIR})
  if (APPLE)
    set(Squish_AUT "${APPLICATION_NAME}.sh")
    set(Squish_AUTPATH "${PROJECT_BINARY_DIR}/_CPack_Packages/${CPACK_TOPLEVEL_TAG}/${CPACK_GENERATOR}/${CPACK_PACKAGE_FILE_NAME}/${APPLICATION_NAME}.app/Contents/MacOS/")
    set(Squish_LIBQTDIR "${PROJECT_BINARY_DIR}/_CPack_Packages/${CPACK_TOPLEVEL_TAG}/${CPACK_GENERATOR}/${CPACK_PACKAGE_FILE_NAME}/${APPLICATION_NAME}.app/Contents/VisIt/current/darwin-x86_64/lib")
  elseif (WIN32)
    set(Squish_AUT "${APPLICATION_NAME}")
    set(Squish_AUTPATH "${PROJECT_BINARY_DIR}/_CPack_Packages/${CPACK_TOPLEVEL_TAG}/${CPACK_GENERATOR}/${CPACK_PACKAGE_FILE_NAME}/Contents/bin")
    set(Squish_LIBQTDIR "${PROJECT_BINARY_DIR}/_CPack_Packages/${CPACK_TOPLEVEL_TAG}/${CPACK_GENERATOR}/${CPACK_PACKAGE_FILE_NAME}/Contents/bin")
  else () #Linux
    set(Squish_AUT "${APPLICATION_NAME}.sh")
    set(Squish_AUTPATH "${PROJECT_BINARY_DIR}/_CPack_Packages/${CPACK_TOPLEVEL_TAG}/${CPACK_GENERATOR}/${CPACK_PACKAGE_FILE_NAME}")
    set(Squish_LIBQTDIR "${PROJECT_BINARY_DIR}/_CPack_Packages/${CPACK_TOPLEVEL_TAG}/${CPACK_GENERATOR}/${CPACK_PACKAGE_FILE_NAME}/Contents/VisIt/current/linux-x86_64/lib")
  endif ()
  add_test(${testName} 
     ${CTEST_RUN_EXEC}
     ${CMAKE_COMMAND} -V -VV
     "-DSquish_SCRIPT:STRING=${PROJECT_SOURCE_DIR}/CMake/sciSquishRunTestCase.sh"
     "-DSquish_LICENSEDIR:STRING=${Squish_LICENSEDIR}"
     "-DSquish_AUT:STRING=${Squish_AUT}"
     "-DSquish_AUTPATH:STRING=${Squish_AUTPATH}"
     "-DSquish_LIBPATH:STRING=${Squish_LIBRARY_DIRS}"
     "-DSquish_SERVER_EXECUTABLE:STRING=${Squish_squishserver}"
     "-DSquish_CLIENT_EXECUTABLE:STRING=${Squish_squishrunner}"
     "-DSquish_LIBQTDIR:STRING=${Squish_LIBQTDIR}"
     "-DSquish_USER_SETTINGS_DIR:STRING=${CMAKE_CURRENT_BINARY_DIR}/sq_user_settings"
     "-DCtests_TESTSUITE:STRING=${CMAKE_CURRENT_SOURCE_DIR}"
     "-DCtests_TEST:STRING=${CMAKE_CURRENT_SOURCE_DIR}/${testName}"
     "-DCtests_LOGFILE:STRING=${CMAKE_CURRENT_BINARY_DIR}/${testName}.txt"
     -P "${Squish_CTESTFILE}"
    )
  set_tests_properties(${testName}
    PROPERTIES FAIL_REGULAR_EXPRESSION "SQUISHFATAL"
    )
endmacro(sciSquishAddTest)    
