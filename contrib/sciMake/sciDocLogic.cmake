######################################################################
#
# To encourage our software to use the same documentation, moving
# functionality to a common cmake file
#
# $Id: sciDocLogic.cmake 1151 2011-12-17 13:51:42Z cary $
#
# This script sets the following variables which are also options:
#   ENABLE_DEVELDOCS
#   ENABLE_USERDOCS
#   ENABLE_WEBDOCS
#
# It also finds the following packages:
#   Doxygen
#   Sphinx
#
# Logic can be found in bilder/README/README-docs.txt
# Reading that document should be done if trying to understand this
# logic
#
#####################################################################

# develdocs
option(ENABLE_DEVELDOCS "Enable developer documentation to be built" ON)
# Below needed?
# set(ENABLE_DEVELDOCS ${ENABLE_DEVELDOCS})
find_package(SciDoxygen)

# Need to know for userdocs
option(ENABLE_USERDOCS "Enable user documentation to be built" ON)
option(ENABLE_WEBDOCS "Install documentation at top" OFF)
find_package(SciSphinx)
if (SPHINX_FOUND)
  # message(STATUS "SPHINX_EXECUTABLE found.")
  # message(STATUS "SPHINX_EXECUTABLE = ${SPHINX_EXECUTABLE}")
else ()
  message(STATUS "SPHINX_EXECUTABLE not found. User documentation cannot be built.")
  set(ENABLE_USERDOCS FALSE)
endif ()

# Enable installation of a minimal MathJax
option(ENABLE_MINMATHJAX "Install minimal MathJax with user documentation" ON)

# ENABLE_WEBDOCS installs documentation at top.  Useful for putting on web.
# If installing at top, cannot do both userdocs and develdocs
if (ENABLE_WEBDOCS AND ENABLE_USERDOCS)
  set(ENABLE_DEVELDOCS FALSE)
endif ()

# Final results
if (ENABLE_USERDOCS)
  message(STATUS "User documentation will be buildable.")
else ()
  message(STATUS "User documentation will not be buildable.")
endif ()
if (ENABLE_DEVELDOCS)
  message(STATUS "Developer documentation will be buildable.")
else ()
  message(STATUS "Developer documentation will not be buildable.")
endif ()
