# - FindSciSphinx: This module looks for Sphinx binary
# Sphinx is a documentation generation tool.  Please see
# http://www.sphinx.org
#
# This modules defines the following variables:
#
#   Sphinx_EXECUTABLE     = The path to the sphinx command.
#   SPHINX_FOUND          = Was Sphinx found or not?
#

#################################################################
# Find Sphinx...
#
# $Id: FindSciSphinx.cmake 1161 2011-12-17 15:44:00Z cary $
#
#################################################################

if (WIN32)
  set(sfxs Scripts)
else ()
  set(sfxs bin)
endif ()
if (DEBUG_CMAKE)
  message(STATUS "Looking for sphinx-build with SUPRA_SEARCH_PATH = ${SUPRA_SEARCH_PATH} and sfxs = ${sfxs}.")
endif ()
find_program(Sphinx_EXECUTABLE
  sphinx-build
  PATHS ${SUPRA_SEARCH_PATH}
  PATH_SUFFIXES ${sfxs}
)
if (Sphinx_EXECUTABLE)
  set(SPHINX_FOUND 1 CACHE BOOL "Found Sphinx binary")
  message(STATUS "Sphinx_EXECUTABLE found.")
  message(STATUS "Sphinx_EXECUTABLE  = ${Sphinx_EXECUTABLE}")
else ()
  message(STATUS "Sphinx_EXECUTABLE NOT found.")
endif ()

# #####################
# Find mathjax...
# #####################

# We only ever need one MathJax?
# Sphinx_MathJax_js is what sphinx uses
if (ENABLE_WEBDOCS)
  if (ENABLE_CTKDOCS)
    message(FATAL_ERROR "Enabling ctkdocs and webdocs is incompatible.")
  endif ()
  set(MathJax_MathJax_js "https://ice.txcorp.com/doc/MathJax/MathJax.js")
  set(Sphinx_MathJax_js "${MathJax_MathJax_js}")
elseif (ENABLE_CTKDOCS)
  SciFindPackage(PACKAGE MathJax INSTALL_DIRS MathJax-ctk FILES MathJax.js)
  get_filename_component(MathJax_DIR ${MathJax_MathJax_js}/.. REALPATH)
  if (NOT MATHJAX_FOUND)
    message(FATAL_ERROR "CTKDOCS is enabled but could not find ctk mathjax.")
  endif ()
  get_filename_component(MathJax_DIR ${MathJax_MathJax_js}/.. REALPATH)
# If not in URL form, Sphinx prepends the ../_static
  set(Sphinx_MathJax_js MathJax/MathJax.js)
  message(STATUS "Be sure to copy ${MathJax_DIR} to your <html build dir>/_static/MathJax.")
else ()
  SciFindPackage(PACKAGE MathJax INSTALL_DIRS MathJax FILES MathJax.js)
  if (MATHJAX_FOUND)
    get_filename_component(MathJax_DIR ${MathJax_MathJax_js}/.. REALPATH)
    set(Sphinx_MathJax_js "file://${MathJax_MathJax_js}")
  else ()
# Fall back to version on web
    set(MathJax_MathJax_js "https://ice.txcorp.com/doc/MathJax/MathJax.js")
    message(WARNING"  MathJax not found.  Using web link.")
  endif ()
endif ()
message(STATUS "  MathJax_DIR          = ${MathJax_DIR}")
message(STATUS "  MathJax_MathJax_js   = ${MathJax_MathJax_js}")
message(STATUS "  Sphinx_MathJax_js    = ${Sphinx_MathJax_js}")

