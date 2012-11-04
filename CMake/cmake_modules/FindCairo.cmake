# Find the Cairo includes and libraries, along with their dependencies
# The following variables are set if the proper things are.  
#  CAIRO_FOUND        - True when the Cairo include directory is found.
#  CAIRO_INCLUDE_DIRS - The path to where Cairo includes are
#  CAIRO_LIBRARIES    - Needed CAIRO libraries

# Usage:
# In your CMakeLists.txt file do something like this:
# ...
# # CAIRO
# FIND_PACKAGE(CAIRO)
# ...
# INCLUDE_DIRECTORIES(${CAIRO_INCLUDE_DIRS})
#
# First, let's check to see if user has pkg-config set up to find these
IF(NOT CAIRO_INCLUDE_DIR )
  find_package( PkgConfig )
  pkg_check_modules( CAIRO cairo>=1.7.4 )
ENDIF(NOT CAIRO_INCLUDE_DIR )

IF(NOT CAIRO_FOUND)

  IF(WIN32)
    SET(CAIRO_INCLUDE_SEARCH 
      C:/gtk/include
    )
    SET(CAIRO_LIB_SEARCH
      C:/gtk/bin
    )
    SET(CAIRO_LIBRARY_NAME
      cairo.lib
    )
  ELSE(WIN32)
    SET(CAIRO_INCLUDE_SEARCH
      /usr/local/include/
      /usr/include/
    )
    SET(CAIRO_LIB_SEARCH
      /usr/local/lib/
      /usr/lib
    )
    SET(CAIRO_LIBRARY_NAME
      libcairo.so
    )
  ENDIF(WIN32)

  IF(NOT CAIRO_INCLUDE_DIRS)
    FIND_PATH(CAIRO_INCLUDE_DIRS cairo/cairo.h
      ${PANGO_INCLUDE_DIRS}
      ${CAIRO_INCLUDE_SEARCH}
      DOC "Location of cairo/cairo.h")
  ENDIF(NOT CAIRO_INCLUDE_DIRS)
  
  IF(CAIRO_INCLUDE_DIRS)
    SET(BASE_INCLUDE_DIR ${CAIRO_INCLUDE_DIRS})
    SET(CAIRO_INCLUDE_DIRS ${CAIRO_INCLUDE_DIRS}/cairo)
  ENDIF(CAIRO_INCLUDE_DIRS)

  IF(NOT CAIRO_LIBRARY_DIRS)
    FIND_PATH(CAIRO_LIBRARY_DIRS ${CAIRO_LIBRARY_NAME}
      ${CAIRO_LIB_SEARCH}
      DOC "Directory that holds ${CAIRO_LIBRARY_NAME}"
      )
  ENDIF(NOT CAIRO_LIBRARY_DIRS)

  IF(CAIRO_LIBRARY_DIRS)
    IF(NOT CAIRO_LIBRARY)
      FIND_LIBRARY(CAIRO_LIBRARY
  NAMES cairo
  PATHS ${CAIRO_LIBRARY_DIRS} ${CAIRO_LIB_SEARCH}
  )
    ENDIF(NOT CAIRO_LIBRARY)
  ENDIF(CAIRO_LIBRARY_DIRS)

  IF(BASE_INCLUDE_DIR)
    IF(NOT GLIB_INCLUDE_DIRS)
      FIND_PATH(GLIB_INCLUDE_DIRS glib.h
  ${BASE_INCLUDE_DIR}/glib-2.0
  ${CAIRO_INCLUDE_SEARCH}
  DOC "Location of directory glib-2.0"
  )
    ENDIF(NOT GLIB_INCLUDE_DIRS)
  ENDIF(BASE_INCLUDE_DIR)

  IF(GLIBCONFIG_INCLUDE_DIRS)
    SET(GLIBCONFIG_INCLUDE_DIRS ${GLIBCONFIG_INCLUDE_DIRS}/glib-2.0/include)
  ENDIF(GLIBCONFIG_INCLUDE_DIRS)

  IF(CAIRO_LIBRARY)
    SET(CAIRO_LIBRARIES
      ${CAIRO_LIBRARY}
      )
  ENDIF(CAIRO_LIBRARY)

  IF(CAIRO_INCLUDE_DIRS AND CAIRO_LIBRARY)
    SET(CAIRO_FOUND 1)
  ENDIF(CAIRO_INCLUDE_DIRS AND CAIRO_LIBRARY)

  MARK_AS_ADVANCED(
    CAIRO_LIBRARY_DIRS
    CAIRO_INCLUDE_DIRS
  )

ENDIF(NOT CAIRO_FOUND)

