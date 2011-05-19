#  Try to find uuid library and header.
#  This file sets the following variables:
#
#  UUID_INCLUDE_DIR, where to find uuid.h, etc.
#  UUID_LIBRARIES, the libraries to link against
#  UUID_FOUND, If false, do not try to use uuid.
#
# Also defined, but not for general use are:
#  UUID_LIBRARY, the full path to the uuid library.
#  UUID_INCLUDE_PATH, for CMake backward compatibility

FIND_PATH(UUID_INCLUDE_DIR uuid.h
  PATHS /usr/local/include
        /usr/include
  PATH_SUFFIXES uuid
)

IF(APPLE)
  # handle the QUIETLY and REQUIRED arguments and set UUID_FOUND to TRUE if 
  # all listed variables are TRUE
  INCLUDE(FindPackageHandleStandardArgs)
  FIND_PACKAGE_HANDLE_STANDARD_ARGS(UUID DEFAULT_MSG UUID_INCLUDE_DIR)

  MARK_AS_ADVANCED(UUID_INCLUDE_DIR)
ELSE(APPLE)

  FIND_LIBRARY(UUID_LIBRARY uuid
    /usr/lib
    /usr/local/lib
  )

  # handle the QUIETLY and REQUIRED arguments and set UUID_FOUND to TRUE if 
  # all listed variables are TRUE
  INCLUDE(FindPackageHandleStandardArgs)
  FIND_PACKAGE_HANDLE_STANDARD_ARGS(UUID DEFAULT_MSG UUID_LIBRARY UUID_INCLUDE_DIR)

  IF(UUID_FOUND)
    SET(UUID_LIBRARIES ${UUID_LIBRARY})
  ENDIF(UUID_FOUND)

  MARK_AS_ADVANCED(
    UUID_INCLUDE_DIR
    UUID_LIBRARY
  )
ENDIF(APPLE)