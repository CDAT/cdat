#  Try to find Readline library and header.
#  This file sets the following variables:
#
#  READLINE_INCLUDE_DIR, where to find readline.h, etc.
#  READLINE_LIBRARIES, the libraries to link against
#  READLINE_FOUND, If false, do not try to use Readline.
#
# Also defined, but not for general use are:
#  READLINE_LIBRARY, the full path to the Readline library.
#  READLINE_INCLUDE_PATH, for CMake backward compatibility

FIND_PATH(READLINE_INCLUDE_DIR readline.h
  PATHS /usr/local/include
        /usr/include
  PATH_SUFFIXES readline
)

FIND_LIBRARY(READLINE_LIBRARY readline
  /usr/lib
  /usr/local/lib
)

# handle the QUIETLY and REQUIRED arguments and set READLINE_FOUND to TRUE if 
# all listed variables are TRUE
INCLUDE(FindPackageHandleStandardArgs)
FIND_PACKAGE_HANDLE_STANDARD_ARGS(READLINE DEFAULT_MSG READLINE_LIBRARY READLINE_INCLUDE_DIR)

IF(READLINE_FOUND)
  SET(READLINE_LIBRARIES ${READLINE_LIBRARY})
ENDIF(READLINE_FOUND)

MARK_AS_ADVANCED(
  READLINE_INCLUDE_DIR
  READLINE_LIBRARY
)
