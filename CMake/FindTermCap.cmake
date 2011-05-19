#  Try to find TermCap library and headers.
#  This file sets the following variables:
#
#  TERMCAP_INCLUDE_DIR, where to find termcap.h, etc.
#  TERMCAP_LIBRARIES, the libraries to link against
#  TERMCAP_FOUND, If false, do not try to use TermCap.
#
# Also defined, but not for general use are:
#  TERMCAP_LIBRARY, the full path to the TermCap library.
#  TERMCAP_INCLUDE_PATH, for CMake backward compatibility

FIND_FILE( TERMCAP_INCLUDE_DIR termcap.h
  PATHS /usr/local/include
        /usr/include
  PATH_SUFFIXES termcap
)

FIND_LIBRARY( TERMCAP_LIBRARY termcap
  /usr/lib
  /usr/local/lib
)

# handle the QUIETLY and REQUIRED arguments and set TERMCAP_FOUND to TRUE if 
# all listed variables are TRUE
INCLUDE(FindPackageHandleStandardArgs)
FIND_PACKAGE_HANDLE_STANDARD_ARGS(TERMCAP DEFAULT_MSG TERMCAP_LIBRARY TERMCAP_INCLUDE_DIR)


MARK_AS_ADVANCED(
  TERMCAP_INCLUDE_DIR
  TERMCAP_LIBRARY
)
