# This is CMake's FindOpenSsl with one change: We look for MT libs instead
# of MD libs.

# - Try to find the OpenSSL encryption library
# Once done this will define
#
#  OPENSSL_ROOT_DIR - Set this variable to the root installation of OpenSSL
#
# Read-Only variables:
#  OPENSSL_FOUND - system has the OpenSSL library
#  OpenSsl_INCLUDE_DIR - the OpenSSL include directory
#  OpenSsl_LIBRARIES - The libraries needed to use OpenSSL

#=============================================================================
# Copyright 2006-2009 Kitware, Inc.
# Copyright 2006 Alexander Neundorf <neundorf@kde.org>
# Copyright 2009-2010 Mathieu Malaterre <mathieu.malaterre@gmail.com>
#
# Distributed under the OSI-approved BSD License(the "License");
# see accompanying file Copyright.txt for details.
#
# This software is distributed WITHOUT ANY WARRANTY; without even the
# implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
# See the License for more information.
#=============================================================================
#(To distribute this file outside of CMake, substitute the full
#  License text for the above reference.)

# http://www.slproweb.com/products/Win32OpenSSL.html
set(_OPENSSL_ROOT_HINTS
  "[HKEY_LOCAL_MACHINE\\SOFTWARE\\Microsoft\\Windows\\CurrentVersion\\Uninstall\\OpenSSL (32-bit)_is1;Inno Setup: App Path]"
  "[HKEY_LOCAL_MACHINE\\SOFTWARE\\Microsoft\\Windows\\CurrentVersion\\Uninstall\\OpenSSL (64-bit)_is1;Inno Setup: App Path]"
  )
set(_OPENSSL_ROOT_PATHS
  "C:/OpenSSL/"
  )
find_path(OPENSSL_ROOT_DIR
  NAMES include/openssl/ssl.h
  HINTS ${_OPENSSL_ROOT_HINTS}
  PATHS ${_OPENSSL_ROOT_PATHS}
)
MARK_AS_ADVANCED(OPENSSL_ROOT_DIR)

# Re-use the previous path:
find_path(OpenSsl_INCLUDE_DIR openssl/ssl.h
  PATHS ${OPENSSL_ROOT_DIR}/include
)

if (WIN32 AND NOT CYGWIN)
  # MINGW should go here too
  if (MSVC)
    # /MD and /MDd are the standard values - if someone wants to use
    # others, the libnames have to change here too
    # use also ssl and ssleay32 in debug as fallback for openssl < 0.9.8b
    # TODO: handle /MT and static lib
    # In Visual C++ naming convention each of these four kinds of Windows libraries has it's standard suffix:
    #   * MD for dynamic-release
    #   * MDd for dynamic-debug
    #   * MT for static-release
    #   * MTd for static-debug

    # Implementation details:
    # We are using the libraries located in the VC subdir instead of the parent directory eventhough :
    # libeay32MD.lib is identical to ../libeay32.lib, and
    # ssleay32MD.lib is identical to ../ssleay32.lib
    find_library(LIB_EAY_DEBUG NAMES libeay32MTd libeay32
      PATHS ${OPENSSL_ROOT_DIR}/lib/VC/static
      )
    find_library(LIB_EAY_RELEASE NAMES libeay32MT libeay32
      PATHS ${OPENSSL_ROOT_DIR}/lib/VC/static
      )
    find_library(SSL_EAY_DEBUG NAMES ssleay32MTd ssleay32 ssl
      PATHS ${OPENSSL_ROOT_DIR}/lib/VC/static
      )
    find_library(SSL_EAY_RELEASE NAMES ssleay32MT ssleay32 ssl
      PATHS ${OPENSSL_ROOT_DIR}/lib/VC/static
      )
    if (CMAKE_CONFIGURATION_TYPES OR CMAKE_BUILD_TYPE )
      set(OpenSsl_LIBRARIES
        optimized ${SSL_EAY_RELEASE} debug ${SSL_EAY_DEBUG}
        optimized ${LIB_EAY_RELEASE} debug ${LIB_EAY_DEBUG}
        )
    else ()
      set(OpenSsl_LIBRARIES ${SSL_EAY_RELEASE} ${LIB_EAY_RELEASE} )
    endif ()
    MARK_AS_ADVANCED(SSL_EAY_DEBUG SSL_EAY_RELEASE)
    MARK_AS_ADVANCED(LIB_EAY_DEBUG LIB_EAY_RELEASE)
  elseif (MINGW)
    # same player, for MingW
    find_library(LIB_EAY NAMES libeay32
      PATHS ${OPENSSL_ROOT_DIR}/lib/MinGW
      )
    find_library(SSL_EAY NAMES ssleay32
      PATHS ${OPENSSL_ROOT_DIR}/lib/MinGW
      )
    MARK_AS_ADVANCED(SSL_EAY LIB_EAY)
    set(OpenSsl_LIBRARIES ${SSL_EAY} ${LIB_EAY} )
  else ()
    # Not sure what to pick for -say- intel, let's use the toplevel ones and hope someone report issues:
    find_library(LIB_EAY NAMES libeay32
      PATHS ${OPENSSL_ROOT_DIR}/lib
      )
    find_library(SSL_EAY NAMES ssleay32
      PATHS ${OPENSSL_ROOT_DIR}/lib
      )
    MARK_AS_ADVANCED(SSL_EAY LIB_EAY)
    set(OpenSsl_LIBRARIES ${SSL_EAY} ${LIB_EAY} )
  endif ()
else ()

  find_library(OPENSSL_SSL_LIBRARIES NAMES ssl ssleay32 ssleay32MT)
  find_library(OPENSSL_CRYPTO_LIBRARIES NAMES crypto)
  MARK_AS_ADVANCED(OPENSSL_CRYPTO_LIBRARIES OPENSSL_SSL_LIBRARIES)

  set(OpenSsl_LIBRARIES ${OPENSSL_SSL_LIBRARIES} ${OPENSSL_CRYPTO_LIBRARIES})

endif ()

#include(${CMAKE_CURRENT_LIST_DIR}/FindPackageHandleStandardArgs.cmake)
#find_package_handle_standard_args(OpenSsl DEFAULT_MSG
#  OpenSsl_LIBRARIES
#  OpenSsl_INCLUDE_DIR
#)

MARK_AS_ADVANCED(OpenSsl_INCLUDE_DIR OpenSsl_LIBRARIES)

set(OpenSsl_INCLUDE_DIRS "${OpenSsl_INCLUDE_DIR}")
