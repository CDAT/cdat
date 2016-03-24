#----------------------------------------------------------------------------
# The following script tries to set important toolchain related variables for
# MacOSX. The variables set include CMAKE_OSX_DEPLOYMENT_TARGET,
# OSX_DEVELOPER_ROOT, CMAKE_OSX_SYSROOT, etc. If the deployment target is not
# set by the user, it is set as the current OS version. Similarly, if the
# sysroot is not set by the user, it is set to the latest available version of
# the MacOSX SDK.
# NOTE: It is assumed that the latest available SDK is the best version that
# goes with the version specified by the deployment target. Matching the
# versions of the deployment target and SDK are not enforced. A warning,
# however, is presented to the user if the versions do not match.

#----------------------------------------------------------------------------
# OSX version number
# Support only 10.8 or higher for now
if (CURRENT_OSX_VERSION VERSION_LESS 10.8)
  message(FATAL_ERROR "Found Mac OSX version ${CURRENT_OSX_VERSION}; "
    "Minimum supported Mac OSX version is 10.8")
endif ()

# Current OSX version as a two-component string: 10.8, 10.9, etc ...
string(REGEX REPLACE "^([0-9]+\\.[0-9]+).*$" "\\1"
  _CURRENT_OSX_VERSION "${CURRENT_OSX_VERSION}")

#----------------------------------------------------------------------------
# CMAKE_OSX_DEPLOYMENT_TARGET
if(NOT CMAKE_OSX_DEPLOYMENT_TARGET)
  message(STATUS "Setting OSX_DEPLOYMENT_TARGET to ${_CURRENT_OSX_VERSION}")
  set(CMAKE_OSX_DEPLOYMENT_TARGET ${_CURRENT_OSX_VERSION})
endif()

#----------------------------------------------------------------------------
# OSX_DEVELOPER_ROOT
if(NOT OSX_DEVELOPER_ROOT)
  message(WARNING "Could not determine developer root directory. "
    "\nAssuming \"/Applications/Xcode.app/Contents/Developer\".") 
  set(OSX_DEVELOPER_ROOT "/Applications/Xcode.app/Contents/Developer")
endif(NOT OSX_DEVELOPER_ROOT)

#----------------------------------------------------------------------------
# CMAKE_OSX_SYSROOT

if(NOT CMAKE_OSX_SYSROOT)
  # Find all SDKs installed in the OSX_DEVELOPER_ROOT
  foreach(d Platforms/MacOSX.platform/Developer/SDKs SDKs)
    file(GLOB _CMAKE_OSX_SDKS ${OSX_DEVELOPER_ROOT}/${d}/*)
    if(_CMAKE_OSX_SDKS)
      set(_CMAKE_OSX_SDKS_DIR ${OSX_DEVELOPER_ROOT}/${d})
      break()
    endif()
  endforeach()

  # find the latest SDK
  set(_CMAKE_OSX_LATEST_SDK_VERSION "0.0")
  file(GLOB _CMAKE_OSX_SDKS RELATIVE "${_CMAKE_OSX_SDKS_DIR}"
    "${_CMAKE_OSX_SDKS_DIR}/MacOSX*.sdk")
  foreach(_SDK ${_CMAKE_OSX_SDKS})
    if(_SDK MATCHES "MacOSX([0-9]+\\.[0-9]+)[^/]*\\.sdk" AND
       CMAKE_MATCH_1 VERSION_GREATER ${_CMAKE_OSX_LATEST_SDK_VERSION})
      set(_CMAKE_OSX_LATEST_SDK_VERSION "${CMAKE_MATCH_1}")
    endif()
  endforeach()

  if(_CMAKE_OSX_LATEST_SDK_VERSION VERSION_GREATER "0.0")
    # Set sysroot to latest SDK
    message(STATUS "Latest SDK version found is "
      "${_CMAKE_OSX_LATEST_SDK_VERSION}. "
      "Setting CMAKE_OSX_SYSROOT to the latest SDK version.")
    set(CMAKE_OSX_SYSROOT
        "${_CMAKE_OSX_SDKS_DIR}/MacOSX${_CMAKE_OSX_LATEST_SDK_VERSION}.sdk")
  else()
    message(AUTHOR_WARNING "No SDK found. "
      "Please set CMAKE_OSX_SYSROOT to path of valid MacOSX SDK.")
  endif()

endif(NOT CMAKE_OSX_SYSROOT)

#----------------------------------------------------------------------------
# DEPLOYMENT_TARGET and SYSROOT version match

if(CMAKE_OSX_DEPLOYMENT_TARGET AND CMAKE_OSX_SYSROOT)
  string(REGEX REPLACE ".*MacOSX([0-9]+\\.[0-9]+).*" "\\1"
    _CURRENT_OSX_SDK_VERSION "${CMAKE_OSX_SYSROOT}")
  if(${CMAKE_OSX_DEPLOYMENT_TARGET} VERSION_GREATER ${_CURRENT_OSX_SDK_VERSION})
    message(AUTHOR_WARNING
      "Deployment target (${CMAKE_OSX_DEPLOYMENT_TARGET}) cannot be greater "
      "than the SYSROOT version (${_CURRENT_OSX_SDK_VERSION}) as specified by "
      "CMAKE_OSX_SYSROOT. "
      "Reverting CMAKE_OSX_DEPLOYMENT_TARGET to SYSROOT version.")
    set(CMAKE_OSX_DEPLOYMENT_TARGET ${_CURRENT_OSX_SDK_VERSION})
  endif()
endif()

#----------------------------------------------------------------------------
# CMAKE_OSX_ARCHITECTURE
# Legacy code that enforces 64-bit-ness...

if(NOT CMAKE_OSX_ARCHITECTURES)
  set(CMAKE_OSX_ARCHITECTURES "x86_64")
  set(CMAKE_OSX_ARCHITECTURES_M "64")
endif()
