cmake_minimum_required(VERSION 2.8.8)

# Usage: add_cdat_package(package_name version_string optional default)
#-----------------------------------------------------------------------------
macro (add_cdat_package package_name version_string msg default)
  string(TOUPPER ${package_name} uc_package)
  string(TOLOWER ${package_name} lc_package)
  set(version)
  set(message)
  set(option_default)

  # ARGV1 will be the version string
  if(NOT "" STREQUAL "${version_string}")
    set(version "${version_string}")
    message("[INFO] version ${version} of ${uc_package} is required by UVCDAT")
  endif()

  # ARGV2 (true = 1 or false != 1)
  if(NOT "" STREQUAL "${msg}")
    set(message "${msg}")
  endif()

  # ARGV3 (ON / OFF)
  if(NOT "" STREQUAL "${default}")
    set(option_default "${default}")
    message("[INFO] ${uc_package} is optional")
  endif()

  # Find system package first and if it exits provide an option to use
  # system package
  if(DEFINED version)
    find_package(${package_name} ${version} QUIET)
  else()
    find_package(${package_name} QUIET)
  endif()

  # Check if package is optional, and if yes populate the GUI appropriately
  if(DEFINED isoptional AND "${isoptional}" STREQUAL "TRUE")
    option(CDAT_BUILD_${uc_package} ${message} ${option_default})
  endif()

  option(CDAT_USE_SYSTEM_${uc_package} "Use system installed ${lc_package}" OFF)
  if(NOT ${uc_package}_FOUND)
    mark_as_advanced(${uc_package}_DIR)
    mark_as_advanced(CDAT_USE_SYSTEM_${uc_package})
  endif()

  # Check if package is found, if not found or found but user prefers to use cdat package
  # then use cdat package or else use system package
  if(NOT CDAT_USE_SYSTEM_${uc_package})
    if(DEFINED CDAT_BUILD_${uc_package} AND CDAT_BUILD_${uc_package})
      message("[INFO] ${uc_package} will be build by the UVCDAT-superbuild")
      list(APPEND external_packages "${package_name}")
      message("external_packages are: ${external_packages}")
      set(${lc_package}_pkg "${package}")
    endif()
    if(NOT DEFINED CDAT_BUILD_${uc_package})
      message("[INFO] ${uc_package} will be build by the UVCDAT-superbuild")
      list(APPEND external_packages "${package_name}")
      set(${lc_package}_pkg "${package_name}")
    endif()
  else()
    if(CDAT_USE_SYSTEM_${uc_package} AND ${uc_package}_FOUND)
      message("[INFO] Removing external package ${package_name}")
      unset(${lc_package}_pkg)
      if(external_packages)
        list(REMOVE_ITEM external_packages ${package_name})
      endif()

      if(${uc_package}_INCLUDE_DIR)
        list(APPEND found_system_include_dirs ${${uc_package}_INCLUDE_DIR})
        message("[INFO] Including: ${uc_package}_INCLUDE_DIR: ${${uc_package}_INCLUDE_DIR}")
      endif()

      if(${uc_package}_LIBRARY)
        get_filename_component(lib_path ${${uc_package}_LIBRARY} PATH)
        list(APPEND found_system_libraries ${lib_path})
        message("[INFO]  Linking: ${uc_package}_LIBRARY: ${lib_path}")
      endif()

    endif() # use system package

  endif()
endmacro()
