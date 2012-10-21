
cmake_minimum_required(VERSION 2.8.7)

#
# Usage: add_cdat_package(package_name version_string) 
macro (add_cdat_package package)
  string(TOUPPER ${package} UC_PACKAGE)
  string(TOLOWER ${package} LC_PACKAGE)
  set(VERSION)

  # ARGV1 will be the version string	
  if(NOT "" STREQUAL "${ARGV1}")
    set(VERSION "${ARGV1}")
    message("[INFO] Version ${VERSION} of ${UC_PACKAGE} is required by UVCDAT") 
  endif()	  

  # Find system package first and if it exits provide an option to use
  # system package
  if(DEFINED version)
    find_package(${UC_PACKAGE} ${VERSION} QUIET)
  else()
    find_package(${UC_PACKAGE} QUIET)
  endif()

  option(CDAT_USE_SYSTEM_${UC_PACKAGE} "Use system installed ${LC_PACKAGE}" OFF)
  if(NOT ${UC_PACKAGE}_FOUND)
    mark_as_advanced(${UC_PACKAGE}_DIR)
    mark_as_advanced(CDAT_USE_SYSTEM_${UC_PACKAGE})
  endif()

  # Check if package is found, if not found or found but user prefers to use cdat package
  # then use cdat package or else use system package
  if(NOT CDAT_USE_SYSTEM_${UC_PACKAGE})
    message("[INFO] ${UC_PACKAGE} will be build by the UVCDAT-superbuild")
    list(APPEND external_packages "${package}")
    set(${package}_pkg "${package}")
  else()
    if(CDAT_USE_SYSTEM_${UC_PACKAGE} AND ${UC_PACKAGE}_FOUND)
      message("Debug removing")
      unset(${package}_pkg)
      if(external_packages)
        list(REMOVE_ITEM external_packages External_${package})
      endif()

      if(${UC_PACKAGE}_INCLUDE_DIR)
        list(APPEND found_system_include_dirs ${${UC_PACKAGE}_INCLUDE_DIR})
        message("[INFO] Including: ${UC_PACKAGE}_INCLUDE_DIR: ${${UC_PACKAGE}_INCLUDE_DIR}")
      endif()

      if(${UC_PACKAGE}_LIBRARY)
        get_filename_component(lib_path ${${UC_PACKAGE}_LIBRARY} PATH)
        list(APPEND found_system_libraries ${lib_path})
        message("[INFO]  Linking: ${UC_PACKAGE}_LIBRARY: ${lib_path}")
      endif()

    endif() # use system package

  endif()
endmacro(add_cdat_package)

# Check for cycle
function(check_for_cycle node ancestors visited)
  foreach(parent ${ancestors})
    set(currently_visited "${visited}")
    list(FIND visited "${parent}" result)
    if(NOT ${result} EQUAL -1)
      message(FATAL_ERROR "[ERROR] Cycle found when visiting ${parent}")
    endif()

    list(LENGTH "${parent}_parents" no_of_parents)
    if(${no_of_parents} EQUAL 0)
      #message("[INFO] No cycle found for ${visited};${parent}")
    else()
      list(APPEND visited "${parent}")
    endif()
    check_for_cycle(${node} "${${parent}_parents}" "${visited}")
    set(visited "${currently_visited}")
  endforeach()
endfunction()

# Include external package
macro(sort_external_package package)
  set(visited ${package})

  # FIXME: In-efficient algorithm to find cycles
  check_for_cycle(${package} "${${package}_parents}" "${visited}")
  
  set(visited)

  foreach(parent ${${package}_parents})
    sort_external_package("${parent}")
  endforeach()
  
  list(FIND sorted_external_packages "${package}" result)
  if(${result} EQUAL -1)
    list(APPEND sorted_external_packages "${package}")
    message("[INFO] Adding ${package}")
  endif()
endmacro()

# Create graph nodes so that we can include external packages in
# proper order (less dependent to more) and check for cycles
macro(create_graph_node package)
  foreach(dep ${${package}_deps})
    list(APPEND ${package}_parents ${dep})
    list(APPEND ${dep}_children ${package})
  endforeach()
endmacro()

# Perform operation
macro(sort_external_packages packages)
  foreach(package ${packages})
    create_graph_node("${package}")
  endforeach()

  foreach(package ${packages})
    sort_external_package("${package}")
  endforeach()
endmacro()
