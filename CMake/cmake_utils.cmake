
cmake_minimum_required(VERSION 2.8.7)

set(a_deps c)
set(b_deps c)
set(c_deps)

set(a_parents)
set(a_children)
set(b_parents)
set(b_children)

set(already_included)

# Check for cycle
macro(check_for_cycle node ancestors)
  message("Visited ${node} ${visited}")
  foreach(parent ${ancestors})
    list(FIND visited "${parent}" result)
    if(NOT ${result} EQUAL -1)
      message(FATAL_ERROR "Cycle found")
    endif()
    list(APPEND visited "${parent}") 
    check_for_cycle(${node} "${${parent}_parents}")
  endforeach()
endmacro()

# Include external package
macro(include_external_package package)
  set(visited ${package})

  # FIXME: In-efficient algorithm to find cycles
  check_for_cycle(${package} "${${package}_parents}")
  
  set(visited)

  foreach(parent ${${package}_parents})
    include_external_package("${parent}")
  endforeach()
  
  list(FIND already_included "${package}" result)
  if(${result} EQUAL -1)
    list(APPEND already_included "${package}")
    message("Adding ${package}")
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
macro(include_external_packages packages)
  foreach(package ${packages})
    create_graph_node("${package}")
  endforeach()

  foreach(package ${packages})
    include_external_package("${package}")
  endforeach()
endmacro()


set(packages "a;b;c")

include_external_packages("${packages}")

message("a parents ${a_parents}")
message("a children ${a_children}")
message("b parents ${b_parents}")
message("b children ${b_children}")
