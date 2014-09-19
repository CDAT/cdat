######################################################################
#
# sciTranspPreprocess:
#    Macros for handling transp 
#
# $Id: sciTranspPreprocess.cmake 1151 2011-12-17 13:51:42Z cary $
#
# Copyright 2010-2012 Tech-X Corporation.
# Arbitrary redistribution allowed provided this copyright remains.
#
######################################################################

include(CMakeParseArguments)

# sciTranspPreprocess.cmake
#
# Args:
#   FPPFILES:   List of files to preprocess
#
macro(sciTranspPreprocess)

# Parse out the args
  set(opts ) # no-value args
  set(oneValArgs PACKAGE;SUBDIR)
  set(multValArgs FPPFILES) # e.g., lists
  cmake_parse_arguments(FD "${opts}" "${oneValArgs}" "${multValArgs}" ${ARGN})
  if(DEFINED FD_SUBDIR)
        set(trincdir ${CMAKE_CURRENT_SOURCE_DIR}/${FD_SUBDIR})
  else()
        set(trincdir ${CMAKE_CURRENT_SOURCE_DIR})
  endif()

#preprocess the fpp sources using transp's python script
set(F_SOURCES)
foreach(fpfile ${FD_FPPFILES} )
  get_filename_component(fbname ${fpfile}
    NAME_WE
  )
  get_filename_component(fext ${fpfile}
    EXT
  )
  if(fext STREQUAL ".F90")
    set(freeFlag "-free")
  else()
    set(freeFlag "")
  endif()
  string(TOLOWER ${fext} fppfext)
  set(newfilename ${fbname}${fppfext})
  add_custom_command(
    OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/${newfilename}
    COMMAND ${PYTHON_EXECUTABLE} ${CMAKE_SOURCE_DIR}/fpreproc/fppfile.py ${fpfile} ${newfilename} ${freeFlag} -I${trincdir} -I${CMAKE_SOURCE_DIR}/incl_cpp/ ${CPPFLAGS}
  )
  set_source_files_properties(${CMAKE_CURRENT_BINARY_DIR}/${newfilename} PROPERTIES GENERATED 1)
  LIST(APPEND F_SOURCES ${CMAKE_CURRENT_BINARY_DIR}/${newfilename})
endforeach()
set(fsources ${FD_PACKAGE}_F_SOURCES)
set(${fsources} ${F_SOURCES} )

endmacro()

# Handle the weird module issues
macro(SciInstallModules)
  set(opts ) # no-value args
  set(oneValArgs PACKAGE)
  set(multValArgs MODFILES) # e.g., lists
  cmake_parse_arguments(FD "${opts}" "${oneValArgs}" "${multValArgs}" ${ARGN})
  if (TX_FC_MODULENAME_CAPITALIZED)
    string(TOUPPER "${FD_MODFILES}" FD_MODFILES)
  endif ()
  set(INC_MODFILES)
  foreach (module ${FD_MODFILES})
    set(INC_MODFILES ${INC_MODFILES} ${CMAKE_CURRENT_BINARY_DIR}/${module}.${TX_FC_MODULE_SUFFIX})
  endforeach ()
  set(modfiles ${FD_PACKAGE}_INC_MODULES)
  set(${modfiles} ${INC_MODFILES} )
endmacro()
