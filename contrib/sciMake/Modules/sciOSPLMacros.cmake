##############################################################################
# Adding OpenSplics DDS Data Model related files to the build.
#
# $Id: SciOSPLMacros.cmake 1245 2012-01-31 21:36:22Z dws $
#
# Copyright 2010-2012 Tech-X Corporation.
# Arbitrary redistribution allowed provided this copyright remains.
#
##############################################################################
#
# User should include this macro definition file and use ADD_OSPL_DATAMODEL
# You must include the extension .idl in the name of the data file.  After
# the macro is executed successfully, the variable
#
#        ${datamodel_name}_OSPLDDS_DATAMODEL
#
# will contain a list of all generated files which should be added as source files
# to appropriate executables or libraries.
#
# Typical usage(Add the following to your CMakeLists.txt):
#
#    # Generate DDS bindings from Chat.idl file
#    include(SciOSPLMacros)
#    ADD_OSPL_DATAMODEL(NAME CHAT_KA
#                       IDLS Chat.idl MoreIDLAllowed.idl
#                       IDL_PREFIX ${CMAKE_CURRENT_SOURCE_DIR}
#                       GENDIR "gen"
#    )
#
#    INCLUDE_DIRECTORIES(${INCLUDES} ${CMAKE_CURRENT_BINARY_DIR}/gen)
#
#    SET(CHATTER_KA_SRCS Chatter.cpp CheckStatus.cpp ${CHAT_KA_OSPLDDS_DATAMODEL})
#    ADD_EXECUTABLE(chat_ka_ospl ${CHATTER_KA_SRCS})
#
# TO-DOs:
#
#    * Better and more informative messages
#    * More comments?
#    * Better error handling/robustness.
#
##############################################################################



# Macro to create a list with all the generated source files for a given .idl filename
macro(DEFINE_OpenSplice_LANG_SOURCES lang idl)
        set(gensources)
        get_filename_component(icore ${idl} NAME_WE)
        if ("${lang}" STREQUAL "cpp")
                set(gensources ${gensources} ${icore}.cpp ${icore}.h)
                set(gensources ${gensources} ${icore}Dcps.cpp ${icore}Dcps.h)
                set(gensources ${gensources} ${icore}Dcps_impl.cpp ${icore}Dcps_impl.h)
                set(gensources ${gensources} ${icore}SplDcps.cpp ${icore}SplDcps.h)
                set(gensources ${gensources} ccpp_${icore}.h)
        elseif ("${lang}" STREQUAL "c")
                set(gensources ${gensources} ${icore}.h)
                set(gensources ${gensources} ${icore}Dcps.h)
                set(gensources ${gensources} ${icore}SacDcps.c ${icore}SacDcps.h)
                set(gensources ${gensources} ${icore}SplDcps.c ${icore}SplDcps.h)
        endif ()
endmacro(DEFINE_OpenSplice_LANG_SOURCES)

#####
# Macro:
#       ADD_OSPL_DATAMODEL(NAME datamodel_name  # Name of the data model
#                          IDLS idl1 idl2 ...   # A list of IDL files
#                                               # defining the data model
#                                               # can be referred by relative
#                                               # or absolute paths
#                          LANG [cpp|c|java]    # Only cpp and c are supported
#                                               # for now.  If not defined,
#                                               # cpp mapping will be used
#                          IDL_PREFIX           # Optional IDL path prefix
#                                               # No trailing slash(fix me)
#                          IDL_INCLUDES p1 p2.. # Optional list of IDL include
#                                               # paths
#                          GENDIR path          # Optional directory for
#                                               # generated files
#                                               # No trailing slash(fix me)
#                          GENERATED_IDLS       # IDL files are generated at run time
#                                               # Define this option to skip file
#                                               # existence checks
#       )
#
# Assumption:
#       This macro assumes the OpenSplice_IDLGEN_BINARY and OSPL_HOME
#       have been properly defined.  Cmake 2.8.3 or above is requires.
#
# Output:
#       The following variables are defined after the
#       ADD_OSPL_DATAMODEL is executed successfully.
#       ${datamodel_name}_OSPLDDS_DATAMODEL contains all the generated
#       files(header and implementation files.)  Obviously, if GENDIR
#       option is defined, it needs to be "included" for compilation.
#
# bugs:
#       LANG is currently ignored.  assume we always deal with cpp mapping.
#       not a whole lot of error checkings now.

include(CMakeParseArguments)
macro(ADD_OSPL_DATAMODEL)
    CMAKE_PARSE_ARGUMENTS(TXADM
        "GENERATED_IDLS"
        "NAME;GENDIR;LANG;IDL_PREFIX"
        "IDLS;IDL_INCLUDES"
        ${ARGN}
        )

# We must have a target name, or else, no way to proceed
    if (DEFINED TXADM_NAME)
        message(INFO " Add datamodel: ${TXADM_NAME}")
    else ()
        message(FATAL_ERROR "ADD_OSPL_DATAMODEL: No datamodel name defined.")
    endif ()

# Check if a non-supported language mapping is used
    if (NOT TXADM_LANG)
        message("Use default cpp mapping for datamodel ${TXADM_NAME}")
        set(TXADM_LANG "cpp")
    endif ()
    if ((NOT "${TXADM_LANG}" STREQUAL "cpp") AND(NOT "${TXADM_LANG}" STREQUAL "c"))
       message(FATAL_ERROR "Langauage mapping for ${TXADM_LANG} is not supported by ADD_OSPL_DATAMODEL.  Use either cpp or c mappings.")
    else ()
       message("Language mapping for ${TXADM_NAME} is ${TXADM_LANG}")
    endif ()

# Go thru all the IDL files.  Prepend IDL_PREFIX if defined.(IDL_PREFIX must not end with '/').
# We can easily fix this.
    set(allidls)
    foreach (idl ${TXADM_IDLS})
        # check existance of all IDLS
        if (DEFINED TXADM_IDL_PREFIX)
            set(idl ${TXADM_IDL_PREFIX}/${idl})
        endif ()
        if (${TXADM_GENERATED_IDLS})
            set(allidls ${allidls} ${idl})
        elseif (EXISTS ${idl})
            message("Adding ${idl} to ${TXADM_NAME} datamodel")
            set(allidls ${allidls} ${idl})
        else ()
            message(WARNING "IDL file(${idl}) does not exist, skipped!")
        endif ()
    endforeach (idl)

    if ("${allidls}" STREQUAL "")
        message(FATAL_ERROR "ADD_OSPL_DATAMODEL: Must provide at least one valid datamodel IDL.")
    endif ()

    set(ospl_idlgen_include_dirs)
    if (DEFINED TXADM_IDL_INCLUDES)
        foreach (dir ${TXADM_IDL_INCLUDES})
            # @@ We should probably check if the dir actually exists and warn otherwise?
            set(ospl_idlgen_include_dirs ${ospl_idlgen_include_dirs} "-I${dir}")
        endforeach (dir)
    endif ()

    set(iall_ospldds_datamodel)
    foreach (idl ${allidls})
        get_filename_component(ipath ${idl} PATH)
        get_filename_component(iname ${idl} NAME)
        set(idlgen_args "-S" "-l" "${TXADM_LANG}" "-I${ipath}")
        DEFINE_OpenSplice_LANG_SOURCES(${TXADM_LANG} ${iname})
        if (DEFINED TXADM_GENDIR)
            set(idlgen_args ${idlgen_args} "-d" ${TXADM_GENDIR})
            set(igensources)
            foreach (igen ${gensources})
                set(igensources ${igensources} "${TXADM_GENDIR}/${igen}")
            endforeach (igen)
        else ()
            set(igensources ${gensources})
        endif ()
        ADD_CUSTOM_COMMAND(
                OUTPUT ${igensources}
                COMMAND ${OpenSplice_EXEC}
                ARGS ${OpenSplice_idlpp} ${idlgen_args} ${idl}
                DEPENDS ${idl}
        )
        set(iall_ospldds_datamodel ${iall_ospldds_datamodel} ${igensources})
    endforeach (idl)

    set(${TXADM_NAME}_OSPLDDS_DATAMODEL ${iall_ospldds_datamodel})

endmacro(ADD_OSPL_DATAMODEL)
