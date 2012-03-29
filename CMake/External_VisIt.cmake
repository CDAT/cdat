

#set(VisIt_source "/work/uv-cdat/visit")
#set(VisIt_binary "/work/uv-cdat/visit")
set(VisIt_source "${CMAKE_CURRENT_BINARY_DIR}/build/VisIt")
set(VisIt_binary "${CMAKE_CURRENT_BINARY_DIR}/build/VisIt")
#set(VisIt_install "${cdat_EXTERNALS}/VisIt")
set(VisIt_install "${CMAKE_INSTALL_PREFIX}")

if(QT_QMAKE_EXECUTABLE)
  get_filename_component(QT_BINARY_DIR ${QT_QMAKE_EXECUTABLE} PATH)
  get_filename_component(QT_ROOT ${QT_BINARY_DIR} PATH)
endif()

GET_FILENAME_COMPONENT(CMAKE_PATH_VAR ${CMAKE_COMMAND} PATH)

GET_FILENAME_COMPONENT(VISIT_C_COMPILER ${CMAKE_C_COMPILER} NAME)
GET_FILENAME_COMPONENT(VISIT_CXX_COMPILER ${CMAKE_CXX_COMPILER} NAME)

MACRO(DETERMINE_VISIT_ARCHITECTURE ARCH)
    IF(${CMAKE_SYSTEM_NAME} STREQUAL "Linux")
        IF(${CMAKE_SYSTEM_PROCESSOR} STREQUAL "ppc")
            SET(${ARCH} linux-ppc)
        ELSEIF(${CMAKE_SYSTEM_PROCESSOR} STREQUAL "ppc64")
            SET(${ARCH} linux-ppc64)
        ELSEIF(${CMAKE_SYSTEM_PROCESSOR} STREQUAL "x86_64")
            SET(${ARCH} linux-x86_64)
        ELSEIF(${CMAKE_SYSTEM_PROCESSOR} STREQUAL "ia64")
            SET(${ARCH} linux-ia64)
        ELSE(${CMAKE_SYSTEM_PROCESSOR} STREQUAL "ppc")
            SET(${ARCH} linux-intel)
        ENDIF(${CMAKE_SYSTEM_PROCESSOR} STREQUAL "ppc")
    ELSEIF(${CMAKE_SYSTEM_NAME} STREQUAL "AIX")
        IF($ENV{OBJECT_MODE} STREQUAL "32")
            SET(${ARCH} "ibm-aix-pwr")
        ELSE($ENV{OBJECT_MODE} STREQUAL "32")
            SET(${ARCH} "ibm-aix-pwr64")
        ENDIF($ENV{OBJECT_MODE} STREQUAL "32")
    ELSEIF(${CMAKE_SYSTEM_NAME} STREQUAL "Darwin")
        IF(${CMAKE_SYSTEM_PROCESSOR} STREQUAL "i386")
            EXECUTE_PROCESS(COMMAND uname -r
               OUTPUT_STRIP_TRAILING_WHITESPACE
               OUTPUT_VARIABLE _OSX_VERSION)
            STRING(SUBSTRING ${_OSX_VERSION} 0 1 _OSX_MAJOR_VERSION)
            IF(${_OSX_MAJOR_VERSION} STREQUAL "1")
                # This will match 10, 11, 12, ...
                SET(${ARCH} darwin-x86_64)
            ELSE(${_OSX_MAJOR_VERSION} STREQUAL "1")
                SET(${ARCH} darwin-i386)
            ENDIF(${_OSX_MAJOR_VERSION} STREQUAL "1")
        ELSE(${CMAKE_SYSTEM_PROCESSOR} STREQUAL "i386")
            SET(${ARCH} darwin-ppc)
        ENDIF(${CMAKE_SYSTEM_PROCESSOR} STREQUAL "i386")
    ELSEIF(${CMAKE_SYSTEM_NAME} STREQUAL "FreeBSD")
        SET(${ARCH} "freebsd-${CMAKE_SYSTEM_VERSION}")
    ELSEIF(${CMAKE_SYSTEM_NAME} STREQUAL "IRIX")
        SET(${ARCH} sgi-irix6-mips2)
    ELSEIF(${CMAKE_SYSTEM_NAME} STREQUAL "SunOS")
        SET(${ARCH} "sun4-${CMAKE_SYSTEM_VERSION}-sparc")
    ELSEIF(${CMAKE_SYSTEM_NAME} STREQUAL "Tru64")
        SET(${ARCH} dec-osf1-alpha)
    ELSE(${CMAKE_SYSTEM_NAME} STREQUAL "Linux")
        # Unhandled case. Make up a string.
        SET(VISITARCHTMP "${CMAKE_SYSTEM_NAME}-${CMAKE_SYSTEM_PROCESSOR}")
        STRING(TOLOWER ${VISITARCHTMP} ${ARCH})
    ENDIF(${CMAKE_SYSTEM_NAME} STREQUAL "Linux")
ENDMACRO(DETERMINE_VISIT_ARCHITECTURE ARCH)

DETERMINE_VISIT_ARCHITECTURE(VISIT_INSTALL_PLATFORM)
#SET(VISIT_HOSTNAME $ENV{HOSTNAME})
SITE_NAME(VISIT_HOSTNAME)
SET(TMP_STR1 "FILE(APPEND ${VisIt_source}/${VISIT_HOSTNAME}.cmake \"VISIT_OPTION_DEFAULT(VISIT_HDF5_DIR ${cdat_EXTERNALS})\\n\")\n")
SET(TMP_STR2 "FILE(APPEND ${VisIt_source}/${VISIT_HOSTNAME}.cmake \"VISIT_OPTION_DEFAULT(VISIT_NETCDF_DIR ${cdat_EXTERNALS})\\n\")\n")
#SET(TMP_STR3 "FILE(APPEND ${VisIt_source}/${VISIT_HOSTNAME}.cmake \"VISIT_OPTION_DEFAULT(VISIT_C_COMPILER ${CMAKE_C_COMPILER} TYPE FILEPATH)\\n\")\n")
#SET(TMP_STR4 "FILE(APPEND ${VisIt_source}/${VISIT_HOSTNAME}.cmake \"VISIT_OPTION_DEFAULT(VISIT_CXX_COMPILER ${CMAKE_CXX_COMPILER} TYPE FILEPATH)\\n\")\n")
SET(TMP_STR3 "FILE(APPEND ${VisIt_source}/${VISIT_HOSTNAME}.cmake \"VISIT_OPTION_DEFAULT(HAVE_PYQT ON TYPE BOOL)\\n\")\n")
SET(TMP_STR4 "FILE(APPEND ${VisIt_source}/${VISIT_HOSTNAME}.cmake \"VISIT_OPTION_DEFAULT(VISIT_SIP_DIR ${CMAKE_INSTALL_PREFIX})\\n\")\n")

FILE(WRITE   ${CMAKE_CURRENT_BINARY_DIR}/visit.cmake ${TMP_STR1})
FILE(APPEND  ${CMAKE_CURRENT_BINARY_DIR}/visit.cmake ${TMP_STR2})
FILE(APPEND  ${CMAKE_CURRENT_BINARY_DIR}/visit.cmake ${TMP_STR3})
FILE(APPEND  ${CMAKE_CURRENT_BINARY_DIR}/visit.cmake ${TMP_STR4})
#FILE(APPEND  ${CMAKE_CURRENT_BINARY_DIR}/visit.cmake ${TMP_STR5})
#FILE(APPEND  ${CMAKE_CURRENT_BINARY_DIR}/visit.cmake ${TMP_STR6})
FILE(APPEND  ${CMAKE_CURRENT_BINARY_DIR}/visit.cmake "FILE(WRITE ${VisIt_source}/CMakeLists.txt \"cmake_minimum_required(VERSION 2.8)\\n\")\n")
FILE(APPEND  ${CMAKE_CURRENT_BINARY_DIR}/visit.cmake "FILE(APPEND ${VisIt_source}/CMakeLists.txt \"add_subdirectory(mangled_src)\\n\")\n")


FILE(WRITE ${CMAKE_CURRENT_BINARY_DIR}/visit_patch "cd ${VisIt_source} | echo yes | ${VisIt_source}/src/svn_bin/build_visit --gpl --console --cc ${VISIT_C_COMPILER} --cxx ${VISIT_CXX_COMPILER} --R --stdout --thirdparty-path ${VisIt_install}/thirdparty --vtk --mesa --cmake-bin-dir ${CMAKE_PATH_VAR} --alt-python-dir ${CMAKE_INSTALL_PREFIX} --alt-qt-dir ${QT_ROOT} --mangle-libraries --makeflags \"-j4\" --no-visit\n")
FILE(APPEND ${CMAKE_CURRENT_BINARY_DIR}/visit_patch "${CMAKE_COMMAND} -P ${CMAKE_CURRENT_BINARY_DIR}/visit.cmake\n")
FILE(APPEND  ${CMAKE_CURRENT_BINARY_DIR}/visit_patch "${CMAKE_COMMAND} -E  copy ${VisIt_source}/${VISIT_HOSTNAME}.cmake ${VisIt_source}/mangled_src/config-site/${VISIT_HOSTNAME}.cmake\n")
FILE(APPEND ${CMAKE_CURRENT_BINARY_DIR}/visit_patch "cd ${VisIt_source} | echo yes | ${VisIt_source}/src/svn_bin/build_visit --console --cc ${VISIT_C_COMPILER} --cxx ${VISIT_CXX_COMPILER} --gpl --R --stdout --thirdparty-path ${VisIt_install}/thirdparty --vtk --mesa --cmake-bin-dir ${CMAKE_PATH_VAR} --alt-python-dir ${CMAKE_INSTALL_PREFIX} --alt-qt-dir ${QT_ROOT} --mangle-libraries --makeflags \"-j4\" --no-hostconf\n")

#build
ExternalProject_Add(VisIt
  #DOWNLOAD_DIR ${VisIt_source} #${CMAKE_CURRENT_BINARY_DIR}
  SOURCE_DIR ${VisIt_source}
  BINARY_DIR ${VisIt_binary}
  INSTALL_DIR ${VisIt_install}
  #SVN_REPOSITORY ${VISIT_SVN}
  URL ${VISIT_URL}/${VISIT_GZ}
  #URL_MD5 ${VISIT_MD5}
  #PATCH_COMMAND ""
  PATCH_COMMAND bash ${CMAKE_CURRENT_BINARY_DIR}/visit_patch
  CMAKE_ARGS -DCMAKE_INSTALL_PREFIX=${VisIt_install}
  DEPENDS ${VisIt_DEPENDENCIES}
  ${EP_LOG_OPTIONS}
)

if(NOT EXISTS ${CMAKE_INSTALL_PREFIX}/lib)
  file(MAKE_DIRECTORY ${CMAKE_INSTALL_PREFIX}/lib)
endif()

#Make symlinks of VisIt's lib, plugins, 
#copy hdf5, netcdf
#move pyqt_pyqtviewer.so into python site-packages
ExternalProject_Add_Step(VisIt InstallVisItLibSymLink
  COMMAND ${CMAKE_COMMAND} -E create_symlink ${VisIt_install}/${VISIT_VERSION}/${VISIT_INSTALL_PLATFORM}/lib ${CMAKE_INSTALL_PREFIX}/lib/VisIt-${VISIT_VERSION}
  DEPENDEES install
  WORKING_DIRECTORY ${cdat_CMAKE_BINARY_DIR})

ExternalProject_Add_Step(VisIt InstallVisItPluginSymLink
  COMMAND ${CMAKE_COMMAND} -E create_symlink ${VisIt_install}/${VISIT_VERSION}/${VISIT_INSTALL_PLATFORM}/plugins ${CMAKE_INSTALL_PREFIX}/lib/VisIt-${VISIT_VERSION}-plugins
  DEPENDEES install
  WORKING_DIRECTORY ${cdat_CMAKE_BINARY_DIR})

FILE(WRITE ${CMAKE_CURRENT_BINARY_DIR}/visit_install_patch "MESSAGE(STATUS \"Executing VisIt post installation steps\")\n")
FILE(APPEND ${CMAKE_CURRENT_BINARY_DIR}/visit_install_patch "file(GLOB hdf5_files ${HDF5_install}/lib/libhdf5*${_LINK_LIBRARY_SUFFIX})\n")
FILE(APPEND ${CMAKE_CURRENT_BINARY_DIR}/visit_install_patch "file(COPY \${hdf5_files} DESTINATION ${CMAKE_INSTALL_PREFIX}/lib/VisIt-${VISIT_VERSION}/)\n")

FILE(APPEND ${CMAKE_CURRENT_BINARY_DIR}/visit_install_patch "file(GLOB netcdf_files ${netcdf_install}/lib/libnetcdf*${_LINK_LIBRARY_SUFFIX})\n")
FILE(APPEND ${CMAKE_CURRENT_BINARY_DIR}/visit_install_patch "file(COPY \${netcdf_files} DESTINATION ${CMAKE_INSTALL_PREFIX}/lib/VisIt-${VISIT_VERSION}/)\n")

FILE(APPEND ${CMAKE_CURRENT_BINARY_DIR}/visit_install_patch "file(GLOB z_files ${zlib_install}/lib/libz*${_LINK_LIBRARY_SUFFIX})\n")
FILE(APPEND ${CMAKE_CURRENT_BINARY_DIR}/visit_install_patch "file(COPY \${z_files} DESTINATION ${CMAKE_INSTALL_PREFIX}/lib/VisIt-${VISIT_VERSION}/)\n")

FILE(APPEND ${CMAKE_CURRENT_BINARY_DIR}/visit_install_patch "file(GLOB curl_files ${curl_install}/lib/libcurl*${_LINK_LIBRARY_SUFFIX})\n")
FILE(APPEND ${CMAKE_CURRENT_BINARY_DIR}/visit_install_patch "file(COPY \${curl_files} DESTINATION ${CMAKE_INSTALL_PREFIX}/lib/VisIt-${VISIT_VERSION}/)\n")


ExternalProject_Add_Step(VisIt InstallVisItExternalLibraries
  COMMAND ${CMAKE_COMMAND} -P ${CMAKE_CURRENT_BINARY_DIR}/visit_install_patch
  DEPENDEES InstallVisItLibSymLink
  WORKING_DIRECTORY ${cdat_CMAKE_BINARY_DIR}
  )

ExternalProject_Add_Step(VisIt InstallVisItPyQtViewer
  COMMAND ${CMAKE_COMMAND} -E copy ${VisIt_install}/${VISIT_VERSION}/${VISIT_INSTALL_PLATFORM}/lib/pyqt_pyqtviewer.so ${PYTHON_SITE_PACKAGES}
  DEPENDEES install
  WORKING_DIRECTORY ${cdat_CMAKE_BINARY_DIR})

ExternalProject_Add_Step(VisIt InstallVisItModule
  COMMAND ${CMAKE_COMMAND} -E copy ${VisIt_install}/${VISIT_VERSION}/${VISIT_INSTALL_PLATFORM}/lib/visit.so ${PYTHON_SITE_PACKAGES}
  DEPENDEES install
  WORKING_DIRECTORY ${cdat_CMAKE_BINARY_DIR})

