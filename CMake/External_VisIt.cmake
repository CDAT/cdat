

set(VisIt_source "${CMAKE_CURRENT_BINARY_DIR}/build/VisIt")
set(VisIt_binary "${CMAKE_CURRENT_BINARY_DIR}/build/VisIt-build")
set(VisIt_install "${cdat_EXTERNALS}/VisIt")

if(QT_QMAKE_EXECUTABLE)
  get_filename_component(QT_BINARY_DIR ${QT_QMAKE_EXECUTABLE} PATH)
  get_filename_component(QT_ROOT ${QT_BINARY_DIR} PATH)
endif()

GET_FILENAME_COMPONENT(CMAKE_PATH_VAR ${CMAKE_COMMAND} PATH)
#SET(VISIT_HOSTNAME $ENV{HOST})
SET(VISIT_HOSTNAME "starsky")
SET(TMP_STR1 "FILE(APPEND ${VisIt_source}/${VISIT_HOSTNAME}.cmake \"VISIT_OPTION_DEFAULT(VISIT_QT_BIN ${QT_BINARY_DIR})\\n\")\n")
SET(TMP_STR2 "FILE(APPEND ${VisIt_source}/${VISIT_HOSTNAME}.cmake \"VISIT_OPTION_DEFAULT(PYTHON_INCLUDE ${PYTHON_INCLUDE})\\n\")\n")
SET(TMP_STR3 "FILE(APPEND ${VisIt_source}/${VISIT_HOSTNAME}.cmake \"VISIT_OPTION_DEFAULT(PYTHON_INCLUDE_PATH ${PYTHON_INCLUDE})\\n\")\n")
SET(TMP_STR4 "FILE(APPEND ${VisIt_source}/${VISIT_HOSTNAME}.cmake \"VISIT_OPTION_DEFAULT(PYTHON_LIBRARY ${PYTHON_LIBRARY})\\n\")\n")
SET(TMP_STR5 "FILE(APPEND ${VisIt_source}/${VISIT_HOSTNAME}.cmake \"VISIT_OPTION_DEFAULT(PYTHON_LIBRARY_DIR ${PYTHON_LIBRARY_DIR})\\n\")\n")
SET(TMP_STR6 "FILE(APPEND ${VisIt_source}/${VISIT_HOSTNAME}.cmake \"VISIT_OPTION_DEFAULT(PYTHON_LIBRARIES ${PYTHON_LIBRARY_DIR})\\n\")\n")
SET(TMP_STR7 "FILE(APPEND ${VisIt_source}/${VISIT_HOSTNAME}.cmake \"VISIT_OPTION_DEFAULT(VISIT_HDF5_DIR ${cdat_EXTERNALS})\\n\")\n")
SET(TMP_STR8 "FILE(APPEND ${VisIt_source}/${VISIT_HOSTNAME}.cmake \"VISIT_OPTION_DEFAULT(VISIT_NETCDF_DIR ${cdat_EXTERNALS})\\n\")\n")
SET(TMP_STR9 "FILE(APPEND ${VisIt_source}/${VISIT_HOSTNAME}.cmake \"VISIT_OPTION_DEFAULT(VISIT_C_COMPILER ${CMAKE_C_COMPILER} TYPE FILEPATH)\\n\")\n")
SET(TMP_STR10 "FILE(APPEND ${VisIt_source}/${VISIT_HOSTNAME}.cmake \"VISIT_OPTION_DEFAULT(VISIT_CXX_COMPILER ${CMAKE_CXX_COMPILER} TYPE FILEPATH)\\n\")\n")

FILE(WRITE   ${CMAKE_CURRENT_BINARY_DIR}/visit_patch_1 ${TMP_STR1})
FILE(APPEND  ${CMAKE_CURRENT_BINARY_DIR}/visit_patch_1 ${TMP_STR2})
FILE(APPEND  ${CMAKE_CURRENT_BINARY_DIR}/visit_patch_1 ${TMP_STR3})
FILE(APPEND  ${CMAKE_CURRENT_BINARY_DIR}/visit_patch_1 ${TMP_STR4})
FILE(APPEND  ${CMAKE_CURRENT_BINARY_DIR}/visit_patch_1 ${TMP_STR5})
FILE(APPEND  ${CMAKE_CURRENT_BINARY_DIR}/visit_patch_1 ${TMP_STR6})
FILE(APPEND  ${CMAKE_CURRENT_BINARY_DIR}/visit_patch_1 ${TMP_STR7})
FILE(APPEND  ${CMAKE_CURRENT_BINARY_DIR}/visit_patch_1 ${TMP_STR8})
FILE(APPEND  ${CMAKE_CURRENT_BINARY_DIR}/visit_patch_1 ${TMP_STR9})
FILE(APPEND  ${CMAKE_CURRENT_BINARY_DIR}/visit_patch_1 ${TMP_STR10})
FILE(APPEND  ${CMAKE_CURRENT_BINARY_DIR}/visit_patch_1 "FILE(WRITE ${VisIt_source}/CMakeLists.txt \"cmake_minimum_required(VERSION 2.8)\\n\")\n")
FILE(APPEND  ${CMAKE_CURRENT_BINARY_DIR}/visit_patch_1 "FILE(APPEND ${VisIt_source}/CMakeLists.txt \"add_subdirectory(src)\\n\")\n")

#FILE(WRITE ${CMAKE_CURRENT_BINARY_DIR}/visit_patch "#!/bin/bash\n")
FILE(WRITE ${CMAKE_CURRENT_BINARY_DIR}/visit_patch
"sed -e s/\"export PYTHON_COMPATIBILITY.*\"/\"export PYTHON_COMPATIBILITY_VERSION=\"${PYVER}\"\"/g ${VisIt_source}/src/svn_bin/build_visit > ${CMAKE_CURRENT_BINARY_DIR}/tmp_visit_out && mv ${CMAKE_CURRENT_BINARY_DIR}/tmp_visit_out ${VisIt_source}/src/svn_bin/build_visit && chmod 700 ${VisIt_source}/src/svn_bin/build_visit\n")
FILE(APPEND ${CMAKE_CURRENT_BINARY_DIR}/visit_patch "echo yes | env CMAKE_INSTALL=${CMAKE_PATH_VAR} VISIT_PYTHON_DIR=${CMAKE_INSTALL_PREFIX} ${VisIt_source}/src/svn_bin/build_visit --console --stdout --thirdparty-path ${VisIt_install}/thirdparty --no-visit --no-thirdparty --vtk --mesa --makeflags \"-j2\"\n")
FILE(APPEND ${CMAKE_CURRENT_BINARY_DIR}/visit_patch "${CMAKE_COMMAND} -E  create_symlink ${QT_ROOT} ${VisIt_install}/thirdparty/qt\n")
FILE(APPEND ${CMAKE_CURRENT_BINARY_DIR}/visit_patch "${CMAKE_COMMAND} -P ${CMAKE_CURRENT_BINARY_DIR}/visit_patch_1\n")
#UPDATE_COMMAND puts the host filename in VisIt_source directory..
FILE(APPEND ${CMAKE_CURRENT_BINARY_DIR}/visit_patch "${CMAKE_COMMAND} -E  create_symlink ${VisIt_source}/${VISIT_HOSTNAME}.cmake ${VisIt_source}/src/config-site/${VISIT_HOSTNAME}.cmake\n")

#build
ExternalProject_Add(VisIt
  DOWNLOAD_DIR ${VisIt_source} #${CMAKE_CURRENT_BINARY_DIR}
  SOURCE_DIR ${VisIt_source}
  BINARY_DIR ${VisIt_binary}
  INSTALL_DIR ${VisIt_install}
  #SVN_REPOSITORY ${VISIT_SVN}
  URL ${VISIT_URL}/${VISIT_GZ}
  URL_MD5 ${VISIT_MD5}
  PATCH_COMMAND ""
  UPDATE_COMMAND bash ${CMAKE_CURRENT_BINARY_DIR}/visit_patch
  CMAKE_ARGS -DCMAKE_INSTALL_PREFIX=${VisIt_install}
  DEPENDS ${VisIt_DEPENDENCIES}
  ${EP_LOG_OPTIONS}
)

