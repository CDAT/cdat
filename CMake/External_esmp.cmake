
set(esmp_source "${CMAKE_CURRENT_BINARY_DIR}/build/ESMP")
set(PYTHON_VER "python${PYTHON_MAJOR}.${PYTHON_MINOR}")
set(esmp_install "${CMAKE_INSTALL_PREFIX}/lib/${PYTHON_VER}/site-packages/")

configure_file(${cdat_CMAKE_SOURCE_DIR}/esmp_install_step.cmake.in
  ${cdat_CMAKE_BINARY_DIR}/esmp_install_step.cmake
  @ONLY)
set(esmp_install_command ${CMAKE_COMMAND} -P ${cdat_CMAKE_BINARY_DIR}/esmp_install_step.cmake)

ExternalProject_Add(ESMP
  DOWNLOAD_DIR ${CMAKE_CURRENT_BINARY_DIR}
  SOURCE_DIR @esmp_source@
  INSTALL_DIR @esmp_install@
  URL ${ESMP_URL}/${ESMP_GZ}
#  URL_MD5 ${ESMP_MD5}
  URL_MD5 ""
  BUILD_IN_SOURCE 1
  PATCH_COMMAND ""
  CONFIGURE_COMMAND ""
  BUILD_COMMAND ""
  INSTALL_COMMAND ${esmp_install_command}
  DEPENDS ${esmp_DEPENDENCIES}
  ${EP_LOG_OPTIONS}
)

## Create a wrapper file for importing ESMP which contains an automatically 
## generated ESMFMKFILE environment variable
## There could be a problem if multiple esmf.mk exist. This will just pick the
## first one.
#set(txesmp_file ${esmp_install}/ESMP/txesmp.py)
#file(GLOB_RECURSE item ${cdat_EXTERNALS}/lib/libO/*)
#foreach(f in ${item})
#  if(f MATCHES ".+esmf.mk")
#    set(esmfmkfile ${f})
#  endif(f MATCHES ".+esmf.mk")
#endforeach(f in ${item})
#file(WRITE ${txesmp_file} "import os\n"
#                       "os.environ["ESMKFILE"]='${esmfmkfile}'\n"
#                       "import ESMP\n")
#file(GLOB test ${txesmp_file})
#message("GLOB test "${test})
#message(${txesmp_file})
