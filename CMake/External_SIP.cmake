
# SIP
set(proj sip)

ExternalProject_Add(${proj}
  URL ${SIP_URL}/${SIP_GZ}
  URL_MD5 ${SIP_MD5}
  SOURCE_DIR ${proj}
  BUILD_IN_SOURCE 1
  CONFIGURE_COMMAND ${LIBRARY_PATH}=${PYTHON_LIBRARY_DIR}
     ${PYTHON_EXECUTABLE} configure.py
#    -b ${CMAKE_BINARY_DIR}/python-build/bin
#    -d ${CMAKE_BINARY_DIR}/python-build/bin/python
#    -e ${CMAKE_BINARY_DIR}/python-build/include
#    -v ${CMAKE_BINARY_DIR}/python-build/share
    CC=${CMAKE_C_COMPILER}
    CXX=${CMAKE_CXX_COMPILER}
  DEPENDS python
  )

#set(CSE_SIP_HOME ${prefix} PARENT_SCOPE)
#split_version(${version} version_major version_minor version_patch)
#set(CSE_SIP_PACKAGE "${name}-${version}" PARENT_SCOPE)
#set(CSE_SIP_NAME "${name}" PARENT_SCOPE)
#set(CSE_SIP_VERSIONXX "${version_major}.${version_minor}" PARENT_SCOPE)
