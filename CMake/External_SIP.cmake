
# SIP
set(proj sip)

ExternalProject_Add(${proj}
  URL ${SIP_URL}/${SIP_GZ}
  URL_MD5 ${SIP_MD5}
  BUILD_IN_SOURCE 1
  CONFIGURE_COMMAND ${PYTHON_EXECUTABLE} configure.py
#    -b ${prefix}/bin
#    -d ${PYTHON_SITE_PACKAGES}
#    -e ${prefix}/include
#    -v ${prefix}/share
    CC=${CMAKE_C_COMPILER}
    CXX=${CMAKE_CXX_COMPILER}
  BUILD_COMMAND make
  DEPENDS python
  )

#set(CSE_SIP_HOME ${prefix} PARENT_SCOPE)
#split_version(${version} version_major version_minor version_patch)
#set(CSE_SIP_PACKAGE "${name}-${version}" PARENT_SCOPE)
#set(CSE_SIP_NAME "${name}" PARENT_SCOPE)
#set(CSE_SIP_VERSIONXX "${version_major}.${version_minor}" PARENT_SCOPE)
