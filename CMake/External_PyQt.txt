
# PyQt
#
set(proj PyQt)

ExternalProject_Add(${proj}
  URL ${PYQT_URL}
  URL_MD5 ${PYQT_MD5}
  BUILD_IN_SOURCE 1
  CONFIGURE_COMMAND ${CMAKE_COMMAND}
    -Dprefix=${prefix}
    -DCSE_PYTHON_NAME=${CSE_PYTHON_NAME}
    -DCSE_PYTHON_VERSIONXX=${CSE_PYTHON_VERSIONXX}
    -Dpython=${CSE_PYTHON_HOME}
    -Dsip=${CSE_SIP_HOME}
    -Dqt=${CSE_QT_HOME}
    -DCC=${CMAKE_C_COMPILER}
    -DCXX=${CMAKE_CXX_COMPILER}
    -P ${cfg}
  DEPENDS ${PyQt_DEPENDENCIES}
  )

ExternalProject_Add_Step(${proj} "check-configure"
   COMMENT "Check ${cfg}"
   DEPENDERS configure
   DEPENDS ${cfg}
)

#split_version(${version} version_major version_minor version_patch)
#set(CSE_PYQT_HOME "${prefix}" PARENT_SCOPE)
#set(CSE_PYQT_PACKAGE "${name}-${version}" PARENT_SCOPE)
#set(CSE_PYQT_NAME "${name}" PARENT_SCOPE)
#set(CSE_PYQT_VERSIONXX "${version_major}.${version_minor}" PARENT_SCOPE)
