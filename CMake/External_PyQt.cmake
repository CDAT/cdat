
# PyQt
#
set(PyQt_source "${CMAKE_CURRENT_BINARY_DIR}/build/PyQt")

if(APPLE)
  set(PyQt_configure_command ${PYTHON_EXECUTABLE} configure.py -q ${QT_QMAKE_EXECUTABLE} --confirm-license)
else()
  set(PyQt_configure_command ${LIBRARY_PATH}=${PYTHON_LIBRARY_DIR} ${PYTHON_EXECUTABLE} configure.py -q ${QT_QMAKE_EXECUTABLE} --confirm-license)
endif()

ExternalProject_Add(PyQt
  DOWNLOAD_DIR ${CMAKE_CURRENT_BINARY_DIR}
  SOURCE_DIR ${PyQt_source}
  URL ${PYQT_URL}/${PYQT_GZ_${CMAKE_PLATFORM}}
  URL_MD5 ${PYQT_MD5_${CMAKE_PLATFORM}}
  BUILD_IN_SOURCE 1
  CONFIGURE_COMMAND ${PyQt_configure_command}
  DEPENDS ${PyQt_DEPENDENCIES}
  )
