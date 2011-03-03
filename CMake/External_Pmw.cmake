
set(Pmw_source "${CMAKE_CURRENT_BINARY_DIR}/Pmw")
set(Pmw_install "${CMAKE_CURRENT_BINARY_DIR}/Pmw-install")

ExternalProject_Add(Pmw
  DOWNLOAD_DIR ${CMAKE_CURRENT_BINARY_DIR}
  SOURCE_DIR ${Pmw_source}
  INSTALL_DIR ${Pmw_install}
  URL ${PMW_URL}/${PMW_GZ}
  URL_MD5 ${PMW_MD5}
  BUILD_IN_SOURCE 1
  PATCH_COMMAND ""
  CONFIGURE_COMMAND ${LIBRARY_PATH}=${PYTHON_LIBRARY_DIR} ${PYTHON_EXECUTABLE} setup.py
)

message("PMW_URL ${PMW_URL} ${PMW_GZ}")

set(Pmw_DIR "${Pmw_binary}" CACHE PATH "Pmw binary directory" FORCE)
mark_as_advanced(Pmw_DIR)
