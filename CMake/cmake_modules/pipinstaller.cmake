# create an external project to install MyProxyClient,
# and configure and build it

include(${cdat_CMAKE_BINARY_DIR}/cdat_common_environment.cmake)
string(TOUPPER ${nm} uc_nm)

if(NOPREFIXOPT )
    set(PRFX )
else()
    set(PRFX --prefix=${PYTHONUSERBASE})
endif()

if(DEFINED GIT_CMD_STR )
    message("[INFO] [PIP] Installing ${nm} from ${GIT_CMD_STR}")
    include(GetGitRevisionDescription)
    set(URL_STR )
    set(URL_MD5_STR )
else()
    message("[INFO] [PIP] Installing ${nm} from tarball")
    set(URL_STR URL ${${uc_nm}_SOURCE})
    set(URL_MD5_STR URL_MD5 ${${uc_nm}_MD5})
    set(GIT_CMD_STR )
    set(GIT_TAG )
endif()

if (DEFINED OLD AND NOT OLD)
  set(OLDSTR "")
else()
  set(OLDSTR "--old-and-unmanageable")
endif()

ExternalProject_Add(${nm}
  DOWNLOAD_DIR ${CDAT_PACKAGE_CACHE_DIR}
  SOURCE_DIR ${CMAKE_CURRENT_BINARY_DIR}/build/${nm}
  ${URL_STR}
  ${URL_MD5_STR}
  ${GIT_CMD_STR}
  ${GIT_TAG}
  BUILD_IN_SOURCE 1
  CONFIGURE_COMMAND ""
  BUILD_COMMAND ""
  INSTALL_COMMAND env LD_LIBRARY_PATH=$ENV{LD_LIBRARY_PATH} CFLAGS=$ENV{CFLAGS} LDFLAGS=$ENV{LDFLAGS} PKG_CONFIG_PATH=$ENV{PKG_CONFIG_PATH} PYTHONPATH=${PYTHONPATH} ${USR_ENVS} ${PYTHON_EXECUTABLE} setup.py install ${OLDSTR} ${USER_INSTALL_OPTIONS} ${PRFX}
  DEPENDS ${${nm}_deps}
  ${ep_log_options}
)

# Reset it
unset(GIT_CMD_STR )
