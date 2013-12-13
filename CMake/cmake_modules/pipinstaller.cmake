# create an external project to install MyProxyClient,
# and configure and build it

include(@cdat_CMAKE_BINARY_DIR@/cdat_common_environment.cmake)
string(TOUPPER ${nm} uc_nm)

if(NOPREFIXOPT )
    set(PRFX )
else()
    set(PRFX --prefix=${PYTHONUSERBASE})
endif()

if(DEFINED GIT_CMD_STR )
    message([INFO] getting ${nm} from git repo: ${GIT_CMD_STR})
    include(GetGitRevisionDescription)
    set(URL_STR )
    set(URL_MD5_STR )
else()
    message([INFO] getting ${nm} from tarball)
    set(URL_STR URL ${${uc_nm}_SOURCE})
    set(URL_MD5_STR URL_MD5 ${${uc_nm}_MD5})
    set(GIT_CMD_STR )
endif()

message([INFO] URL STRING IS: ${URL_STR})

ExternalProject_Add(${nm}
  DOWNLOAD_DIR ${CDAT_PACKAGE_CACHE_DIR}
  SOURCE_DIR ${CMAKE_CURRENT_BINARY_DIR}/build/${nm}
  ${URL_STR}
  ${URL_MD5_STR}
  ${GIT_CMD_STR}
  BUILD_IN_SOURCE 1
  CONFIGURE_COMMAND ""
  BUILD_COMMAND ""
  INSTALL_COMMAND env PYTHONPATH=${PYTHONPATH} ${USR_ENVS} ${PYTHON_EXECUTABLE} setup.py install ${PRFX}
  DEPENDS ${${nm}_deps}
  ${ep_log_options}
)

# Reset it
unset(GIT_CMD_STR )
