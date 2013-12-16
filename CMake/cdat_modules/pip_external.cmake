# create an external project to install MyProxyClient,
# and configure and build it
set(nm pip)

# create an external project to install MyProxyClient,
# and configure and build it

include(@cdat_CMAKE_BINARY_DIR@/cdat_common_environment.cmake)
string(TOUPPER ${nm} uc_nm)

ExternalProject_Add(${nm}
    DOWNLOAD_DIR ${CDAT_PACKAGE_CACHE_DIR}
    URL ${${uc_nm}_SOURCE}
    URL_MD5 ${${uc_nm}_MD5} 
    BUILD_IN_SOURCE 1
    CONFIGURE_COMMAND ""
    BUILD_COMMAND ""
    INSTALL_COMMAND ${EASY_INSTALL_BINARY} ${CDAT_PACKAGE_CACHE_DIR}/${${uc_nm}_GZ}
    DEPENDS ${${nm}_deps}
    ${ep_log_options}
    )
