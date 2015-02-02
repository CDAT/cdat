set(VISTRAILS_VERSION ${VISTRAILS_TAG_POINT})
set(VISTRAILS_SOURCE "${GIT_PROTOCOL}github.com/UV-CDAT/VisTrails.git")
set(VISTRAILS_VERSION uvcdat-2.1.0)
## Now tarball so faking md5 to tell checkout point
set(VISTRAILS_MD5 uvcdat-master)
add_cdat_package_dependent(vistrails "" "" ON "CDAT_BUILD_GUI" OFF)
