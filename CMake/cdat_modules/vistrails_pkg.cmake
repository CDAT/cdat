set(VISTRAILS_VERSION ${VISTRAILS_TAG_POINT})
set(VISTRAILS_SOURCE git://vistrails.org/vistrails.git)
## Now tarball so faking md5 to tell checkout point
set(VISTRAILS_MD5 uvcdat-master)
add_cdat_package_dependent(vistrails "" "" ON "CDAT_BUILD_GUI" OFF)
