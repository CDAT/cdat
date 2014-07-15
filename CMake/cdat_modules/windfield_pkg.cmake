set(windfield_MAJOR )
set(windfield_MINOR )
set(windfield_VERSION 547534c636efc)
set(windfield_URL ${LLNL_URL} )
set(windfield_GZ windfield-${windfield_VERSION}.tar.bz2)
set(windfield_MD5 48989935760da881424b6adb2cb96f44 )
set(windfield_SOURCE ${windfield_URL}/${windfield_GZ})

add_cdat_package_dependent(windfield "" "" ON "CDAT_BUILD_WO_ESGF" OFF)
