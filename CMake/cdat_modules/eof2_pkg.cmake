set(eof2_MAJOR )
set(eof2_MINOR )
set(eof2_VERSION 620a921b46b)
set(eof2_URL ${LLNL_URL} )
set(eof2_GZ eof2-${eof2_VERSION}.zip)
set(eof2_MD5 39e21a8633f272dc8dc748adb4c7f0e8)
set(eof2_SOURCE ${eof2_URL}/${eof2_GZ})

add_cdat_package_dependent(eof2 "" "" ON "CDAT_BUILD_WO_ESGF" OFF)
