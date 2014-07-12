set(windspharm_MAJOR )
set(windspharm_MINOR )
set(windspharm_VERSION 76a47fca1a)
set(windspharm_URL ${LLNL_URL} )
set(windspharm_GZ windspharm-${windspharm_VERSION}.zip)
set(windspharm_MD5 8456da340724d332955f2ec946204cad)
set(windspharm_SOURCE ${windspharm_URL}/${windspharm_GZ})

add_cdat_package_dependent(windspharm "" "" ON "CDAT_BUILD_WO_ESGF" OFF)
