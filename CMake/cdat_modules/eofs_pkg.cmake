set(eofs_MAJOR )
set(eofs_MINOR )
set(eofs_VERSION 129f135d7a)
set(eofs_URL ${LLNL_URL} )
set(eofs_GZ eofs-${eofs_VERSION}.zip)
set(eofs_MD5 e5208c69b0edbbfec6558bc72604838d )
set(eofs_SOURCE ${eofs_URL}/${eofs_GZ})

add_cdat_package_dependent(eofs "" "" ON "CDAT_BUILD_WO_ESGF" OFF)
