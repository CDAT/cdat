set(JPEG_URL ${LLNL_URL})
set(JPEG_GZ jpegsrc.v8c.tar.gz)
set(JPEG_MD5 a2c10c04f396a9ce72894beb18b4e1f9)

set(JPEG_VERSION v8c)
set(JPEG_SOURCE ${JPEG_URL}/${JPEG_GZ})

#grib2/jasper need this therefore cdms2 can't turn off
#if (CDAT_BUILD_GRAPHICS)
add_cdat_package(jpeg "" "" "")
#endif()

