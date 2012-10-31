set(JPEG_URL ${LLNL_URL})
set(JPEG_GZ jpegsrc.v8c.tar.gz)
set(JPEG_MD5 a2c10c04f396a9ce72894beb18b4e1f9)

add_cdat_package(jpeg "" "" "" "")
set(jpeg_deps ${pkgconfig_pkg})

