set(jinja2_MAJOR_SRC 2)
set(jinja2_MINOR_SRC 7)
set(jinja2_PATCH_SRC )
set(JINJA2_VERSION ${jinja2_MAJOR_SRC}.${jinja2_MINOR_SRC})
set(JINJA2_GZ Jinja2-${JINJA2_VERSION}.tar.gz)
set(JINJA2_SOURCE ${LLNL_URL}/${JINJA2_GZ})
set(JINJA2_MD5 c2fb12cbbb523c57d3d15bfe4dc0e8fe )

add_cdat_package_dependent(jinja2 "" "" ON "CDAT_BUILD_GUI" OFF)
