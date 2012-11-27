set(VISIT_MAJOR 2)
set(VISIT_MINOR 6)
set(VISIT_PATCH 0)
set(VISIT_VERSION ${VISIT_MAJOR}.${VISIT_MINOR}.${VISIT_PATCH})
set(VISIT_URL http://vis.lbl.gov/~visit)
set(VISIT_GZ visit${VISIT_VERSION}.tar.gz)
set(VISIT_MD5 5cac82f543c5c1e5e6462a82a58db725)

add_cdat_package(VisIt "" "Build VisIt" ON)
