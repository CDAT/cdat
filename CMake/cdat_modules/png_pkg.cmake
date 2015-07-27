set(PNG_MAJOR 1)
set(PNG_MINOR 2)
set(PNG_PATCH 53)
set(PNG_MAJOR_SRC 1)
set(PNG_MINOR_SRC 2)
set(PNG_PATCH_SRC 53)
set(PNG_VERSION ${PNG_MAJOR_SRC}.${PNG_MINOR_SRC}.${PNG_PATCH_SRC})
set(PNG_URL ${LLNL_URL})
set(PNG_GZ libpng-${PNG_VERSION}.tar.gz)
set(PNG_MD5 da5bd79c8f0e1b4b77f54004ca44827c)
set(PNG_SOURCE ${PNG_URL}/${PNG_GZ})

# Turns out grib2 (therefore cdms2 needs it so dont turn this off
add_cdat_package(png "" "" ON)
