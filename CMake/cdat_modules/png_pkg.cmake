set(PNG_MAJOR 1)
set(PNG_MINOR 4)
set(PNG_PATCH 1)
set(PNG_MAJOR_SRC 1)
set(PNG_MINOR_SRC 5)
set(PNG_PATCH_SRC 1)
set(PNG_VERSION ${PNG_MAJOR_SRC}.${PNG_MINOR_SRC}.${PNG_PATCH_SRC})
set(PNG_URL ${LLNL_URL})
set(PNG_GZ libpng-${PNG_VERSION}.tar.gz)
set(PNG_MD5 220035f111ea045a51e290906025e8b5)
set(PNG_SOURCE ${PNG_URL}/${PNG_GZ})


# Turns out grib2 (therefore cdms2 needs it so dont turn this off
add_cdat_package(png "" "" "")
