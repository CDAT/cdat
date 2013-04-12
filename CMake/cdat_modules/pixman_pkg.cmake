set(PIX_MAJOR 0)
set(PIX_MINOR 22)
set(PIX_PATCH 2)
set(PIX_MAJOR_SRC 0)
set(PIX_MINOR_SRC 28)
set(PIX_PATCH_SRC 2)
set(PIX_URL ${LLNL_URL})
set(PIX_GZ pixman-${PIX_MAJOR_SRC}.${PIX_MINOR_SRC}.${PIX_PATCH_SRC}.tar.gz)
set(PIX_MD5 f68916a612921c24e5f94f1eae71d121 )


set (nm PIX)
string(TOUPPER ${nm} uc_nm)
set(${uc_nm}_VERSION ${${nm}_MAJOR_SRC}.${${nm}_MINOR_SRC}.${${nm}_PATCH_SRC})
set(PIXMAN_VERSION ${PIX_VERSION})

add_cdat_package_dependent(pixman "" "" ON "CDAT_BUILD_GRAPHICS" OFF)
