set(FT_MAJOR_SRC 2)
set(FT_MINOR_SRC 6)
set(FT_PATCH_SRC 3)
set(FT_URL ${LLNL_URL})
set(FT_GZ freetype-${FT_MAJOR_SRC}.${FT_MINOR_SRC}.${FT_PATCH_SRC}.tar.gz)
set(FT_MD5 8b0c80b64042b7c39b9fa9debe6305c3)

set (nm FT)
string(TOUPPER ${nm} uc_nm)
set(${uc_nm}_VERSION ${${nm}_MAJOR_SRC}.${${nm}_MINOR_SRC}.${${nm}_PATCH_SRC})
set(FREETYPE_VERSION ${FT_VERSION})
set(FREETYPE_SOURCE ${FT_URL}/${FT_GZ})

add_cdat_package_dependent(freetype "" "" OFF "CDAT_BUILD_GRAPHICS" OFF)
