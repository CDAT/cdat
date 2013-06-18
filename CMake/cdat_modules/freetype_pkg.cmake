set(FT_MAJOR 9)
set(FT_MINOR 7)
set(FT_PATCH 3)
set(FT_MAJOR_SRC 2)
set(FT_MINOR_SRC 4)
set(FT_PATCH_SRC 10)
set(FT_URL ${LLNL_URL})
set(FT_GZ freetype-${FT_MAJOR_SRC}.${FT_MINOR_SRC}.${FT_PATCH_SRC}.tar.gz)
set(FT_MD5 4b1887901730ff2e12562ef30fa521d5)

set (nm FT)
string(TOUPPER ${nm} uc_nm)
set(${uc_nm}_VERSION ${${nm}_MAJOR_SRC}.${${nm}_MINOR_SRC}.${${nm}_PATCH_SRC})
set(FREETYPE_VERSION ${FT_VERSION})
set(FREETYPE_SOURCE ${FT_URL}/${FT_GZ})


add_cdat_package_dependent(freetype "" "" ON "CDAT_BUILD_GRAPHICS" OFF)
