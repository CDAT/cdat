set(UUID_MAJOR_SRC 1)
set(UUID_MINOR_SRC 6)
set(UUID_PATCH_SRC 2)
set(UUID_URL ${LLNL_URL})
set(UUID_GZ uuid-${UUID_MAJOR_SRC}.${UUID_MINOR_SRC}.${UUID_PATCH_SRC}.tar.gz)
set(UUID_MD5 5db0d43a9022a6ebbbc25337ae28942f)
set(UUID_SOURCE ${UUID_URL}/${UUID_GZ})

set (nm UUID)
string(TOUPPER ${nm} uc_nm)
set(${uc_nm}_VERSION ${${nm}_MAJOR_SRC}.${${nm}_MINOR_SRC}.${${nm}_PATCH_SRC})
#apparently libcf needs it
#add_cdat_package_dependent(uuid "" "" ON "CDAT_BUILD_WO_ESGF" OFF)
add_cdat_package(uuid "" "" "")

