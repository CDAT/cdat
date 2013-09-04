set(YASM_MAJOR_SRC 1)
set(YASM_MINOR_SRC 2)
set(YASM_PATCH_SRC 0)
set(YASM_URL ${LLNL_URL})
set(YASM_GZ yasm-${YASM_MAJOR_SRC}.${YASM_MINOR_SRC}.${YASM_PATCH_SRC}.tar.gz)
set(YASM_MD5 4cfc0686cf5350dd1305c4d905eb55a6)
set(YASM_SOURCE ${YASM_URL}/${YASM_GZ})

set (nm YASM)
string(TOUPPER ${nm} uc_nm)
set(${uc_nm}_VERSION ${${nm}_MAJOR_SRC}.${${nm}_MINOR_SRC}.${${nm}_PATCH_SRC})
add_cdat_package(YASM "" "" "")

