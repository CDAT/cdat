set(R_MAJOR_SRC 2)
set(R_MINOR_SRC 15)
set(R_PATCH_SRC 1)
set(R_URL ${LLNL_URL})
set(R_GZ R-${R_MAJOR_SRC}.${R_MINOR_SRC}.${R_PATCH_SRC}.tar.gz)
set(R_MD5 fcdf247e707fdade82b78bcf911a54f1)

add_cdat_package(R "" "Build R" ON)
