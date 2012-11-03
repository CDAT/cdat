set(UUID_MAJOR_SRC 1)
set(UUID_MINOR_SRC 6)
set(UUID_PATCH_SRC 2)
set(UUID_URL ${LLNL_URL})
set(UUID_GZ uuid-${UUID_MAJOR_SRC}.${UUID_MINOR_SRC}.${UUID_PATCH_SRC}.tar.gz)
set(UUID_MD5 5db0d43a9022a6ebbbc25337ae28942f)

add_cdat_package(uuid "" "" "" "")
