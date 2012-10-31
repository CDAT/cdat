set(HDF5_MAJOR_SRC 1)
set(HDF5_MINOR_SRC 8)
set(HDF5_PATCH_SRC 8)
set(HDF5_URL ${LLNL_URL})
set(HDF5_GZ hdf5-${HDF5_MAJOR_SRC}.${HDF5_MINOR_SRC}.${HDF5_PATCH_SRC}.tar.gz)
set(HDF5_MD5 1196e668f5592bfb50d1de162eb16cff)

add_cdat_package(HDF5 "" "" "" "")
