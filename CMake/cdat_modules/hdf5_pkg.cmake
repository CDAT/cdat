set(HDF5_MAJOR_SRC 1)
set(HDF5_MINOR_SRC 8)
set(HDF5_PATCH_SRC 16)
set(HDF5_URL ${LLNL_URL})
set(HDF5_GZ hdf5-${HDF5_MAJOR_SRC}.${HDF5_MINOR_SRC}.${HDF5_PATCH_SRC}.tar.gz)
set(HDF5_MD5 b8ed9a36ae142317f88b0c7ef4b9c618)

set (nm HDF5)
string(TOUPPER ${nm} uc_nm)
set(${uc_nm}_VERSION ${${nm}_MAJOR_SRC}.${${nm}_MINOR_SRC}.${${nm}_PATCH_SRC})
set(HDF5_SOURCE ${HDF5_URL}/${HDF5_GZ})

add_cdat_package(HDF5 "" "" ON)
