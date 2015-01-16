set(HDF5_MAJOR_SRC 1)
set(HDF5_MINOR_SRC 8)
set(HDF5_PATCH_SRC 13)
set(HDF5_URL ${LLNL_URL})
set(HDF5_GZ hdf5-${HDF5_MAJOR_SRC}.${HDF5_MINOR_SRC}.${HDF5_PATCH_SRC}.tar.gz)
set(HDF5_MD5 c03426e9e77d7766944654280b467289)

set (nm HDF5)
string(TOUPPER ${nm} uc_nm)
set(${uc_nm}_VERSION ${${nm}_MAJOR_SRC}.${${nm}_MINOR_SRC}.${${nm}_PATCH_SRC})
set(HDF5_SOURCE ${HDF5_URL}/${HDF5_GZ})

add_cdat_package(HDF5 "" "" ON)
