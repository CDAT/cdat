set( H5PY_MAJOR 2 )
set( H5PY_MINOR 4 )
set( H5PY_PATCH 0 )
set( H5PY_VERSION ${H5PY_MAJOR}.${H5PY_MINOR}.${H5PY_PATCH} )
set( H5PY_URL ${LLNL_URL} )
set( H5PY_GZ h5py-${H5PY_VERSION}.tar.gz )
set( H5PY_MD5 80c9a94ae31f84885cc2ebe1323d6758)

set (nm H5PY)
string(TOUPPER ${nm} uc_nm)
set(${uc_nm}_VERSION ${${nm}_MAJOR}.${${nm}_MINOR}.${${nm}_PATCH})
set(H5PY_SOURCE ${H5PY_URL}/${H5PY_GZ})

add_cdat_package_dependent(h5py "" "" OFF "CDAT_BUILD_GRAPHICS" OFF)
