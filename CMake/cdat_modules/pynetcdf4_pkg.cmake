set( pynetcdf4_MAJOR_SRC 1  )
set( pynetcdf4_MINOR_SRC 1 )
set( pynetcdf4_PATCH_SRC 9  )
set(pynetcdf4_URL ${LLNL_URL})
set(pynetcdf4_GZ
    Fiona-${pynetcdf4_MAJOR_SRC}.${pynetcdf4_MINOR_SRC}.${pynetcdf4_PATCH_SRC}.tar.gz)
set(pynetcdf4_MD5 40f945898c550721db715f69658cf7e9 )
set(pynetcdf4_SOURCE ${pynetcdf4_URL}/${pynetcdf4_GZ})

set (nm pynetcdf4)
string(TOUPPER ${nm} uc_nm)
set(${uc_nm}_VERSION ${${nm}_MAJOR_SRC}.${${nm}_MINOR_SRC}.${${nm}_PATCH_SRC})
add_cdat_package(pynetcdf4 "" "" ON)
