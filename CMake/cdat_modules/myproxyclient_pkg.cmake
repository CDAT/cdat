set(MYPROXYCLIENT_MAJOR_SRC 1)
set(MYPROXYCLIENT_MINOR_SRC 3)
set(MYPROXYCLIENT_PATCH_SRC 0)

set (nm MYPROXYCLIENT)
string(TOUPPER ${nm} uc_nm)
set(${uc_nm}_VERSION ${${nm}_MAJOR_SRC}.${${nm}_MINOR_SRC}.${${nm}_PATCH_SRC})
add_cdat_package_dependent(MyProxyClient "" "" ON "CDAT_BUILD_WO_ESGF" OFF)
