set(CMOR_VERSION 2.9.2)
set(CMOR_URL https://github.com/PCMDI/cmor/archive)
set(CMOR_GZ CMOR-${CMOR_VERSION}.tar.gz)
set(CMOR_MD5 f7e78104878d93ffbd3cae2ccd95db2c)
set (nm CMOR)
string(TOUPPER ${nm} uc_nm)

add_cdat_package_dependent(CMOR "" "" ON "CDAT_BUILD_CMOR" OFF)
