set(CMOR_VERSION 2.9.1)
set(CMOR_URL https://github.com/PCMDI/cmor/archive)
set(CMOR_GZ CMOR-${CMOR_VERSION}.tar.gz)
set(CMOR_MD5 3568dd5e42083494ff4b5b2b2b60ba39)
set (nm CMOR)
string(TOUPPER ${nm} uc_nm)

add_cdat_package_dependent(CMOR "" "" ON "CDAT_BUILD_CMOR" OFF)
