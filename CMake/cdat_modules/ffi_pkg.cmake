set( FFI_MAJOR 3  )
set( FFI_MINOR 1  )
set( FFI_PATCH 5  )
set(FFI_URL ${LLNL_URL})
set(FFI_BZ2 libffi-${FFI_MAJOR}.${FFI_MINOR}.tar.gz)
set(FFI_MD5 f5898b29bbfd70502831a212d9249d10)

set (nm FFI)
string(TOUPPER ${nm} uc_nm)
set(${uc_nm}_VERSION ${${nm}_MAJOR}.${${nm}_MINOR})
set(FFI_SOURCE ${FFI_URL}/${FFI_BZ2})

add_cdat_package_dependent(FFI "" "" ON "CDAT_BUILD_GRAPHICS" OFF)
