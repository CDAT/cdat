set(X264_DATE 20151006)
set(X264_TIME 2245)
set(X264_ADDENDUM "")
set(X264_URL ${LLNL_URL})
set(X264_GZ x264-snapshot-${X264_DATE}-${X264_TIME}${X264_ADDENDUM}.tar.gz)
set(X264_MD5 e8f5a0fc8db878bcdd256715472fe379)

set (nm X264)
string(TOUPPER ${nm} uc_nm)
set(${uc_nm}_VERSION ${${nm}_DATE}.${${nm}_TIME})
set(X264_SOURCE ${X264_URL}/${X264_GZ})

add_cdat_package_dependent(X264 "" "" ON "CDAT_BUILD_GRAPHICS" OFF)
