set(FFMPEG_MAJOR_SRC 2)
set(FFMPEG_MINOR_SRC 7)
set(FFMPEG_PATCH_SRC 0)
set(FFMPEG_URL ${LLNL_URL})
set(FFMPEG_GZ ffmpeg-${FFMPEG_MAJOR_SRC}.${FFMPEG_MINOR_SRC}.tar.gz)
set(FFMPEG_MD5 3ad0554981faf2c6deef23a1cd4c8c57)

set (nm FFMPEG)
string(TOUPPER ${nm} uc_nm)
set(${uc_nm}_VERSION ${${nm}_MAJOR_SRC}.${${nm}_MINOR_SRC})
set(FFMPEG_SOURCE ${FFMPEG_URL}/${FFMPEG_GZ})

add_cdat_package_dependent(FFMPEG "" "" ON "CDAT_BUILD_GRAPHICS" OFF) 
