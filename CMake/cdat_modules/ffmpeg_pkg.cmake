set(FFMPEG_MAJOR_SRC 0)
set(FFMPEG_MINOR_SRC 11)
set(FFMPEG_PATCH_SRC 1)
set(FFMPEG_URL ${LLNL_URL})
set(FFMPEG_GZ ffmpeg-${FFMPEG_MAJOR_SRC}.${FFMPEG_MINOR_SRC}.${FFMPEG_PATCH_SRC}.tar.gz)
set(FFMPEG_MD5 bded7c1e03c5b11a1d50fa1b5f07c653 )

set (nm FFMPEG)
string(TOUPPER ${nm} uc_nm)
set(${uc_nm}_VERSION ${${nm}_MAJOR_SRC}.${${nm}_MINOR_SRC}.${${nm}_PATCH_SRC})
set(FFMPEG_SOURCE ${FFMPEG_URL}/${FFMPEG_GZ})

add_cdat_package_dependent(FFMPEG "" "" ON "CDAT_BUILD_GRAPHICS" OFF)

