set(VTK_SOURCE ${GIT_PROTOCOL}github.com/UV-CDAT/VTK.git )
set(VTK_MD5)
set(VTK_BRANCH update_proj4_47_or_higher)
add_cdat_package_dependent(VTK "" "" ON "CDAT_BUILD_GRAPHICS" OFF)
