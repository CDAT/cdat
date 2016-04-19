set(VTK_SOURCE ${GIT_PROTOCOL}github.com/UV-CDAT/VTK.git )
set(VTK_MD5)
set(VTK_BRANCH uvcdat-2.4.1)
add_cdat_package_dependent(VTK "" "" ON "CDAT_BUILD_GRAPHICS" OFF)
