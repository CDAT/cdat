set(VTK_SOURCE ${GIT_PROTOCOL}github.com/UV-CDAT/VTK.git )

add_cdat_package_dependent(VTK "" "" ON "CDAT_BUILD_GRAPHICS" OFF)
