set(RPY2_MAJOR_SRC 2)
set(RPY2_MINOR_SRC 6)
set(RPY2_PATCH_SRC 0)

set(RPY2_VERSION ${RPY2_MAJOR_SRC}.${RPY2_MINOR_SRC}.${RPY2_PATCH_SRC})
set(RPY2_GZ rpy2-${RPY2_VERSION}.tar.gz)
set(RPY2_SOURCE https://pypi.python.org/packages/source/r/rpy2/${RPY2_GZ})
set(RPY2_MD5 679898fbc832d4f05a5efcf1a7eb1a68)

add_cdat_package_dependent(RPY2 "" "" OFF "NOT CDAT_BUILD_LEAN" OFF)
