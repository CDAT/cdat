set(package OSMesa)
string(TOUPPER ${package} package_uc)

# We're using an older mesa (7.6.1) as it is known to work well in many
# supercomputing environments.
set(${package_uc}_MAJOR_SRC 7)
set(${package_uc}_MINOR_SRC 6)
set(${package_uc}_PATCH_SRC 1)
set(${package_uc}_VERSION "${${package_uc}_MAJOR_SRC}.${${package_uc}_MINOR_SRC}.${${package_uc}_PATCH_SRC}")
set(${package_uc}_URL
  "ftp://ftp.freedesktop.org/pub/mesa/older-versions/${${package_uc}_MAJOR_SRC}.x/${${package_uc}_VERSION}/")
set(${package_uc}_GZ "MesaLib-${${package_uc}_VERSION}.tar.gz")
set(${package_uc}_MD5 e80fabad2e3eb7990adae773d6aeacba)
set(${package_uc}_SOURCE "${${package_uc}_URL}/${${package_uc}_GZ}")

add_cdat_package(${package} "7.6.1" "" OFF)
