option(CDAT_BUILD_ESMF_PARALLEL "Build parallel version of Earth System Modeling Framework library" ON)
set(ESMP_deps ${pkgconfig_pkg} ${numpy_pkg} ${python_pkg} ${esmf_pkg})
