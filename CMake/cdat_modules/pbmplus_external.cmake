
set(pbmplus_source "${CMAKE_CURRENT_BINARY_DIR}/build/pbmplus")
set(pbmplus_install "${cdat_EXTERNALS}")

#cp ../../exsrc/src/pbmplus/pbmplus.h . ; cp ../../exsrc/src/pbmplus/libpbm1.c pbm  ;cp ../../exsrc/src/pbmplus/Makefile .

configure_file(${cdat_CMAKE_SOURCE_DIR}/cdat_modules_extra/pbmplus_configure_step.cmake.in
    ${CMAKE_CURRENT_BINARY_DIR}/pbmplus_configure_step.cmake
    @ONLY)

ExternalProject_Add(pbmplus
  DOWNLOAD_DIR ${CDAT_PACKAGE_CACHE_DIR}
  SOURCE_DIR ${pbmplus_source}
  INSTALL_DIR ${pbmplus_install}
  URL ${PBMPLUS_URL}/${PBMPLUS_GZ}
  URL_MD5 ${PBMPLUS_MD5}
  BUILD_IN_SOURCE 1
  PATCH_COMMAND ${CMAKE_COMMAND} -E copy_if_different ${cdat_external_patch_dir}/src/pbmplus/libpbm1.c ${pbmplus_source}/pbm/
  BUILD_COMMAND ${CMAKE_COMMAND} -Dmake=$(MAKE) -DWORKING_DIR=<SOURCE_DIR> -P ${cdat_CMAKE_BINARY_DIR}/cdat_make_step.cmake
  CONFIGURE_COMMAND ${CMAKE_COMMAND} -P ${CMAKE_CURRENT_BINARY_DIR}/pbmplus_configure_step.cmake
  DEPENDS ${pbmplus_deps}
  ${ep_log_options}
)

ExternalProject_Add_Step(pbmplus CopyPbmplusHeader
  COMMAND ${CMAKE_COMMAND} -E copy_if_different ${cdat_external_patch_dir}/src/pbmplus/pbmplus.h ${pbmplus_source}/
  DEPENDEES patch
  DEPENDERS configure
  )

#pbmplus install fails if this directory doesnt already exist.
file(MAKE_DIRECTORY ${pbmplus_install}/man/mann)
