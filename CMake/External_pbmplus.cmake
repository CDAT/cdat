
set(pbmplus_source "${CMAKE_CURRENT_BINARY_DIR}/build/pbmplus")
set(pbmplus_install "${CMAKE_CURRENT_BINARY_DIR}/Externals")

#cp ../../exsrc/src/pbmplus/pbmplus.h . ; cp ../../exsrc/src/pbmplus/libpbm1.c pbm  ;cp ../../exsrc/src/pbmplus/Makefile .

set(EXTERNALS ${pbmplus_install})
configure_file(${cdat_external_patch_dir}/src/pbmplus/Makefile.in
  ${pbmplus_source}/Makefile
  @ONLY)

ExternalProject_Add(pbmplus
  DOWNLOAD_DIR ${CMAKE_CURRENT_BINARY_DIR}
  SOURCE_DIR ${pbmplus_source}
  INSTALL_DIR ${pbmplus_install}
  URL ${PBMPLUS_URL}/${PBMPLUS_GZ}
  URL_MD5 ${PBMPLUS_MD5}
  BUILD_IN_SOURCE 1
  PATCH_COMMAND ${CMAKE_COMMAND} -E copy_if_different ${cdat_external_patch_dir}/src/pbmplus/libpbm1.c ${pbmplus_source}/pbm/
  BUILD_COMMAND ${CMAKE_COMMAND} -DINSTALL_DIR=<INSTALL_DIR> -DWORKING_DIR=<SOURCE_DIR> -P ${cdat_CMAKE_BINARY_DIR}/cdat_make_step.cmake
  CONFIGURE_COMMAND ""
  DEPENDS ${pbmplus_DEPENDENCIES}
)

ExternalProject_Add_Step(pbmplus CopyPbmplusHeader
  COMMAND ${CMAKE_COMMAND} -E copy_if_different ${cdat_external_patch_dir}/src/pbmplus/pbmplus.h ${pbmplus_source}/
  DEPENDEES patch
  DEPENDERS configure
  )

#pbmplus install fails if this directory doesnt already exist.
file(MAKE_DIRECTORY ${pbmplus_install}/man/mann)
