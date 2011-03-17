
set(cmor_source "${CMAKE_CURRENT_SOURCE_DIR}/Packages/cmor")
set(cmor_install "${CMAKE_CURRENT_BINARY_DIR}/Externals")

#ifeq (@DOCMOR@,yes)
# @echo "Configuring CMOR2"
# @(cd Packages/cmor ; ./configure --prefix=@EXTERNALS@ --with-netcdf=@NC4LOC@ --with-udunits2=@UDUNITS2LOC@ --with-uuid=@UUIDLOC@ --with-zlib=@EXTERNALS@ --enable-fortran=no; cd ../..) >> @MAINDIR@/logs/cmor_config.LOG 
set(cmor_configure_args "--with-netcdf=${netcdf_install} --with-udunits2=${udunits_install} --with uuid= --with-zlib=${zlib_install} --enable-fortran=no")

ExternalProject_Add(cmor
  DOWNLOAD_DIR ""
  SOURCE_DIR ${freetype_source}
  INSTALL_DIR ${freetype_install}
  URL ${FT_URL}/${FT_GZ}
  URL_MD5 ${FT_MD5}
  BUILD_IN_SOURCE 1
  PATCH_COMMAND ""
  CONFIGURE_COMMAND ${CMAKE_COMMAND} -DINSTALL_DIR=<INSTALL_DIR> -DWORKING_DIR=<SOURCE_DIR> -DCONFIGURE_ARGS=${cmor_configure_args} -P ${cdat_CMAKE_BINARY_DIR}/cdat_configure_step.cmake
  DEPENDS ${freetype_DEPENDENCIES}
)

set(freetype_DIR "${freetype_binary}" CACHE PATH "freetype binary directory" FORCE)
mark_as_advanced(freetype_DIR)
