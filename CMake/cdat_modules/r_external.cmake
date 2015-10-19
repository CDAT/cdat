
set(R_source "${CMAKE_CURRENT_BINARY_DIR}/build/R")
set(R_install "${cdat_EXTERNALS}")
if (APPLE)
    message("[INFO] Building R without X support for MacOS")
    set(WITHX "no")
    set(WITH_AQUA "yes")
else ()
    set(WITHX "yes")
    set(WITH_AQUA "no")
endif()

if (CDAT_BUILD_PARALLEL)
  message([INFO] Enabling openmp for R)
  set(R_OPENMP "--enable-openmp")
else ()
  message([INFO] Disabling openmp for R)
  set(R_OPENMP "--disable-openmp")
endif ()

list(APPEND USR_ENVS
  "CPPFLAGS=-I${cdat_EXTERNALS}/include $ENV{CPPFLAGS}"
  "LDFLAGS=-L${cdat_EXTERNALS}/lib"
  )
ExternalProject_Add(R
  DOWNLOAD_DIR ${CDAT_PACKAGE_CACHE_DIR}
  SOURCE_DIR ${R_source}
  INSTALL_DIR ${R_install}
  URL ${R_URL}/${R_GZ}
  URL_MD5 ${R_MD5}
  BUILD_IN_SOURCE 1
  PATCH_COMMAND ""
  DEPENDS ${R_deps}
  CONFIGURE_COMMAND env ${USR_ENVS} <SOURCE_DIR>/configure --prefix=<INSTALL_DIR> LIBnn=lib --without-jpeglib --disable-R-framework --enable-R-shlib ${R_OPENMP} --without-cairo --without-ICU --without-system-xz --with-aqua=${WITH_AQUA} --without-tcltk --with-x=${WITHX}
  INSTALL_COMMAND ${CMAKE_MAKE_PROGRAM}  install
  ${ep_log_options}
)
if(APPLE)
    #change id and then change dependencies.. 
    ExternalProject_Add_Step(R InstallNameToolR 
        COMMAND install_name_tool -id ${R_install}/lib/R/lib/libR.dylib ${R_install}/lib/R/lib/libR.dylib 
        COMMAND install_name_tool -id ${R_install}/lib/R/lib/libRblas.dylib ${R_install}/lib/R/lib/libRblas.dylib 
        COMMAND install_name_tool -id ${R_install}/lib/R/lib/libRlapack.dylib ${R_install}/lib/R/lib/libRlapack.dylib 
        COMMAND install_name_tool -change libRblas.dylib ${R_install}/lib/R/lib/libRblas.dylib ${R_install}/lib/R/lib/libR.dylib 
        COMMAND install_name_tool -change libR.dylib ${R_install}/lib/R/lib/libR.dylib -change libRblas.dylib ${R_install}/lib/R/lib/libRblas.dylib ${R_install}//lib/R/lib/libRlapack.dylib 
        DEPENDEES install 
        WORKING_DIRECTORY ${cdat_CMAKE_BINARY_DIR}) 
endif(APPLE)

set(R_DIR "${R_binary}" CACHE PATH "R binary directory" FORCE)
mark_as_advanced(R_DIR)
