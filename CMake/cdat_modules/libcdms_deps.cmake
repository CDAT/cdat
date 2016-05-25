set(libcdms_deps ${netcdf_pkg}
              ${jasper_pkg} ${g2clib_pkg} ${tiff_pkg}
              ${png_pkg} ${jpeg_pkg} )
if (CDAT_BUILD_LIBDRS)
    message("[INFO] ADDING LIBDRS TO LIBCDMS DEPNDENCIES")
    LIST(APPEND libcdms_deps ${libdrs_pkg})
endif()
