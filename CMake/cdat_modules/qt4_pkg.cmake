if (CDAT_BUILD_GRAPHICS)
  find_package(Qt4 4.7.2 REQUIRED)

  if (CDAT_BUILD_GUI)
    if (NOT DEFINED QT_QTOPENGL_INCLUDE_DIR)
      message(FATAL_ERROR "QT_QTOPENGL_INCLUDE_DIR is not set but required")
    endif()
  endif()
endif()

