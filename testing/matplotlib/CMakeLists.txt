cdat_add_test(matplotlib_verify_import
  "{PYTHON_EXECUTABLE}"
  ${cdat_SOURCE_DIR}/testing/matplotlib/test_matplotlib_import.py
)

if (CDAT_BUILD_BASEMAP)
  cdat_add_test(basemap_verify_import
    "{PYTHON_EXECUTABLE}"
    ${cdat_SOURCE_DIR}/testing/matplotlib/test_basemap_import.py
  )
endif()
