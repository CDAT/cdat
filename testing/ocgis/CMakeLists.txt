cdat_add_test(ocgis_verify_import
  "${PYTHON_EXECUTABLE}"
  ${cdat_SOURCE_DIR}/testing/ocgis/test_ocgis_import.py
  )
if (CDAT_DOWNLOAD_SAMPLE_DATA)
    cdat_add_test(ocgis_verify_cdms
      "${PYTHON_EXECUTABLE}"
      ${cdat_SOURCE_DIR}/testing/ocgis/test_ocgis_cdms.py
    )
endif()
