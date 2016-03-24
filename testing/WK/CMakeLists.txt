set(BASELINE_DIR "${UVCDAT_GIT_TESTDATA_DIR}/baselines/WK")

add_test(flake8_WK
  "${FLAKE8_EXECUTABLE}" "${cdat_SOURCE_DIR}/Packages/WK/Lib/"
  --show-source # Show context for detected errors
  --statistics  # Show summary of errors at end of output
)

cdat_add_test(test_wk_01
  "${PYTHON_EXECUTABLE}"
  ${cdat_SOURCE_DIR}/testing/WK/test_wk_01.py
  ${UVCDAT_GIT_TESTDATA_DIR}
  )

if (CDAT_DOWNLOAD_SAMPLE_DATA)
  cdat_add_test(test_wk_02
    "${PYTHON_EXECUTABLE}"
    ${cdat_SOURCE_DIR}/testing/WK/test_wk_02.py
    )
endif()
