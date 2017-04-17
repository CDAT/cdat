# unidata tests

add_test(flake8_unidata
  "${FLAKE8_EXECUTABLE}" "${cdat_SOURCE_DIR}/Packages/unidata/Lib/"
  --show-source # Show context for detected errors
  --statistics  # Show summary of errors at end of output
)

cdat_add_test(test_unidata_basic_functionalities
    "${PYTHON_EXECUTABLE}"
    ${cdat_SOURCE_DIR}/testing/unidata/test_udunits.py
    )
cdat_add_test(test_unidata_undunits2_functionalities
    "${PYTHON_EXECUTABLE}"
    ${cdat_SOURCE_DIR}/testing/unidata/test_udunits2_additions.py
    )
