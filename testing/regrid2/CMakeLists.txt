add_test(flake8_regrid2
  "${FLAKE8_EXECUTABLE}" "${cdat_SOURCE_DIR}/Packages/regrid2/Lib/"
  --show-source # Show context for detected errors
  --statistics  # Show summary of errors at end of output
)
