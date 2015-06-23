add_test(flake8_WK
  "${FLAKE8_EXECUTABLE}" "${cdat_SOURCE_DIR}/Packages/WK/Lib/"
  --show-source # Show context for detected errors
  --statistics  # Show summary of errors at end of output
)
