add_test(flake8_distarray
  "${FLAKE8_EXECUTABLE}" "${cdat_SOURCE_DIR}/Packages/distarray/Lib/"
  --show-source # Show context for detected errors
  --statistics  # Show summary of errors at end of output
)
