add_test(flake8_xmgrace
  "${FLAKE8_EXECUTABLE}" "${cdat_SOURCE_DIR}/Packages/xmgrace/Lib/"
  --show-source # Show context for detected errors
  --statistics  # Show summary of errors at end of output
)
