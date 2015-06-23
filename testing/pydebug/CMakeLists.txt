add_test(flake8_pydebug
  "${FLAKE8_EXECUTABLE}" "${cdat_SOURCE_DIR}/Packages/pydebug/Lib/"
  --show-source # Show context for detected errors
  --statistics  # Show summary of errors at end of output
)
