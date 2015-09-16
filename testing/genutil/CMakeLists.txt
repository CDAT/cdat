add_test(flake8_genutil
  "${FLAKE8_EXECUTABLE}" "${cdat_SOURCE_DIR}/Packages/genutil/Lib/"
  --show-source # Show context for detected errors
  --statistics  # Show summary of errors at end of output
)
