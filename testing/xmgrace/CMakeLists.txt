add_test(flake8_xmgrace
  "${FLAKE8_EXECUTABLE}" "${cdat_SOURCE_DIR}/Packages/xmgrace/Lib/ValidationFunctions.py"
  "${cdat_SOURCE_DIR}/Packages/xmgrace/Lib/xmgrace.py"
  --show-source # Show context for detected errors
  --statistics  # Show summary of errors at end of output
)
