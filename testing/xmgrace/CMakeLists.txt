cdat_add_test(flake8_xmgrace
    flake8 "${cdat_BINARY_DIR}/xmgrace/Lib/"
  --show-source # Show context for detected errors
  --statistics  # Show summary of errors at end of output
  --max-line-length=128 # Max line 128 not 80
)
