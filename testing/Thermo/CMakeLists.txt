cdat_add_test(flake8_Thermo
    flake8 "${cdat_BINARY_DIR}/thermo/Lib/"
  --show-source # Show context for detected errors
  --statistics  # Show summary of errors at end of output
  --max-line-length=120 # Reasonable line length
)
