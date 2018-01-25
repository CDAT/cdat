cdat_add_test(flake8_xmgrace
    flake8 "${cdat_BINARY_DIR}/xmgrace/Lib/"
  --show-source # Show context for detected errors
  --statistics  # Show summary of errors at end of output
  --max-line-length=128 # Max line 128 not 80
  --ignore=F405,E305,F999,E121,E123,E126,E226,E24,E704 # recent version show zillions of errors if object come from an import * line
)
