cdat_add_test(pcmdi_verify_compilation_works
    cd77 -ezget -lats -cdms ${cdat_SOURCE_DIR}/testing/pcmdi/test_all_pcmdi_libs.f -o test_all_pcmdi_libs
)

cdat_add_test(pcmdi_verify_executable
    test_all_pcmdi_libs
)

set_tests_properties (pcmdi_verify_executable PROPERTIES ENVIRONMENT "BASELINE_DIR=${UVCDAT_GIT_TESTDATA_DIR}/data/pcmdi/")
set_tests_properties(pcmdi_verify_executable PROPERTIES DEPENDS pcmdi_verify_compilation_works)

cdat_add_test(pcmdi_verify_executable_went_ok
    "${PYTHON_EXECUTABLE}"
    ${cdat_SOURCE_DIR}/testing/pcmdi/pcmdi_verify_executable_went_ok.py
    test_all_pcmdi_libs.nc
    "${UVCDAT_GIT_TESTDATA_DIR}/data/pcmdi/test_all_pcmdi_libs.nc"
)
set_tests_properties(pcmdi_verify_executable_went_ok PROPERTIES DEPENDS pcmdi_verify_executable)
