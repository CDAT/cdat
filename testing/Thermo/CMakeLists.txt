set(BASELINE_DIR "${UVCDAT_GIT_TESTDATA_DIR}/baselines/Thermo")

if (CDAT_DOWNLOAD_SAMPLE_DATA)
  cdat_add_test(test_thermo_T
    "${PYTHON_EXECUTABLE}"
    ${cdat_SOURCE_DIR}/testing/Thermo/test_thermoT.py
    ${UVCDAT_GIT_TESTDATA_DIR}
    )
  cdat_add_test(test_thermo_W
    "${PYTHON_EXECUTABLE}"
    ${cdat_SOURCE_DIR}/testing/Thermo/test_thermoW.py
    ${UVCDAT_GIT_TESTDATA_DIR}
    )
  FOREACH (type stuve emagram tephigram skewT)
    cdat_add_test(test_thermo_${type}
      "${PYTHON_EXECUTABLE}"
      "${cdat_SOURCE_DIR}/testing/Thermo/test_thermo_types.py"
      "--data=${UVCDAT_GIT_TESTDATA_DIR}/data/Thermo/dar.meteo.mod.cam3.era.v31.h0.l3.nrstpt.cp.2000070100.2000080100.tau.12.36.nc"
      "--type=${type}"
      "--baseline=${BASELINE_DIR}/test_thermo_${type}.png"
    )
  ENDFOREACH(type)
endif()
add_test(flake8_Thermo
  "${FLAKE8_EXECUTABLE}" "${cdat_SOURCE_DIR}/Packages/Thermo/Lib/"
  --show-source # Show context for detected errors
  --statistics  # Show summary of errors at end of output
  --max-line-length=120 # Reasonable line length
)
