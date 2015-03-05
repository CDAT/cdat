if (CDAT_DOWNLOAD_SAMPLE_DATA)
  message("[INFO] ------------------------------------------------------------------------------------------------------------------------------")
  configure_file(
    "${cdat_CMAKE_SOURCE_DIR}/cdat_modules_extra/cdat_download_sample_data.cmake.in"
    "${cdat_CMAKE_BINARY_DIR}/cdat_download_sample_data.cmake"
    @ONLY
    )
  set(sampledata_cmd ${CMAKE_COMMAND} -P ${cdat_CMAKE_BINARY_DIR}/cdat_download_sample_data.cmake)
  ExternalProject_Add(sampledata
    SOURCE_DIR ${cdat_SOURCE_DIR}/Packages/dat
    CONFIGURE_COMMAND ${sampledata_cmd}
    BUILD_COMMAND ""
    INSTALL_COMMAND ""
    DEPENDS ${sampledata_deps}
    ${ep_log_options}
    )
endif()
