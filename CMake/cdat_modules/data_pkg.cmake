# Do we download the data ?
option(CDAT_DOWNLOAD_SAMPLE_DATA "Download sample data" ON)
if (NOT CDAT_BUILD_WO_ESGF)
    message("[INFO] Disabling download data for ESGF")
    set(CDAT_DOWNLOAD_SAMPLE_DATA OFF)
endif()

if (OFFLINE_BUILD)
    message("[INFO] Disabling download data for offline build")
    set(CDAT_DOWNLOAD_SAMPLE_DATA OFF)
endif()

if (CDAT_DOWNLOAD_SAMPLE_DATA)
  set(SAMPLE_DATA "")
else()
  set(SAMPLE_DATA --disable-sampledata)
endif()
