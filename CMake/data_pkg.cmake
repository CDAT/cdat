# Do we download the data ?
option(CDAT_DOWNLOAD_SAMPLE_DATA "Download sample data" ON)
if (CDAT_DOWNLOAD_SAMPLE_DATA)
  set(SAMPLE_DATA "")
else()
  set(SAMPLE_DATA --disable-sampledata)
endif()
