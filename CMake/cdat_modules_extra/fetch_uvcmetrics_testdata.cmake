# used variables:
#   FILE_URL    The url where the file is available
#   FILE_PATH   The destination for the file
#   FILE_MD5    The expected md5

# check if the file already exists
if(EXISTS "${FILE_PATH}")
  # check md5sum
  file(MD5 "${FILE_PATH}" output_md5)

  if(${output_md5} STREQUAL ${FILE_MD5})
    return() # nothing to do
  endif()
endif()

# add a build target to download the file
file(DOWNLOAD "${FILE_URL}" "${FILE_PATH}" STATUS stat)
list(GET stat 0 exit_code)
list(GET stat 1 msg)

# fail on error
if(NOT exit_code EQUAL 0)
  file(REMOVE "${FILE_PATH}")
  message(FATAL_ERROR "Error downloading: ${msg}")
endif()
