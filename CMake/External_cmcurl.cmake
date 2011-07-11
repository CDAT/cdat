# The cmCurl external project for Titan

set(curl_source "${CMAKE_CURRENT_SOURCE_DIR}/cmcurl")
set(curl_binary "${CMAKE_CURRENT_BINARY_DIR}/cmcurl")

ExternalProject_Add(cmcurl
  DOWNLOAD_COMMAND ""
  SOURCE_DIR "${curl_source}"
  BINARY_DIR "${curl_binary}"
  CMAKE_GENERATOR ${gen}
  CMAKE_ARGS
    -DCMAKE_BUILD_TYPE:STRING=${CMAKE_BUILD_TYPE}
    -DBUILD_SHARED_LIBS:BOOL=ON
    -DBUILD_TESTING:BOOL=OFF
    -DBUILD_CURL_TESTS:BOOL=OFF
    -DBUILD_CURL_EXE:BOOL=OFF
    -DCURL_DISABLE_LDAP:BOOL=ON
    -DCURL_DISABLE_LDAPS:BOOL=ON
    ${titan_compiler_args}
    ${titan_binary_args}
    ${cmcurl_EXTRA_ARGS}
    -DTRIGGER_REBUILD:STRING=0
  INSTALL_COMMAND ""
  DEPENDS ${cmcurl_DEPENDENCIES}
)
