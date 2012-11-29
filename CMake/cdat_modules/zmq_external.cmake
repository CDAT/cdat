
set(zmq_source "${CMAKE_CURRENT_BINARY_DIR}/build/ZMQ")
set(zmq_install "${cdat_EXTERNALS}")

ExternalProject_Add(ZMQ
  DOWNLOAD_DIR ${CDAT_PACKAGE_CACHE_DIR}
  SOURCE_DIR ${zmq_source}
  INSTALL_DIR ${zmq_install}
  URL ${ZMQ_URL}/${ZMQ_GZ}
  URL_MD5 ${ZMQ_MD5}
  BUILD_IN_SOURCE 1
  PATCH_COMMAND ""
  CONFIGURE_COMMAND ${CMAKE_COMMAND} -DINSTALL_DIR=<INSTALL_DIR> -DWORKING_DIR=<SOURCE_DIR> -P ${cdat_CMAKE_BINARY_DIR}/cdat_configure_step.cmake
  DEPENDS ${ZMQ_deps}
  ${ep_log_options}
)
