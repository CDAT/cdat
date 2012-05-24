
set(esmf_source "${CMAKE_CURRENT_BINARY_DIR}/build/esmf" CACHE INTERNAL "")
set(esmf_install "${cdat_EXTERNALS}" CACHE INTERNAL "")
set(esmf_pthreads "OFF")

# Set the os
if(LINUX)
  set(esmf_os "Linux")
elseif(APPLE)
  set(esmf_os "Darwin")
endif()

# check if ESMF should be built in parallel
set(emsf_enable_mpi FALSE)
if(CDAT_BUILD_ESMF_PARALLEL)
  set(emsf_enable_mpi TRUE)
endif()
if(NOT MPI_FOUND)
  message("MPI was not found")
  set(emsf_enable_mpi FALSE)
endif()
if("${emsf_enable_mpi}" AND "${MPI_CXX_COMPILER}" STREQUAL "MPI_CXX_COMPILER_NOTFOUND")
  message("MPI C++ compiler was not found")
  set(emsf_enable_mpi FALSE)
endif()
if("${emsf_enable_mpi}" AND "${MPI_Fortran_COMPILER}" STREQUAL "MPI_Fortran_COMPILER_NOTFOUND")
  message("MPI Fortran compiler was not found")
  set(emsf_enable_mpi FALSE)
endif()
if("${emsf_enable_mpi}" AND "${MPI_C_COMPILER}" STREQUAL "MPI_C_COMPILER_NOTFOUND")
  message("MPI C compiler was not found")
  set(emsf_enable_mpi FALSE)
endif()
if("${emsf_enable_mpi}")
  # other possibilites are "mpich" "mpich2" "intelmpi" "lam" ...
  message("Will build ESMF assuming openmpi")
  set(esmf_comm "openmpi")
  # on Crays it should be
  #set(esmf_comm "mpi")
else()
  message("Will build ESMF serial")
  set(esmf_comm "mpiuni")
endif()

configure_file(${cdat_CMAKE_SOURCE_DIR}/esmf_make_step.cmake.in
  ${cdat_CMAKE_BINARY_DIR}/esmf_make_step.cmake
  @ONLY)
  
configure_file(${cdat_CMAKE_SOURCE_DIR}/esmf_install_step.cmake.in
  ${cdat_CMAKE_BINARY_DIR}/esmf_install_step.cmake
  @ONLY)

set(esmf_build_command ${CMAKE_COMMAND} -P ${cdat_CMAKE_BINARY_DIR}/esmf_make_step.cmake)
set(esmf_install_command ${CMAKE_COMMAND} -P ${cdat_CMAKE_BINARY_DIR}/esmf_install_step.cmake)

ExternalProject_Add(ESMF
  DOWNLOAD_DIR ${CMAKE_CURRENT_BINARY_DIR}
  SOURCE_DIR ${esmf_source}
  INSTALL_DIR ${esmf_install}
  URL ${ESMF_URL}/${ESMF_GZ}
#  URL_MD5 ${ESMF_MD5}
  URL_MD5 ""
  BUILD_IN_SOURCE 1
  PATCH_COMMAND ""
  CONFIGURE_COMMAND ""
  BUILD_COMMAND ${esmf_build_command}
  INSTALL_COMMAND ${esmf_install_command}
  DEPENDS ${esmf_DEPENDENCIES}
  ${EP_LOG_OPTIONS}
)
