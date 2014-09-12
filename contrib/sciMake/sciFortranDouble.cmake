######################################################################
#
# : Compute ntcc specific options
#
# $Id: sciFortranDouble.cmake 1245 2012-01-31 21:36:22Z dws $
#
# Copyright 2010-2012 Tech-X Corporation.
# Arbitrary redistribution allowed provided this copyright remains.
#
######################################################################

set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} ${FC_DOUBLE_FLAGS}")
# message(STATUS "CMAKE_BUILD_TYPE = ${CMAKE_BUILD_TYPE}")
message(STATUS "CMAKE_Fortran_FLAGS = ${CMAKE_Fortran_FLAGS}")

