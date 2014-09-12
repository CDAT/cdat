######################################################################
#
# sciCChecks: check various C capabilities
#
# $Id: sciCChecks.cmake 1245 2012-01-31 21:36:22Z dws $
#
# Copyright 2010-2012 Tech-X Corporation.
# Arbitrary redistribution allowed provided this copyright remains.
#
######################################################################

# Determine the compiler id
set(C_COMPILER ${CMAKE_C_COMPILER})
set(C_COMPILER_ID ${CMAKE_C_COMPILER_ID})
message(STATUS "C_COMPILER_ID = ${C_COMPILER_ID}.")

# Check whether time and sys/time can both be included
include(CheckCSourceCompiles)
check_c_source_compiles(
"
#include <sys/time.h>
#include <time.h>
int main(int argc, char** argv) {return 0;}
"
TIME_WITH_SYS_TIME
)
if (TIME_WITH_SYS_TIME)
  if (DEBUG_CMAKE)
    message("time.h and sys/time.h are compatible.")
  endif ()
  set(TIME_WITH_SYS_TIME 1 CACHE BOOL "Whether time and sys/time are compatible")
else ()
  if (DEBUG_CMAKE)
    message("time.h and sys/time.h are NOT compatible.")
  endif ()
endif ()

# Check whether struct tm is in sys/time
include(CheckCSourceCompiles)
check_c_source_compiles(
"#include <sys/time.h>\nint main(int argc, char** argv) {struct tm tm;int *p = &tm.tm_sec;return !p;}"
TM_IN_SYS_TIME
)
if (TM_IN_SYS_TIME)
  if (DEBUG_CMAKE)
    message("struct tm is in time.h.")
  endif ()
  set(TM_IN_SYS_TIME 1 CACHE BOOL "Whether struct tm is in sys/time.")
else ()
  if (DEBUG_CMAKE)
    message("struct tm is NOT in time.h.")
  endif ()
endif ()

# Check variable sizes
check_c_source_compiles(
"#include <stdio.h>
void f(unsigned int i){}
void f(size_t i){}
int main(int argc, char** argv) {return 0;}"
UINT_IS_NOT_SIZE_T
)
if (UINT_IS_NOT_SIZE_T)
  if (DEBUG_CMAKE)
    message("uint and size_t are not the same.")
  endif ()
else ()
  if (DEBUG_CMAKE)
    message("uint and size_t are the same.")
  endif ()
  set(UINT_IS_SIZE_T 1 CACHE BOOL "Whether uint is the same as size_t")
endif ()

check_c_source_compiles(
"#ifdef _WIN32
 #include <BaseTsd.h>
 void f(int i){}
 void f(SSIZE_T i){}
#else
 #include <unistd.h>
 void f(int i){}
 void f(ssize_t i){}
#endif
int main(int argc, char** argv) {return 0;}"
INT_IS_NOT_SSIZE_T
)
if (INT_IS_NOT_SSIZE_T)
  if (DEBUG_CMAKE)
    message("int and ssize_t are not the same size.")
  endif ()
else ()
  if (DEBUG_CMAKE)
    message("int and ssize_t are the same size.")
  endif ()
  set(INT_IS_SSIZE_T 1 CACHE BOOL "Whether int is the same as ssize_t")
endif ()

# Get math into C for Windows
if (WIN32)
  set(_USE_MATH_DEFINES 1 CACHE BOOL "To bring in math defines on Windows.")
endif ()

#
# Tech-X Builde type: FULL, meaning as optimized as possible
# for this specific processor
#

# Determine the processor and sse capabilities
if (EXISTS /proc/cpuinfo)
  message(STATUS "Working on LINUX")
  file(READ /proc/cpuinfo cpuinfo)
  execute_process(COMMAND cat /proc/cpuinfo
      COMMAND grep "model name"
      COMMAND head -1
      OUTPUT_VARIABLE TXC_CPU
      OUTPUT_STRIP_TRAILING_WHITESPACE)
  string(REGEX REPLACE "^.*: " "" TXC_CPU ${TXC_CPU})
  execute_process(COMMAND cat /proc/cpuinfo
      COMMAND grep "flags"
      COMMAND head -1
      OUTPUT_VARIABLE CPU_CAPABILITIES
      OUTPUT_STRIP_TRAILING_WHITESPACE)
elseif (APPLE)
  execute_process(COMMAND sysctl -a machdep.cpu.brand_string
      OUTPUT_VARIABLE TXC_CPU
      OUTPUT_STRIP_TRAILING_WHITESPACE)
  string(REGEX REPLACE "^.*: " "" TXC_CPU "${TXC_CPU}")
  execute_process(COMMAND sysctl -a machdep.cpu.features
      COMMAND head -1
      OUTPUT_VARIABLE CPU_CAPABILITIES
      OUTPUT_STRIP_TRAILING_WHITESPACE)
  string(REGEX REPLACE "^.*: *" "" CPU_CAPABILITIES "${CPU_CAPABILITIES}")
  # string(REGEX REPLACE "SSE" "sse" CPU_CAPABILITIES "${CPU_CAPABILITIES}")
  string(TOLOWER "${CPU_CAPABILITIES}" CPU_CAPABILITIES)
endif ()
message(STATUS "CPU = ${TXC_CPU}.")
message(STATUS "CPU_CAPABILITIES = ${CPU_CAPABILITIES}.")

if (CPU_CAPABILITIES)
  separate_arguments(CPU_CAPABILITIES)
  # message(STATUS "CPU capabilities are ${CPU_CAPABILITIES}")
  foreach (cap ${CPU_CAPABILITIES})
    # MESSAGE("Examining ${cap}")
    if (${cap} MATCHES "^sse")
      list(APPEND SSE_CAPABILITIES ${cap})
    elseif (${cap} MATCHES "^avx")
      list(APPEND AVX_CAPABILITIES ${cap})
    endif ()
  endforeach ()
  foreach(cap SSE AVX)
    if (${cap}_CAPABILITIES)
      list(SORT ${cap}_CAPABILITIES)
      list(REVERSE ${cap}_CAPABILITIES)
      list(GET ${cap}_CAPABILITIES 0 ${cap}_CAPABILITY)
      string(REPLACE "_" "." ${cap}_CAPABILITY "${${cap}_CAPABILITY}")
    endif ()
  endforeach ()
endif ()

foreach(cap SSE AVX)
  message(STATUS "${cap} capabilities are ${${cap}_CAPABILITIES}")
  message(STATUS "${cap} capability is ${${cap}_CAPABILITY}")
endforeach ()

#
# Determine flags by compiler

set(CMAKE_C_FLAGS_FULL "${CMAKE_C_FLAGS_RELEASE}")
if (${C_COMPILER_ID} STREQUAL GNU OR ${C_COMPILER_ID} STREQUAL Clang)

  set(CMAKE_C_FLAGS_FULL "${CMAKE_C_FLAGS_FULL} -ffast-math")
  if (SSE_CAPABILITY)
    set(CMAKE_C_FLAGS_FULL "${CMAKE_C_FLAGS_FULL} -m${SSE_CAPABILITY}")
  endif ()
# Apple automatically sets mtune
  # IF(LINUX)
    if ("${TXC_CPU}" MATCHES ".*Athlon.*MP.*")
      set(CMAKE_C_FLAGS_FULL "${CMAKE_C_FLAGS_FULL} -march=athlon-mp")
    elseif (${TXC_CPU} MATCHES ".*Athlon.*XP.*")
      set(CMAKE_C_FLAGS_FULL "${CMAKE_C_FLAGS_FULL} -march=athlon-xp")
    elseif (${TXC_CPU} MATCHES ".*Athlon.*")
      set(CMAKE_C_FLAGS_FULL "${CMAKE_C_FLAGS_FULL} -march=athlon")
    elseif (${TXC_CPU} MATCHES ".*Opteron.*")
      set(CMAKE_C_FLAGS_FULL "${CMAKE_C_FLAGS_FULL} -mtune=amdfam10")
    endif ()
  # ENDIF()
  set(AVX_FLAG -mavx)
  set(OPENMP_FLAG -fopenmp)

elseif (${C_COMPILER_ID} STREQUAL Intel)

elseif (${C_COMPILER_ID} STREQUAL MSVC)

elseif (${C_COMPILER_ID} STREQUAL PathScale)

elseif (${C_COMPILER_ID} STREQUAL PGI)

# Compiler optimization flags set based on "ultra" optimization in
# flags.m4.  Overrides CMake default, since that had -Mipa=fast
# (no inline).
  set(CMAKE_C_FLAGS_FULL
      "-fast -O3 -DNDEBUG -Munroll -Minline=levels:5 -Mipa=fast,inline -Mmovnt")

elseif (${C_COMPILER_ID} STREQUAL XL)

else ()
  message(STATUS "FULL flags not known for ${C_COMPILER_ID}")
endif ()

# Check whether have avx.
# Tests show that one must pass ${AVX_FLAG} to compile avx instructions
include(CheckCSourceRuns)
if (AVX_CAPABILITIES)
  message("Checking avx compilation")
  set(CMAKE_REQUIRED_FLAGS_SAV "${CMAKE_REQUIRED_FLAGS}")
  set(CMAKE_REQUIRED_FLAGS "${CMAKE_REQUIRED_FLAGS} ${AVX_FLAG}")
  # check_c_source_compiles(
  check_c_source_runs(
"
#include <immintrin.h>
int main(int argc, char** argv) {
  double a[4] = {1.0, 2.0, 3.0, 4.0};
  __m256d t = _mm256_load_pd(a);
}
"
  HAVE_AVX
  )
  set(CMAKE_REQUIRED_FLAGS "${CMAKE_REQUIRED_FLAGS_SAV}")
endif ()
if (HAVE_AVX)
  if (DEBUG_CMAKE)
    message(STATUS "Have AVX.")
  endif ()
  set(HAVE_AVX 1 CACHE BOOL "Whether have avx.")
else ()
  message(STATUS "AVX instructions not found.")
endif ()

if (HAVE_AVX)
  set(CMAKE_C_FLAGS_FULL "${CMAKE_C_FLAGS_FULL} ${AVX_FLAG}")
endif ()
if (USE_OPENMP)
  set(CMAKE_C_FLAGS_FULL "${CMAKE_C_FLAGS_FULL} ${OPENMP_FLAG}")
endif ()

sciPrintVar(CMAKE_C_FLAGS_FULL)
sciPrintVar(CMAKE_C_FLAGS_RELEASE)
sciPrintVar(CMAKE_C_FLAGS_RELWITHDEBINFO)
sciPrintVar(CMAKE_C_FLAGS_DEBUG)
sciPrintVar(CMAKE_C_FLAGS)

