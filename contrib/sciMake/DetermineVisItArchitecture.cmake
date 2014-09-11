#*****************************************************************************
#
# Copyright(c) 2000 - 2011, Lawrence Livermore National Security, LLC
# Produced at the Lawrence Livermore National Laboratory
# LLNL-CODE-400142
# All rights reserved.
#
# This file is  part of VisIt. For  details, see https://visit.llnl.gov/.  The
# full copyright notice is contained in the file COPYRIGHT located at the root
# of the VisIt distribution or at http://www.llnl.gov/visit/copyright.html.
#
# Redistribution  and  use  in  source  and  binary  forms,  with  or  without
# modification, are permitted provided that the following conditions are met:
#
#  - Redistributions of  source code must  retain the above  copyright notice,
#    this list of conditions and the disclaimer below.
#  - Redistributions in binary form must reproduce the above copyright notice,
#    this  list of  conditions  and  the  disclaimer(as noted below)  in  the
#    documentation and/or other materials provided with the distribution.
#  - Neither the name of  the LLNS/LLNL nor the names of  its contributors may
#    be used to endorse or promote products derived from this software without
#    specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT  HOLDERS AND CONTRIBUTORS "AS IS"
# AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING,  BUT NOT  LIMITED TO, THE
# IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR  PURPOSE
# ARE  DISCLAIMED. IN  NO EVENT  SHALL LAWRENCE  LIVERMORE NATIONAL  SECURITY,
# LLC, THE  U.S.  DEPARTMENT OF  ENERGY OR CONTRIBUTORS BE  LIABLE  FOR  ANY
# DIRECT,  INDIRECT,   INCIDENTAL,   SPECIAL,   EXEMPLARY, OR CONSEQUENTIAL
# DAMAGES(INCLUDING, BUT NOT  LIMITED TO, PROCUREMENT OF  SUBSTITUTE GOODS OR
# SERVICES; LOSS OF  USE, DATA, OR PROFITS; OR  BUSINESS INTERRUPTION) HOWEVER
# CAUSED AND ON  ANY  THEORY  OF  LIABILITY,  WHETHER  IN  CONTRACT,  STRICT
# LIABILITY, OR TORT(INCLUDING NEGLIGENCE OR OTHERWISE)  ARISING IN ANY  WAY
# OUT OF THE  USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
# DAMAGE.
#
# Modifications:
#   Eric Brugger, Thu Feb 25 16:06:22 PST 2010
#   I enclosed all variables that were used in tests involving STREQUAL
#   in ${} and enclosed all literal strings that were used in tests
#   involving STREQUAL in double quotes so that it would create the
#   correct architecture string on AIX.
#
#   I modified the architecture string it generates on AIX to be either
#   ibm-aix-pwr or ibm-aix-pwr64.
#
#   Brad Whitlock, Tue Jan 25 12:28:55 PST 2011
#   I made Mac 10.x and later use darwin-x86_64.
#
#****************************************************************************/

macro(DETERMINE_VISIT_ARCHITECTURE ARCH)
    if (${CMAKE_SYSTEM_NAME} STREQUAL "Linux")
        if (${CMAKE_SYSTEM_PROCESSOR} STREQUAL "ppc")
            set(${ARCH} linux-ppc)
        elseif (${CMAKE_SYSTEM_PROCESSOR} STREQUAL "ppc64")
            set(${ARCH} linux-ppc64)
        elseif (${CMAKE_SYSTEM_PROCESSOR} STREQUAL "x86_64")
            set(${ARCH} linux-x86_64)
        elseif (${CMAKE_SYSTEM_PROCESSOR} STREQUAL "ia64")
            set(${ARCH} linux-ia64)
        else ()
            set(${ARCH} linux-intel)
        endif ()
    elseif (${CMAKE_SYSTEM_NAME} STREQUAL "AIX")
        if ($ENV{OBJECT_MODE} STREQUAL "32")
            set(${ARCH} "ibm-aix-pwr")
        else ()
            set(${ARCH} "ibm-aix-pwr64")
        endif ()
    elseif (${CMAKE_SYSTEM_NAME} STREQUAL "Darwin")
        if (${CMAKE_SYSTEM_PROCESSOR} STREQUAL "i386")
            execute_process(COMMAND uname -r
               OUTPUT_STRIP_TRAILING_WHITESPACE
               OUTPUT_VARIABLE _OSX_VERSION)
            string(SUBSTRING ${_OSX_VERSION} 0 1 _OSX_MAJOR_VERSION)
            if (${_OSX_MAJOR_VERSION} STREQUAL "1")
                # This will match 10, 11, 12, ...
                set(${ARCH} darwin-x86_64)
            else ()
                set(${ARCH} darwin-i386)
            endif ()
        else ()
            set(${ARCH} darwin-ppc)
        endif ()
    elseif (${CMAKE_SYSTEM_NAME} STREQUAL "FreeBSD")
        set(${ARCH} "freebsd-${CMAKE_SYSTEM_VERSION}")
    elseif (${CMAKE_SYSTEM_NAME} STREQUAL "IRIX")
        set(${ARCH} sgi-irix6-mips2)
    elseif (${CMAKE_SYSTEM_NAME} STREQUAL "SunOS")
        set(${ARCH} "sun4-${CMAKE_SYSTEM_VERSION}-sparc")
    elseif (${CMAKE_SYSTEM_NAME} STREQUAL "Tru64")
        set(${ARCH} dec-osf1-alpha)
    else ()
        # Unhandled case. Make up a string.
        set(VISITARCHTMP "${CMAKE_SYSTEM_NAME}-${CMAKE_SYSTEM_PROCESSOR}")
        string(TOLOWER ${VISITARCHTMP} ${ARCH})
    endif ()
endmacro(DETERMINE_VISIT_ARCHITECTURE ARCH)
