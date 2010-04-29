/***********************************************************************
*
* Copyright (c) 2008, Lawrence Livermore National Security, LLC.  
* Produced at the Lawrence Livermore National Laboratory  
* Written by bremer5@llnl.gov,pascucci@sci.utah.edu.  
* LLNL-CODE-406031.  
* All rights reserved.  
*   
* This file is part of "Simple and Flexible Scene Graph Version 2.0."
* Please also read BSD_ADDITIONAL.txt.
*   
* Redistribution and use in source and binary forms, with or without
* modification, are permitted provided that the following conditions are
* met:
*   
* @ Redistributions of source code must retain the above copyright
*   notice, this list of conditions and the disclaimer below.
* @ Redistributions in binary form must reproduce the above copyright
*   notice, this list of conditions and the disclaimer (as noted below) in
*   the documentation and/or other materials provided with the
*   distribution.
* @ Neither the name of the LLNS/LLNL nor the names of its contributors
*   may be used to endorse or promote products derived from this software
*   without specific prior written permission.
*   
*  
* THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
* "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
* LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
* A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL LAWRENCE
* LIVERMORE NATIONAL SECURITY, LLC, THE U.S. DEPARTMENT OF ENERGY OR
* CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
* EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
* PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
* PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
* LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
* NEGLIGENCE OR OTHERWISE) ARISING
*
***********************************************************************/


%{
#include <string>
%}

%include "std_string.i"

/* ======================================================= */
/* ===== Macro To Assist In 'Implicit' Conversion of ===== */
/* ===== std::string and char* to STRINGTYPE         ===== */
/* ======================================================= */
%define VisusStringType(STRINGTYPE)

%typemap(in) STRINGTYPE &, const STRINGTYPE & 
{
    std::string* pString=NULL;

    // Explicit Conversion of String To STRINGTYPE 
    if (SWIG_ConvertPtr($input, (void**) &$1,
          $1_descriptor,
          SWIG_POINTER_EXCEPTION) != SWIG_ERROR) {
        // We are the correct type
    }
    else if (SWIG_ConvertPtr($input, (void**) &pString,
               $descriptor(std::string&),
               SWIG_POINTER_EXCEPTION) != SWIG_ERROR) {
        $1 = new VisusDataDescription(pString->c_str());
    }
    else if (PyString_Check($input)) {
        char* pChar = PyString_AsString($input);
        $1 = new VisusDataDescription(pChar);
    }
    else {
        PyErr_SetString(PyExc_TypeError, "STRINGTYPE or std::string or const char* expected");
        return NULL;
    }
}

%typemap(typecheck) STRINGTYPE &, const STRINGTYPE & 
{
    std::string* pString=NULL;

    // Explicit Conversion of String To STRINGTYPE 
    if (SWIG_ConvertPtr($input, (void**) &$1,
          $1_descriptor,
          SWIG_POINTER_EXCEPTION) != SWIG_ERROR) {
        $1 = 1;
    }
    else if (SWIG_ConvertPtr($input, (void**) &pString,
               $descriptor(std::string&),
               SWIG_POINTER_EXCEPTION) != SWIG_ERROR) {
        $1 = 1;
    }
    else if (PyString_Check($input)) {
        $1 = 1;
    }
    else {
        $1 = 0;
    }
}
%enddef

