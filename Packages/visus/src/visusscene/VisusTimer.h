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


#ifndef VISUSTIMER_H
#define VISUSTIMER_H

#include <string>
#include <stdio.h>

class VisusTimer
{
public:
  VisusTimer(const char* title):mTitle(title) 
  {
#ifdef WIN32
    mStart = getTime();
#else
    mStart = time(NULL);
#endif
  }

  ~VisusTimer() 
  {
#ifdef WIN32
    ULONGLONG stop = getTime();
#else
    time_t stop = time(NULL);
#endif
    printf("Time for %s: %ld\n", mTitle.c_str(), (stop - mStart));
  }
private:
#ifdef WIN32
  ULONGLONG getTime()
  {
    SYSTEMTIME systemTime;
    GetSystemTime( &systemTime );

    FILETIME fileTime;
    SystemTimeToFileTime( &systemTime, &fileTime );

    ULARGE_INTEGER uli;
    uli.LowPart = fileTime.dwLowDateTime; // could use memcpy here!
    uli.HighPart = fileTime.dwHighDateTime;

    return uli.QuadPart/10000;
  }
  ULONGLONG mStart;
#else
  time_t mStart;
#endif
  std::string mTitle;
};


#endif // VISUS_TIMER


