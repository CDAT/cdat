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

//#define VSP_ABSTRACT_CLASS

#include <math.h>

#include "VisusAssert.h"
#include "VisusTime.h"



const char* VisusTime::TIMESTEP_XML_TAG = "timeStep";


double VisusTime::relativeTime(const VisusTime& begin, const VisusTime& end) const
{
  // First we have to test whether we are given a degenerate interval
  if (fabs(begin.toDouble() - end.toDouble()) < 10e-6) {
    
    // If the interval is degenerate but out time in question is equal to it
    if (fabs(toDouble() - begin.toDouble()) < 10e-6) 
      return 0; // There is no problem
    else {
      // Otherwise, there exists no correct interpolation and we return a default
      vwarning("Cannot interpolate times based on degenerate interval.");
      return 0; 
    }
  }
  else {
    return (toDouble() - begin.toDouble()) / (end.toDouble()-begin.toDouble());
  }
}

int VisusTime::timeIndex(const VisusTime& begin, const VisusTime& end, const VisusTime& step) const
{
  if (fabs(step.toDouble()) < 10e-6) {
    if (fabs(end.toDouble() - begin.toDouble()) >= 10e-6) 
      vwarning("Cannot determine time index, step size too small.");
    
    return 0;
  }
      

  int n = (int)((end.toDouble() - begin.toDouble()) / step.toDouble() + 0.5);
  double p = relativeTime(begin,end);

  return (int)(p*n + 0.5);
}

