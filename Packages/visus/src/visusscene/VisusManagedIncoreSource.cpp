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

#include "VisusTimeLoader.h"
#include "VisusManagedIncoreSource.h"

VisusManagedIncoreSource::VisusManagedIncoreSource(const std::string& args) : 
  VisusIncoreDataSource() 
{
  std::vector<std::string> tokens;

  tokens = tokenize(args);
  
  if (initializeFromString(tokens) == 0) {
     
    vwarning("Data source string inconsistent. Could not intialzize managed incore source.");
    
    mSamples = std::vector<int>(3,0);
  }

}

int VisusManagedIncoreSource::timeOffset(const VisusTime& current_time)
{
  if (this->mTimeInfo.time() != current_time) {

    if (mTimeLoader == NULL) {
      vwarning("Time loader un-initialized cannot switch time steps.");
      return 0;
    }

    this->mTimeInfo.time(current_time);

    mTimeLoader->loadTimeStep(this->mTimeInfo.time(),this->mData);
  }

  return 0;
}


//! Initialize a managed incore source using the standard string
int VisusManagedIncoreSource::initializeFromString(std::vector<std::string>& tokens)
{
  char *ep;
  VisusGlobalTime complete_time;
  int time_steps;
  std::vector<std::string>::iterator it;

  it = tokens.begin();

  mTimeLoader = (VisusTimeLoader*)strtol(it->c_str(),&ep,10);

  it++;
  time_steps = strtol(it->c_str(),&ep,10);

  it++;
  complete_time.initializeFromString(it,time_steps);

  it++;
  tokens.erase(tokens.begin(),it);
  
  if (VisusIncoreDataSource::initializeFromString(tokens)) {
    this->mTimeInfo = complete_time;
    return 1;
  }
  else 
    return 0;
}
