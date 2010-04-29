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


#include <limits.h>
#include <float.h>
#include <string>
#include <sstream>
#include <cstring>

#include "VisusAssert.h"

#include "xmlParser.h"
#include "PvTimeState.h"

const char* PvTimeState::XML_TAG = "PvTimeState";

PvTimeState::PvTimeState(int timeIndex)
{
    mInterpolate = false;
    mIndex = timeIndex;
    mValue = FLT_MIN;  // We must rely on DataSource to provide this information
    std::ostringstream os;
    os << timeIndex;
    mStringValue = os.str();
}

PvTimeState::PvTimeState(double timeValue)
{
    mInterpolate = false;
    mIndex = INT_MIN;  // The data source knows how to map real time to time index
    mValue = timeValue;
    std::ostringstream os;
    os << timeValue;
    mStringValue = os.str();
}

PvTimeState::PvTimeState(const PvTimeState& other)
{
    *this = other;
}

PvTimeState& 
PvTimeState::operator=(const PvTimeState& other)
{
    this->mIndex = other.index();
    this->mValue = other.value();
    this->mStringValue = other.stringValue();
    this->mInterpolate = other.mInterpolate;
	return *this;
}


int
PvTimeState::index(void) const
{
    return mIndex;
}

double
PvTimeState::value(void) const
{
    return mValue;
}

std::string
PvTimeState::stringValue(void) const
{
    return mStringValue;
}

void
PvTimeState::index(int i)
{
    mIndex = i;
}
void
PvTimeState::value(double t, bool interpolate)
{
    mValue = t;
    mInterpolate = interpolate;
}
void
PvTimeState::stringValue(std::string v, bool interpolate)
{
    mStringValue = v;
    mInterpolate = interpolate;
}

bool
PvTimeState::interpolate(void) const
{
    return mInterpolate;
}

void PvTimeState::toXML(XMLNode& parent) const
{
  XMLNode pts = parent.addChild(XML_TAG);
  pts.addAttribute("interpolate", mInterpolate);
  pts.addAttribute("index", mIndex);
  pts.addAttribute("value", mValue);
  pts.addAttribute("strValue", mStringValue.c_str());
}

bool PvTimeState::fromXML(XMLNode& node)
{
  if (strcmp(XML_TAG, node.getName())) {
    vwarning("PvTimeState did not receive its top level node");
    return false;
  }
  mInterpolate = xmltobool(node.getAttribute("interpolate"), mInterpolate);
  mIndex = xmltoi(node.getAttribute("index"), mIndex);
  mValue = xmltof(node.getAttribute("value"), mValue);
  mStringValue = node.getAttribute("strValue");

  return true;
}
