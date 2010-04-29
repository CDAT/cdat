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

#include "VisusAssert.h"
#include "VisusGlobalTime.h"
#include "VisusIntTime.h"
#include "VisusDoubleTime.h"
#include "xmlParser.h"
#include <string.h>


const char* VisusGlobalTime::XML_TAG = "VisusGlobalTime";

VisusGlobalTime::VisusGlobalTime()
{
  mCurrentTime = new VisusIntTime(0);
  mBegin = new VisusIntTime(0);
  mEnd = new VisusIntTime(0);
  mStep = new VisusIntTime(0);
}
 
VisusGlobalTime::VisusGlobalTime(const VisusGlobalTime& t) 
{
  mCurrentTime = t.mCurrentTime->clone();
  mBegin = t.mBegin->clone();
  mEnd = t.mEnd->clone();
  mStep = t.mStep->clone();
}

VisusGlobalTime::~VisusGlobalTime()
{
  delete mCurrentTime;
  delete mBegin;
  delete mEnd;
  delete mStep;
}

VisusGlobalTime& VisusGlobalTime::operator=(const VisusGlobalTime& value)
{
  copyTime(&mCurrentTime,value.mCurrentTime);
  copyTime(&mBegin,value.mBegin);
  copyTime(&mEnd,value.mEnd);
  copyTime(&mStep,value.mStep);


  return *this;
}

bool VisusGlobalTime::operator==(const VisusGlobalTime& value) const
{
  return ((*mCurrentTime == *value.mCurrentTime) 
          && (*mBegin == *value.mBegin) && (*mEnd == *value.mEnd) 
          && (*mStep == *value.mStep));
}

bool VisusGlobalTime::operator!=(const VisusGlobalTime& value) const
{
  return !(*this == value);
}

const VisusTime& VisusGlobalTime::time(const VisusTime& t) 
{ 
  if (t.type() != type()) {
    vwarning("Cannot use TimeType %d to set TimeType %d.",t.type(),type());
    return *mCurrentTime;
  }

  mCurrentTime->set(t);
  
  clampTime();

  return time();
}

std::string VisusGlobalTime::toString() const
{
  return mCurrentTime->toString();
}

void VisusGlobalTime::toXML(XMLNode& parent) const
{
  XMLNode node = parent.addChild(XML_TAG);
  node.addAttribute("type", type());
  mCurrentTime->toXML(node);
  mBegin->toXML(node);
  mEnd->toXML(node);
  mStep->toXML(node);
}

bool VisusGlobalTime::fromXML(XMLNode& node)
{
  if (strcmp(XML_TAG, node.getName())) {
    vwarning("VisusGlobalTime did not receive top node");
    return false;
  }

  TimeType type = (TimeType)xmltoi(node.getAttribute("type"), UNDEFINED_TIME);

  switchType(type);
  
  bool success = true;

  success &= mCurrentTime->fromXML(node);
  success &= mBegin->fromXML(node);
  success &= mEnd->fromXML(node);
  success &= mStep->fromXML(node);

  return success;
}

void VisusGlobalTime::timeStep(const VisusTime& timeStep)
{
  if (timeStep.type() != type()) {
    vwarning("Cannot use time step of TimeType %d to set step of type %d.",timeStep.type(),type());
    return;
  }

  mStep->set(timeStep);
}

void VisusGlobalTime::inc()
{
  *mCurrentTime += *mStep;

  clampTime();
}

void VisusGlobalTime::dec()
{
  *mCurrentTime -= *mStep;

  clampTime();
}

double VisusGlobalTime::relativeTime() const
{
  return mCurrentTime->relativeTime(*mBegin,*mEnd);
}

int VisusGlobalTime::timeIndex() const
{
  return mCurrentTime->timeIndex(*mBegin,*mEnd,*mStep);
}

int VisusGlobalTime::initializeFromString(std::vector<std::string>::iterator& it, 
                                          int time_steps)
{
  char* ep;
  TimeType t = (TimeType)strtol(it->c_str(),&ep,10);

  switchType(t);

  it++;
  mBegin->fromString(*it);
  
  it++;
  mEnd->fromString(*it);
  
  mCurrentTime->set(*mBegin);

  mStep->setToStep(*mBegin,*mEnd,time_steps-1);

  return 1;
}

void VisusGlobalTime::copyTime(VisusTime** target, const VisusTime* source)
{
  VisusTime* backup = *target;

  *target = source->clone();
  
  if (backup != NULL)
    delete backup;
}

void VisusGlobalTime::clampTime()
{
  if (*mCurrentTime < *mBegin) {
    mCurrentTime->set(*mBegin);
    return;
  }

  if (time() > *mEnd) 
    mCurrentTime->set(*mEnd);      
}

void VisusGlobalTime::switchType(TimeType t)
{
  switch (t) {
  case INT_TIME:
    mCurrentTime = new VisusIntTime();
    break;
  case DOUBLE_TIME:
    mCurrentTime = new VisusDoubleTime();
    break;
  case UNDEFINED_TIME:
    vwarning("VisusGlobalTime could not recognize the time type,");
    mCurrentTime = new VisusIntTime();
  }
  
  copyTime(&mBegin,mCurrentTime);
  copyTime(&mEnd,mCurrentTime);
  copyTime(&mStep,mCurrentTime);
  
}
