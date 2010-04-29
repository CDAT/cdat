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

#ifndef VISUSGENERICTIME_H
#define VISUSGENERICTIME_H

#include <string>
#include <sstream>

#include "VisusAssert.h"
#include "VisusTime.h"
#include "xmlParser.h"

//! A generic time implements the time interface for different C-types
template<typename ValueType>
class VisusGenericTime : public VisusTime
{
public:

  //! Default constructor
  explicit VisusGenericTime(ValueType t = 0);
  
  //! Copy constructor
  explicit VisusGenericTime(const VisusGenericTime& t) {*this = t;}
  
  //! Desctructor
  virtual ~VisusGenericTime() {}

  //! Incrementing a time value
  virtual const VisusTime& operator+=(const VisusTime& t);

  //! Decrementing a time value
  virtual const VisusTime& operator-=(const VisusTime& t);

  //! Smaller
  virtual bool operator<(const VisusTime& t) const;
  
  //! Greater
  virtual bool operator>(const VisusTime& t) const;

  //! Comparison
  virtual bool operator==(const VisusTime& t) const;

  //! Comparison
  virtual bool operator!=(const VisusTime& t) const {return !(*this == t);}  

  // Type casting
  virtual operator ValueType() const {return mTime;}

  //! Return a string representation of time
  virtual std::string toString() const;

  //! Return double representation of time
  virtual double toDouble() const {return mTime;}
  
  //! Assignment operator
  virtual int set(const VisusTime& t);

  //! Save time to XML representation
  virtual void toXML(XMLNode& parent) const;

protected:

  //! The current value
  ValueType mTime;

private:

  virtual void setToStep(const VisusTime& begin, const VisusTime& end, int steps);

};
  

template<typename ValueType>
VisusGenericTime<ValueType>::VisusGenericTime(ValueType t) : 
  mTime(t)
{
}

template<typename ValueType>
std::string VisusGenericTime<ValueType>::toString() const
{
  std::stringstream ss;
  ss << mTime;
  return ss.str();
}

template<typename ValueType>
void VisusGenericTime<ValueType>::toXML(XMLNode& node) const
{
  node.addAttribute("value", mTime);
}

template<typename ValueType>
int VisusGenericTime<ValueType>::set(const VisusTime& t)
{
  VisusGenericTime<ValueType> const* cast;
  
  cast = dynamic_cast<VisusGenericTime<ValueType> const*>(&t);
  
  if (cast == NULL) {
    vwarning("Cannot use TimeType %d to set TimeType %d.",t.type(),type());
    return 0;
  }

  mTime = cast->mTime;

  return 0;
}


template<typename ValueType>
const VisusTime& VisusGenericTime<ValueType>::operator+=(const VisusTime& t)
{
  if (type() != t.type()) {
    vwarning("Cannot add time type %d from time type %d.",t.type(),type());
    return *this;
  }

  VisusGenericTime<ValueType> const* cast;
  
  cast = dynamic_cast<VisusGenericTime<ValueType> const*>(&t);

  if (cast == NULL) {
    vwarning("TimeType information inconsistent.");
    return *this;
  }
  
  mTime += cast->mTime;

  return *this;
}

template<typename ValueType>
const VisusTime& VisusGenericTime<ValueType>::operator-=(const VisusTime& t)
{
  if (type() != t.type()) {
    vwarning("Cannot subtract time type %d from time type %d.",t.type(),type());
    return *this;
  }

  VisusGenericTime<ValueType> const* cast;
  
  cast = dynamic_cast<VisusGenericTime<ValueType> const*>(&t);

  if (cast == NULL) {
    vwarning("TimeType information inconsistent.");
    return *this;
  }
  
  mTime -= cast->mTime;

  return *this;
}

template<typename ValueType>
bool VisusGenericTime<ValueType>::operator==(const VisusTime& t) const
{
  if (type() != t.type())
    return false;

  VisusGenericTime<ValueType> const* cast;
  
  cast = dynamic_cast<VisusGenericTime<ValueType> const*>(&t);

  if (cast == NULL) {
    vwarning("TimeType information inconsistent.");
    return 0;
  }
  
  return (mTime == cast->mTime);
}

template<typename ValueType>
bool VisusGenericTime<ValueType>::operator<(const VisusTime& t) const
{
  if (type() != t.type())
    return false;

  VisusGenericTime<ValueType> const* cast;
  
  cast = dynamic_cast<VisusGenericTime<ValueType> const*>(&t);

  if (cast == NULL) {
    vwarning("TimeType information inconsistent.");
    return 0;
  }
  
  return (mTime < cast->mTime);
}

template<typename ValueType>
bool VisusGenericTime<ValueType>::operator>(const VisusTime& t) const
{
  if (type() != t.type())
    return false;

  VisusGenericTime<ValueType> const* cast;
  
  cast = dynamic_cast<VisusGenericTime<ValueType> const*>(&t);

  if (cast == NULL) {
    vwarning("TimeType information inconsistent.");
    return 0;
  }
  
  return (mTime > cast->mTime);
}

template<typename ValueType>
void VisusGenericTime<ValueType>::setToStep(const VisusTime& begin, const VisusTime& end, int steps)
{
  if (type() != begin.type()) {
    vwarning("TimeTypes do not match.");
    return;
  }
 
  if (type() != end.type()) {
    vwarning("TimeTypes do not match.");
    return;
  }

  VisusGenericTime<ValueType> const* b;
  VisusGenericTime<ValueType> const* e;
  
  b = dynamic_cast<VisusGenericTime<ValueType> const*>(&begin);
  e = dynamic_cast<VisusGenericTime<ValueType> const*>(&end);

  mTime = (e->mTime - b->mTime) / steps;
}


#endif
