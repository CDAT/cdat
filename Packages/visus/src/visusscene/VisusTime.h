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


#ifndef VISUSTIME_H
#define VISUSTIME_H

#include <string>

enum TimeType
{
  DOUBLE_TIME=0,
  INT_TIME=1,
  UNDEFINED_TIME=9999
};

struct XMLNode;


//! VisusTime defines the common interface for time values within ViSUS
class VisusTime
{
public:

  //! VisusGlobalTime needs access to clone()
  friend class VisusGlobalTime;

  //! Destructor
  virtual ~VisusTime() {}
  
  //! Incrementing a time value: Effects *only* the value
  virtual const VisusTime& operator+=(const VisusTime& t) = 0;

  //! Decrementing a time value: Effects *only* the value
  virtual const VisusTime& operator-=(const VisusTime& t) = 0;

  //! Smaller
  virtual bool operator<(const VisusTime& t) const = 0;
  
  //! Greater
  virtual bool operator>(const VisusTime& t) const = 0;

  //! Comparison
  virtual bool operator==(const VisusTime& t) const = 0;

  //! Comparison
  virtual bool operator!=(const VisusTime& t) const = 0;  
 
  //! Get the type of the time
  virtual TimeType type() const = 0;

  //! Return a string representation of time
  virtual std::string toString() const = 0;

  //! Return double representation of time
  virtual double toDouble() const = 0;

  //! Assignment operator that can be virtual unlike operator=
  virtual int set(const VisusTime& t) = 0;

  //! Save time to XML representation
  virtual void toXML(XMLNode& parent) const = 0;

  //! Load time from XML representation
  virtual bool fromXML(XMLNode& node) = 0;

  //! Return the time coordinate relative to the given interval
  /*! Compute the time coordinate of this relative to the time interval
   *  [start,end]. The return value is a parameter p such that
   *  (1-p)*begin + p*end = this. 
   *  @param begin: start of the time interval
   *  @param end: end of the time interval
   *  @return relative time coordinate of this wrt. [begin,end]
   */
  virtual double relativeTime(const VisusTime& begin, const VisusTime& end) const;

  //! Return the current time index 
  /*! Assuming that the given interval [begin,end] is devided into
   *  step long pieces return the index of the piece closest to
   *  *this
   */
  virtual int timeIndex(const VisusTime& begin, const VisusTime& end, const VisusTime& step) const;

protected:

  explicit VisusTime() {}

  explicit VisusTime(const VisusTime&) {}

  //! Create an identical copy
  virtual VisusTime* clone() const = 0;

  //! Read the time value from a string
  virtual void fromString(const std::string& str) = 0;

  //! Compute the best step size for n pieces
  virtual void setToStep(const VisusTime& begin, const VisusTime& end, int steps) = 0;
  
private:
  

  static const char* TIMESTEP_XML_TAG;
};


#endif

