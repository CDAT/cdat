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


#ifndef VISUSGLOBALTIME_H
#define VISUSGLOBALTIME_H

#include <vector>

#include "VisusTime.h"

struct XMLNode;

//! Global time parameter encapsulating different time types
class VisusGlobalTime
{
public:

  static const char* XML_TAG;

  /***************************************************************
   ******       Constructors/Destructors                 *********
   **************************************************************/

  //! Default constructor 
  VisusGlobalTime();
  
  //! Copy constructor;
  VisusGlobalTime(const VisusGlobalTime& t);

  //! Destructor
  virtual ~VisusGlobalTime();
 
  //! Assignment operator Implicitly can change the TimeType
  virtual VisusGlobalTime& operator=(const VisusGlobalTime& value);

  //! Comparison operator between time values
  bool operator==(const VisusGlobalTime& value) const;

  //! Comparison operator between time values
  bool operator!=(const VisusGlobalTime& value) const;

  /***************************************************************
   ******       Access functionality                     *********
   **************************************************************/

  //! Return the time type (assuming all members have the same type)
  TimeType type() const {return mCurrentTime->type();}

  //! Return the current time value
  const VisusTime& time() const {return *mCurrentTime;}

  //! Set a new time value 
  /*! This function sets the GlobalTime to a new value. The new value
   *  will automatically be checked against the current interval and
   *  clamped to its bounds if necessary
   *  @param time: the new time value
   *  @return The new time value
   */
  const VisusTime& time(const VisusTime& t);

  // Set the starting time
  void begin(const VisusTime& t) const {mBegin->set(t);}

  // Set the end time
  void end(const VisusTime& t) const {mEnd->set(t);}

  // Set the end time
  void step(const VisusTime& t) const {mStep->set(t);}  

  // Convert the current time step into a string
  std::string toString() const;

  void toXML(XMLNode& parent) const;
  bool fromXML(XMLNode& node);

  //! Set the time step
  void timeStep(const VisusTime& timeStep);

  //! Go to next time step
  void inc();

  //! Go to previous time step
  void dec();

  //! Return the time coordinate relative to the current interval
  /*! Compute the time coordinate of the current time  relative to the time interval
   *  [mBegin,mEnd]. The return value is a parameter p such that
   *  (1-p)*mBegin + p*mEnd = McurrentTime. 
   */  
  double relativeTime() const;

  //! Return the current time index 
  /*! Assuming that the current interval [mBegin,mEnd] is devided into
   *  mStep long pieces return the index of the piece closest to
   *  mCurrentTime
   */
  int timeIndex() const;
  
  //! Read a string representation
  int initializeFromString(std::vector<std::string>::iterator& it, int time_steps);

protected:
  
  //! The current time
  VisusTime* mCurrentTime;

  //! Beginning of the time interval
  VisusTime* mBegin;

  //! End of the time interval
  VisusTime* mEnd;

  //! Smallest useful interval
  VisusTime* mStep;

private:
  
  //! Copy the time information and potentially changing its type
  void copyTime(VisusTime** target, const VisusTime* source);

  //! Make sure that the time remains within the given interval
  void clampTime();

  //! Sets all times to the default value of the given type
  void switchType(TimeType t);
};

#endif

