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


#ifndef VISUSGENERICVALUE_H
#define VISUSGENERICVALUE_H

struct XMLNode;

/*! A GenericValue is designed to be used as baseclass for all shared
 *  values that are build types, e.g. int's, float's, double's etc..
 */
template<class DataType>
class VisusGenericValue 
{
 public:
  
  /***************************************************************
   ******       Constructors/Destructors                 *********
   **************************************************************/

  //! Default constructor
  VisusGenericValue(): mData() {}

  //! Copy constructor
  VisusGenericValue(const VisusGenericValue& value);

  //! Copy constructor
  VisusGenericValue(const DataType& value) {*this = value;}

  //! Destructor
  virtual ~VisusGenericValue() {}
  
  virtual VisusGenericValue& operator=(const VisusGenericValue& value);
  virtual VisusGenericValue& operator=(const DataType& value);

  virtual VisusGenericValue& operator*=(const DataType& value);
  virtual VisusGenericValue& operator/=(const DataType& value);

  operator DataType() const {return mData;}

  //! Build XML instance data into XML tree
  void toXML(XMLNode& node) const;

  /***************************************************************
   ******       Access functionality                     *********
   **************************************************************/
  
  //! Return the local data 
  virtual DataType get() const {return mData;}

  //! Alternative access to the local data 
  virtual DataType data() const {return get();}
  
  //! Set the local data
  virtual void set(const DataType& data) {mData = data;}

  //! Alternative method to set the local data 
  virtual void data(const DataType& data) {set(data);}

protected:
  
  //! Local data value 
  DataType mData;

};

template<class DataType>
VisusGenericValue<DataType>::VisusGenericValue(const VisusGenericValue& value)
{
  *this = value;
}

template<class DataType>
VisusGenericValue<DataType>& VisusGenericValue<DataType>::operator=(const VisusGenericValue& value)
{
  mData = value.mData;
  return *this;
}

template<class DataType>
VisusGenericValue<DataType>& VisusGenericValue<DataType>::operator=(const DataType& value)
{
  mData = value;
  return *this;
}

template<class DataType>
VisusGenericValue<DataType>& VisusGenericValue<DataType>::operator*=(const DataType& value)
{
  mData = mData * value;
  return *this;
}

template<class DataType>
VisusGenericValue<DataType>& VisusGenericValue<DataType>::operator/=(const DataType& value)
{
  mData = mData / value;
  return *this;
}



#endif
