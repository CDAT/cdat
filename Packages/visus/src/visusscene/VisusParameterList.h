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


#ifndef VISUSPARAMETERLIST_H
#define VISUSPARAMETERLIST_H

#include <vector>
#include "VisusParameter.h"
#include "VisusTypeInfo.h"

//! COntainer for a set of VisusParameters
/*! VisusParameterList creates a container for a set of
 *  VisusParameters. We could represent the set differently, for
 *  example, using a hash table, an stl::set or as done here as a
 *  vector. VisusParameterList creates a common wrapper that should
 *  allow an easy re-implementation if we decide to re-implement the
 *  local storage model
 */
class VisusParameterList {

public:

  class iterator {
    
  public:
    
    iterator() {}
    iterator(const std::vector<VisusParameter>::iterator &it) {mIt = it;}
    iterator(const iterator& it) {*this = it;}
    ~iterator() {}

    VisusParameter& operator*() {return *mIt;}
    VisusParameter* operator->() {return mIt.operator->();}
    void operator++(int i) {mIt++;}
    iterator &operator=(const iterator& it) {mIt = it.mIt; return *this;}
    bool operator==(const iterator& it) {return (mIt == it.mIt);}
    bool operator!=(const iterator& it) {return (mIt != it.mIt);}
    bool operator<(const iterator& it) {return (mIt < it.mIt);}
    
  private:

    std::vector<VisusParameter>::iterator mIt;
  };

  static const int sNumTypes = VISUS_NUM_TYPEIDS;

  /***************************************************************
   ******       Constructors/Destructors                 *********
   **************************************************************/
  
  //! Default constructor  
  VisusParameterList();

  //! Default destructor
  ~VisusParameterList() {}


  //! Overloaded access function
  VisusParameter &operator[](int i) {return mContainer[i];}
  const VisusParameter &operator[](int i) const {return mContainer[i];}
  const VisusParameter& get(int i) const {return mContainer[i];}
  
  int size() const {return (int)mContainer.size();}
  iterator begin() {return iterator(mContainer.begin());}
  iterator end() {return iterator(mContainer.end());}
  
private:

  std::vector<VisusParameter> mContainer;
};
  



#endif

