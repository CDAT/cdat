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

#ifndef SHAREDVALUEMENU_H
#define SHAREDVALUEMENU_H

#include "vfltk/ValueMenu.h"
#include "VisusGroup.h"

template<class ValueClass>
class SharedValueMenu : public ValueMenu<ValueClass>
{
public:

  SharedValueMenu(pVisusGroup node, fltk::Window* parent=NULL);

  SharedValueMenu(ValueClass& value, fltk::Window* parent=NULL);

  virtual ~SharedValueMenu() {}

  //! Return the reference to the current value
  virtual ValueClass& value();
  
  //! Commit the current value to the system
  virtual void commit();

protected:

  pVisusGroup mNode;
};

template<class ValueClass>
SharedValueMenu<ValueClass>::SharedValueMenu(pVisusGroup node, fltk::Window* parent) : 
  ValueMenu<ValueClass>(parent), mNode(node)
{
  if (mNode == NULL)
    vwarning("Creating shared value menu from NULL node will have no effect.");
}
template<class ValueClass>
SharedValueMenu<ValueClass>::SharedValueMenu(ValueClass& value, fltk::Window* parent) : 
  ValueMenu<ValueClass>(value,parent)
{
}


template<class ValueClass>
ValueClass& SharedValueMenu<ValueClass>::value()
{
  if (mNode != NULL)
    mNode->getValue(this->mLocalValue);

  return this->mValue;
}


template<class ValueClass>
void SharedValueMenu<ValueClass>::commit()
{
  if (mNode != NULL)
    mNode->setValue(this->mLocalValue);
}


#endif
