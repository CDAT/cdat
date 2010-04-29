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


#ifndef VISUSSHAREDISOVALUE_H
#define VISUSSHAREDISOVALUE_H

#include "VisusTypeInfo.h"
#include "VisusIsoValue.h"
#include "VisusSharedValue.h"
#include "VisusFactory.h"
#include "VisusSmartPointer.h"

struct XMLNode;

class VisusSharedIsoValue;
typedef VisusSmartPointer<VisusSharedIsoValue> pVisusSharedIsoValue;

class VisusSharedIsoValue : public VisusSharedValue, public VisusIsoValue
{
  VISUS_DECLARE_ATOMIC(VisusSharedIsoValue,VisusIsoValue);
public:

  static const char* XML_TAG;

  /***************************************************************
   ******       Constructors/Destructors                 *********
   **************************************************************/
  
  //! Default constructor
  VisusSharedIsoValue();
  
  //! Copy constructor
  VisusSharedIsoValue(const VisusSharedIsoValue &iso);
      
  //! Destructor
  virtual ~VisusSharedIsoValue() {}
 
  /***************************************************************
   ******       Access functionality                     *********
   **************************************************************/

protected:
  //! Build instance data from XML tree
  bool fromXML(XMLNode& node);

  //! Build instance data into XML tree
  void toXML(XMLNode& parent) const;
   
};


#endif

