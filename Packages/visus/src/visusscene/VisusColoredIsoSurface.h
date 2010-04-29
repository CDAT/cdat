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


#ifndef VISUSCOLOREDISOSURFACE_H
#define VISUSCOLOREDISOSURFACE_H

#include "VisusIsoSurface.h"
#include "VisusTexture.h"

enum CISDataIndex {
	CIS_ISO_DATA=0,
	CIS_COLOR_DATA
};

class VisusColoredIsoSurface;
typedef VisusSmartPointer<VisusColoredIsoSurface> pVisusColoredIsoSurface;

//! Node which extracts an isosurface
/*! VisusIsoSurface implements a node which extracts an isosurface
 *  from a rectangular piece of volume data. The node is both a
 *  consumer getting its data from a data extractor and a producer
 *  creating a mesh data structure of the resulting iso-surface.
 */
class VisusColoredIsoSurface : public VisusIsoSurface
{
public:

  static pVisusColoredIsoSurface instantiate();
  
  /***************************************************************
   ******       Constructors/Destructors                 *********
   **************************************************************/  

  //! Default constructor
  VisusColoredIsoSurface();

  //! Destructor
  virtual ~VisusColoredIsoSurface();

  //! Attach a data extractor as the color data
  int connectColor(pVisusProducer producer);

  virtual int checkData(const std::vector<VisusData*>& inputs);

protected:

  void toXMLLocalVariables(XMLNode& node);
  bool fromXMLLocalVariables(XMLNode& node);

  //! Get the contour data from the libcontour library
  virtual Contour3dData* getContourData(ConDataset* contourData, const VisusIsoValue& isoValue);


private:

  /***************************************************************
   ******       Private Variables And Functions          *********
   **************************************************************/  
  //! The local copy of colored data
  VisusTexture mColorData;
  
  //! Local copy of combined iso and color data
  unsigned char* mLocalData;
};

 

#endif
