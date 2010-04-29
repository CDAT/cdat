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


#ifndef VISUSDATA_H
#define VISUSDATA_H

#include <vector>

#include "VisusTransformation3D.h"

#include "VisusXMLInterface.h"

struct XMLNode;

#define VISUS_DATA_TYPE(category,id) ((id) | (category << VisusDataCategoryBits)) 

static const int VisusDataCategoryBits = 5;
enum VisusDataCategory {
  VISUS_GRID_DATA = 0,
  VISUS_INDEXED_SET = 1,

  VISUS_UNDEFINED_DATA_CATEGORY = ((unsigned int)(-1)),
};

enum VisusDataFormat {
  
  VISUS_BLOCK_DATA     = VISUS_DATA_TYPE( VISUS_GRID_DATA, 1),
  VISUS_TEXTURE_DATA   = VISUS_DATA_TYPE( VISUS_GRID_DATA, 2),

  VISUS_INDEXED_DATA   = VISUS_DATA_TYPE( VISUS_INDEXED_SET, 1),
  VISUS_MESH_DATA      = VISUS_DATA_TYPE( VISUS_INDEXED_SET, 2),

  VISUS_UNDEFINED_DATA = -1,
};
  



//! Data type used to implement a unique id for each snap-shot of a
//! data set
typedef int VisusDataID;

const VisusDataID NULLID = -1;

//! Baseclass for all data passed around in the scene graph
/*! VisusData forms the base class for all data handed around in the
 *  scene graph. It contains an enum indicating
 *  the data format and an id indicating the "version number" of a piece
 *  of data.
 */
class VisusData
{
public:
  
  static const char* XML_TAG;

  /***************************************************************
   ******       Constructors/Destructors                 *********
   **************************************************************/
  
  //! Default constructor 
  VisusData(VisusDataFormat t = VISUS_UNDEFINED_DATA);

  //! Standard destructor
  virtual ~VisusData();
  
  //! Return the data type
  VisusDataFormat dataFormat() const {return mDataFormat;}

  //! Return the data category
  VisusDataCategory dataCategory() const {return (VisusDataCategory)(mDataFormat >> VisusDataCategoryBits);}

  //! Get the current ID
  VisusDataID id() const {return mId;}
  
  //! Increase ID
  void incId() {mId++;}
  
  //! Set the id
  void id(VisusDataID id) {mId = id;}

  //! Reset the id after a pipe change
  void resetId() {mId = NULLID;}

  //! Swap the content of \e this with the content of data.
  /*! Swap the content of \e this with the content of data. This
   *  function is used to pass data through pipes as the fastest/best
   *  way of updating a given data type (e.g. block data). Note that,
   *  depending on the actual types involved an implementation might
   *  choose to copy data rather than swap. The only assumptions are
   *  that both \e this and data are valid (though potentially empty)
   *  data objects and \e this will contain the contents of data after the
   *  call. The call will NOT protect either \e this or data so the
   *  caller must lock all appropriate mutexes. Finally, a swap will
   *  not modify the id of a data set just its "content".
   *  @param data: pointer to a valid data object
   *  @return 1 if successful 0 otherwise
   */ 
  virtual int swapContent(VisusData* data) = 0;

  //! Copy the content of data into \e this
  virtual int copyContent(const VisusData* data) = 0;

  //! Determine whether \e this can load its content from data
  virtual bool readCompatible(const VisusData* data) const = 0;

  //! Build XML instance data into XML tree
  void toXML(XMLNode& parent) const;

  //! Build XML instance data from XML tree
  bool fromXML(XMLNode& node);

protected:

  virtual void toXMLLocalVariables(XMLNode& parent) const=0;
  virtual bool fromXMLLocalVariables(XMLNode& parent, XMLDataStorage storageType)=0;
  
  //! The data type
  VisusDataFormat mDataFormat;
  
  //! ID number 
  VisusDataID mId;
};


#endif
