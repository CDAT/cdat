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
#include <cstring>


#include "VisusFactory.h"
#include "VisusAssert.h"
#include "VisusGroup.h"
#include "VisusSceneNode.h"
#include "VisusAxisAlignedExtractor.h"
#include "VisusTextNode.h"
#include "VisusSphereNode.h"
#include "VisusOrthogonalSlice.h"
#include "VisusTickMarks.h"
#include "VisusSphereSlice.h"
#include "VisusShapesNode.h"
#include "VisusEarthNode.h"
#include "VisusMeshDisplay.h"
#include "VisusColorBar.h"
#include "VisusDataProbe.h"
#include "VisusIsoSurface.h"
#include "VisusColoredIsoSurface.h"
#include "VisusTextureViewer.h"
#include "VisusLabelNode.h"
#include "VisusLineGraph.h"
#include "VisusHistogram.h"
#include "VisusHeightField.h"


VisusFactory gObjectFactory;

VisusFactory::~VisusFactory()
{
  std::list<GarbageItem>::iterator it;
  for (it=mGarbage.begin();it!=mGarbage.end();it++) {
    delete it->counter;
  }
}

void VisusFactory::releaseCounter(VisusAtomicCounter<short>* counter)
{
  GarbageItem item;

  item.counter = counter;
  item.timeStamp = time(NULL);

  mGarbage.insert(mGarbage.end(),item);
}


VisusSmartPointer<VisusGroup> VisusFactory::createNode(VisusNodeType nodeType)
{
  switch (nodeType)
  {
  case VISUS_GROUP_NODE:
    vverbose("creating group node\n", VISUS_XML_VERBOSE);
    return constructNode<VisusGroup>();

  case VISUS_SCENE_NODE:
    vverbose("creating scene node\n", VISUS_XML_VERBOSE);
    return constructNode<VisusSceneNode>();

  case VISUS_ALIGNED_EXTRACTOR:
    vverbose("creating aligned extractor node\n", VISUS_XML_VERBOSE);
    return constructNode<VisusAxisAlignedExtractor>();

  case VISUS_ORTHO_SLICE:
    vverbose("creating orthogonal slice\n", VISUS_XML_VERBOSE);
    return constructNode<VisusOrthogonalSlice>();

  case VISUS_TEXT_NODE:
    vverbose("creating text node\n", VISUS_XML_VERBOSE);
    return constructNode<VisusTextNode>();

  case VISUS_SPHERE_NODE:
    vverbose("creating sphere node\n", VISUS_XML_VERBOSE);
    return constructNode<VisusSphereNode>();

  case VISUS_COLOR_BAR:
    vverbose("creating color bar\n", VISUS_XML_VERBOSE);
    return constructNode<VisusColorBar>();

  case VISUS_TICK_MARKS:
    vverbose("creating tick marks\n", VISUS_XML_VERBOSE);
    return constructNode<VisusTickMarks>();

  case VISUS_ISOSURFACE:
    vverbose("creating isosurface\n", VISUS_XML_VERBOSE);
    return constructNode<VisusIsoSurface>();

  case VISUS_DATA_PROBE:
    vverbose("creating data probe\n", VISUS_XML_VERBOSE);
    return constructNode<VisusDataProbe>();

  case VISUS_SHAPES_NODE:
    vverbose("creating shapes\n", VISUS_XML_VERBOSE);
    return constructNode<VisusShapesNode>();

  case VISUS_MESH_NODE:
    vverbose("creating mesh node\n", VISUS_XML_VERBOSE);
    return constructNode<VisusMeshDisplay>();

  case VISUS_LABEL_NODE:
    vverbose("creating label node\n", VISUS_XML_VERBOSE);
    return constructNode<VisusLabelNode>();

  case VISUS_TEXTURE_VIEWER:
    vverbose("creating texture viewer\n", VISUS_XML_VERBOSE);
    return constructNode<VisusTextureViewer>();

  case VISUS_EARTH_NODE:
    vverbose("creating earth node\n", VISUS_XML_VERBOSE);
    return constructNode<VisusEarthNode>();

  case VISUS_SPHERE_SLICE:
    vverbose("creating sphere slice\n", VISUS_XML_VERBOSE);
    return constructNode<VisusSphereSlice>();

  case VISUS_LINE_GRAPH:
    vverbose("creating line graph\n", VISUS_XML_VERBOSE);
    return constructNode<VisusLineGraph>();

  case VISUS_HISTOGRAM:
    vverbose("creating histogram\n", VISUS_XML_VERBOSE);
    return constructNode<VisusHistogram>();

  case VISUS_COLORED_ISOSURFACE:
    vverbose("creating colored isosurface\n", VISUS_XML_VERBOSE);
    return constructNode<VisusColoredIsoSurface>();

  case VISUS_HEIGHT_FIELD:
    vverbose("creating height field\n", VISUS_XML_VERBOSE);
    return constructNode<VisusHeightField>();

  default:
    {
      std::stringstream ss;
      ss << "unknown type (" << nodeType << "). VisusFactory does not know how to create";
      vwarning(ss.str().c_str());
    }
  }
  return pVisusGroup();
}

VisusSmartPointer<VisusSharedValue> VisusFactory::constructSharedValue(int type_index)
{
  switch (type_index) {
  case VISUS_ISOVALUE_TYPEID:
    return gObjectFactory.constructSharedValue<VisusSharedIsoValue>();
    break;
  case VISUS_BOUNDINGBOX_TYPEID:
    return gObjectFactory.constructSharedValue<VisusSharedBoundingBox>();
    break;
  case VISUS_FONT_TYPEID:
    return gObjectFactory.constructSharedValue<VisusSharedFont>();
    break;
  case VISUS_TRANSFORMATION2D_TYPEID:
    return gObjectFactory.constructSharedValue<VisusSharedTransformation2D>();
    break;
  case VISUS_TRANSFORMATION3D_TYPEID:
    return gObjectFactory.constructSharedValue<VisusSharedTransformation3D>();
    break;
  case VISUS_DATAREQUEST_TYPEID:
    return gObjectFactory.constructSharedValue<VisusSharedDataRequest>();
    break;
  case VISUS_DATADESCRIPTION_TYPEID:
    return gObjectFactory.constructSharedValue<VisusSharedDataDescription>();
    break;
  case VISUS_FIELDINDEX_TYPEID:
    return gObjectFactory.constructSharedValue<VisusSharedFieldIndex>();
    break;
  case VISUS_COLORMAP_TYPEID:
    return gObjectFactory.constructSharedValue<VisusSharedColorMap>();
    break;
  case VISUS_COLOR_TYPEID:
    return gObjectFactory.constructSharedValue<VisusSharedColor>();
    break;
  case VISUS_CAMERA_TYPEID:
    return gObjectFactory.constructSharedValue<VisusSharedCamera>();
    break;
  case VISUS_EARTHRADIUS_TYPEID:
    return gObjectFactory.constructSharedValue<VisusSharedEarthRadius>();
    break;
  case VISUS_GLOBALTIME_TYPEID:
    return gObjectFactory.constructSharedValue<VisusSharedGlobalTime>();
    break;
  case VISUS_OPENGLSTATE_TYPEID:
    return gObjectFactory.constructSharedValue<VisusSharedOpenGLState>();
    break;
  default:
    vwarning("No such shared value with type index %d exists or VisusFactor::constructSharedValue function needs to be updated.",type_index);
    return VisusSmartPointer<VisusSharedValue>();
  }
}

