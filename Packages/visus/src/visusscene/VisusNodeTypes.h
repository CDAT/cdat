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

#ifndef VISUS_NODE_TYPES_H
#define VISUS_NODE_TYPES_H


#define VISUS_NODE_CATEGORY_SIZE 5

enum VisusNodeCategory {
  VISUS_GENERAL_NODE    = 0 << VISUS_NODE_CATEGORY_SIZE,
  VISUS_EXTRACTOR_NODE  = 1 << VISUS_NODE_CATEGORY_SIZE,
  VISUS_PROCESSING_NODE = 2 << VISUS_NODE_CATEGORY_SIZE,
  VISUS_DISPLAY_NODE    = 3 << VISUS_NODE_CATEGORY_SIZE,

  VISUS_UNDEFINED_NODE_CATEGORY = ~((1 << VISUS_NODE_CATEGORY_SIZE) - 1),
};

enum VisusNodeType {
  
  VISUS_GROUP_NODE        = 0 | VISUS_GENERAL_NODE,
  VISUS_SCENE_NODE        = 1 | VISUS_GENERAL_NODE,

  VISUS_ALIGNED_EXTRACTOR = 0 | VISUS_EXTRACTOR_NODE,

  VISUS_ORTHO_SLICE       = 0 | VISUS_DISPLAY_NODE,
  VISUS_TEXT_NODE         = 1 | VISUS_DISPLAY_NODE,
  VISUS_SPHERE_NODE       = 2 | VISUS_DISPLAY_NODE,
  VISUS_COLOR_BAR         = 3 | VISUS_DISPLAY_NODE,
  VISUS_TICK_MARKS        = 4 | VISUS_DISPLAY_NODE,
  VISUS_ISOSURFACE        = 5 | VISUS_DISPLAY_NODE,
  VISUS_DATA_PROBE        = 6 | VISUS_DISPLAY_NODE,
  VISUS_SHAPES_NODE       = 7 | VISUS_DISPLAY_NODE,
  VISUS_MESH_NODE         = 8 | VISUS_DISPLAY_NODE,
  VISUS_LABEL_NODE        = 9 | VISUS_DISPLAY_NODE,
  VISUS_TEXTURE_VIEWER    = 10 | VISUS_DISPLAY_NODE,
  VISUS_EARTH_NODE        = 11 | VISUS_DISPLAY_NODE,
  VISUS_SPHERE_SLICE      = 12 | VISUS_DISPLAY_NODE,
  VISUS_LINE_GRAPH        = 13 | VISUS_DISPLAY_NODE,
  VISUS_HISTOGRAM         = 14 | VISUS_DISPLAY_NODE,
  VISUS_COLORED_ISOSURFACE= 15 | VISUS_DISPLAY_NODE,
  VISUS_HEIGHT_FIELD      = 16 | VISUS_DISPLAY_NODE,
  VISUS_HALO              = 17 | VISUS_DISPLAY_NODE,

  VISUS_FILTER_NODE       = 0 | VISUS_PROCESSING_NODE,
  VISUS_PROJECTOR_NODE    = 1 | VISUS_PROCESSING_NODE,

  VISUS_UNDEFINED_NODE    = ((1 << VISUS_NODE_CATEGORY_SIZE) - 1) | VISUS_UNDEFINED_NODE_CATEGORY,
};

#endif
