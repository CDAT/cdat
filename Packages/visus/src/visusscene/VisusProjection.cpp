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

#include "VisusProjection.h"

VisusProjection::VisusProjection()
{
}

// Destructor
VisusProjection::~VisusProjection()
{
}


  
int VisusProjection::project(VisusIndexedData const* source, VisusIndexedData* sink)
{
  vassert(source!=NULL);
  vassert(sink!=NULL);

  VisusTransformation3D matrix;


  if (!isCompatibleInput(source)) {
    vwarning("This projection cannot be used on this indexed data set. The input units to not match.");
    return 0;
  }

  //fprintf(stderr,"VisusProjection::project\n");

  // For simplicity we simply first copy all data from the
  // source. This takes care of dimension, vertex numbers etc.
  sink->copyContent(source);

  // Once the projection is complete we are in a new (presumably
  // correct) space and thus our transformation is no longer necessary
  sink->matrix(VisusTransformation3D());

  // Set the output units
  sink->unit(this->outputUnits());

  matrix = source->matrix();

  //fprintf(stderr,"%f %f %f %f\n",matrix[0],matrix[4],matrix[8],matrix[12]);
  //fprintf(stderr,"%f %f %f %f\n",matrix[1],matrix[5],matrix[9],matrix[13]);
  //fprintf(stderr,"%f %f %f %f\n",matrix[2],matrix[6],matrix[10],matrix[14]);
  //fprintf(stderr,"%f %f %f %f\n\n",matrix[3],matrix[7],matrix[11],matrix[15]);

  // For each vertex in the input
  for (int i=0;i<sink->nrOfVertices();i++) {
    
    //fprintf(stderr,"Domain vertex %f %f %f\n",sink->vertex(i)[0],sink->vertex(i)[1],sink->vertex(i)[2]);

    // First map the data coordinates into world space 
    matrix.map(sink->vertexAddress(i),&sink->vertex(i)[0]);
  
    //fprintf(stderr,"Domain vertex %f %f %f\n\n",sink->vertex(i)[0],sink->vertex(i)[1],sink->vertex(i)[2]);

    // Now project the vertex 
    projectVertex(sink->vertex(i),sink->vertex(i),source->unit());
  }

  return 1;
}
