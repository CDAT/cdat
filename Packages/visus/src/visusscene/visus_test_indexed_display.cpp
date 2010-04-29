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


#include "VisusFLTKGui.h"
#include "VisusSceneNode.h"
#include "VisusIndexedDisplay.h"
#include "VisusIndexedData.h"
#include <fltk/Window.h>
#include <fltk/Widget.h>
#include <fltk/run.h>
#include <fltk/visual.h>
#include <cstdio>
#include <vector>

using namespace fltk;
using namespace std;

int main(int argc, char** argv)
{
  pVisusIndexedDisplay root = VisusIndexedDisplay::instantiate();
  VisusBoundingBox box,world_box;
  VisusIndexedData outlines(2);
  vector<vector<VisusIndexedData::VertexDataType> > line;

  box[0] = -180;
  box[1] = -90;
  box[2] = 0;
  box[3] = 180;
  box[4] = 90;
  box[5] = 40;

  root->setValue(box);

  if (argc < 2) {
    fprintf(stderr,"Usage:  %s <filename>\n", argv[0]);
    return 0;
  }

  FILE* input = fopen(argv[1],"r");
  
  if (input == NULL) {
    vwarning("Could not open input file %s.\n",argv[1]);
    return 0;
  }

  int np;
  float dummy;

  while (fscanf(input,"%d",&np) != EOF) {
    
    if (np < 0)
      break;
    np /= 2;
    
    fscanf(input,"%f %f %f %f %f",&dummy,&dummy,&dummy,&dummy,&dummy);

    line.resize(np);
    for (int i=0;i<np;i++) {
      line[i].resize(2);
      fscanf(input,"%f %f",&line[i][1],&line[i][0]);
      //fprintf(stderr,"Vertex %d: %f %f\n",i,line[i][0],line[i][1]);
    }
    
    outlines.constructElement(line);
  }
  fclose(input);

  root->loadData(&outlines,0);

  VisusFLTKGui gui;
  
  root->drawBoundingBox(true);
  gui.createWindow(root);
  
  return gui.mainLoop();
}
