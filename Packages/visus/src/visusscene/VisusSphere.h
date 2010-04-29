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


#ifndef VISUSPHERE_H
#define VISUSPHERE_H

#ifdef WIN32
#pragma warning(disable:4244)
#pragma warning(disable:4996)
#endif

#include "glew.h"

#include <cmath>
#include <iostream>

class VisusSphere
{
public:
    VisusSphere();
    ~VisusSphere();

    void setRadius(double n) { _radius = n; }
    double getRadius() const { return _radius; }

    void setOrigin(const double *n);
    void getOrigin(double *n) const;
    
    void setNumberOfTheta(int n);
    int getNumberOfTheta() const { return _numtheta; }

    void setNumberOfPhi(int n);
    int getNumberOfPhi() const { return _numphi; }

    void loadTexture(unsigned char *pix, int wid, int hgt, GLenum format);

    void render();

protected:
    double _radius;
    double _origin[3];

    int _numtheta, _numphi;
    double *_costheta, *_sintheta;
    double *_cosphi, *_sinphi;

    GLenum _format;
    GLuint _texName;
    bool _texEnabled;
    unsigned char *_pixels;
    int _wid, _hgt;
    double _texScale[2];
    
    void setupTexture();
};

#endif
