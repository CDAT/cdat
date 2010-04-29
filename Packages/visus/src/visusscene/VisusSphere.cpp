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

#include "VisusSphere.h"
#include "VisusMath.h"

const double M_TWO_PI = 6.28318530717958647693;

inline int getpow2(int x)
{
    int n = 1;
    while (n < x) n <<= 1;
    return n;
}

VisusSphere::VisusSphere()
    : _radius(1),
      _numtheta(0),
      _numphi(0),
      _costheta(NULL),
      _sintheta(NULL),
      _cosphi(NULL),
      _sinphi(NULL),
      _texName(0),
      _texEnabled(false)
{
    _origin[0] = _origin[1] = _origin[2] = 0;
    _texScale[0] = _texScale[1] = 1;

    setNumberOfTheta(20);
    setNumberOfPhi(40);
}

VisusSphere::~VisusSphere()
{
    delete[] _costheta;
    delete[] _sintheta;
    delete[] _cosphi;
    delete[] _sinphi;
}

void VisusSphere::setOrigin(const double *n)
{
    _origin[0] = n[0];
    _origin[1] = n[1];
    _origin[2] = n[2];
}

void VisusSphere::getOrigin(double *n) const
{
    n[0] = _origin[0];
    n[1] = _origin[1];
    n[2] = _origin[2];
}

void VisusSphere::setNumberOfTheta(int n)
{
  if (n != _numtheta) {
	  delete[] _costheta;
	  delete[] _sintheta;

	  _numtheta = n;
  	_costheta = new double[n+1];
	  _sintheta = new double[n+1];

	  double scale = M_TWO_PI / (n << 1);
	  for (int i = 0; i <= n; ++i) {
      double theta = i * scale - M_PI_2;
	    _costheta[i] = cos(theta);
	    _sintheta[i] = sin(theta);
	  }
  }
}

void VisusSphere::setNumberOfPhi(int n)
{
    if (n != _numphi) {
	delete[] _cosphi;
	delete[] _sinphi;

	_numphi = n;
	_cosphi = new double[n+1];
	_sinphi = new double[n+1];

	double scale = M_TWO_PI / n;
	for (int i = 0; i <= n; ++i) {
	    double phi = i * scale;
	    _cosphi[i] = cos(phi);
	    _sinphi[i] = sin(phi);
	}
    }
}

void VisusSphere::loadTexture(unsigned char *pix, int wid, int hgt, GLenum format)
{
    _pixels = pix;
    _wid = wid;
    _hgt = hgt;
    _format = format;
    _texEnabled = true;
}

void VisusSphere::render()
{
    if (_texEnabled && _texName == 0)
	setupTexture();

    // Need to rebind our texture in case another texture was bound to 2D
    glBindTexture(GL_TEXTURE_2D, _texName);

    glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
    glEnable(GL_NORMALIZE);
    glEnable(GL_TEXTURE_2D);

    glRotatef(-90, 1, 0, 0);

    double thetaScale = _texScale[1] / (_numtheta << 1);
    double phiScale = _texScale[0] / _numphi;

    double n[3], p[3];
    for (int j = 0; j < _numtheta; ++j) {
	int k = j + 1;
	double tj = (j << 1) * thetaScale;
	double tk = (k << 1) * thetaScale;
	
	glBegin(GL_QUAD_STRIP);
	for (int i = 0; i <= _numphi; ++i) {
	    double ti = i * phiScale;

	    n[0] = _costheta[k] * _cosphi[i];
	    n[1] = _sintheta[k];
	    n[2] = _costheta[k] * _sinphi[i];

	    p[0] = _origin[0] + _radius * n[0];
	    p[1] = _origin[1] + _radius * n[1];
	    p[2] = _origin[2] + _radius * n[2];
	    
	    glNormal3f(n[0], n[1], n[2]);
	    glTexCoord2f(ti, tk);
	    glVertex3f(p[0], p[1], p[2]);
	    
	    n[0] = _costheta[j] * _cosphi[i];
	    n[1] = _sintheta[j];
	    n[2] = _costheta[j] * _sinphi[i];

	    p[0] = _origin[0] + _radius * n[0];
	    p[1] = _origin[1] + _radius * n[1];
	    p[2] = _origin[2] + _radius * n[2];

	    glNormal3f(n[0], n[1], n[2]);
	    glTexCoord2f(ti, tj);
	    glVertex3f(p[0], p[1], p[2]);
	}
	glEnd();
    }
    glDisable(GL_NORMALIZE);
    glDisable(GL_TEXTURE_2D);
}

void VisusSphere::setupTexture()
{
    glGenTextures(1, &_texName);
    glBindTexture(GL_TEXTURE_2D, _texName);
    
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
    
    glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_REPLACE);

    int wid = _wid, hgt = _hgt;
    if (!strstr((const char*)glGetString(GL_EXTENSIONS), "GL_ARB_texture_non_power_of_two")) {
	wid = getpow2(wid);
	hgt = getpow2(hgt);
    }
    GLint size;
    glGetIntegerv(GL_MAX_TEXTURE_SIZE, &size);

    if (wid > size || hgt > size) {
	_texEnabled = false;
	return;
    }
    if (wid != _wid || hgt != _hgt) {
	glTexImage2D(GL_TEXTURE_2D, 0, _format, wid, hgt, 0, _format, GL_UNSIGNED_BYTE, NULL);
	glTexSubImage2D(GL_TEXTURE_2D, 0, 0, 0, _wid, _hgt, _format, GL_UNSIGNED_BYTE, _pixels);
	
	_texScale[0] = double(_wid) / wid;
	_texScale[1] = double(_hgt) / hgt;
    }
    else {
	glTexImage2D(GL_TEXTURE_2D, 0, _format, wid, hgt, 0, _format, GL_UNSIGNED_BYTE, _pixels);
    }
    if (glGetError() != GL_NO_ERROR) {
      std::cerr << "Warning: OpenGL texture 2D is unavailable" << std::endl;
      _texEnabled = false;
    }
}
