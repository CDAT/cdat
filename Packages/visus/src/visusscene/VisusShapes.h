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


#ifndef VISUSHAPES_H
#define VISUSHAPES_H

#ifdef WIN32
#pragma warning(disable:4244)
#pragma warning(disable:4996)
#endif

#include "glew.h"
#include <GL/glut.h>

#include <algorithm>
#include <cmath>
#include <fstream>
#include <iostream>
#include <map>
#include <string>
#include <vector>


struct OrbitPosition
{
  //float x,y,alt;
  float lat,lon,rad;
};

class VisusShapes
{
    static const double EARTH_SCALE;
    static const double EARTH_RADIUS;
    static const double TIME_FRAME;
    static const double PERSISTENCE;

    static const unsigned char CATALOG_COLOR[4];
    static const unsigned char DEBRIS_COLOR[4];
    static const unsigned char HUBBLE_COLOR[4];
    static const unsigned char RADAR_COLOR[4];
    static const unsigned char OPTICAL_COLOR[4];
    static const unsigned char BERMUDA_COLOR[4];
    
public:
    static const char* NAMES;
    static const char* ORBIT_FORMAT;
    static const char* TIMESTEP_FORMAT;
    static const char* POSITION_FORMAT;
    static const char* DETECTED_FORMAT;

    VisusShapes();
    virtual ~VisusShapes();

    bool loadGeometry(const char *file);
    bool loadNames(const char *file);
    bool loadTimestep(const char *file);
    bool loadBermuda(const char *file);
    bool loadRadarBeam(const char *file);

    bool loadOrbit(const char* file, const int objectId, OrbitPosition*& data);
    bool loadOrbit(const char* file, const char* objectName, OrbitPosition*& data);

    bool loadPosition(const int timestep, const char* baseDirectory=".");
    bool loadDetected(const int timestep, const char* baseDirectory=".");

    void setMagnification(int m);
    void setFrameNumber(int f);
    int getFrameNumber() const { return _frame + 1; }
    
    void render();
    
    bool drawCatalog() const { return mDrawCatalog; }
    void drawCatalog(bool value) { mDrawCatalog = value; }

    bool drawHubble() const { return mDrawHubble; }
    void drawHubble(bool value) { mDrawHubble = value; }

    bool drawDebris() const { return mDrawDebris; }
    void drawDebris(bool value) { mDrawDebris = value; }

    bool drawStations() const { return mDrawStations; }
    void drawStations(bool value) { mDrawStations = value; }

    bool drawBermuda() const { return mDrawBermuda; }
    void drawBermuda(bool value) { mDrawBermuda = value; }

#ifndef SWIG
  struct SphereObj
  {
	  double x, y, z, alt;
	  int optical, radar;
    bool opticalDet, radarDet;

	  void set(int r, int o) { radar = r; optical = o; }
	  void compute(double lat, double lon, double rad);
  };

  std::vector<SphereObj>& getDebris() { return _debris; }
  std::vector<SphereObj>& getCatalog() { return _catalog; }
  std::vector<SphereObj*>& getAllSpheres() { return _sphereReadOrder; }
#endif

  std::string getTimestepFile() { return _timestepFile; }

protected:
  struct ConeObj
  {
	  double x, y, z;
	  double b, h;
	  double ax, ay;
	  double angle;
	
	  void set(double base, double height) { b = base; h = height; }
	  void compute(double lat, double lon, double rad);
	  void rotate(double lat, double lon, double rad);
  };
  struct Node
  {
    double x, y, z;
	
    Node() {}
    Node(double a, double b, double c) : x(a), y(b), z(c) {}
	
    friend std::istream& operator>>(std::istream &in, Node &node)
    {
	    in >> node.x >> node.y >> node.z;
	    return in;
    }
    friend std::ostream& operator<<(std::ostream &out, const Node &node)
    {
	    out << node.x << ' ' << node.y << ' ' << node.z;
	    return out;
    }
  };
  struct Tri
  {
    int i0, i1, i2;
	
    bool exist(int i)
    {
	    return (i == i0 || i == i1 || i == i2);
    }
    friend std::istream& operator>>(std::istream &in, Tri &tri)
    {
	    in >> tri.i0 >> tri.i1 >> tri.i2;
	    return in;
    }
    friend std::ostream& operator<<(std::ostream &out, const Tri &tri)
    {
	    out << tri.i0 << ' ' << tri.i1 << ' ' << tri.i2;
	    return out;
    }
  };
  struct Quad
  {
    int i0, i1, i2, i3;
	
    bool exist(int i)
    {
	    return (i == i0 || i == i1 || i == i2 || i == i3);
    }
    friend std::istream& operator>>(std::istream &in, Quad &quad)
    {
	    in >> quad.i0 >> quad.i1 >> quad.i2 >> quad.i3;
	    return in;
    }
    friend std::ostream& operator<<(std::ostream &out, const Quad &quad)
    {
	    out << quad.i0 << ' ' << quad.i1 << ' ' << quad.i2 << ' ' << quad.i3;
	    return out;
    }
  };
  struct Chunk
  {
    int id;
    Node center;
    std::vector<Tri> faces;
  };
  struct FootPrint
  {
    int id, index;
    double x, y, z;
    double time;
    double ax, ay;
    double angle;
    
    void set(int i, double t) { id = i; time = t; }
    void compute(double lat, double lon, double rad);
  };
    int _numDebris;
    int _numCatalog;

    std::vector<SphereObj> _catalog;
    std::vector<SphereObj> _debris;
    std::vector<SphereObj*> _sphereReadOrder;
    
    double _catalogRadius, _debrisRadius;

    std::vector<ConeObj> _radar;
    std::vector<ConeObj> _optical;

    std::vector<int> _catalogIds;
    std::vector<int> _debrisIds;
    std::vector<int> _chunkIds;

    std::vector<Node> _nodes;
    std::vector<Node> _normals;
    std::vector<Chunk> _chunks;

    double _debrisScale, _hstScale;

    SphereObj _hst;
    GLUquadricObj *_HSTBase, *_HSTCap1, *_HSTCap2;
    GLUquadricObj *_HSTHead, *_HSTTip;
    GLUquadricObj *_HSTBar, *_HSTEnd1, *_HSTEnd2;

    int _frame;
    double _timestep;
    
    std::vector<FootPrint> _bermuda;
    GLUquadricObj *_bermudaDisk;

    std::vector<Node> _beam;

    std::string _timestepFile;

    void loadStations();
    
    void renderCatalog();
    void renderHubble();
    void renderDebris();
    void renderStations();
    void renderBermuda();

    void interpolate(int i, const unsigned char *a, const unsigned char *b, unsigned char *c);
  
    void reset();

    bool mDrawCatalog;
    bool mDrawHubble;
    bool mDrawDebris;
    bool mDrawStations;
    bool mDrawBermuda;
};

#endif
