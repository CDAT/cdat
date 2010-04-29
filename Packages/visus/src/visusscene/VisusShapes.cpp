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
#include <ctime>

#include "VisusShapes.h"
#include "VisusTimer.h"

const double DEG_TO_RAD = 0.01745329251994329577;
const double RAD_TO_DEG = 57.2957795130823208768;

const double VisusShapes::EARTH_SCALE = 0.000001;
const double VisusShapes::EARTH_RADIUS = 6.378137;
const double VisusShapes::TIME_FRAME = (54505.7375 - 54504.4875) / 10800;
const double VisusShapes::PERSISTENCE = 24;

const char* VisusShapes::NAMES = "names.txt";
const char* VisusShapes::TIMESTEP_FORMAT = "%s/time_%05d.txt";
const char* VisusShapes::ORBIT_FORMAT = "%s/orbit_%05d.bin";
const char* VisusShapes::POSITION_FORMAT = "%s/position_%05d.bin";
const char* VisusShapes::DETECTED_FORMAT = "%s/detected_%05d.bin";

const unsigned char VisusShapes::CATALOG_COLOR[4] = {180, 180, 180, 255};
const unsigned char VisusShapes::DEBRIS_COLOR[4]  = {255, 255,   0, 255};
const unsigned char VisusShapes::HUBBLE_COLOR[4]  = {255,  64,   0, 255};
const unsigned char VisusShapes::RADAR_COLOR[4]   = {  0, 255, 255, 255};
const unsigned char VisusShapes::OPTICAL_COLOR[4] = {255,   0, 255, 255};
const unsigned char VisusShapes::BERMUDA_COLOR[4] = {  0, 128,   0, 255};

static std::map<int,int> gRadar, gOptical;  // waiting for python implementation
static std::map<int,std::string> gName;
static std::vector<int> gNameIdAsRead;


template<class T>
int binarySearch(const std::vector<T> &vec, const T &val)
{
    typename std::vector<T>::const_iterator i =
        lower_bound(vec.begin(), vec.end(), val);
    return (i != vec.end() && *i == val) ? i-vec.begin() : -1;
}


VisusShapes::VisusShapes()
    : _numDebris(0),
      _numCatalog(0),
      _catalogRadius(0.03),
      _debrisRadius(0.01),
      _debrisScale(0.001),
      _hstScale(0.1),
      mDrawCatalog(true),
      mDrawHubble(true),
      mDrawDebris(true),
      mDrawStations(true),
      mDrawBermuda(true)
{
    loadStations();
    
    _HSTBase = gluNewQuadric();
    gluQuadricDrawStyle(_HSTBase, GLU_FILL);
    gluQuadricNormals(_HSTBase, GLU_SMOOTH);
    gluQuadricOrientation(_HSTBase, GLU_OUTSIDE);
    
    _HSTCap1 = gluNewQuadric();
    gluQuadricDrawStyle(_HSTCap1, GLU_FILL);
    gluQuadricNormals(_HSTCap1, GLU_SMOOTH);
    gluQuadricOrientation(_HSTCap1, GLU_INSIDE);

    _HSTCap2 = gluNewQuadric();
    gluQuadricDrawStyle(_HSTCap2, GLU_FILL);
    gluQuadricNormals(_HSTCap2, GLU_SMOOTH);
    gluQuadricOrientation(_HSTCap2, GLU_OUTSIDE);

    _HSTHead = gluNewQuadric();
    gluQuadricDrawStyle(_HSTHead, GLU_FILL);
    gluQuadricNormals(_HSTHead, GLU_SMOOTH);
    gluQuadricOrientation(_HSTHead, GLU_OUTSIDE);

    _HSTTip = gluNewQuadric();
    gluQuadricDrawStyle(_HSTTip, GLU_FILL);
    gluQuadricNormals(_HSTTip, GLU_SMOOTH);
    gluQuadricOrientation(_HSTTip, GLU_OUTSIDE);

    _HSTBar = gluNewQuadric();
    gluQuadricDrawStyle(_HSTBar, GLU_FILL);
    gluQuadricNormals(_HSTBar, GLU_SMOOTH);
    gluQuadricOrientation(_HSTBar, GLU_OUTSIDE);

    _HSTEnd1 = gluNewQuadric();
    gluQuadricDrawStyle(_HSTEnd1, GLU_FILL);
    gluQuadricNormals(_HSTEnd1, GLU_SMOOTH);
    gluQuadricOrientation(_HSTEnd1, GLU_INSIDE);

    _HSTEnd2 = gluNewQuadric();
    gluQuadricDrawStyle(_HSTEnd2, GLU_FILL);
    gluQuadricNormals(_HSTEnd2, GLU_SMOOTH);
    gluQuadricOrientation(_HSTEnd2, GLU_OUTSIDE);

    _bermudaDisk = gluNewQuadric();
    gluQuadricDrawStyle(_bermudaDisk, GLU_FILL);
    gluQuadricNormals(_bermudaDisk, GLU_SMOOTH);
    gluQuadricOrientation(_bermudaDisk, GLU_OUTSIDE);
}

VisusShapes::~VisusShapes()
{
    gluDeleteQuadric(_HSTBase);
    gluDeleteQuadric(_HSTCap1);
    gluDeleteQuadric(_HSTCap2);
    gluDeleteQuadric(_HSTHead);
    gluDeleteQuadric(_HSTTip);
    gluDeleteQuadric(_HSTBar);
    gluDeleteQuadric(_HSTEnd1);
    gluDeleteQuadric(_HSTEnd2);
    gluDeleteQuadric(_bermudaDisk);
}

// bool VisusShapes::loadGeometry(const char *file)
// {
//     std::ifstream in(file);
//     if (!in) {
// 	std::cerr << "Error: cannot open file " << file << std::endl;
// 	return false;
//     }
//     int nNodes, nChunks;
//     in >> nNodes >> nChunks;
    
//     _nodes.resize(nNodes);
//     _normals.resize(nNodes);
//     _chunks.resize(nChunks);
//     _chunkIds.resize(nChunks);
    
//     for (int i = 0; i < nNodes; ++i)
// 	in >> _nodes[i] >> _normals[i];
    
//     int nQuads;
//     for (int i = 0; i < nChunks; ++i) {
// 	in >> _chunks[i].id >> nQuads >> _chunks[i].center;
// 	_chunks[i].faces.resize(nQuads);
// 	_chunkIds[i] = _chunks[i].id;

// 	_chunks[i].center.x -= _chunks[0].center.x;
// 	_chunks[i].center.y -= _chunks[0].center.y;
// 	_chunks[i].center.z -= _chunks[0].center.z;
	
// 	for (int j = 0; j < nQuads; ++j)
// 	    in >> _chunks[i].faces[j];
//     }
//     return true;
// }

bool VisusShapes::loadGeometry(const char *file)
{
    std::ifstream in(file);
    if (!in) {
	std::cerr << "Error: cannot open file " << file << std::endl;
	return false;
    }
    int nNodes, nChunks;
    in >> nNodes >> nChunks;
    
    _nodes.resize(nNodes);
    _normals.resize(nNodes);
    _chunks.resize(nChunks);
    _chunkIds.resize(nChunks);
    
    for (int i = 0; i < nNodes; ++i)
	in >> _nodes[i] >> _normals[i];
    
    int nTris;
    for (int i = 0; i < nChunks; ++i) {
	in >> _chunks[i].id >> nTris >> _chunks[i].center;
	_chunks[i].faces.resize(nTris);
	_chunkIds[i] = _chunks[i].id;

	_chunks[i].center.x -= _chunks[0].center.x;
	_chunks[i].center.y -= _chunks[0].center.y;
	_chunks[i].center.z -= _chunks[0].center.z;
	
	for (int j = 0; j < nTris; ++j)
	    in >> _chunks[i].faces[j];
    }
    return true;
}

void VisusShapes::reset()
{
  _debris.clear();
  _debrisIds.clear();
  _catalog.clear();
  _catalogIds.clear();
  _sphereReadOrder.clear();
}

bool VisusShapes::loadNames(const char *file)
{
  std::ifstream in(file);
  if (!in) {
    std::cerr << "Error: cannot open file " << file << std::endl;
    return false;
  }
  gName.clear();
  gNameIdAsRead.clear();

  char name[29];
  name[28] = 0;
    
  _numDebris = 0;
  _numCatalog= 0;
  int numObjects = 0;

  while (true) 
  {
    in.get(name, 28);
    if (in.eof()) break;

    int id;
    in >> id;
    in.ignore(1024, '\n');
  
    gName[id] = name;
    gNameIdAsRead.push_back(id);

	  if (strstr(name, "DEBRIS"))
      ++_numDebris;
    else if (strstr(name, "HST"))
      ;
    else 
      ++_numCatalog;

    ++numObjects;
  }
  return true;
}


bool VisusShapes::loadPosition(const int timestep, const char* baseDirectory)
{ 
  //VisusTimer timer("loadPosition()");
  struct Data_v1 { float lat, lon, rad; };

  reset();

  // Ensure that loadNames was called first
  if (gName.size() == 0) {
    std::cerr << "Error: loadBinaryNames() must be called before loadPosition() is called" << std::endl;
    std::cerr.flush();
    return false;
  }

  // Open binary position file
  char filename[1024];
  sprintf(filename, POSITION_FORMAT, baseDirectory, timestep); 
  std::ifstream in(filename, std::ios_base::in | std::ios_base::binary);
  if (!in.is_open()) {
    std::cerr << "Error: cannot open file " << filename << std::endl;
    return false;
  }

  // Read version of position file
  int version, numObjects;
  in.read((char*)&version,    sizeof(int));
  in.read((char*)&numObjects, sizeof(int));

  // Names array should be same size as id - verification of match to names.txt
  if (gNameIdAsRead.size() != numObjects) {
    std::cerr << "Error: given names file (count " << gNameIdAsRead.size() 
      << ") does not match to binary file " << filename << "(count " << numObjects << ")" << std::endl;
    std::cerr.flush();
    return false;
  }

  // Allocate for objects
  _debris.resize(_numDebris);
  _debrisIds.resize(_numDebris);
  _catalog.resize(_numCatalog);
  _catalogIds.resize(_numCatalog);

  // Allocate and read all objects
  Data_v1* data = new Data_v1[numObjects];
  in.read((char*)data, sizeof(Data_v1)*numObjects);  

  // Store Objects Appropriately
  SphereObj* sphere = NULL;
  std::vector<SphereObj>::iterator debrisIter=_debris.begin();
  std::vector<int>::iterator debrisIdIter=_debrisIds.begin();
  std::vector<SphereObj>::iterator catalogIter=_catalog.begin();
  std::vector<int>::iterator catalogIdIter=_catalogIds.begin();
   for (int i=0; i<numObjects; ++i)
  {
    const int id = gNameIdAsRead[i];

	  const char *name = gName[id].c_str();
    if (name==NULL) {
      std::cerr << "unable to find name for id " << id << std::endl;
      std::cerr.flush();
      return false;
    }

	  if (strstr(name, "DEBRIS")) {
      sphere = &(*debrisIter);
      ++debrisIter;
      (*debrisIdIter) = atoi(name+7);
      ++debrisIdIter;
    }
    else if (strstr(name, "HST")) {
      sphere = &_hst;
    }
    else {
      sphere = &(*catalogIter);
      ++catalogIter;
      (*catalogIdIter) = id;
      ++catalogIdIter;
    }    
    
    // Initialize sphere
    sphere->opticalDet = false;
    sphere->radarDet = false;
    sphere->compute(data[i].lat, data[i].lon, data[i].rad*EARTH_SCALE);
  	sphere->set(1, 1);

    _sphereReadOrder.push_back(sphere);
  }

  // cleanup temporary memory
  delete [] data;

  // update timestep file name
  _timestepFile = filename;

  return true;
}

bool VisusShapes::loadDetected(const int timestep, const char* baseDirectory)
{
  //VisusTimer timer("loadDetected()");

  struct Data_v1 { int radar, optical; };

  // Ensure that loadNames was called first
  if (_sphereReadOrder.size() == 0) {
    std::cerr << "Error: loadPosition() must be called before loadDetected() is called" << std::endl;
    std::cerr.flush();
    return false;
  }

  // Open binary detected file
  char filename[1024];
  sprintf(filename, DETECTED_FORMAT, baseDirectory, timestep);
  std::ifstream in(filename, std::ios_base::in | std::ios_base::binary);
  if (!in.is_open()) {
    std::cerr << "Error: cannot open file " << filename << std::endl;
    return false;
  }

  // Read version of detected file
  int version=0;
  in.read((char*)&version, sizeof(int));

  // Read number of objects
  int numObjects=0;
  in.read((char*)&numObjects, sizeof(int));

  // Ensure number of objects aligns with loadPosition()
  if (numObjects != (int)_sphereReadOrder.size()) {
    std::cerr << "Error: detected file " << filename 
      << " does not correspond to loaded position file as num objects differ" << std::endl;
    std::cerr.flush();
    return false;
  }

  // Allocate and read all objects
  Data_v1* data = new Data_v1[numObjects];
  in.read((char*)data, sizeof(Data_v1)*numObjects);  

  // Store Objects Appropriately
  std::vector<SphereObj*>::iterator iter=_sphereReadOrder.begin();
  for (int i=0; i<numObjects; ++i, ++iter)
  {
    SphereObj* sphere = (*iter);

    // Initialize sphere
    sphere->opticalDet = data[i].optical > 0;
    sphere->radarDet = data[i].radar > 0;

    if (data[i].radar > 0) {
	    data[i].radar = gRadar[i] = PERSISTENCE;
	  }
	  else {
	    std::map<int,int>::iterator itr = gRadar.find(i);
	    if (itr != gRadar.end()) {
		  data[i].radar = itr->second;
		  if (--itr->second == 0)
		    gRadar.erase(itr);
	    }
	  }
	  if (data[i].optical) {
	    data[i].optical = gOptical[i] = PERSISTENCE;
	  }
	  else {
	    std::map<int,int>::iterator itr = gOptical.find(i);
	    if (itr != gOptical.end()) {
		  data[i].optical = itr->second;
		  if (--itr->second == 0)
		    gOptical.erase(itr);
	    }
	  }

    // Set Persistent Radar/Optical
	  sphere->set(data[i].radar, data[i].optical);
  }

  // cleanup temporary memory
  delete [] data;

  return true;
}

bool VisusShapes::loadOrbit(const char* baseDirectory, const int objectId, OrbitPosition*& data)
{
  // Open binary detected file
  char filename[1024];
  sprintf(filename, ORBIT_FORMAT, baseDirectory, objectId);
  printf("Reading file %s\n", filename);
  std::ifstream in(filename, std::ios_base::in | std::ios_base::binary);
  if (!in.is_open()) {
    std::cerr << "Error: cannot open file " << filename << std::endl;
    return false;
  }

  // Read version of file
  int version=0;
  in.read((char*)&version, sizeof(int));

  // Read number of time steps in file
  int numTimes=0;
  in.read((char*)&numTimes, sizeof(int));

  // Allocate for number of time steps
  if (data!=NULL)
    delete [] data;
  data = new OrbitPosition[numTimes];

  // Read positions from file
  in.read((char*)data, sizeof(OrbitPosition) * numTimes);

  return true;
}

bool VisusShapes::loadOrbit(const char* baseDirectory, const char* objectName, OrbitPosition*& data)
{
  // Find object containing given object name and process first found
  for (std::map<int, std::string>::iterator iter=gName.begin(); iter!=gName.end(); ++iter) {
    if (iter->second == objectName)
      return loadOrbit(baseDirectory, iter->first, data);
  }
  return false;
}

bool VisusShapes::loadTimestep(const char *file)
{
  //VisusTimer timer("loadTimestep()");

    std::map<int,int> &radar = gRadar;
    std::map<int,int> &optical = gOptical;
    
    std::ifstream in(file);
    if (!in) {
	std::cerr << "Error: cannot open file " << file << std::endl;
  	return false;
    }

  reset();

  // update timestep file name
  _timestepFile = file;

    while (true) {
	int id;
	in >> id;

	if (in.eof()) break;

	double lat, lon, rad;
	in >> lat >> lon >> rad;

	int rdr, opt;
	in >> rdr >> opt;
    
  // Initialize Sphere Object
	SphereObj sphere;
  sphere.radarDet = rdr;
  sphere.opticalDet = opt;
	sphere.compute(lat, lon, rad*EARTH_SCALE);

  if (rdr > 0) {
	    rdr = radar[id] = PERSISTENCE;
	}
	else {
	    std::map<int,int>::iterator itr = radar.find(id);
	    if (itr != radar.end()) {
		rdr = itr->second;
		if (--itr->second == 0)
		    radar.erase(itr);
	    }
	}
	if (opt) {
	    opt = optical[id] = PERSISTENCE;
	}
	else {
	    std::map<int,int>::iterator itr = optical.find(id);
	    if (itr != optical.end()) {
		opt = itr->second;
		if (--itr->second == 0)
		    optical.erase(itr);
	    }
	}

  // Set Persistent Radar/Optical
	sphere.set(rdr, opt);

	const char *name = gName[id].c_str();
	if (strstr(name, "DEBRIS")) {
	    _debris.push_back(sphere);
	    _debrisIds.push_back(atoi(name+7));
	}
	else if (strstr(name, "HST")) {
	    _hst = sphere;
	}
	else {
	    _catalog.push_back(sphere);
	    _catalogIds.push_back(id);
	}
    }

  return true;
}

bool VisusShapes::loadRadarBeam(const char *file)
{
    std::ifstream in(file);
    if (!in) {
	std::cerr << "Error: cannot open file " << file << std::endl;
	return false;
    }
    _beam.clear();
    
    Node node;
    while (true) {
	in >> node;
	if (in.eof()) break;
	
	_beam.push_back(node);
    }
    return true;
}

bool VisusShapes::loadBermuda(const char *file)
{
    std::ifstream in(file);
    if (!in) {
	std::cerr << "Error: cannot open file " << file << std::endl;
	return false;
    }
    _bermuda.clear();
    
    char name[29];
    name[28] = 0;
    while (true) {
	in.get(name, 28);
	if (in.eof()) break;
	
	double values[5];
	for (int i = 0; i < 5; ++i)
	    in >> values[i];
	in.ignore(1024, '\n');
	
	FootPrint foot;
	foot.set(values[0], values[1]);
	foot.compute(values[2], values[3], EARTH_RADIUS+0.01);
	foot.index = binarySearch(_catalogIds, foot.id);
	
	if (foot.index > -1)
	    _bermuda.push_back(foot);
	else
	    std::cerr << "Warning: invalid bermuda id " << values[0] << std::endl;
    }
    return true;
}

void VisusShapes::setMagnification(int m)
{
    _catalogRadius = 3 * m * EARTH_SCALE;
    _debrisRadius = m * EARTH_SCALE;
    _debrisScale = 0.1 * m * EARTH_SCALE;
    _hstScale = 10 * m * EARTH_SCALE;
}

void VisusShapes::setFrameNumber(int f)
{
    _frame = f - 1;
    _timestep = 54504.4875 + _frame * TIME_FRAME;
    
    // elevate radius slightly for better visualization
    //_radar.rotate(_beam[_frame].x, _beam[_frame].y, _beam[_frame].z*EARTH_SCALE+0.1);
}

void VisusShapes::render()
{
    glEnable(GL_COLOR_MATERIAL);
    glEnable(GL_NORMALIZE);

    if (mDrawStations)
      renderStations();
    if (mDrawCatalog)
      renderCatalog();
    if (mDrawHubble)
      renderHubble();
    if (mDrawDebris)
      renderDebris();
    if (mDrawBermuda)
      renderBermuda();

    glDisable(GL_COLOR_MATERIAL);
    glDisable(GL_NORMALIZE);
}

void VisusShapes::loadStations()
{
    _radar.resize(6);
    
    _radar[0].set(0.1, 1);
    _radar[0].compute(9.39528, 167.479, EARTH_RADIUS);

    _radar[1].set(0.1, 1);
    _radar[1].compute(54.3616, 0.6697, EARTH_RADIUS);

    _radar[2].set(0.1, 1);
    _radar[2].compute(39.136111, 121.350833, EARTH_RADIUS);

    _radar[3].set(0.1, 1);
    _radar[3].compute(41.752222, 70.538056, EARTH_RADIUS);

    _radar[4].set(0.1, 1);
    _radar[4].compute(64.300278, 149.189722, EARTH_RADIUS);

    _radar[5].set(0.1, 1);
    _radar[5].compute(76.569, 68.318, EARTH_RADIUS);

    _optical.resize(5);

    _optical[0].set(0.1, 1);
    _optical[0].compute(-30.1653, -70.815, EARTH_RADIUS);

    _optical[1].set(0.1, 1);
    _optical[1].compute(-7.4117, 72.4572, EARTH_RADIUS);

    _optical[2].set(0.1, 1);
    _optical[2].compute(37.17, -5.609, EARTH_RADIUS);

    _optical[3].set(0.1, 1);
    _optical[3].compute(20.7088, -156.258, EARTH_RADIUS);

    _optical[4].set(0.1, 1);
    _optical[4].compute(33.187, -106.66, EARTH_RADIUS);
}

void VisusShapes::renderCatalog()
{
    glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);

    for (size_t i = 0; i < _catalog.size(); ++i) {
	double detect = 1;
	unsigned char color[4];

	if (_catalog[i].optical > 0) {
	    detect += _catalog[i].optical * 2 / PERSISTENCE;
	    interpolate(_catalog[i].optical, OPTICAL_COLOR, CATALOG_COLOR, color);
	    glColor4ubv(color);
	}
	else if (_catalog[i].radar > 0) {
	    detect += _catalog[i].radar * 2 / PERSISTENCE;
	    interpolate(_catalog[i].radar, RADAR_COLOR, CATALOG_COLOR, color);
	    glColor4ubv(color);
	}
	else {
	    glColor4ubv(CATALOG_COLOR);
	}
	glPushMatrix();
	glTranslated(_catalog[i].x, _catalog[i].y, _catalog[i].z);
	glutSolidSphere(_catalogRadius*detect, 16, 8);
	glPopMatrix();
    }
}

void VisusShapes::renderHubble()
{
    glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
    
    double detect = 1;
    unsigned char color[4];

    if (_hst.optical > 0) {
	detect += _hst.optical * 2 / PERSISTENCE;
	interpolate(_hst.optical, OPTICAL_COLOR, HUBBLE_COLOR, color);
	glColor4ubv(color);
    }
    else if (_hst.radar > 0) {
	detect += _hst.radar * 2 / PERSISTENCE;
	interpolate(_hst.radar, RADAR_COLOR, HUBBLE_COLOR, color);
	glColor4ubv(color);
    }
    else {
	glColor4ubv(HUBBLE_COLOR);
    }
    glPushMatrix();
    glTranslated(_hst.x, _hst.y, _hst.z);
    glScaled(_hstScale*detect, _hstScale*detect, _hstScale*detect);
    
    gluCylinder(_HSTBase, 0.4, 0.4, 1, 16, 1);
    gluDisk(_HSTCap1, 0, 0.4, 16, 1);
    glTranslated(0, 0, 1);
    gluDisk(_HSTCap2, 0, 0.4, 16, 1);
    gluCylinder(_HSTHead, 0.3, 0.3, 0.9, 16, 1);
    glTranslated(0, 0, 0.9);
    gluDisk(_HSTTip, 0, 0.3, 16, 1);

    glTranslated(-1.2, 0, -0.9);
    glRotated(90, 0, 1, 0);
    gluCylinder(_HSTBar, 0.025, 0.025, 2.4, 8, 1);
    gluDisk(_HSTEnd1, 0, 0.025, 8, 1);
    glTranslated(0, 0, 2.4);
    gluDisk(_HSTEnd2, 0, 0.025, 8, 1);

    glTranslated(0, 0, -0.32);
    glRotated(30, 0, 0, 1);
    glScaled(2, 0.03, 0.6);
    glutSolidCube(1);
    glTranslated(0, 0, -2.94);
    glutSolidCube(1);

    glPopMatrix();
}

void VisusShapes::renderDebris()
{
    glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);

    for (size_t i = 0; i < _debris.size(); ++i) {
	double detect = 1;
	unsigned char color[4];

	if (_debris[i].optical > 0) {
	    detect += _debris[i].optical * 2 / PERSISTENCE;
	    interpolate(_debris[i].optical, OPTICAL_COLOR, DEBRIS_COLOR, color);
	    glColor4ubv(color);
	}
	else if (_debris[i].radar > 0) {
	    detect += _debris[i].radar * 2 / PERSISTENCE;
	    interpolate(_debris[i].radar, RADAR_COLOR, DEBRIS_COLOR, color);
	    glColor4ubv(color);
	}
	else {
	    glColor4ubv(DEBRIS_COLOR);
	}
	glPushMatrix();
	glTranslated(_debris[i].x, _debris[i].y, _debris[i].z);

	int id = binarySearch(_chunkIds, _debrisIds[i]);
	if (id > -1 && _chunks[id].faces.size() > 12) {
	    glScaled(_debrisScale*detect, _debrisScale*detect, _debrisScale*detect);

	    //glBegin(GL_QUADS);
	    glBegin(GL_TRIANGLES);
	    for (size_t j = 0; j < _chunks[id].faces.size(); ++j) {
		const Node &p0 = _nodes[_chunks[id].faces[j].i0];
		const Node &n0 = _normals[_chunks[id].faces[j].i0];
		glNormal3d(n0.x, n0.y, n0.z);
		glVertex3d(p0.x, p0.y, p0.z);
		
		const Node &p1 = _nodes[_chunks[id].faces[j].i1];
		const Node &n1 = _normals[_chunks[id].faces[j].i1];
		glNormal3d(n1.x, n1.y, n1.z);
		glVertex3d(p1.x, p1.y, p1.z);
		
		const Node &p2 = _nodes[_chunks[id].faces[j].i2];
		const Node &n2 = _normals[_chunks[id].faces[j].i2];
		glNormal3d(n2.x, n2.y, n2.z);
		glVertex3d(p2.x, p2.y, p2.z);
		
// 		const Node &p3 = _nodes[_chunks[id].faces[j].i3];
// 		const Node &n3 = _normals[_chunks[id].faces[j].i3];
// 		glNormal3d(n3.x, n3.y, n3.z);
// 		glVertex3d(p3.x, p3.y, p3.z);
	    }
	    glEnd();
	}
	else {
	    glutSolidSphere(_debrisRadius*detect, 16, 8);
	}
	glPopMatrix();
    }
}

void VisusShapes::renderStations()
{
    glLineWidth(1);
    glEnable(GL_LINE_SMOOTH);

    glEnable(GL_BLEND);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

    float ambient0[4];
    glGetLightfv(GL_LIGHT0, GL_AMBIENT,  ambient0);

    float ambient1[4] = {1, 1, 1, 1};
    glLightfv(GL_LIGHT0, GL_AMBIENT,  ambient1);

    glColor4ubv(RADAR_COLOR);
    for (size_t i = 0; i < _radar.size(); ++i) {
	glPushMatrix();
	glTranslated(_radar[i].x, _radar[i].y, _radar[i].z);
	glRotated(_radar[i].angle, _radar[i].ax, _radar[i].ay, 0);
	glTranslated(0, 0, -_radar[i].h);
	glScaled(_radar[i].b, _radar[i].b, _radar[i].h);
	glutWireCone(1, 1, 8, 1);
	glPopMatrix();
    }
    glColor4ubv(OPTICAL_COLOR);
    for (size_t i = 0; i < _optical.size(); ++i) {
	glPushMatrix();
	glTranslated(_optical[i].x, _optical[i].y, _optical[i].z);
	glRotated(_optical[i].angle, _optical[i].ax, _optical[i].ay, 0);
	glTranslated(0, 0, -_optical[i].h);
	glScaled(_optical[i].b, _optical[i].b, _optical[i].h);
	glutWireCone(1, 1, 8, 1);
	glPopMatrix();
    }
    glDisable(GL_LINE_SMOOTH);
    glDisable(GL_BLEND);

    glLightfv(GL_LIGHT0, GL_AMBIENT,  ambient0);
}

void VisusShapes::renderBermuda()
{
    glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
    glColor4ubv(BERMUDA_COLOR);

    float ambient0[4];
    glGetLightfv(GL_LIGHT0, GL_AMBIENT,  ambient0);

    float ambient1[4] = {1, 1, 1, 1};
    glLightfv(GL_LIGHT0, GL_AMBIENT,  ambient1);

    glEnable(GL_BLEND);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    
    for (size_t i = 0; i < _bermuda.size(); ++i)
	if (_bermuda[i].time <= _timestep) {
	    glPushMatrix();
	    glTranslated(_bermuda[i].x, _bermuda[i].y, _bermuda[i].z);
	    glRotated(_bermuda[i].angle, _bermuda[i].ax, _bermuda[i].ay, 0);
	    glScaled(0.1, 0.1, 0.1);
	    gluDisk(_bermudaDisk, 0, 1, 32, 1);
	    glPopMatrix();
	}
    glDisable(GL_BLEND);
    glLightfv(GL_LIGHT0, GL_AMBIENT,  ambient0);
    
    for (size_t i = 0; i < _bermuda.size(); ++i)
	if (_bermuda[i].time <= _timestep) {
	    const SphereObj &obj = _catalog[_bermuda[i].index];
	    glPushMatrix();
	    glTranslated(obj.x, obj.y, obj.z);
	    glutSolidSphere(_catalogRadius*2, 16, 8);
	    glPopMatrix();
	}

  // Reset color - not sure why but not doing this messes with texture fonts
  glColor4f(1.0,1.0,1.0,1.0);
}

inline void VisusShapes::interpolate(int i, const unsigned char *a, const unsigned char *b,
				     unsigned char *c)
{
    double u = i / PERSISTENCE;
    double v = 1 - u;
    c[0] = u * a[0] + v * b[0];
    c[1] = u * a[1] + v * b[1];
    c[2] = u * a[2] + v * b[2];
    c[3] = u * a[3] + v * b[3];
}

void VisusShapes::SphereObj::compute(double lat, double lon, double rad)
{
    double phi = (90 - lat) * DEG_TO_RAD;
    double theta = (lon + 180) * DEG_TO_RAD;
    
    double sin_phi = sin(phi);
    x = rad * cos(theta) * sin_phi;
    y = rad * sin(theta) * sin_phi;
    z = rad * cos(phi);
    alt = rad - EARTH_RADIUS;
}

void VisusShapes::ConeObj::compute(double lat, double lon, double rad)
{
    double phi = (90 - lat) * DEG_TO_RAD;
    double theta = (lon + 180) * DEG_TO_RAD;

    double sin_phi = sin(phi);
    x = rad * cos(theta) * sin_phi;
    y = rad * sin(theta) * sin_phi;
    z = rad * cos(phi);

    // (0,0,-h) x (x,y,z)
    ax =  h * y;
    ay = -h * x;
    // (0,0,-h) . (x,y,z)
    angle = RAD_TO_DEG * acos(-z / sqrt(x*x + y*y + z*z));
}

void VisusShapes::ConeObj::rotate(double lat, double lon, double rad)
{
    double phi = (90 - lat) * DEG_TO_RAD;
    double theta = (lon + 180) * DEG_TO_RAD;

    double sin_phi = sin(phi);
    double x1 = rad * cos(theta) * sin_phi;
    double y1 = rad * sin(theta) * sin_phi;
    double z1 = rad * cos(phi);

    double dx = x1 - x;
    double dy = y1 - y;
    double dz = z1 - z;
    
    // (0,0,-1) x (dx,dy,dz)
    ax =  dy;
    ay = -dx;
    // (0,0,-1) . (dx,dy,dz)
    angle = RAD_TO_DEG * acos(-dz / sqrt(dx*dx + dy*dy + dz*dz));
}

void VisusShapes::FootPrint::compute(double lat, double lon, double rad)
{
    double phi = (90 - lat) * DEG_TO_RAD;
    double theta = (lon + 180) * DEG_TO_RAD;
    
    double sin_phi = sin(phi);
    x = rad * cos(theta) * sin_phi;
    y = rad * sin(theta) * sin_phi;
    z = rad * cos(phi);
    
    // (0,0,1) x (x,y,z)
    ax = -y;
    ay =  x;
    // (0,0,1) . (x,y,z)
    angle = RAD_TO_DEG * acos(z / sqrt(x*x + y*y + z*z));  
}

