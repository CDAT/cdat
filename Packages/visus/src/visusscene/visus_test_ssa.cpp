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


#include "VisusTextNode.h"
#include "VisusSphereNode.h"
#include "VisusShapesNode.h"
#include "VisusSceneNode.h"
#include "VisusMeshData.h"
#include "VisusMeshDisplay.h"

#include "VisusDefaultColorMaps.h"
#include "VisusHalo.h"

#include "VisusXMLInterface.h"

using namespace std;

pVisusSceneNode gRoot;
pVisusGroup gFocus;
int gMouseX, gMouseY, gPressed;
int gKeyboardMod = 0;
int gWid = 1024, gHgt = 768;
int gMagnification = 20000;
int gTimestep = 1, gStart = 1, gStop = 10801;
int gDay = 8, gHour = 11, gMinute = 42, gSecond = 0;
int gAnimation = 0;
bool gEnableDraw = false, gDrawImage = false;
bool gReadBinary = false;


int readPNM(const char *file, int &wid, int &hgt, unsigned char* &data)
{
    ifstream in(file, ios::in|ios::binary);
    if (!in) {
	cerr << "Error: cannot open " << file << endl;
	return 0;
    }
    char header[3];
    in.getline(header, 3);

    int comps = (strcmp(header, "P5") == 0) ? 1 : 3;
    in >> wid >> hgt;

    int val;
    in >> val;
    in.ignore(1024, '\n');

    unsigned int size = comps * wid * hgt;
    data = new unsigned char[size];
    
    in.read((char*)data, size);
    in.close();
    
    return comps;
}

bool loadTimestep(pVisusShapesNode shapes)
{
    char file[1024];

    if (gReadBinary) 
    {
      if (! shapes->loadPosition(gTimestep, "payload"))
        return false;
    }
    else {
      sprintf(file, VisusShapes::TIMESTEP_FORMAT, "payload", gTimestep);
      shapes->loadTimestep(file);
    }
    shapes->setFrameNumber(gTimestep);
    return true;
}

void printImage()
{
    int len = 3 * gWid, size = len * gHgt;
    unsigned char *data = new unsigned char[size];
    
    glReadBuffer(GL_FRONT);
    glReadPixels(0, 0, gWid, gHgt, GL_RGB, GL_UNSIGNED_BYTE, data);

    char file[1024];
    sprintf(file, "ssa_%05d.ppm", gTimestep);
     
    ofstream out(file, ios::out|ios::binary);
    if (!out) {
	cerr << "Error: cannot open " << file << endl;
	return;
    }
    out << "P6" << endl << gWid << ' ' << gHgt << endl << 255 << endl;

    // flip image vertically
    unsigned char *row = data + size;
    while (row > data) {
	row -= len;
	out.write((char*)row, len);
    }
    delete[] data;

    cout << "Saving image file: " << file << endl;
}

void increaseTime()
{
    ++gTimestep;
    gSecond += 10;
    if (gSecond == 60) {
	gSecond = 0;
	if (++gMinute == 60) {
	    gMinute = 0;
	    if (++gHour == 24) {
		gHour = 0;
		++gDay;
	    }
	}
    }
}

void decreaseTime()
{
    --gTimestep;
    gSecond -= 10;
    if (gSecond == -10) {
	gSecond = 50;
	if (--gMinute == -1) {
	    gMinute = 59;
	    if (--gHour == -1) {
		gHour = 23;
		--gDay;
	    }
	}
    }
}

void updateScreen()
{
    char str[1024];
    sprintf(str, "%d-%02d-%02dT %02d:%02d:%02dZ", 2008, 2, gDay, gHour, gMinute, gSecond);
    ((pVisusTextNode)gRoot->child(2))->text(str);
    
    loadTimestep((pVisusShapesNode)gRoot->child(1));
    
    glutPostRedisplay();
}

void initDisplay()
{
    float ambient[4]  = {0, 0, 0, 1};
    float diffuse[4]  = {1, 1, 1, 1};
    float specular[4] = {1, 1, 1, 1};
    float position[4] = {1, 1, 1, 0};
    
    glLightfv(GL_LIGHT0, GL_AMBIENT,  ambient);
    glLightfv(GL_LIGHT0, GL_DIFFUSE,  diffuse);
    glLightfv(GL_LIGHT0, GL_SPECULAR, specular);
    glLightfv(GL_LIGHT0, GL_POSITION, position);

    glViewport(0, 0, gWid, gHgt);
//     glMatrixMode(GL_PROJECTION);
//     glLoadIdentity();
//     glOrtho(-12, 12, -12*gHgt/(float)gWid, 12*gHgt/(float)gWid, -1000, 1000);

    glEnable(GL_DEPTH_TEST);
    glEnable(GL_LIGHTING);
    glEnable(GL_LIGHT0);
}

void displayFunc()
{
  VisusOpenGLState state;
  state.fetchState();
  gRoot->setValue(state);

    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
    gRoot->display();
    glutSwapBuffers();

    if (gDrawImage) {
	printImage();
	gDrawImage = false;
    }
}

void reshapeFunc(int x, int y)
{
    glViewport(0, 0, x, y);
//     glMatrixMode(GL_PROJECTION);
//     glLoadIdentity();
//     glOrtho(-12, 12, -12*y/(float)x, 12*y/(float)x, -100, 100);
}

void mouseFunc(int button, int state, int x, int y)
{
    if (state == GLUT_DOWN) {
      gKeyboardMod = glutGetModifiers();
        gPressed = button;
	gMouseX = x;
	gMouseY = y;
    }
    else {
	gPressed = -1;
    }
}

void motionFunc(int x, int y)
{
    if (gKeyboardMod & GLUT_ACTIVE_SHIFT) {
	switch (gPressed) {
	case GLUT_LEFT_BUTTON:
	    gFocus->rotate((x-gMouseX)/(float)gWid*2, (y-gMouseY)/(float)gHgt*2);
	    break;
	case GLUT_MIDDLE_BUTTON:
	    gFocus->translate((x-gMouseX)/(float)gWid*16, (y-gMouseY)/(float)gHgt*16);
	    break;
	case GLUT_RIGHT_BUTTON:
	    gFocus->scale((x-gMouseX)/(float)gWid*2, (y-gMouseY)/(float)gHgt*2);
	    break;
	default:
	    return;
	}
    }
    else {
	switch (gPressed) {
	case GLUT_LEFT_BUTTON:
	    gRoot->rotate((x-gMouseX)/(float)gWid*2, (y-gMouseY)/(float)gHgt*2);
	    break;
	case GLUT_MIDDLE_BUTTON:
	    gRoot->translate((x-gMouseX)/(float)gWid*16, (y-gMouseY)/(float)gHgt*16);
	    break;
	case GLUT_RIGHT_BUTTON:
          //gRoot->scale((x-gMouseX)/(float)gWid*2, (y-gMouseY)/(float)gHgt*2);
          gRoot->scaleJoystick((x-gMouseX)/(float)gWid*2, (y-gMouseY)/(float)gHgt*2);
	    break;
	default:
	    return;
	}
    }
    gMouseX = x;
    gMouseY = y;
    glutPostRedisplay();
}

void keyboardFunc(unsigned char key, int x, int y)
{
    gKeyboardMod = glutGetModifiers();

    switch (key) {
    case 'f':
	glutFullScreen();
	break;
    case 'g':
	if (gTimestep > gStart) {
	    decreaseTime();
	    updateScreen();
	}
	break;
    case 'G':
	gAnimation = -1;
	break;
    case 'h':
	gAnimation = 0;
	break;
    case 'j':
	if (gTimestep < gStop) {
	    increaseTime();
	    updateScreen();
	}
	break;
    case 'J':
	gAnimation = 1;
	break;
    case 'p':
	printImage();
	break;
    case 'P':
	gEnableDraw = !gEnableDraw;
	cout << ((gEnableDraw)?"Enable":"Disable") << " animation draw" << endl;
	break;
    case 'q':
	exit(0);
    case 's': {
	FILE *output = fopen("camera.txt", "w");
	gRoot->save(output);
	fclose(output);
	break;
    }
    case 'l': {
	FILE *input = fopen("camera.txt", "r");
	gRoot->load(input);
	fclose(input);
	glutPostRedisplay();
	break;
    }
    case 'x':
	if (gMagnification < 100000) {
	    gMagnification += 1000;
	    ((pVisusShapesNode)gRoot->child(1))->setMagnification(gMagnification);

	    char magstr[1024];
	    sprintf(magstr, "x%dK", gMagnification/1000);
	    ((pVisusTextNode)gRoot->child(3))->text(magstr);

	    glutPostRedisplay();
	}
	break;
    case 'z':
	if (gMagnification > 1000) {
	    gMagnification -= 1000;
	    ((pVisusShapesNode)gRoot->child(1))->setMagnification(gMagnification);

	    char magstr[1024];
	    sprintf(magstr, "x%dK", gMagnification/1000);
	    ((pVisusTextNode)gRoot->child(3))->text(magstr);

	    glutPostRedisplay();
	}
	break;
    case 'S': {
	FILE *output = fopen("transformation.txt", "w");
        VisusTransformation3D t;
        gFocus->getValue(t);
	t.save(output);
	fclose(output);
	break;
    }
    case 'L': {
	FILE *input = fopen("transformation.txt", "r");
        VisusTransformation3D t;
        t.load(input);
	fclose(input);
        gFocus->setValue(t);
	glutPostRedisplay();
	break;
    }
    case 'm': {
      // Write XML data
      VisusXMLInterface xml;
      xml.write("restart.xml", gRoot);
      break;
    }
    case 'e': 
      exit(0);
    default:
	cout << "Warning: unrecognized keyboard input " << key << endl;
    }
}

void specialFunc(int key, int x, int y)
{ 
    switch (key) {
    case 100:  // left arrow
	gRoot->translate(-12/(float)gWid*16, 0);
	glutPostRedisplay();
	break;
    case 101:  // up arrow
	gRoot->translate(0, -12/(float)gHgt*16);
	glutPostRedisplay();
	break;
    case 102:  // right arrow
	gRoot->translate(12/(float)gWid*16, 0);
	glutPostRedisplay();
	break;
    case 103:  // down arrow
	gRoot->translate(0, 12/(float)gHgt*16);
	glutPostRedisplay();
	break;
    default:
	cout << "Warning: unrecognized special input " << key << endl;
    }
}

void idleFunc()
{
    if (gAnimation != 0) {
	if (gAnimation < 0 && gTimestep > gStart) {
	    decreaseTime();
	    updateScreen();
	    if (gEnableDraw)
		gDrawImage = true;
	}
	else if (gAnimation > 0 && gTimestep < gStop) {
	    increaseTime();
	    updateScreen();
	    if (gEnableDraw)
		gDrawImage = true;
	}
    }
}

enum Params{
  TEXTURE_FILE=1,
  GEOMETRY_FILE,
  BERMUDA_FILE,
  BEAM_FILE,
  TIME_START,
  TIME_STOP,

  FINAL_COUNT
};

#include "VisusMath.h"

int main(int argc, char *argv[])
{
    if (argc < FINAL_COUNT) {
	    cout << "Usage: visus_test_ssa <texture file> <geometry file> <bermuda file> <beam file> t0 t1" << endl;
	    exit(1);
    }
    glutInit(&argc, argv);
    glutInitWindowSize(gWid, gHgt);
    
    glutInitDisplayMode(GLUT_RGBA | GLUT_DOUBLE | GLUT_DEPTH);
    glutCreateWindow("LLNL Space Situational Awareness Toolkit");
    
    glutDisplayFunc(displayFunc);
    glutReshapeFunc(reshapeFunc);
    glutMouseFunc(mouseFunc);
    glutMotionFunc(motionFunc);
    glutKeyboardFunc(keyboardFunc);
    glutSpecialFunc(specialFunc);
    glutIdleFunc(idleFunc);
    
    initDisplay();

    // Process optional arguments
    bool drawHalo = false;
    for (int i=FINAL_COUNT; i<argc; ++i) {
      if (strcmp(argv[i], "--halo")==0)
        drawHalo = true;
      else if (strcmp(argv[i], "--binary")==0)
        gReadBinary = true;
    }

    gRoot = VisusSceneNode::instantiate();
    gStart = atoi(argv[TIME_START]);
    gStop  = atoi(argv[TIME_STOP]);

    if (gStart < 1) gStart = 1;
    if (gStop > 10801) gStop = 10801;

    while (gTimestep < gStart)
	increaseTime();
    
    unsigned char *data;
    int wid, hgt;
    int comps = readPNM(argv[TEXTURE_FILE], wid, hgt, data);
    GLenum format = (comps == 1) ? GL_LUMINANCE : GL_RGB;

    VisusEarthRadius radius;
    radius.data(6.378137);

    pVisusSphereNode sphere;
    sphere = gObjectFactory.constructNode<VisusSphereNode>();
    sphere->setRadius(radius.get());
    sphere->loadTexture(data, wid, hgt, format);
    gRoot->attachSubTree(sphere);

    if (drawHalo) {
      pVisusHalo halo = VisusHalo::instantiate();
      halo->setValue(VisusDefaultColorMaps::ice());
      halo->setValue(radius);
      sphere->attachSubTree(halo);
    }
    
    pVisusShapesNode shapes;
    shapes = gObjectFactory.constructNode<VisusShapesNode>();
    shapes->loadGeometry(argv[GEOMETRY_FILE]);
    shapes->loadNames(VisusShapes::NAMES);
    loadTimestep(shapes);
    shapes->loadBermuda(argv[BERMUDA_FILE]);
    shapes->loadRadarBeam(argv[BEAM_FILE]);
    shapes->setMagnification(gMagnification);
    gRoot->attachSubTree(shapes);

    /* To eyespot validate the detected file reader/writer 
    // eyespot timestep files shows that obj 2725 in timestep 1001 has rdr,opt 0,1
    int time=1001, id=2725;
    shapes->loadDetected(time);
    std::vector<VisusShapes::SphereObj*>& spheres = shapes->getAllSpheres();
    printf("Obj[%d] - Time %d - rdr,opt  %d, %d\n", id, time, spheres[id]->radarDet, spheres[id]->opticalDet);
    */

    // To eyespot validate the orbit file reader/writer 
    /* 
    OrbitPosition* orbit=NULL;  // 36989
    const int objectId = 36990;  int beginTime=95;
    if (shapes->loadOrbit(".", objectId, orbit)) {
      for (int i=beginTime; i<beginTime+3; ++i) {
        printf("Obj: Id=%d  Time=%05d - %f %f %f\n", objectId, gStart+i, orbit[i].lat, orbit[i].lon, orbit[i].rad);
      }
    }
    delete [] orbit;
    */

    VisusFont timeFont;
    timeFont.fontSize(5);
    timeFont.fontColor(255, 255, 255);
    
    char timestr[1024];
    sprintf(timestr, "%d-%02d-%02dT %02d:%02d:%02dZ", 2008, 2, gDay, gHour, gMinute, gSecond);
    
    pVisusTextNode timeText;
    timeText = gObjectFactory.constructNode<VisusTextNode>();
    timeText->text(timestr);
    timeText->setValue(timeFont);
    timeText->translate(-0.002f, 0.00495f);
    //timeText->translate(-0.0125f, 0.0395f);
    gRoot->attachSubTree(timeText);
    
    VisusFont magFont;
    magFont.fontSize(5);
    magFont.fontColor(255, 255, 255);
    
    char magstr[1024];
    sprintf(magstr, "x%dK", gMagnification/1000);
    
    pVisusTextNode magText;
    magText = gObjectFactory.constructNode<VisusTextNode>();
    magText->text(magstr);
    magText->setValue(magFont);
    magText->translate(-0.0066f, -0.0047f);
    //magText->translate(-0.049f, -0.038f);
    gRoot->attachSubTree(magText);
    
    VisusMeshData rocketMesh(3, 3);
    rocketMesh.loadObj("rocket.obj");

    pVisusMeshDisplay rocket;
    rocket = VisusMeshDisplay::instantiate();
    rocket->loadData(&rocketMesh, 0);
    //rocket->normalIndex(3);
    rocket->polygonMode(GL_FILL);
    rocket->setValue(scaleMatrix(0.001f, 0.001f, 0.001f));
    gRoot->attachSubTree(rocket);
    gFocus = rocket;
    
    glutMainLoop();
    return 0;
}
