#include <fstream>
#include <iostream>

#include <windows.h>			/* must include this before GL/gl.h */
#include "VisusRenderer.h"
#include <GL/glut.h>


bool getError()
{
  char lpBuffer[1024];
  DWORD  nSize = 1024;

  DWORD err = GetLastError();

  if (! FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM, 0, err, 0, lpBuffer, nSize, 0))
    fprintf(stderr, "Unknown error %u occurred and could not retrieve system string\n", err);
  else
    fprintf(stderr, "Error: %s\n", lpBuffer);

  return false;
}



void VisusRenderer::init()
{

#if 1
  // Create Window
  glutInitWindowSize(mWidth,mHeight);
  glutInitWindowPosition(0,0);
  glutCreateWindow("Renderer");
  glutHideWindow();

  // Retrieve Device Context
  mDeviceContext = wglGetCurrentDC();
#else
  HDC dc = mDeviceContext = CreateDC("DISPLAY",NULL,NULL,NULL);
  mDeviceContext = CreateCompatibleDC(dc);
#endif

  if (mDeviceContext == NULL) {
    vwarning("Unable to get device context for use");
    getError();
    return;
  }

  PIXELFORMATDESCRIPTOR pfd = { 
    sizeof(PIXELFORMATDESCRIPTOR),   // size of this pfd 
    1,                     // version number 
    PFD_DRAW_TO_BITMAP | PFD_SUPPORT_OPENGL | PFD_DOUBLEBUFFER,  
    PFD_TYPE_RGBA,         // RGBA type 
    24,                    // 24-bit color depth 
    0, 0, 0, 0, 0, 0,      // color bits ignored 
    0,                     // no alpha buffer 
    0,                     // shift bit ignored 
    0,                     // no accumulation buffer 
    0, 0, 0, 0,            // accum bits ignored 
    32,                    // 32-bit z-buffer 
    0,                     // no stencil buffer 
    0,                     // no auxiliary buffer 
    PFD_MAIN_PLANE,        // main layer 
    0,                     // reserved 
    0, 0, 0                // layer masks ignored 
  }; 
 
  // Get the best available match of pixel format for the device context  
  int iPixelFormat = ChoosePixelFormat(mDeviceContext, &pfd); 
 
  // Make that the pixel format of the device context 
  SetPixelFormat(mDeviceContext, iPixelFormat, &pfd); 

  // Create The Drawing Context
  mContext = wglCreateContext(mDeviceContext);
  if (mContext==NULL) {
    vwarning("Error creating WGL context for off-screen rendering");
    getError();
    return;
  }

  // Create Settings For Bitmap
  BITMAPINFO pbmi;
  pbmi.bmiHeader.biSize = sizeof(BITMAPINFO);
  pbmi.bmiHeader.biWidth = mWidth;
  pbmi.bmiHeader.biHeight = mHeight;
  pbmi.bmiHeader.biPlanes = 1;
  pbmi.bmiHeader.biBitCount = 8;
  pbmi.bmiHeader.biCompression = BI_RGB;
  pbmi.bmiHeader.biSizeImage = 0;
  pbmi.bmiHeader.biClrUsed = 0;
  pbmi.bmiHeader.biClrImportant = 0;

  // Create The Bitmap
  mPixels = NULL; 
  mBitmap = CreateDIBSection (mDeviceContext, &pbmi,  DIB_RGB_COLORS, (void**)&mPixels, NULL, 0 );
  if (mBitmap == NULL || mPixels == NULL) {
    vwarning("Error creating DIB bitmap for off-screen rendering");
    getError();
    return;
  }

  // Get information about the bitmap that was allocated 
  DIBSECTION dibsection;
  GetObject (mBitmap, sizeof (dibsection), &dibsection);

  // Ensure Correctness of Bitmap
  vassert(mWidth == dibsection.dsBmih.biWidth);
  vassert(mHeight == dibsection.dsBmih.biHeight);
  vassert(8 == dibsection.dsBmih.biBitCount);
  vassert(BI_RGB == dibsection.dsBmih.biCompression);

  mValid = true;
}


VisusRenderer::~VisusRenderer()
{
  if (mBitmap!=NULL) {
    DeleteObject(mBitmap);
  }
  if (mContext!=NULL) {
    wglDeleteContext(mContext);
  }
  if (mDeviceContext!=NULL) {
    DeleteDC(mDeviceContext);
  }
  delete [] mImage;
}


bool VisusRenderer::makeCurrent()
{
  //  Make it the calling thread's current rendering context
  if (! wglMakeCurrent (mDeviceContext, mContext)) {
    vwarning("Unable to make WGL off-screen rendering context current drawable");
    return getError();
  }

  return true;
}


bool VisusRenderer::revertPrevious()
{
  // make it the calling thread's current rendering context
  if (!wglMakeCurrent (NULL, NULL)) {
    vwarning("Unable to reset previous context as current drawable");
    return getError();
  }
  
  return true;
}

void VisusRenderer::getImage()
{
  // There should be a way to extract image from bitmap for speed sake
  // for now do what everyone else does
  glReadPixels(0,0,mWidth,mHeight,GL_RGB,GL_UNSIGNED_BYTE,mImage);
}

