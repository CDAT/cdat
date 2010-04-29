//  ______________________________________________________________________
//
//    NAME
//      Contour3d - Class for a contour surface
//
//      Copyright (c) 1998 Emilio Camahort, Dan Schikore
//
//    SYNOPSIS
//      #include <contour3d.h>
//  ______________________________________________________________________

// $Id: contour3d.C,v 1.2.2.1 2007/09/04 17:31:45 tbremer Exp $

#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include "contour3d.h"

#define WRITE_TMESH
//#define WRITE_IPOLY

//------------------------------------------------------------------------
//
// Contour3d() - basic constructor
//
//------------------------------------------------------------------------
Contour3d::Contour3d(int _vf)
{
   done = 0;

   nvert = 0;
   ntri = 0;
   vf = _vf;

   vsize =  500;
   tsize = 1000;

   vert  = (float (*)[3])malloc(sizeof(float[3]) * vsize);
   vnorm = (float (*)[3])malloc(sizeof(float[3]) * vsize);
   tri   = (u_int (*)[3])malloc(sizeof(u_int[3]) * tsize);

   vfun = (float *)malloc(sizeof(float)*vsize);

   color = (vf>1);
}

//------------------------------------------------------------------------
//
// ~Contour3d() - free allocated memory
//
//------------------------------------------------------------------------
Contour3d::~Contour3d()
{
   free(vert);
   free(vnorm);
   free(tri);

   // Add this 08/07 ptb any reason why this was not freed before ???
   free(vfun);
}

//------------------------------------------------------------------------
//
// AddVert() - add a vertex with the given (unit) normal
//
//------------------------------------------------------------------------
int
Contour3d::AddVert(float x, float y, float z, float nx, float ny, float nz, float f)
{
   int n = nvert++;

   if (nvert > vsize) {
      vsize<<=1;
      vert  = (float (*)[3])realloc(vert, sizeof(float[3]) * vsize);
      vnorm = (float (*)[3])realloc(vnorm, sizeof(float[3]) * vsize);
      vfun = (float *)realloc(vfun, sizeof(float)*vsize);
   }

   vert[n][0] = x;
   vert[n][1] = y;
   vert[n][2] = z;

   vnorm[n][0] = nx;
   vnorm[n][1] = ny;
   vnorm[n][2] = nz;

   vfun[n] = f;
// printf("f = %f\n", f);

   return(n);
}

//------------------------------------------------------------------------
//
// AddVertUnique() - add a vertex with the given (unit) normal
//
//------------------------------------------------------------------------
int
Contour3d::AddVertUnique(float x, float y, float z, float nx, float ny, float nz, float f)
{
   int i;

   for (i=nvert-1; i>=0; i--) {
      if ((fabs(vert[i][0]-x) < 0.00001) &&
          (fabs(vert[i][1]-y) < 0.00001) &&
          (fabs(vert[i][2]-z) < 0.00001))
         return(i);
   }

   return(AddVert(x,y,z,nx,ny,nz,f));
}

//------------------------------------------------------------------------
//
// AddTri() - add a triangle indexed by it's 3 vertices
//
//------------------------------------------------------------------------
int
Contour3d::AddTri(u_int v1, u_int v2, u_int v3)
{
   int n = ntri++;

   if (ntri > tsize) {
      tsize<<=1;
      tri = (u_int (*)[3])realloc(tri, sizeof(u_int[3]) * tsize);
   }

   tri[n][0] = v1;
   tri[n][1] = v2;
   tri[n][2] = v3;

   return(n);
}

//------------------------------------------------------------------------
//
// Reset() - clear vertex and surface info
//
//------------------------------------------------------------------------
void
Contour3d::Reset(void)
{
   nvert = 0;
   ntri  = 0;
   done = 0;
}


void
Contour3d::Done(void)
{
   done = 1;
}

//------------------------------------------------------------------------
//
// write() - write vertex and triangles to a file
//
//------------------------------------------------------------------------
void
Contour3d::write(char *filename)
{
#ifdef WRITE_IPOLY
   FILE *fp;
   int v, t;

   fp = fopen(filename, "w");

   // silent failure
   if (fp == NULL)
      return;

   fprintf(fp, "%d 0 %d 0 0 0 0\n0 0 0\n", nvert, ntri);

   for (v=0; v<nvert; v++)
      fprintf(fp, "%g %g %g\n", vert[v][0], vert[v][1], vert[v][2]);

   fprintf(fp, "0 0\n");

   for (t=0; t<ntri; t++)
      fprintf(fp, "3\n%d %d %d\n", tri[t][0], tri[t][1], tri[t][2]);

   fclose(fp);

#elif defined WRITE_TMESH

   FILE *fp;
   int t;
   int nvf, nmat;
   u_int _tri[3];
   float vn[3];

   fp = fopen(filename, "w");

   // silent failure
   if (fp == NULL)
      return;

   nvf = 1;
   nmat = 0;

   fwrite(&nvert, sizeof(int), 1, fp);
   fwrite(&ntri,  sizeof(int), 1, fp);
   fwrite(&nvf,   sizeof(int), 1, fp);
   fwrite(&nmat,  sizeof(int), 1, fp);

   fwrite(vert, sizeof(float[3]), nvert, fp);
   for (t=0; t<nvert; t++) {
      vn[0] = -vnorm[t][0];
      vn[1] = -vnorm[t][1];
      vn[2] = -vnorm[t][2];
     fwrite(vn, sizeof(float[3]), 1, fp);
   }

   // reverse the triangles
   for (t=0; t<ntri; t++) {
      _tri[0] = tri[t][1];
      _tri[1] = tri[t][0];
      _tri[2] = tri[t][2];
      fwrite(_tri, sizeof(int[3]), 1, fp);
   }

   // write the vertex function
   fwrite(vfun, sizeof(float), nvert, fp);

   fclose(fp);

#else /* write a simple poly file */
   fprintf(fp, "%d\n", ntri);
   for (t=0; t<ntri; t++) {
      fprintf(fp, "3\n%g %g %g\n",
              vert[tri[t][0]][0], vert[tri[t][0]][1], vert[tri[t][0]][2]);
      fprintf(fp, "%g %g %g\n",
              vert[tri[t][2]][0], vert[tri[t][2]][1], vert[tri[t][2]][2]);
      fprintf(fp, "%g %g %g\n",
              vert[tri[t][1]][0], vert[tri[t][1]][1], vert[tri[t][1]][2]);
   }
#endif
}
