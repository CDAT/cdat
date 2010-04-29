//  ______________________________________________________________________
//
//    NAME
//      Contour2d - Class for a 2d contour curve
//
//      Copyright (c) 1998 Emilio Camahort, Dan Schikore
//
//    SYNOPSIS
//      #include <contour2d.h>
//  ______________________________________________________________________

// $Id: contour2d.C,v 1.3 2005/05/23 17:13:36 rcook Exp $
extern "C" {
#include <stdio.h>
#include <stdlib.h>
}

#include "contour2d.h"

//------------------------------------------------------------------------
//
// Contour2d() - basic constructor
//
//------------------------------------------------------------------------
Contour2d::Contour2d()
{
   done = 0;

   nvert = 0;
   nedge = 0;

   vsize =  500;
   tsize = 1000;

   vert  = (float (*)[2])malloc(sizeof(float[2]) * vsize);
   edge  = (u_int (*)[2])malloc(sizeof(u_int[2]) * tsize);
}

//------------------------------------------------------------------------
//
// ~Contour2d() - free allocated memory
//
//------------------------------------------------------------------------
Contour2d::~Contour2d()
{
   free(vert);
   free(edge);
}

//------------------------------------------------------------------------
//
// AddVert() - add a vertex with the given (unit) normal
//
//------------------------------------------------------------------------
int
Contour2d::AddVert(float x, float y)
{
   int n = nvert++;

   if (nvert > vsize) {
      vsize<<=1;
      vert  = (float (*)[2])realloc(vert, sizeof(float[2]) * vsize);
   }

   vert[n][0] = x;
   vert[n][1] = y;

   return(n);
}

//------------------------------------------------------------------------
//
// AddEdge() - add an edge indexed by its 2 vertices
//
//------------------------------------------------------------------------
int
Contour2d::AddEdge(u_int v1, u_int v2)
{
   int n = nedge++;

   if (nedge > tsize) {
      tsize<<=1;
      edge = (u_int (*)[2])realloc(edge, sizeof(u_int[2]) * tsize);
   }

   edge[n][0] = v1;
   edge[n][1] = v2;

   return(n);
}

//------------------------------------------------------------------------
//
// Reset() - clear vertex and edge info
//
//------------------------------------------------------------------------
void
Contour2d::Reset(void)
{
   nvert = 0;
   nedge  = 0;
   done = 0;
}

void
Contour2d::Done(void)
{
   done = 1;
}

//------------------------------------------------------------------------
//
// write() - write vertex and triangles to a file
//
//------------------------------------------------------------------------
void
Contour2d::write(char *filename)
{
   FILE *fp;
   int v, t;

   fp = fopen(filename, "w");

   // silent failure
   if (fp == NULL)
      return;

   fprintf(fp, "%d %d 0 0 0 0 0\n0 0 0\n", nvert, nedge);

   // Emilio: following construct gives an out-of-bounds warning, I'll just
   //	      write a zero instead for the time being, since writing nothing
   //	      may screw up the file format
#ifdef EMILIO
   for (v=0; v<nvert; v++)
      fprintf(fp, "%g %g %g\n", vert[v][0], vert[v][1], vert[v][2]);
#endif /* of EMILIO */
   for (v=0; v<nvert; v++)
      fprintf(fp, "%g %g %g\n", vert[v][0], vert[v][1], 0.0);

   fprintf(fp, "0 0\n");

   for (t=0; t<nedge; t++)
      fprintf(fp, "%d %d\n", edge[t][0], edge[t][1]);

   fclose(fp);
}

