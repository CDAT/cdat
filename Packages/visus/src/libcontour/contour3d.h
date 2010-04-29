//  ______________________________________________________________________
//
//    FILE
//      contour3d.h - class for a 3d isocontour surface
//
//      Copyright (c) 1998 Emilio Camahort, Dan Schikore
//
//    DESCRIPTION
//      contour3d is a class for representing a contour surface (or any
//      3d triangular mesh).
//  ______________________________________________________________________

// $Id: contour3d.h,v 1.1 2003/09/02 17:27:16 scorzell Exp $

#ifndef _CONTOUR_3D_H
#define _CONTOUR_3D_H

#include <string.h>
#include <sys/types.h>

#ifndef u_int
#define u_int unsigned int
#endif

class Contour3d {

   public:

      // constructor
      Contour3d(int fn=0);

      // destructor
      ~Contour3d();

      // to color isosurface using another variable (for multivariate data)
      void colorByFun(int c) { color =  c; }
      void minmaxFun(float mn, float mx) { fmin=mn; fmax=mx; }

      // reset (delete all vertices and triangles)
      void Reset(void);
      void Done(void);
      int  isDone(void) { return(done); }

      // add a vertex with the given position and normal
      int AddVert(float p[3], float n[3], float f=0.0)
                { return(AddVert(p[0], p[1], p[2], n[0], n[1], n[2], f)); }
      int AddVert(float, float, float, float, float, float, float f=0.0);

      int AddVertUnique(float p[3], float n[3], float f=0.0)
                { return(AddVertUnique(p[0], p[1], p[2], n[0], n[1], n[2], f)); }
      int AddVertUnique(float, float, float, float, float, float, float f=0.0);

      // add a triangle indexed by the given 3 vertices
      int AddTri(u_int v[3])   { return(AddTri(v[0], v[1], v[2])); }
      int AddTri(u_int, u_int, u_int);

      // get the number of vertices or triangles
      int getSize(void)        { return(ntri); }
      int getNVert(void)       { return(nvert); }
      int getNTri(void)        { return(ntri);  }

      // write vertices and triangles to a file
      void write(char *filename);

      void setExtent(float min[3], float max[3])
           {
              memcpy(minext, min, sizeof(float[3]));
              memcpy(maxext, max, sizeof(float[3]));
           }

   protected:

      int	done;				// done with isocontour??

      // the size of the vertex and triangle arrays
      int	vsize, tsize;

      // the number of vertices and triangles
      int	nvert, ntri;

      float	minext[3], maxext[3];

   public :				// made public by Emilio

      // true if colored by function on contour
      int	color;
      int	vf;				// variable used for coloring
      float	fmin, fmax;			// min and max color values

      // arrays of vertices, vertex normals, and triangles
      float	(*vert)[3];			// isosurface vertex array
      float	(*vnorm)[3];			// array of vertex normals
      float	(*vfun);			// color values at vertices

      unsigned int (*tri)[3];			// triangle mesh array
};

#endif
