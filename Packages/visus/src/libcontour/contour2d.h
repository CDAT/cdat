//  ______________________________________________________________________
//
//    FILE
//      contour2d.h - Class for a 2d isocontour polyline
//
//      Copyright (c) 1998 Emilio Camahort, Dan Schikore
//
//    DESCRIPTION
//      contour2d is a class for representing a 2d isocontour polyline.
//  ______________________________________________________________________

// $Id: contour2d.h,v 1.1 2003/09/02 17:27:15 scorzell Exp $

#ifndef _CONTOUR_2D_H
#define _CONTOUR_2D_H

#include <string.h>
#include <sys/types.h>

#ifndef u_int
#define u_int unsigned int
#endif

class Contour2d {

   public:

      // constructor
      Contour2d();

      // destructor
      ~Contour2d();

      // reset (delete all vertices and triangles)
      void Reset(void);
      void Done(void);
      int  isDone(void) { return(done); }

      // add a vertex with the given position and normal
      int AddVert(float p[2])
                { return(AddVert(p[0], p[1])); }
      int AddVert(float, float);

      // add an edge indexed by the given 2 vertices
      int AddEdge(u_int v[2])   { return(AddEdge(v[0], v[1])); }
      int AddEdge(u_int, u_int);

      // get the number of vertices or edges
      int getSize(void)         { return(nedge); }
      int getNVert(void)        { return(nvert); }
      int getNEdge(void)        { return(nedge);  }

      // write vertices and triangles to a file
      void write(char *filename);

      void setExtent(float min[3], float max[3])
           {
              memcpy(minext, min, sizeof(float[3]));
              memcpy(maxext, max, sizeof(float[3]));
           }

   protected :

      int done;				// done with isocontour ??

      // the size of the vertex and edge arrays
      int vsize, tsize;

      // the number of vertices and edges
      int nvert, nedge;

      float minext[3], maxext[3];

   public :				// made public by Emilio

      // arrays of vertices, and edges
      float	(*vert)[2];			// polyline vertex array
      unsigned int (*edge)[2];			// array of polyline edges

};

#endif
