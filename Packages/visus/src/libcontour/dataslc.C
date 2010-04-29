//------------------------------------------------------------------------
//
// dataslc.C - class for a triangular slice of scalar data
//
// Copyright (c) 1997 Dan Schikore - modified by Emilio Camahort, 1999
//
//------------------------------------------------------------------------

// $Id: dataslc.C,v 1.2.2.1 2008/07/23 21:44:18 cottom1 Exp $

#include <stdio.h>
#include <stdlib.h>
#ifndef WIN32
#include <unistd.h>
#endif
#include <math.h>
#include <string.h>
#include "dataslc.h"
#include "compute.h"

#define SQR(x) ((x)*(x))

#define FSAMPLES 256

//------------------------------------------------------------------------
//
// commonConstructor() - called by the constructors to initialize a
//                       volume
//
//------------------------------------------------------------------------
void
Dataslc::commonConstructor(Data::DataType t, int ndata, char *fn)
{
   int i;
   float grad[3];
   float len;

   verts = (double (*)[2])malloc(sizeof(double[2])*getNVerts());
   vgrad = (float (*)[3])malloc(sizeof(float[3])*getNVerts());
   cells = (u_int (*)[3])malloc(sizeof(u_int[3])*getNCells());
   celladj = (int (*)[3])malloc(sizeof(int[3])*getNCells());

printf("reading verts\n");
   fread(verts, sizeof(double[2]), getNVerts(), fp);
printf("reading cells\n");
   for (i=0; i<getNCells(); i++) {
      fread(cells[i], sizeof(u_int[3]), 1, fp);
      fread(celladj[i], sizeof(int[3]), 1, fp);
   }

   for (i=0; i<getNCells(); i++) {
      for (int j=0; j<getNCellFaces(); j++) {
         int adj = celladj[i][j];
         int same = 0;
         if (adj != -1) {
         for (int k=0; k<3; k++)
            for (int l=0; l<3; l++)
               if (cells[i][k] == cells[adj][l])
                  same++;
         if (same != 2)
            printf("cell %d (%d %d %d) not adj to %d (%d %d %d)\n",
                   i, cells[i][0], cells[i][1], cells[i][2],
                 adj, cells[adj][0], cells[adj][1], cells[adj][2]);
        }
      }
   }

   readData();

   for (i=0; i<getNCells(); i++) {
      getCellGrad(i, grad);
      vgrad[getCellVert(i,0)][0] += grad[0];
      vgrad[getCellVert(i,0)][1] += grad[1];
      vgrad[getCellVert(i,0)][2] += grad[2];
      vgrad[getCellVert(i,1)][0] += grad[0];
      vgrad[getCellVert(i,1)][1] += grad[1];
      vgrad[getCellVert(i,1)][2] += grad[2];
      vgrad[getCellVert(i,2)][0] += grad[0];
      vgrad[getCellVert(i,2)][1] += grad[1];
      vgrad[getCellVert(i,2)][2] += grad[2];
   }

   for (i=0; i<getNVerts(); i++) {
//printf("scaling vgrad %d\n", i);
      len = sqrt(vgrad[i][0]*vgrad[i][0] + vgrad[i][1]*vgrad[i][1] +
                 vgrad[i][2]*vgrad[i][2]);
      if (len != 0.0) {
         vgrad[i][0] /= len;
         vgrad[i][1] /= len;
         vgrad[i][2] /= len;
      }
   }
}

//------------------------------------------------------------------------
//
// Dataslc() - alternative constructor for the libcontour library
//
//------------------------------------------------------------------------

Dataslc::Dataslc(Data::DataType t, int ndata, int nverts, int ncells,
		 double *_verts, u_int *_cells, int *_celladj, u_char *data)
		 : Data(t, ndata, data) 
{
   int i;
   float grad[3];
   float len;

   Dataslc::nverts = nverts;			// initializations
   Dataslc::ncells = ncells;

   verts   = (double (*)[2])_verts;
   cells   = (u_int (*)[3])_cells;
   celladj = (int (*)[3])_celladj;

   printf("computing extent\n");		// compute data extents

   minext[0] = minext[1] =
   maxext[0] = maxext[1] =
   minext[2] = maxext[2] = 0.0;

   for (i = 0; i < nverts; i++)
        {
        if (verts[i][0] < minext[0])
            minext[0] = verts[i][0];
        if (verts[i][0] > maxext[0])
            maxext[0] = verts[i][0];
        if (verts[i][1] < minext[1])
            minext[1] = verts[i][1];
        if (verts[i][1] > maxext[1])
            maxext[1] = verts[i][1];
	} 

   //fread(minext, sizeof(float[3]), 1, fp);
   //fread(maxext, sizeof(float[3]), 1, fp);

   printf("  min = %f %f %f  max = %f %f %f\n",
	   minext[0], minext[1], minext[2],
	   maxext[0], maxext[1], maxext[2]);

   //fread(&nverts, sizeof(int), 1, fp);
   //fread(&ncells, sizeof(int), 1, fp);

   printf("%d verts, %d cells\n", Dataslc::nverts, Dataslc::ncells);

						// compute gradients

   vgrad = (float (*)[3])malloc(sizeof(float[3])*getNVerts());

   printf("processing cells\n");

   for (i=0; i<getNCells(); i++) {
      for (int j=0; j<getNCellFaces(); j++) {
         int adj = celladj[i][j];
         int same = 0;
         if (adj != -1) {
         for (int k=0; k<3; k++)
            for (int l=0; l<3; l++)
               if (cells[i][k] == cells[adj][l])
                  same++;
         if (same != 2)
            printf("cell %d (%d %d %d) not adj to %d (%d %d %d)\n",
                   i, cells[i][0], cells[i][1], cells[i][2],
                 adj, cells[adj][0], cells[adj][1], cells[adj][2]);
        }
      }
   }

   preprocessData(data);

   for (i=0; i<getNCells(); i++) {
      getCellGrad(i, grad);
      vgrad[getCellVert(i,0)][0] += grad[0];
      vgrad[getCellVert(i,0)][1] += grad[1];
      vgrad[getCellVert(i,0)][2] += grad[2];
      vgrad[getCellVert(i,1)][0] += grad[0];
      vgrad[getCellVert(i,1)][1] += grad[1];
      vgrad[getCellVert(i,1)][2] += grad[2];
      vgrad[getCellVert(i,2)][0] += grad[0];
      vgrad[getCellVert(i,2)][1] += grad[1];
      vgrad[getCellVert(i,2)][2] += grad[2];
   }

   for (i=0; i<getNVerts(); i++) {
//printf("scaling vgrad %d\n", i);
      len = sqrt(vgrad[i][0]*vgrad[i][0] + vgrad[i][1]*vgrad[i][1] +
                 vgrad[i][2]*vgrad[i][2]);
      if (len != 0.0) {
         vgrad[i][0] /= len;
         vgrad[i][1] /= len;
         vgrad[i][2] /= len;
      }
   }
}

//------------------------------------------------------------------------
//
//  compLength() - 
//
//------------------------------------------------------------------------

float *Dataslc::compLength(int &len, float **funx)
{
   float *val = (float *)malloc(sizeof(float)*FSAMPLES);
   float *fx = (float *)malloc(sizeof(float)*FSAMPLES);
   int c;
   u_int *v;

   len = FSAMPLES;
   memset(val, 0, sizeof(float)*len);

   *funx = fx;
   for (c=0; c<len; c++)
      fx[c] = getMin() + (c/(len-1.0)) * (getMax()-getMin());

   for (c=0; c<getNCells(); c++) {
      v = getCellVerts(c);

      triSurfIntegral(getVert(v[0]), getVert(v[1]), getVert(v[2]),
                      getValue(v[0]), getValue(v[1]), getValue(v[2]), fx, val, len,
                      getMin(), getMax(), 1.0);

   }

   return(val);
}

//------------------------------------------------------------------------
//
//  compArea() - 
//
//------------------------------------------------------------------------

float *Dataslc::compArea(int &len, float **funx)
{
   float *val = (float *)malloc(sizeof(float)*FSAMPLES);
   float *cum = (float *)malloc(sizeof(float)*FSAMPLES);
   float *fx = (float *)malloc(sizeof(float)*FSAMPLES);
   int c;
   u_int *v;
   float sum;
   int b;

   len = FSAMPLES;
   memset(val, 0, sizeof(float)*len);
   memset(cum, 0, sizeof(float)*len);

   *funx = fx;
   for (c=0; c<len; c++)
      fx[c] = getMin() + (c/(len-1.0)) * (getMax()-getMin());

   for (c=0; c<getNCells(); c++) {
      v = getCellVerts(c);

      triVolIntegral(getVert(v[0]), getVert(v[1]), getVert(v[2]),
                     getValue(v[0]), getValue(v[1]), getValue(v[2]), fx, val, cum,
                     len, getMin(), getMax(), 1.0);

   }

   // now we need to sum from the first to last
   sum = 0.0;
   for (b=0; b<len; b++) {
      val[b]+=sum;
      sum+=cum[b];
   }

   free(cum);

   return(val);
}

//------------------------------------------------------------------------
//
//  compMaxArea() - 
//
//------------------------------------------------------------------------

float *Dataslc::compMaxArea(int &len, float **funx)
{
   float *f;
   float max;
   int i;

   f = compArea(len, funx);

   max = f[len-1];
   for (i=0; i<len; i++)
      f[i] = max-f[i];

   return(f);
}

//------------------------------------------------------------------------
//
//  compGradient() - 
//
//------------------------------------------------------------------------

float *Dataslc::compGradient(int &len, float **funx)
{
   float *val = (float *)malloc(sizeof(float)*FSAMPLES);
   float *fx = (float *)malloc(sizeof(float)*FSAMPLES);
   int c;
   u_int *v;
   float cellgrad[3], scaling;
//   u_int cv[3];

   len = FSAMPLES;
   memset(val, 0, sizeof(float)*len);

   *funx = fx;
   for (c=0; c<len; c++)
      fx[c] = getMin() + (c/(len-1.0)) * (getMax()-getMin());

   for (c=0; c<getNCells(); c++) {
      v = getCellVerts(c);
      getCellGrad3(c, cellgrad);
      scaling = sqrt(cellgrad[0]*cellgrad[0] + cellgrad[1]*cellgrad[1]) / (cellgrad[2]);
//      scaling = (cellgrad[0]*cellgrad[0] + cellgrad[1]*cellgrad[1]) / sqr(cellgrad[2]);
//      cv[0] = v[0]; cv[1]=v[1]; cv[2]=v[2];

      triSurfIntegral(getVert(v[0]), getVert(v[1]), getVert(v[2]),
                      getValue(v[0]), getValue(v[1]), getValue(v[2]), fx, val, len,
                      getMin(), getMax(), fabs(scaling));
   }

   return(val);
}

//------------------------------------------------------------------------
//
//  compFunction(), fName() -
//
//------------------------------------------------------------------------

float *
Dataslc::compFunction(int n, int &len, float **fx)
{
   switch (n) {
      case 0:
         return(compLength(len, fx));
      case 1:
         return(compArea(len, fx));
      case 2:
         return(compMaxArea(len, fx));
      case 3:
         return(compGradient(len, fx));
   }
   return(NULL);
}

const char *Dataslc::fName(int n)
{
   switch (n) {
      case 0:
         return("Length");
      case 1:
         return("Min Area");
      case 2:
         return("Max Area");
      case 3:
         return("Gradient");
   }
   return(NULL);
}
