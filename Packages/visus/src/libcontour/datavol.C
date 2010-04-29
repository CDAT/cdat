//------------------------------------------------------------------------
//
// volume.C - class for a regular volume of scalar data
//
// Copyright (c) 1997 Dan Schikore - modified by Emilio Camahort, 1999
//
//------------------------------------------------------------------------

// $Id: datavol.C,v 1.2.2.1 2008/07/23 21:44:19 cottom1 Exp $

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "datavol.h"
#include "compute.h"

#define FSAMPLES 256

//------------------------------------------------------------------------
//
// Datavol() - alternative constructor for the libcontour library
//
//------------------------------------------------------------------------

Datavol::Datavol(Data::DataType t, int ndata, int nverts, int ncells,
	         double *_verts, u_int *_cells, int *_celladj, u_char *data)
		 : Data(t, ndata, data)
{
   int i;

   Datavol::nverts = nverts;			// initializations
   Datavol::ncells = ncells;

   verts =   (float (*)[3])_verts;
   cells =   (u_int (*)[4])_cells;
   celladj = (int (*)[4])_celladj;

   printf("computing extent\n");		// compute data extents

   minext[0] = minext[1] = minext[2] = 1e10;
   maxext[0] = maxext[1] = maxext[2] = -1e10;

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
	if (verts[i][2] < minext[2])
	    minext[2] = verts[i][2];
	if (verts[i][2] > maxext[2])
	    maxext[2] = verts[i][2];
	}

   //fread(minext, sizeof(float[3]), 1, fp);
   //fread(maxext, sizeof(float[3]), 1, fp);

   printf("  min = %f %f %f  max = %f %f %f\n",
	   minext[0], minext[1], minext[2],
	   maxext[0], maxext[1], maxext[2]);

   //fread(&nverts, sizeof(int), 1, fp);
   //fread(&ncells, sizeof(int), 1, fp);

   printf("%d verts, %d cells\n", Datavol::nverts, Datavol::ncells);

						// compute gradients

   grad = (float (*)[3])malloc(sizeof(float[3])*getNVerts());

   //fread(verts, sizeof(float[3]), getNVerts(), fp);
   for (i=0; i<getNCells(); i++) {
      //fread(cells[i], sizeof(u_int[4]), 1, fp);
      //fread(celladj[i], sizeof(int[4]), 1, fp);
if (cells[i][0] == 100 ||
    cells[i][1] == 100 ||
    cells[i][2] == 100 ||
    cells[i][3] == 100) {
   printf("%d %d %d %d\n", cells[i][0], cells[i][1], cells[i][2], cells[i][3]);
}
if (cells[i][0] == 101 ||
    cells[i][1] == 101 ||
    cells[i][2] == 101 ||
    cells[i][3] == 101) {
   printf("%d %d %d %d\n", cells[i][0], cells[i][1], cells[i][2], cells[i][3]);
}

#if 0
printf("cell %d: %d %d %d %d (%d %d %d %d)\n", i,
 cells[i][0],
 cells[i][1],
 cells[i][2],
 cells[i][3],
 celladj[i][0],
 celladj[i][1],
 celladj[i][2],
 celladj[i][3]);
#endif
   }

   for (i=0; i<getNCells(); i++) {
      for (int j=0; j<getNCellFaces(); j++) {
         int adj = celladj[i][j];
         int same = 0;
         if (adj != -1) {
         for (int k=0; k<4; k++)
            for (int l=0; l<4; l++)
               if (cells[i][k] == cells[adj][l])
                  same++;
         if (same != 3)
            printf("cell %d (%d %d %d %d) not adj to %d (%d %d %d %d)\n",
                   i, cells[i][0], cells[i][1], cells[i][2], cells[i][3],
                 adj, cells[adj][0], cells[adj][1], cells[adj][2], cells[adj][3]);
        }
      }
   }

   preprocessData(data);

   compGrad();
}

//------------------------------------------------------------------------
//
//  compLength() -
//
//------------------------------------------------------------------------

float *Datavol::compLength(int &len, float **funx)
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

      tetSurfIntegral(getVert(v[0]), getVert(v[1]), getVert(v[2]), getVert(v[3]),
                      getValue(v[0]), getValue(v[1]), getValue(v[2]), getValue(v[3]),
                      fx, val, len, getMin(), getMax(), 1.0);

   }

   return(val);
}

//------------------------------------------------------------------------
//
//  compGradient() -
//
//------------------------------------------------------------------------

float *Datavol::compGradient(int &len, float **funx)
{
   float *val = (float *)malloc(sizeof(float)*FSAMPLES);
   float *fx = (float *)malloc(sizeof(float)*FSAMPLES);
   float cellgrad[4], scaling;
   int c;
   u_int *v;

   len = FSAMPLES;
   memset(val, 0, sizeof(float)*len);

   *funx = fx;
   for (c=0; c<len; c++)
      fx[c] = getMin() + (c/(len-1.0)) * (getMax()-getMin());

   for (c=0; c<getNCells(); c++) {
      v = getCellVerts(c);
      getCellGrad4(c, cellgrad);
//   printf("grad: %f %f %f %f\n", cellgrad[0], cellgrad[1], cellgrad[2],
//          cellgrad[3]);
//      scaling = sqrt(sqr(cellgrad[0]) + sqr(cellgrad[1]) + sqr(cellgrad[2])) /
//                     (cellgrad[3]);
      scaling = (sqr(cellgrad[0]) + sqr(cellgrad[1]) + sqr(cellgrad[2])) /
                     sqr(cellgrad[3]);

      tetSurfIntegral(getVert(v[0]), getVert(v[1]), getVert(v[2]), getVert(v[3]),
                      getValue(v[0]), getValue(v[1]), getValue(v[2]), getValue(v[3]),
                      fx, val, len, getMin(), getMax(), fabs(scaling));

   }

   return(val);
}

//------------------------------------------------------------------------
//
//  compArea() -
//
//------------------------------------------------------------------------

float *Datavol::compArea(int &len, float **funx)
{
   float *val = (float *)malloc(sizeof(float)*FSAMPLES);
   float *cum = (float *)malloc(sizeof(float)*FSAMPLES);
   float *fx = (float *)malloc(sizeof(float)*FSAMPLES);
   float sum;
   int c;
   u_int *v;

   len = FSAMPLES;
   memset(val, 0, sizeof(float)*len);
   memset(cum, 0, sizeof(float)*len);

   *funx = fx;
   for (c=0; c<len; c++)
      fx[c] = getMin() + (c/(len-1.0)) * (getMax()-getMin());

   for (c=0; c<getNCells(); c++) {
      v = getCellVerts(c);

      tetVolIntegral(getVert(v[0]), getVert(v[1]), getVert(v[2]), getVert(v[3]),
                      getValue(v[0]), getValue(v[1]), getValue(v[2]), getValue(v[3]),
                      fx, val, cum, len, getMin(), getMax(), 1.0);

   }

   // sum the results to add all
   sum=0;
   for (c=0; c<len; c++) {
      val[c] += sum;
      sum+=cum[c];
   }

   return(val);
}

//------------------------------------------------------------------------
//
//  compMaxArea() -
//
//------------------------------------------------------------------------

float *Datavol::compMaxArea(int &len, float **funx)
{
   float *val;
   float max;
   int i;

   val = compArea(len, funx);

   max = val[len-1];
   for (i=0; i<len; i++)
      val[i] = max-val[i];

   return(val);
}

//------------------------------------------------------------------------
//
//  compFunction(), fName() -
//
//------------------------------------------------------------------------

float *
Datavol::compFunction(int n, int &len, float **fx)
{
   switch (n) {
      case 0:
         return(compLength(len, fx));
      case 1:
         return(compGradient(len, fx));
      case 2:
         return(compArea(len, fx));
      case 3:
         return(compMaxArea(len, fx));
   }
   return(NULL);
}

const char *Datavol::fName(int n)
{
   switch (n) {
      case 0:
         return("Surface Area");
      case 1:
         return("Gradient");
      case 2:
         return("Min Volume");
      case 3:
         return("Max Volume");
   }
   return(NULL);
}

