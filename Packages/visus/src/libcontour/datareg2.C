//------------------------------------------------------------------------
//
// datareg2.C - class for a regular 2d grid of scalar data
//
// Copyright (c) 1997 Dan Schikore - modified by Emilio Camahort, 1999
//
//------------------------------------------------------------------------

// $Id: datareg2.C,v 1.2.2.1 2008/07/23 21:44:19 cottom1 Exp $

#include <stdio.h>
#include <stdlib.h>
#ifndef WIN32
#include <unistd.h>
#endif
#include <math.h>
#include <stdlib.h>
#include <string.h>
#include "datareg2.h"
#include "compute.h"

#define SQR(x) ((x)*(x))

#define FSAMPLES 256

//------------------------------------------------------------------------
//
// bitsize - return the number of bits in an int (easier way?)
//
//------------------------------------------------------------------------
static int
bitsize(unsigned int i)
{
   int b = 1, size=0;

   while (b <= i) {
      b<<=1;
      size++;
   }
   return(size);
}

//------------------------------------------------------------------------
//
// commonConstructor() - called by the constructors to initialize 
//
//------------------------------------------------------------------------
void
Datareg2::commonConstructor(Data::DataType t, int ndata, char *fn)
{

printf("reading dimensions\n");
   fread(dim, sizeof(u_int), 2, fp);
   fread(orig, sizeof(float), 2, fp);
   fread(span, sizeof(float), 2, fp);

printf("dim: %d %d\n", dim[0], dim[1]);
printf("orig: %f %f\n", orig[0], orig[1]);
printf("span: %f %f\n", span[0], span[1]);

   xbits = bitsize(dim[0] - 2);
   ybits = bitsize(dim[1] - 2);
   if (xbits == 0)
      xbits = 1;
   if (ybits == 0)
      ybits = 1;

   yshift = xbits;

   xmask = (1<<xbits) - 1;
   ymask = (1<<ybits) - 1;

printf("xbits %d, ybits %d\n", xbits, ybits);
printf("yshift %d\n", yshift);
printf("xmask %d\n", xmask);
printf("ymask %d\n", ymask);

   readData();
}

//------------------------------------------------------------------------
//
// Datareg2() - alternative constructor for the libcontour library
//
//------------------------------------------------------------------------

Datareg2::Datareg2(Data::DataType t, int ndata, int *dim, u_char *data)
		   : Data(t, ndata, data)
{
    printf("computing extent\n");

    minext[0] = minext[1] = minext[2] = 0.0;
    maxext[0] = dim[0] - 1.0;
    maxext[1] = dim[1] - 1.0;
    maxext[2] = 0.0;

    //fread(minext, sizeof(float[3]), 1, fp);
    //fread(maxext, sizeof(float[3]), 1, fp);

    printf("  min = %f %f %f  max = %f %f %f\n", 
	    minext[0], minext[1], minext[2],
	    maxext[0], maxext[1], maxext[2]);
    
    nverts = dim[0] * dim[1];
    ncells = (dim[0]-1) * (dim[1]-1);

    //fread(&nverts, sizeof(int), 1, fp);
    //fread(&ncells, sizeof(int), 1, fp);

    printf("%d verts, %d cells\n", nverts, ncells);


printf("reading dimensions\n");
   //fread(dim, sizeof(u_int), 2, fp);
   //fread(orig, sizeof(float), 2, fp);
   //fread(span, sizeof(float), 2, fp);

   Datareg2::dim[0] = dim[0];
   Datareg2::dim[1] = dim[1];

   orig[0] = orig[1] = 0.0;
   span[0] = span[1] = 1.0;

printf("dim: %d %d\n", dim[0], dim[1]);
printf("orig: %f %f\n", orig[0], orig[1]);
printf("span: %f %f\n", span[0], span[1]);

   xbits = bitsize(dim[0] - 2);
   ybits = bitsize(dim[1] - 2);
   if (xbits == 0)
      xbits = 1;
   if (ybits == 0)
      ybits = 1;

   yshift = xbits;

   xmask = (1<<xbits) - 1;
   ymask = (1<<ybits) - 1;

printf("xbits %d, ybits %d\n", xbits, ybits);
printf("yshift %d\n", yshift);
printf("xmask %d\n", xmask);
printf("ymask %d\n", ymask);

   preprocessData(data);
}

//------------------------------------------------------------------------
//
//  compLength() -
//
//------------------------------------------------------------------------

float *Datareg2::compLength(int &len, float **funx)
{
   float *val = (float *)malloc(sizeof(float)*FSAMPLES);
   float *fx = (float *)malloc(sizeof(float)*FSAMPLES);
   double p1[2], p2[2], p3[2], p4[2];
   int i, j;

   len = FSAMPLES;
   memset(val, 0, sizeof(float)*len);

   *funx = fx;
   for (i=0; i<len; i++)
      fx[i] = getMin() + (i/(len-1.0)) * (getMax()-getMin());

   for (j=0; j<dim[1]-1; j++) {
      for (i=0; i<dim[0]-1; i++) {
         p1[0] = orig[0] + i*span[0];
         p1[1] = orig[1] + j*span[1];
         p2[0] = orig[0] + (i+1)*span[0];
         p2[1] = orig[1] + j*span[1];
         p3[0] = orig[0] + (i+1)*span[0];
         p3[1] = orig[1] + (j+1)*span[1];
         p4[0] = orig[0] + i*span[0];
         p4[1] = orig[1] + (j+1)*span[1];
         triSurfIntegral(p1, p3, p4,
                      getValue(index2vert(i,j)),
                      getValue(index2vert(i+1,j+1)),
                      getValue(index2vert(i,j+1)), fx, val, len,
                      getMin(), getMax(), 1.0);
         triSurfIntegral(p1, p2, p3,
                      getValue(index2vert(i,j)),
                      getValue(index2vert(i+1,j)),
                      getValue(index2vert(i+1,j+1)),
                      fx, val, len,
                      getMin(), getMax(), 1.0);
      }
   }

   return(val);
}

//------------------------------------------------------------------------
//
//  compArea() -
//
//------------------------------------------------------------------------

float *Datareg2::compArea(int &len, float **funx)
{
   float *val = (float *)malloc(sizeof(float)*FSAMPLES);
   float *cum = (float *)malloc(sizeof(float)*FSAMPLES);
   float *fx = (float *)malloc(sizeof(float)*FSAMPLES);
   double p1[2], p2[2], p3[2], p4[2];
   int i, j;
   float sum;
   int b;

   len = FSAMPLES;
   memset(val, 0, sizeof(float)*len);
   memset(cum, 0, sizeof(float)*len);

   *funx = fx;
   for (i=0; i<len; i++)
      fx[i] = getMin() + (i/(len-1.0)) * (getMax()-getMin());

   for (j=0; j<dim[1]-1; j++) {
      for (i=0; i<dim[0]-1; i++) {
         p1[0] = orig[0] + i*span[0];
         p1[1] = orig[1] + j*span[1];
         p2[0] = orig[0] + (i+1)*span[0];
         p2[1] = orig[1] + j*span[1];
         p3[0] = orig[0] + (i+1)*span[0];
         p3[1] = orig[1] + (j+1)*span[1];
         p4[0] = orig[0] + i*span[0];
         p4[1] = orig[1] + (j+1)*span[1];
         triVolIntegral(p1, p3, p4,
                        getValue(index2vert(i,j)),
                        getValue(index2vert(i+1,j+1)),
                        getValue(index2vert(i,j+1)),
                        fx, val, cum, len,
                        getMin(), getMax(), 1.0);
         triVolIntegral(p1, p2, p3,
                        getValue(index2vert(i,j)),
                        getValue(index2vert(i+1,j)),
                        getValue(index2vert(i+1,j+1)),
                        fx, val, cum, len,
                        getMin(), getMax(), 1.0);
      }
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

float *Datareg2::compMaxArea(int &len, float **funx)
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

float *Datareg2::compGradient(int &len, float **funx)
{
   float *val = (float *)malloc(sizeof(float)*FSAMPLES);
   float *fx = (float *)malloc(sizeof(float)*FSAMPLES);
   double p1[2], p2[2], p3[2], p4[2];
   float v1, v2, v3, v4;
   double grad[3];
   float scaling;
   int i, j;

   len = FSAMPLES;
   memset(val, 0, sizeof(float)*len);

   *funx = fx;
   for (i=0; i<len; i++)
      fx[i] = getMin() + (i/(len-1.0)) * (getMax()-getMin());

   for (j=0; j<dim[1]-1; j++) {
      for (i=0; i<dim[0]-1; i++) {
         p1[0] = orig[0] + i*span[0];
         p1[1] = orig[1] + j*span[1];
         p2[0] = orig[0] + (i+1)*span[0];
         p2[1] = orig[1] + j*span[1];
         p3[0] = orig[0] + (i+1)*span[0];
         p3[1] = orig[1] + (j+1)*span[1];
         p4[0] = orig[0] + i*span[0];
         p4[1] = orig[1] + (j+1)*span[1];
         v1 = getValue(index2vert(i,j));
         v2 = getValue(index2vert(i+1,j));
         v3 = getValue(index2vert(i+1,j+1));
         v4 = getValue(index2vert(i,j+1));

         grad[0] = (p1[1]-p2[1])*(v1+v2) +
                   (p2[1]-p3[1])*(v2+v3) +
                   (p3[1]-p4[1])*(v3+v4) +
                   (p4[1]-p1[1])*(v4+v1);
         grad[1] = (v1-v2)*(p1[0]+p2[0]) +
                   (v2-v3)*(p2[0]+p3[0]) +
                   (v3-v4)*(p3[0]+p4[0]) +
                   (v4-v1)*(p4[0]+p1[0]);
         grad[2] = (p1[0]-p2[0])*(p1[1]+p2[1]) +
                   (p2[0]-p3[0])*(p2[1]+p3[1]) +
                   (p3[0]-p4[0])*(p3[1]+p4[1]) +
                   (p4[0]-p1[0])*(p4[1]+p1[1]);
//         scaling = sqrt(grad[0]*grad[0] + grad[1]*grad[1])/grad[2];
         scaling = (grad[0]*grad[0] + grad[1]*grad[1])/sqr(grad[2]);

         triSurfIntegral(p1, p3, p4, v1, v3, v4,
                      fx, val, len,
                      getMin(), getMax(), fabs(scaling));
         triSurfIntegral(p1, p2, p3, v1, v2, v3,
                      fx, val, len,
                      getMin(), getMax(), fabs(scaling));
      }
   }

   return(val);
}

//------------------------------------------------------------------------
//
//  compFunction(), fName() -
//
//------------------------------------------------------------------------

float *
Datareg2::compFunction(int n, int &len, float **fx)
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

const char *Datareg2::fName(int n)
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
