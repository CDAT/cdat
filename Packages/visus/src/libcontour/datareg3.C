//------------------------------------------------------------------------
//
// datareg3.C - class for a regular 3d grid of scalar data
//
//------------------------------------------------------------------------

// $Id: datareg3.C,v 1.2.2.2 2008/07/23 21:44:19 cottom1 Exp $

#include <stdio.h>
#include <stdlib.h>
#ifndef WIN32
#include <unistd.h>
#endif
#include <math.h>
#include <string.h>
#include "datareg3.h"
#include "compute.h"

#define	TRUE	1
#define	FALSE	0

#define FUNSTEP 2

#define SQR(x) ((x)*(x))

#define FSAMPLES 256
#define FSAMPLES_TOPO 128

int  Datareg3::cellFaceVert[6][4] = {
   { 0, 1, 2, 3 },
   { 0, 3, 4, 7 },
   { 4, 5, 6, 7 },
   { 1, 2, 5, 6 },
   { 2, 3, 6, 7 },
   { 0, 1, 4, 5 },
};

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
// Datareg3() - the regular object constructor
//
//------------------------------------------------------------------------

Datareg3::Datareg3(DataType t, int ndata, char *fn)
		   : Data(t, ndata, fn)
{

#ifdef VERBOSE
printf("reading dimensions\n");
#endif
   fread(dim, sizeof(u_int), 3, fp);
   fread(orig, sizeof(float), 3, fp);
   fread(span, sizeof(float), 3, fp);

#ifdef VERBOSE
printf("dim: %d %d %d\n", dim[0], dim[1], dim[2]);
printf("orig: %f %f %f\n", orig[0], orig[1], orig[2]);
printf("span: %f %f %f\n", span[0], span[1], span[2]);
#endif

   xbits = bitsize(dim[0] - 2);
   ybits = bitsize(dim[1] - 2);
   zbits = bitsize(dim[2] - 2);
   if (xbits == 0)
      xbits = 1;
   if (ybits == 0)
      ybits = 1;
   if (zbits == 0)
      zbits = 1;

   yshift = xbits;
   zshift = xbits+ybits;

   xmask = (1<<xbits) - 1;
   ymask = (1<<ybits) - 1;
   zmask = (1<<zbits) - 1;

#ifdef VERBOSE
printf("xbits %d, ybits %d, zbits %d\n", xbits, ybits, zbits);
printf("yshift %d\n", yshift);
printf("zshift %d\n", zshift);
printf("xmask %d\n", xmask);
printf("ymask %d\n", ymask);
printf("zmask %d\n", zmask);
#endif

   readData();
}

//------------------------------------------------------------------------
//
// Datareg3() - alternative constructor for the libcontour library
//
//------------------------------------------------------------------------

Datareg3::Datareg3(Data::DataType t, int ndata, int *dim, u_char *data)
		   : Data(t, ndata, data)
{
#ifdef VERBOSE
    printf("computing extent\n");
#endif
    minext[0] = minext[1] = minext[2] = 0.0;
    maxext[0] = dim[0] - 1.0;
    maxext[1] = dim[1] - 1.0;
    maxext[2] = dim[2] - 1.0;

    //fread(minext, sizeof(float[3]), 1, fp);
    //fread(maxext, sizeof(float[3]), 1, fp);

#ifdef VERBOSE
    printf("  min = %f %f %f  max = %f %f %f\n", 
	    minext[0], minext[1], minext[2],
	    maxext[0], maxext[1], maxext[2]);
#endif
    
    nverts = dim[0] * dim[1] * dim[2];
    ncells = (dim[0]-1) * (dim[1]-1) * (dim[2]-1);

    //fread(&nverts, sizeof(int), 1, fp);
    //fread(&ncells, sizeof(int), 1, fp);

#ifdef VERBOSE
    printf("%d verts, %d cells\n", nverts, ncells);
#endif


#ifdef VERBOSE
printf("reading dimensions\n");
#endif
   //fread(dim, sizeof(u_int), 3, fp);
   //fread(orig, sizeof(float), 3, fp);
   //fread(span, sizeof(float), 3, fp);

   Datareg3::dim[0] = dim[0];
   Datareg3::dim[1] = dim[1];
   Datareg3::dim[2] = dim[2];

   orig[0] = orig[1] = orig[2] = 0.0;
   span[0] = span[1] = span[2] = 1.0;

#ifdef VERBOSE
printf("dim: %d %d %d\n", dim[0], dim[1], dim[2]);
printf("orig: %f %f %f\n", orig[0], orig[1], orig[2]);
printf("span: %f %f %f\n", span[0], span[1], span[2]);
#endif

   xbits = bitsize(dim[0] - 2);
   ybits = bitsize(dim[1] - 2);
   zbits = bitsize(dim[2] - 2);
   if (xbits == 0)
      xbits = 1;
   if (ybits == 0)
      ybits = 1;
   if (zbits == 0)
      zbits = 1;

   yshift = xbits;
   zshift = xbits+ybits;

   xmask = (1<<xbits) - 1;
   ymask = (1<<ybits) - 1;
   zmask = (1<<zbits) - 1;

#ifdef VERBOSE
printf("xbits %d, ybits %d, zbits %d\n", xbits, ybits, zbits);
printf("yshift %d\n", yshift);
printf("zshift %d\n", zshift);
printf("xmask %d\n", xmask);
printf("ymask %d\n", ymask);
printf("zmask %d\n", zmask);
#endif
   preprocessData(data);
}

//------------------------------------------------------------------------
//
// getSlice() - extract a 2d slice from the 3d grid
//
//------------------------------------------------------------------------

int Datareg3::getSlice(int variable, char axis, int index, datatypes *buffer)
{
    int		v;			// variable index
    int		y,z;			// integer index variables
    int		size;			// slice size
    int		error;			// did an error occur?

    u_char	*bc, *dc;		// buffer and data pointer indices
    u_short	*bs, *ds; 		// buffer and data pointer indices
    float	*bf, *df; 		// buffer and data pointer indices
    double  *bd, *dd;

    v = variable;
    error = FALSE;

    switch (axis)
	{
	case 'x' :
	    if (index < 0 || index >= dim[0])
		{
		error = TRUE;
		break;
		}
	    size = dim[1] * dim[2];
	    switch (type)
		{
		case UCHAR :
		    if (!buffer->ucdata)
			buffer->ucdata = new u_char[size];
		    bc = buffer->ucdata;
		    dc = data[v].ucdata + index;
		    for (z = 0; z < dim[2]; z++)
			for (y = 0; y < dim[1]; y++)
			    {
			    (*bc++) = *dc;
			    dc += dim[0];
			    }
		    break;

		case USHORT :
		    if (!buffer->usdata)
			buffer->usdata = new u_short[size];
		    bs = buffer->usdata;
		    ds = data[v].usdata + index;
		    for (z = 0; z < dim[2]; z++)
			for (y = 0; y < dim[1]; y++)
			    {
			    (*bs++) = *ds;
			    ds += dim[0];
			    }
		    break;

		case FLOAT :
		    if (!buffer->fdata)
			buffer->fdata = new float[size];
		    bf = buffer->fdata;
		    df = data[v].fdata + index;
		    for (z = 0; z < dim[2]; z++)
			for (y = 0; y < dim[1]; y++)
			    {
			    (*bf++) = *df;
			    df += dim[0];
			    }
		    break;
		case DOUBLE :
		    if (!buffer->ddata) {
                buffer->ddata = new double[size];
            }
		    bd = buffer->ddata;
		    dd = data[v].ddata + index;
		    for (z = 0; z < dim[2]; z++)
			for (y = 0; y < dim[1]; y++)
			    {
			    (*bd++) = *dd;
			    dd += dim[0];
			    }
		    break;
		}
	    break;


	case 'y' :
	    if (index < 0 || index >= dim[1])
		{
		error = TRUE;
		break;
		}
	    size = dim[2] * dim[0];
	    switch (type)
		{
		case UCHAR :
		    if (!buffer->ucdata)
			buffer->ucdata = new u_char[size];
		    bc = buffer->ucdata;
		    dc = data[v].ucdata + index * dim[0];
		    for (z = 0; z < dim[2]; z++)
			{
			memcpy(bc, dc, dim[0] * sizeof(u_char));
			bc += dim[0];
			dc += dim[0] * dim[1];
			}
		    break;

		case USHORT :
		    if (!buffer->usdata)
			buffer->usdata = new u_short[size];
		    bs = buffer->usdata;
		    ds = data[v].usdata + index * dim[0];
		    for (z = 0; z < dim[2]; z++)
			{
			memcpy(bs, ds, dim[0] * sizeof(u_short));
			bs += dim[0];
			ds += dim[0] * dim[1];
			}
		    break;

		case FLOAT :
		    if (!buffer->fdata)
			buffer->fdata = new float[size];
		    bf = buffer->fdata;
		    df = data[v].fdata + index * dim[0];
		    for (z = 0; z < dim[2]; z++)
			{
			memcpy(bf, df, dim[0] * sizeof(float));
			bf += dim[0];
			df += dim[0] * dim[1];
			}
		    break;
		case DOUBLE :
		    if (!buffer->ddata)
			buffer->ddata = new double[size];
		    bd = buffer->ddata;
		    dd = data[v].ddata + index * dim[0];
		    for (z = 0; z < dim[2]; z++)
			{
			memcpy(bd, dd, dim[0] * sizeof(double));
			bd += dim[0];
			dd += dim[0] * dim[1];
			}
		    break;

		}
	    break;

	case 'z' :
	    if (index < 0 || index >= dim[2])
		{
		error = TRUE;
		break;
		}
	    size = dim[0] * dim[1];
	    switch (type)
		{
		case UCHAR :
		    if (!buffer->ucdata)
			buffer->ucdata = new u_char[size];
		    memcpy(buffer->ucdata, data[v].ucdata + index * size,
					   size * sizeof(u_char));
		    break;

		case USHORT :
		    if (!buffer->usdata)
			buffer->usdata = new u_short[size];
		    memcpy(buffer->usdata, data[v].usdata + index * size,
					   size * sizeof(u_short));
		    break;

		case FLOAT :
		    if (!buffer->fdata)
			buffer->fdata = new float[size];
		    memcpy(buffer->fdata, data[v].fdata + index * size,
					  size * sizeof(float));
		    break;
		case DOUBLE :
		    if (!buffer->ddata)
			buffer->ddata = new double[size];
		    memcpy(buffer->ddata, data[v].ddata + index * size,
					  size * sizeof(double));
		    break;

		}
	    break;
	default :
	    error = TRUE;
	    break;
	}	

    return error;
}

//------------------------------------------------------------------------
//
// compLength() -
//
//------------------------------------------------------------------------

float *Datareg3::compLength(int &len, float **funx)
{
   float *val = (float *)malloc(sizeof(float)*FSAMPLES);
   float *fx = (float *)malloc(sizeof(float)*FSAMPLES);
   float p[8][3];
   int i, j, k;

   len = FSAMPLES;
   memset(val, 0, sizeof(float)*len);

   *funx = fx;
   for (i=0; i<len; i++)
      fx[i] = getMin() + (i/(len-1.0)) * (getMax()-getMin());

   for (k=0; k<dim[2]-1; k+=FUNSTEP) {
      for (j=0; j<dim[1]-1; j+=FUNSTEP) {
         for (i=0; i<dim[0]-1; i+=FUNSTEP) {
            p[0][0] = xCoord(i);
            p[0][1] = yCoord(j);
            p[0][2] = zCoord(k);

            p[1][0] = xCoord(i+1);
            p[1][1] = yCoord(j);
            p[1][2] = zCoord(k);

            p[2][0] = xCoord(i+1);
            p[2][1] = yCoord(j);
            p[2][2] = zCoord(k+1);

            p[3][0] = xCoord(i);
            p[3][1] = yCoord(j);
            p[3][2] = zCoord(k+1);

            p[4][0] = xCoord(i);
            p[4][1] = yCoord(j+1);
            p[4][2] = zCoord(k);

            p[5][0] = xCoord(i+1);
            p[5][1] = yCoord(j+1);
            p[5][2] = zCoord(k);

            p[6][0] = xCoord(i+1);
            p[6][1] = yCoord(j+1);
            p[6][2] = zCoord(k+1);

            p[7][0] = xCoord(i);
            p[7][1] = yCoord(j+1);
            p[7][2] = zCoord(k+1);

            tetSurfIntegral(p[0], p[1], p[3], p[4],
                            getValue(i,j,k),
                            getValue(i+1,j,k),
                            getValue(i,j,k+1),
                            getValue(i,j+1,k),
                            fx, val, len, getMin(), getMax(), 1.0);
            tetSurfIntegral(p[3], p[1], p[2], p[6],
                            getValue(i,j,k+1),
                            getValue(i+1,j,k),
                            getValue(i+1,j,k+1),
                            getValue(i+1,j+1,k+1),
                            fx, val, len, getMin(), getMax(), 1.0);
            tetSurfIntegral(p[6], p[1], p[5], p[4],
                            getValue(i+1,j+1,k+1),
                            getValue(i+1,j,k),
                            getValue(i+1,j+1,k),
                            getValue(i,j+1,k),
                            fx, val, len, getMin(), getMax(), 1.0);
            tetSurfIntegral(p[7], p[3], p[6], p[4],
                            getValue(i,j+1,k+1),
                            getValue(i,j,k+1),
                            getValue(i+1,j+1,k+1),
                            getValue(i,j+1,k),
                            fx, val, len, getMin(), getMax(), 1.0);
            tetSurfIntegral(p[6], p[3], p[1], p[4],
                            getValue(i+1,j+1,k+1),
                            getValue(i,j,k+1),
                            getValue(i+1,j,k),
                            getValue(i,j+1,k),
                            fx, val, len, getMin(), getMax(), 1.0);
         }
      }
   }

   return(val);
}

//------------------------------------------------------------------------
//
// compGradient() -
//
//------------------------------------------------------------------------

float *Datareg3::compGradient(int &len, float **funx)
{
   float *val = (float *)malloc(sizeof(float)*FSAMPLES);
   float *fx = (float *)malloc(sizeof(float)*FSAMPLES);
   float p[8][3];
   float v[8];
   float scaling;
   float grad[4];
   int i, j, k;

   len = FSAMPLES;
   memset(val, 0, sizeof(float)*len);

   *funx = fx;
   for (i=0; i<len; i++)
      fx[i] = getMin() + (i/(len-1.0)) * (getMax()-getMin());

   for (k=0; k<dim[2]-1; k+=FUNSTEP) {
      for (j=0; j<dim[1]-1; j+=FUNSTEP) {
         for (i=0; i<dim[0]-1; i+=FUNSTEP) {
            p[0][0] = xCoord(i);
            p[0][1] = yCoord(j);
            p[0][2] = zCoord(k);

            p[1][0] = xCoord(i+1);
            p[1][1] = yCoord(j);
            p[1][2] = zCoord(k);

            p[2][0] = xCoord(i+1);
            p[2][1] = yCoord(j);
            p[2][2] = zCoord(k+1);

            p[3][0] = xCoord(i);
            p[3][1] = yCoord(j);
            p[3][2] = zCoord(k+1);

            p[4][0] = xCoord(i);
            p[4][1] = yCoord(j+1);
            p[4][2] = zCoord(k);

            p[5][0] = xCoord(i+1);
            p[5][1] = yCoord(j+1);
            p[5][2] = zCoord(k);

            p[6][0] = xCoord(i+1);
            p[6][1] = yCoord(j+1);
            p[6][2] = zCoord(k+1);

            p[7][0] = xCoord(i);
            p[7][1] = yCoord(j+1);
            p[7][2] = zCoord(k+1);

            getCellValues(i, j, k, v);

            grad[0] = (-v[0]+v[1] -v[4]+v[5] -v[3]+v[2] -v[7]+v[6])/4.0;
            grad[1] = (-v[0]-v[1] +v[4]+v[5] -v[3]-v[2] +v[7]+v[6])/4.0;
            grad[2] = (-v[0]-v[1] -v[4]-v[5] +v[3]+v[2] +v[7]+v[6])/4.0;

//            scaling = sqrt(sqr(grad[0]) + sqr(grad[1]) + sqr(grad[2]));
            scaling = sqr(grad[0]) + sqr(grad[1]) + sqr(grad[2]);

            tetSurfIntegral(p[0], p[1], p[3], p[4],
                            getValue(i,j,k),
                            getValue(i+1,j,k),
                            getValue(i,j,k+1),
                            getValue(i,j+1,k),
                            fx, val, len, getMin(), getMax(), fabs(scaling));
            tetSurfIntegral(p[3], p[1], p[2], p[6],
                            getValue(i,j,k+1),
                            getValue(i+1,j,k),
                            getValue(i+1,j,k+1),
                            getValue(i+1,j+1,k+1),
                            fx, val, len, getMin(), getMax(), fabs(scaling));
            tetSurfIntegral(p[6], p[1], p[5], p[4],
                            getValue(i+1,j+1,k+1),
                            getValue(i+1,j,k),
                            getValue(i+1,j+1,k),
                            getValue(i,j+1,k),
                            fx, val, len, getMin(), getMax(), fabs(scaling));
            tetSurfIntegral(p[7], p[3], p[6], p[4],
                            getValue(i,j+1,k+1),
                            getValue(i,j,k+1),
                            getValue(i+1,j+1,k+1),
                            getValue(i,j+1,k),
                            fx, val, len, getMin(), getMax(), fabs(scaling));
            tetSurfIntegral(p[6], p[3], p[1], p[4],
                            getValue(i+1,j+1,k+1),
                            getValue(i,j,k+1),
                            getValue(i+1,j,k),
                            getValue(i,j+1,k),
                            fx, val, len, getMin(), getMax(), fabs(scaling));
         }
      }
   }

   return(val);
}

//------------------------------------------------------------------------
//
// compArea() -
//
//------------------------------------------------------------------------

float *Datareg3::compArea(int &len, float **funx)
{
   float *val = (float *)malloc(sizeof(float)*FSAMPLES);
   float *cum = (float *)malloc(sizeof(float)*FSAMPLES);
   float *fx = (float *)malloc(sizeof(float)*FSAMPLES);
   float p[8][3];
   int i, j, k;
   int b;
   float sum;

   len = FSAMPLES;
   memset(val, 0, sizeof(float)*len);
   memset(cum, 0, sizeof(float)*len);

   *funx = fx;
   for (i=0; i<len; i++)
      fx[i] = getMin() + (i/(len-1.0)) * (getMax()-getMin());

   for (k=0; k<dim[2]-1; k+=FUNSTEP) {
      for (j=0; j<dim[1]-1; j+=FUNSTEP) {
         for (i=0; i<dim[0]-1; i+=FUNSTEP) {
            p[0][0] = xCoord(i);
            p[0][1] = yCoord(j);
            p[0][2] = zCoord(k);

            p[1][0] = xCoord(i+1);
            p[1][1] = yCoord(j);
            p[1][2] = zCoord(k);

            p[2][0] = xCoord(i+1);
            p[2][1] = yCoord(j);
            p[2][2] = zCoord(k+1);

            p[3][0] = xCoord(i);
            p[3][1] = yCoord(j);
            p[3][2] = zCoord(k+1);

            p[4][0] = xCoord(i);
            p[4][1] = yCoord(j+1);
            p[4][2] = zCoord(k);

            p[5][0] = xCoord(i+1);
            p[5][1] = yCoord(j+1);
            p[5][2] = zCoord(k);

            p[6][0] = xCoord(i+1);
            p[6][1] = yCoord(j+1);
            p[6][2] = zCoord(k+1);

            p[7][0] = xCoord(i);
            p[7][1] = yCoord(j+1);
            p[7][2] = zCoord(k+1);

            tetVolIntegral(p[0], p[1], p[3], p[4],
                            getValue(i,j,k),
                            getValue(i+1,j,k),
                            getValue(i,j,k+1),
                            getValue(i,j+1,k),
                            fx, val, cum, len, getMin(), getMax(), 1.0);
            tetVolIntegral(p[3], p[1], p[2], p[6],
                            getValue(i,j,k+1),
                            getValue(i+1,j,k),
                            getValue(i+1,j,k+1),
                            getValue(i+1,j+1,k+1),
                            fx, val, cum, len, getMin(), getMax(), 1.0);
            tetVolIntegral(p[6], p[1], p[5], p[4],
                            getValue(i+1,j+1,k+1),
                            getValue(i+1,j,k),
                            getValue(i+1,j+1,k),
                            getValue(i,j+1,k),
                            fx, val, cum, len, getMin(), getMax(), 1.0);
            tetVolIntegral(p[7], p[3], p[6], p[4],
                            getValue(i,j+1,k+1),
                            getValue(i,j,k+1),
                            getValue(i+1,j+1,k+1),
                            getValue(i,j+1,k),
                            fx, val, cum, len, getMin(), getMax(), 1.0);
            tetVolIntegral(p[6], p[3], p[1], p[4],
                            getValue(i+1,j+1,k+1),
                            getValue(i,j,k+1),
                            getValue(i+1,j,k),
                            getValue(i,j+1,k),
                            fx, val, cum, len, getMin(), getMax(), 1.0);
         }
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
// compMaxArea() -
//
//------------------------------------------------------------------------

float *Datareg3::compMaxArea(int &len, float **funx)
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
// compFunction(), compFunction() -
//
//------------------------------------------------------------------------

float *
Datareg3::compFunction(int n, int &len, float **fx)
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

float *
Datareg3::compFunction(int n, int &len, float ***fx, 
		       float ***sx, float ***sy)
{
   switch (n) {
      case 5:
         return(compVolRelation(len, fx, sx, sy));
   }
   return(NULL);
}

const char *Datareg3::fName(int n)
{
   switch (n) {
      case 0:
         return("Surface Area");
      case 1:
         return("Min Volume");
      case 2:
         return("Max Volume");
      case 3:
         return("Gradient");
   }
   return(NULL);
}

//------------------------------------------------------------------------
//
// compVolRelation() - 
//
//------------------------------------------------------------------------
// add by Fan:
float *Datareg3::compVolRelation(int &len, float ***funx, 
				  float ***sx, float ***sy)
{
#if 1
   float **val1 = (float **)malloc(sizeof(float *)*FSAMPLES_TOPO);
   float **val2 = (float **)malloc(sizeof(float *)*FSAMPLES_TOPO);
   float **fx =  (float **)malloc(sizeof(float *)*2);
#endif
   float p[8][3];
   int i, j, k;
   float u[8], v[8];
   float max1, max2, min1,min2;

   len = FSAMPLES_TOPO;
   for(i=0; i<FSAMPLES_TOPO; i++){
     val1[i] = new float[FSAMPLES_TOPO];
     val2[i] = new float[FSAMPLES_TOPO];
     memset(val1[i], 0, sizeof(float)*len);
     memset(val2[i], 0, sizeof(float)*len);
   }

   min1 = getMin(funtopol1);
   max1 = getMax(funtopol1);
   min2 = getMin(funtopol2);
   max2 = getMax(funtopol2);
   fx[0] =  new float[FSAMPLES_TOPO];
   fx[1] =  new float[FSAMPLES_TOPO];   
   for (i=0; i<len; i++){
      fx[0][i] = min1 + (i/(len-1.0)) * (max1-min1);
      fx[1][i] = min2 + (i/(len-1.0)) * (max2-min2);
#if 0
      printf("fx[0][%d]=%f, fx[1][%d]=%f\n",i,fx[0][i],fx[1][i]);
#endif
   } 

   *funx = fx;
   *sx = val1;
   *sy = val2;

   for (k=0; k<dim[2]-1; k+=FUNSTEP) {
      for (j=0; j<dim[1]-1; j+=FUNSTEP) {
         for (i=0; i<dim[0]-1; i+=FUNSTEP) {
            p[0][0] = xCoord(i);
            p[0][1] = yCoord(j);
            p[0][2] = zCoord(k);

            p[1][0] = xCoord(i+1);
            p[1][1] = yCoord(j);
            p[1][2] = zCoord(k);

            p[2][0] = xCoord(i+1);
            p[2][1] = yCoord(j);
            p[2][2] = zCoord(k+1);

            p[3][0] = xCoord(i);
            p[3][1] = yCoord(j);
            p[3][2] = zCoord(k+1);

            p[4][0] = xCoord(i);
            p[4][1] = yCoord(j+1);
            p[4][2] = zCoord(k);

            p[5][0] = xCoord(i+1);
            p[5][1] = yCoord(j+1);
            p[5][2] = zCoord(k);

            p[6][0] = xCoord(i+1);
            p[6][1] = yCoord(j+1);
            p[6][2] = zCoord(k+1);

            p[7][0] = xCoord(i);
            p[7][1] = yCoord(j+1);
            p[7][2] = zCoord(k+1);
   
	    getCellValues(i,j,k,u,funtopol1);
	    getCellValues(i,j,k,v,funtopol2);

	    intVolIntegral((float **)p, u, v, fx[0], val1, fx[1], val2,
		   len, min1, max1, min2,  max2, 1.0);

	 }
      }
   }

   return(NULL);

}
