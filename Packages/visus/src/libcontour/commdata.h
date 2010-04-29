//------------------------------------------------------------------------
//
// commdata.h - client/server data communication structures
//	      - this module defines the client/server DATA interface 
//	      - this, data.h and seedCells.h are the only modules common 
//		to both the server and the client programs
//
// Description:	these are the data structures produced at the server, sent
//		and received by the client. They are structs and NOT
//		classes because:
//		    * they have no methods associated: no processing is
//		      done on them at the client side
//		    * they are read only: neither of their members is ever
//		      modified by the client's user interface
//		Hence having classes instead of structs greatly simplifies
//		building, sending and accessing these data structures.
//
// Copyright (c) 1998 Emilio Camahort - ecamahor@cs.utexas.edu
//
//------------------------------------------------------------------------

// $Id: commdata.h,v 1.1 2003/09/02 17:27:15 scorzell Exp $

#ifndef COMMDATA_H
#define COMMDATA_H

#include "data.h"
#include "seedCells.h"

//------------------------------------------------------------------------
//  a constant that says use no variable for coloring an isocontour
//------------------------------------------------------------------------

#define NO_COLOR_VARIABLE	-1

//------------------------------------------------------------------------
//  data set parameters (some parameters unused for some dataset types)
//------------------------------------------------------------------------

typedef struct DatasetParams	// structure to store a dataset parameters 
{
    int		data_id;		// dataset identifier
    int		nvars;			// number of variables
    int		ntime;			// number of time steps

    u_int	dim[3];			// data set x, y, z dimensions
    float	orig[3];		// coords of data set origin
    float	span[3];		// how far does the data set go

    float	minext[3];		// min corner of data set
    float	maxext[3];		// max corner of data set

    char	*error;			// client/server error string
} DatasetParams;

//------------------------------------------------------------------------
//  data structures for storing seed cells
//------------------------------------------------------------------------

typedef struct SeedData		// structure to store seeds (see seedCells.h)
{
    int		data_id;		// dataset identifier
    int		variable;		// dataset variable
    int		timestep;		// dataset timestep

    int		nseeds;			// number of seed cells
    SeedCell	*seeds;			// array of seed cells

    char	*error;			// client/server error string
} SeedData;

//------------------------------------------------------------------------
//  data structures for storing signature functions
//------------------------------------------------------------------------

typedef struct Signature	// a single signature function
{
    char	*name;			// fuction name
    int		nval;			// number of function values
    float	*fx;			// x-values of function
    float	*fy;			// y-values of function
} Signature;

typedef struct SignatureData	// a structure for signature function data
{
    int		data_id;		// dataset identifier
    int		variable;		// dataset variable
    int		timestep;		// dataset timestep

    float	minvalue;		// minimum value for dataset variable
    float	maxvalue;		// maximum value for dataset variable

    int		nsfun;			// number of signature functions
    Signature	*sfun;			// signature function array

    char	*error;			// client/server error string
} SignatureData;

//------------------------------------------------------------------------
//  a data structure for storing a set of signature function values
//------------------------------------------------------------------------

typedef struct SignatureValues
{
    int		data_id;		// dataset identifier
    int		variable;		// dataset variable
    int		timestep;		// dataset timestep
    float	isovalue;		// x-value (isovalue)

    int		nsfun;			// number of signature functions
    float	*yvalues;		// array of signature function values

    char	*error;			// client/server error string
} SignatureValues;

//------------------------------------------------------------------------
//  a slice of a 3D regular grid
//------------------------------------------------------------------------

typedef struct SliceData	// structure to store a 2d scalar data slice
{
    int		data_id;		// dataset identifier
    int		variable;		// dataset variable
    int		timestep;		// dataset timestep

    char	axis;			// either x, y or z
    int		index;			// index of slice along axis
    int		width, height;		// slice width and height

    Data::DataType	type;		// type of data: uchar, ushort, float
    Data::datatypes	data;		// slice data

    char	*error;			// client/server error string
} SliceData;

//------------------------------------------------------------------------
//  store a 2D isocontour
//------------------------------------------------------------------------

typedef struct Contour2dData	// structure to store a 2d isocontour
{
    int		data_id;		// dataset identifier
    int		variable;		// dataset variable
    int		timestep;		// dataset timestep
    float	isovalue;		// isovalue for this isocontour

    int		nvert;			// size of vertex array
    int		nedge;			// size of edge array

    float	(*vert)[2];		// polyline vertex array
    u_int	(*edge)[2];		// array of edge indices

    char	*error;			// client/server error string
} Contour2dData;

//------------------------------------------------------------------------
//  store a 3D isocontour
//------------------------------------------------------------------------

typedef struct Contour3dData	// structure to store a 3d isocontour
{
    int		data_id;		// dataset identifier
    int		variable;		// dataset variable
    int		timestep;		// dataset timestep
    float	isovalue;		// isovalue for this isocontour

    int		nvert;			// number of vertices in vertex array
    int		ntri;			// triangles in triangle array

    float	(*vert)[3];		// isosurface vertex array
    float	(*vnorm)[3];		// array of vertex normals
    float	(*vfun);		// color values at vertices

    u_int	(*tri)[3];		// triangle mesh array

    int		colorvar;		// color variable (-1 if no color)
    float	fmin, fmax;		// min and max color values

    char	*error;			// client/server error string
} Contour3dData;

#endif /* of COMMDATA_H */
