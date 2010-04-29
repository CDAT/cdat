//------------------------------------------------------------------------
//
// cellQueue.C - queue of cell identifiers.  The circular queue dyanmically
//               resizes itself when full.  Elements in the queue are of
//               indicies of i,j,k (specialized for 3d structured grids)
//
// Copyright (c) 1997 Dan Schikore
//------------------------------------------------------------------------

// $Id: cellQueue.C,v 1.2 2005/05/23 17:13:36 rcook Exp $

#include <stdio.h>
#include <math.h>
#include <memory.h>
#include <stdlib.h>
#include <string.h>
#include "cellQueue.h"
