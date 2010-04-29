#!/usr/bin/env python

##################################################################
"""

Masks a variable.  Also shows how to create a new iovar variable. This
script uses the landuse data to mask out o3 over water.


"""
##################################################################

import ioapiTools as ioT
import numpy
import sys, os

## Setup
## input file
iFile = "CCTM_CONC.D1.001"
iFile = os.path.join (sys.prefix,'sample_data/' + iFile)

gridFile = "GRIDCRO2D_D1.001"
gridFile = os.path.join (sys.prefix,'sample_data/' + gridFile)

## output file
oFile1 = "o3_masked.nc"

##------------------------------------------------------------##

## open input files for reading
f = ioT.open(iFile)
grid = ioT.open(gridFile)

## read  o3 and landuse variables
o3 = f("o3")
landuse = grid("dluse")
f.close()
grid.close()

## Mask o3 variable
## Future versions of ioapiTools will make this easier

## Need to have landuse data same shape as o3
## landuse data has shape (1,1,nrows,ncols)
## o3 data has shape (ntime,nlev,nrows, ncols)
## so repeat landuse in time and level dimensions
ntime = o3.shape[0]
nlev = o3.shape[1]
## repeat landuse in each level, axis=1
landuse4D = numpy.repeat(landuse, nlev, 1)
## repeat landuse in each time, axis=0
landuse4D = numpy.repeat(landuse4D, ntime, 0)

## Change to ppb
## NOTE: if do this after created the masked array
## the o3MA will have 1000* fill_value in each element,
## instead want to convert original data in that cell
o3 *= 1000

## Mask out o3 where landuse is water (i.e. equal 16)
## this returns a masked array object, not an iovar object
o3MA = numpy.ma.masked_where(landuse4D == 16, o3)

## Convert masked array into an iovar object
## to create the iovar object, use the original o3 iovar
## to get axes, attributes, and metadata

## get the attributes
atts = o3.attributes

## get the axes,
## first create a cdms2M object this contains all the
## cdms2 specific information
cdmsM = ioT.cdmsmeta(o3, ioT.cdmsvarFlag)
axesLst = cdmsM.getAxes()

## get ioM - IOAPI metadata
ioM = o3.ioM.copy()

## create a new iovar object
## Note:  you can create a new iovar from a numpy, numpy.ma,
## cdms2, or iovar arrays
o3Masked = ioT.createVariable(o3MA, ioM, axesLst, "O3", atts)

## change some metadata
o3Masked.IOmodVar(vunits="ppb", desc="O3 variable, water masked out")

## Change missing value to 0, i.e. all values over water will be 0
o3Masked.setMissing(0.)

## print some diagnostics
## where landuse == water in your data may be different
print "Original data (horizontal slice at t=12, lev=0, row=43, col=30 - 38):"
print o3Masked[12,0,43,30:38].raw_data()
print ""
print "Masked data (horizontal slice at t=12, lev=0, row=43, col=30 - 38):"
print o3Masked[12,0,43,30:38].getValue()

## Could also do some plots, to show masked area
## set interp to 'nearest' so don't try and smooth
## out 0 values
## Note: for script to end, need to kill the figure window
#o3Masked(12).contour(interp="nearest")

## write to file
## NOTE: by writing it to a file, you have effectively
## destroyed the data at the mask areas. When you reread
## the data, those areas that where over water will have
## raw values = 0
g = ioT.open(oFile1, "w")
g.write(o3Masked)
g.close()
