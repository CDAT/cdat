#!/usr/bin/env python
# Adapted for numpy/ma/cdms2 by convertcdms.py

##################################################################
"""

Advanced spatial subsets of a variable.  Shows how to have finer
control over the subset.  This uses CDMS subsetting, so all the
subsets should be done in the native coordinates.

    3 different spatial coordinate systems:
    
       proj - native coordinate system.  In this case Lambert Confomal
              Conic, which is in meters from the projection center

       ll   - longitude, Latitude in degrees East and North

       cr   - col, row,  0-based indices
       


"""
##################################################################

import ioapiTools as ioT
import cdms2 as cdms
import cdat_info
import cdtime
import os, sys

## Setup
## input file
iFile = "CCTM_ACONC.D1.001"
iFile = os.path.join (cdat_info.get_prefix(),'sample_data/' + iFile)

## variable of interest
varName = "o3"

## subset domain in col, row. needs to be a list of lists x,y order
domainCR = [[2, 5], [55, 61]] ## col, row, 0-based

##------------------------------------------------------------##

## open input file for reading
f = ioT.open(iFile)

## read  variable
var = f(varName)
f.close()

## convert the domain into Xlon, Ylat
domainXY = var.IOcoordConv(domainCR, ioT.cr2projFlag)

## print equivalency
print "col/row --> proj:"
print "%s --> %s" %(domainCR, domainXY)

## Calculate the cell size
## get the bounds, ie the extent of the first cell
## diff is the cell size, in both x and y in my case.
bounds = var.getLatitude().getBounds()[0]
cellSize = bounds[1] - bounds[0]

## Narrow the subset domain by a fourth the cellsize this means that
## the first coordinate (SE) is between the cell center and the NE
## corner of the cell
domainSmallXY = [[domainXY[0][0]+cellSize/4.,domainXY[0][1]+cellSize/4.], \
                 [domainXY[1][0], domainXY[1][1]]]

print "New SE corner: %s" %domainSmallXY[0]

## subset the variable normal way by Xlon, Ylat
## this will include the SE cell 
var_sub1 = var.IOsubset(domainSmallXY, coordFlag = ioT.projFlag)

## get the ylat and xlon axes
ylatAxis = var_sub1.getLatitude()
xlonAxis = var_sub1.getLongitude()
ncols = len(xlonAxis)
nrows = len(ylatAxis)
## print some diagnostics
print "subsetting through IOsubset:"
print "Xlon: "
print "\t# elements: %d, range: %.1f -- %.1f" \
      %(ncols, xlonAxis.getValue()[0], xlonAxis.getValue()[-1])
print "Ylat: "
print "\t# elements: %d, range: %.1f -- %.1f" \
      %(nrows, ylatAxis.getValue()[0], ylatAxis.getValue()[-1])
print ""

## subset using cdms.  B/c we are using
## cdms, it doesn't know anything about the coordinate
## transformations, therefore everything needs to
## be done in proj coordinates

## identical call to var_sub1
## the 'ccb' stands for closed, closed, bounds
## i.e. this will inclused anything cell
## where one boundary of that cell is w/in the range:
## lat1 <= bounds <= lat2
lat1 = domainSmallXY[0][1]
lat2 = domainSmallXY[1][1]
lon1 = domainSmallXY[0][0]
lon2 = domainSmallXY[1][0]
var_sub2 = var(latitude=(lat1,lat2,"ccb"), \
               longitude=(lon1,lon2,"ccb"))

## get the ylat and xlon axes
ylatAxis = var_sub2.getLatitude()
xlonAxis = var_sub2.getLongitude()
ncols = len(xlonAxis)
nrows = len(ylatAxis)
## print some diagnostics
print "identical subsetting but through cdms:"
print "Xlon: "
print "\t# elements: %d, range: %.1f -- %.1f" \
      %(ncols, xlonAxis.getValue()[0], xlonAxis.getValue()[-1])
print "Ylat: "
print "\t# elements: %d, range: %.1f -- %.1f" \
      %(nrows, ylatAxis.getValue()[0], ylatAxis.getValue()[-1])
print ""

## the 'ccn' stands for closed, closed, nodes
## i.e. this will inclused anything cell
## where the node (center) of that cell is w/in the range:
## lat1 <= node <= lat2
## Note: the SE corner cell is now excluded
var_sub3 = var(latitude=(lat1,lat2,"ccn"), \
               longitude=(lon1,lon2,"ccn"))


## get the ylat and xlon axes
ylatAxis = var_sub3.getLatitude()
xlonAxis = var_sub3.getLongitude()
ncols = len(xlonAxis)
nrows = len(ylatAxis)
## print some diagnostics
print "closed node subsetting through cdms:"
print "Xlon: "
print "\t# elements: %d, range: %.1f -- %.1f" \
      %(ncols, xlonAxis.getValue()[0], xlonAxis.getValue()[-1])
print "Ylat: "
print "\t# elements: %d, range: %.1f -- %.1f" \
      %(nrows, ylatAxis.getValue()[0], ylatAxis.getValue()[-1])
print ""

## the 'con' stands for closed, opened, nodes
## i.e. this will inclused anything cell
## where the node (center) of that cell is w/in the range:
## lat1 <= node < lat2
## Note: the SE and NE corner cells are now excluded
var_sub4 = var(latitude=(lat1,lat2,"con"), \
               longitude=(lon1,lon2,"con"))


## get the ylat and xlon axes
ylatAxis = var_sub4.getLatitude()
xlonAxis = var_sub4.getLongitude()
ncols = len(xlonAxis)
nrows = len(ylatAxis)
## print some diagnostics
print "opened node subsetting through cdms:"
print "Xlon: "
print "\t# elements: %d, range: %.1f -- %.1f" \
      %(ncols, xlonAxis.getValue()[0], xlonAxis.getValue()[-1])
print "Ylat: "
print "\t# elements: %d, range: %.1f -- %.1f" \
      %(nrows, ylatAxis.getValue()[0], ylatAxis.getValue()[-1])
print ""

## You can combine closed and opened in multiple orders depending on
## how each range element should be tested:
## ie 'cob' or 'ocb' or 'oob' or 'ccb'

## You can also create selectors, these are good if you want
## to make more general code or apply these selectors multiple times
sel = cdms.selectors.Selector(latitude=(lat1, lat2, "oob"), \
                              longitude=(lon1, lon2, "oob"))

var_sub5 = var(sel)

## get the ylat and xlon axes
ylatAxis = var_sub5.getLatitude()
xlonAxis = var_sub5.getLongitude()
ncols = len(xlonAxis)
nrows = len(ylatAxis)
## print some diagnostics
print "opened bound subsetting through cdms Selectors:"
print "Xlon: "
print "\t# elements: %d, range: %.1f -- %.1f" \
      %(ncols, xlonAxis.getValue()[0], xlonAxis.getValue()[-1])
print "Ylat: "
print "\t# elements: %d, range: %.1f -- %.1f" \
      %(nrows, ylatAxis.getValue()[0], ylatAxis.getValue()[-1])
print ""


## Could also subset time, etc
## Note: time cdtime object and level is a sigma value
date1 = cdtime.componenttime(2001,6,1,12)
sel = cdms.selectors.Selector(time = date1, level = 1., \
                              latitude = (lat1,lat2, "ccb"), \
                              longitude = (lon1, lon2, "ccb"))

var_sub6 = var(sel)

## get  axes
timeAxis = var_sub6.getTime()
levAxis = var_sub6.getLevel()
ylatAxis = var_sub6.getLatitude()
xlonAxis = var_sub6.getLongitude()
ncols = len(xlonAxis)
nrows = len(ylatAxis)

print "Selector in time, level, and space:"
print var_sub6
print "time: %s" %timeAxis.asComponentTime()
print "level: %s" %levAxis.getValue()
print "Xlon: "
print "\t# elements: %d, range: %.1f -- %.1f" \
      %(ncols, xlonAxis.getValue()[0], xlonAxis.getValue()[-1])
print "Ylat: "
print "\t# elements: %d, range: %.1f -- %.1f" \
      %(nrows, ylatAxis.getValue()[0], ylatAxis.getValue()[-1])
print ""

