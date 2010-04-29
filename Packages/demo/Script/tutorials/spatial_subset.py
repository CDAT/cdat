#!/usr/bin/env python

##################################################################
"""
Spatial subsets a variable

    3 different spatial coordinate systems:
    
       proj - native coordinate system.  In this case Lambert Confomal
              Conic, which is in meters from the projection center

       ll   - longitude, Latitude in degrees East and North

       cr   - col, row,  0-based indices
       

Note: These subsets do not change the native projection or reproject
the data.  In fact, all it does is find the nearest cell center to
each of the coordinate pairs, and use this to define the domain.  In
subset2, you can see that the returned variable actually has a
slightly larger domain than domainXY.


"""
##################################################################

import ioapiTools as ioT
import sys, os


## Setup
## input file
iFile = "CCTM_ACONC.D1.001"
iFile = os.path.join (sys.prefix,'sample_data/' + iFile)

## output files
oFile1 = "var_sub1.nc"
oFile2 = "var_sub2.nc"
oFile3 = "var_sub3.nc"

## variable of interest
varName = "o3"

## subset domain, needs to be a list of tuples x,y order
domainLL = [(-92,35), (-82, 42)]   ## Lon, Lat in degrees
domainXY = [(-450000, -950000), (20000, 340000)] ## Xlon, Ylat in meters
domainCR = [(2, 5), (55, 61)]  ## col, row, 0-based

##------------------------------------------------------------##

## open input file for reading
f = ioT.open(iFile)

## read  variable
var = f(varName)

## print some diagnostics
print var

## get Latitude and Longitude axes, print diagnostics
## units are in meters, Xlon, Ylat
print var.getLatitude()
print var.getLongitude()
print ""

## Subset the variable using lat, lon domain
var_sub1 = var.IOsubset(domainLL)

## print some diagnostics
print "subset 1:"
print var_sub1
print var_sub1.getLatitude()
print var_sub1.getLongitude()
print ""


## subset the variable using Xlon, Ylat
var_sub2 = var.IOsubset(domainXY, coordFlag = ioT.projFlag)

## print some diagnostics
print "subset 2:"
print var_sub2
print var_sub2.getLatitude()
print var_sub2.getLongitude()
print ""

## subset the variable using col row
var_sub3 = var.IOsubset(domainCR, coordFlag = ioT.crFlag)

## print some diagnostics
print "subset 3:"
print var_sub3
print var_sub3.getLatitude()
print var_sub3.getLongitude()
print ""


## write variables to output files
g1 = ioT.open(oFile1, "w")
g2 = ioT.open(oFile2, "w")
g3 = ioT.open(oFile3, "w")

##
print "Writing subsets to out files"
g1.write(var_sub1)
g2.write(var_sub2)
g3.write(var_sub3)

## close files
g1.close()
g2.close()
g3.close()
f.close()
