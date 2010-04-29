#!/usr/bin/env python

##################################################################
"""
Coordinate conversions.  Also tests whether a coordinate or date is
w/in the spatial or temporal domain.

    3 different spatial coordinate systems:
    
       proj - native coordinate system.  In this case Lambert Confomal
              Conic, which is in meters from the projection center

       ll   - longitude, Latitude in degrees East and North

       cr   - col, row,  0-based indices
       

Note: The Lambert Conformal Conic coordinate system is the coordinates
      recorded in the Latitude and Longitude axes.  I refer to them as
      Xlon, Ylat.  This is a regular coordinate system.  If you
      calculate the locations in terms of Lon,Lat (degrees) the
      coordinate system is irregular.  In other words, the cell
      locations are not 'equally' spaced.


"""
##################################################################

import ioapiTools as ioT
import sys, os


## Setup
## input file
iFile = "CCTM_ACONC.D1.001"
iFile = os.path.join (sys.prefix,'sample_data/' + iFile)

## variable of interest
varName = "o3"

## coordinate of interest, needs to be a list of tuples x,y order
## could be multiple coordinates
coordLL = [(-92,35)]                            ## Lon, Lat in degrees
coordXY = [(-450000, 340000), (20000, 340000)] ## Xlon, Ylat in meters
coordCR = [(55, 61)]                            ## col, row, 0-based

## coordinate Outside the domain in lon,lat
outsideLL = (24.32, 34.68)

## date inside and outside temporal domain
dateIn = "2001-06-01 22:00"
dateOut = "2001-05-03 12:00"

##------------------------------------------------------------##

## open input file for reading
f = ioT.open(iFile)

## read  variable
var = f(varName)
f.close()

## lon/lat --> proj
## Note, the 3rd element is the vertical coordinate, ignore
## this is 0 for all values
coordOutLst = var.IOcoordConv(coordLL)
print "lon/lat --> proj:"
print "\t%s  -->  %s" %(coordLL, coordOutLst)

## lon/lat --> col,row
coordOutLst = var.IOcoordConv(coordLL, ioT.ll2crFlag)
print "lon/lat --> col/row:"
print "\t%s --> %s\n" %(coordLL, coordOutLst)

## proj --> lon/lat
## Note, multiple coordinates.  Also note that although the Ylat is
## the same, b/c of the irregular grid in Lon,Lat coordinates, the
## Lats are not the same
coordOutLst = var.IOcoordConv(coordXY, ioT.proj2llFlag)
print "proj --> lon/lat:"
print "\t%s --> %s" %(coordXY, coordOutLst)

## proj --> col/row
coordOutLst = var.IOcoordConv(coordXY, ioT.proj2crFlag)
print "proj --> col/row:"
print "\t%s --> %s\n" %(coordXY, coordOutLst)

## col/row --> proj
## this can also be done --> lon/lat
## For col/row can find the coordinate values at
## the center of the cell, or at any of the corners,
## ie.  the coordinates at the SE corner, NW, ...

## center of cell
coordOutLst = var.IOcoordConv(coordCR, ioT.cr2projFlag)
print "c/r --> proj  (center):"
print "\t%s --> %s" %(coordCR, coordOutLst)

## SW corner of cell
coordOutLst = var.IOcoordConv(coordCR, ioT.cr2projFlag, ioT.SWFlag)
print "c/r --> proj  (SW corner):"
print "\t%s --> %s" %(coordCR, coordOutLst)

## SE corner of cell
coordOutLst = var.IOcoordConv(coordCR, ioT.cr2projFlag, ioT.SEFlag)
print "c/r --> proj  (SE corner):"
print "\t%s --> %s" %(coordCR, coordOutLst)

## NE corner of cell
coordOutLst = var.IOcoordConv(coordCR, ioT.cr2projFlag, ioT.NEFlag)
print "c/r --> proj  (NE corner):"
print "\t%s --> %s" %(coordCR, coordOutLst)

## NW corner of cell
coordOutLst = var.IOcoordConv(coordCR, ioT.cr2projFlag, ioT.NWFlag)
print "c/r --> proj  (NW corner):"
print "\t%s --> %s\n" %(coordCR, coordOutLst)


## Domain tests:
print "Testing spatial domain:"
## Test if this lon/lat coordinate inside domain
if var.IOcoordInDomain(outsideLL):
    print "(%.1f,%.1f) inside spatial domain" \
          %(outsideLL[0],outsideLL[1])
else:
    print "(%.1f,%.1f) outside spatial domain" \
          %(outsideLL[0],outsideLL[1])

## Test if this proj coordinate inside the domain
if var.IOcoordInDomain(coordXY[0], ioT.projFlag):
    print "(%.1f,%.1f) inside spatial domain" \
          %(coordXY[0][0],coordXY[0][1])
else:
    print "(%.1f,%.1f) outside spatial domain" \
          %(coordXY[0][0],coordXY[0][1])

## get the full extent of the domain in proj units
domainProj = var.IOcoordInDomain(typeFlag = ioT.projFlag, domainFlag=True)
print "Domain extent in proj (SW, NE): "
print "\t%s\n" %domainProj

## test the temporal domain
print "Testing temporal domain:"
if var.IOdateInDomain(dateIn):
    print "%s inside temporal domain" %dateIn
else:
    print "%s outside temporal domain" %dateIn
    
if var.IOdateInDomain(dateOut):
    print "%s inside temporal domain" %dateOut
else:
    print "%s outside temporal domain" %dateOut
    
    
