#!/usr/bin/env python

##################################################################
"""
Change the metadata and the values of a variable

This example will create a NOx variable from NO and NO2, convert it
into ppb from ppm, and change some metadata.

Note: All the IOAPI specific metadata is stored in the ioM object.
      See the documentation for more info on ioM and how to change it.

"""
##################################################################

import ioapiTools as ioT
import sys, os

## Setup
## input file
iFile = "CCTM_ACONC.D1.001"
iFile = os.path.join (sys.prefix,'sample_data/' + iFile)

oFile1 = "nox.ioapi"


##------------------------------------------------------------##

## open input file for reading
f = ioT.open(iFile)

## read  NO and NO2
no = f("no")
no2 = f("no2")
f.close()

## create NOx, simply add 2 variables together
nox = no + no2

## print the metadata for NOx, notice that it is
## the same as NO
## all the IOAPI metadata is stored in the ioM object
print "Original metadata:"
print nox.ioM


## Convert the variable to ppb, ie. multiply by 1000
nox *= 1000.

## Change the metadata for the variable,
## use the modVar method to change variable name, units, or
## or long description
nox.IOmodVar("NOX", "PPB", "NOx variable -- NO + NO2")

## Change some specific aspects of the IOAPI metadata
## be CAREFUL !! easy to break ioapi if you don't know
## what your doing
nox.ioM.gridName = "M_test"
nox.ioM.fileDesc = "NOx variable constructed using ioapiTools"

## print new metadata
print "Updated metadata:"
print nox.ioM

## write variable to new file
print "writing new nox variable to a file:", oFile1
g = ioT.open(oFile1, "w", ioT.iofileFlag)
g.write(nox)
g.close()
