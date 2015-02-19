#!/usr/bin/env python

##################################################################
"""
Reads in data from a file and writes out Climate and Forcast compliant
netCDF files.

"""
##################################################################

import ioapiTools as ioT
import sys, os
import cdat_info

## Setup

## input file
iFile = "CCTM_ACONC.D1.001"
iFile = os.path.join (cdat_info.get_prefix(),'sample_data/' + iFile)

## output files
oFile1 = "var1.nc"
oFile2 = "multvar.nc"

## variable of interest
varName = "o3"

##------------------------------------------------------------##


## open input file for reading
f = ioT.open(iFile)

## read 1 variable
var1 = f(varName)

## print some diagnostics
print "Read %s from infile: %s" %(varName, iFile)
print "Writing to CF netCDF file: %s" %oFile1

## open an output file for writing (CF netCDF format)
## write the single variable to an output file
g = ioT.open(oFile1, "w")
g.write(var1)
g.close()

## Reading all the variables in a file
## create a list of all the variable names
varNameLst = f.listvariables()

## print some diagnostics
print "Reading %d variables from infile: %s" %(len(varNameLst), iFile)
print "This might take a while..."

## create a list of all the iovar's, one for each name
varLst = [f(varN) for varN in varNameLst]

## print some diagnostics
print "Writing variables to CF netCDF file: %s" %oFile2
print "This might take a while..."

## open an output file for writing (CF netCDF format)
## write all the variables
g = ioT.open(oFile2, "w")
g.write(varLst)
g.close()

## close the input file
f.close()


