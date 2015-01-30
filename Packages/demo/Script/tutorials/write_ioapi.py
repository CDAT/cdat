#!/usr/bin/env python

##################################################################
"""
Reads in data from a file and writes out IOAPI files.

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
oFile1 = "var1.ioapi"
oFile2 = "multvar1.ioapi"
oFile3 = "multvar2.ioapi"

## variable of interest
varName = "o3"

## logfile for ioapi log info
logName = "ioapi.log"

##------------------------------------------------------------##


## open input file for reading
f = ioT.open(iFile)

## read 1 variable
var1 = f(varName)

## print some diagnostics
print "Read %s from infile: %s" %(varName, iFile)
print "Writing to ioapi file: %s" %oFile1

## open an output file for writing (CF netCDF format)
## write the single variable to an output file
## send ioapi stdout to a log (optional)
## all subsequent ioapi writes will be written to this logFile
## do NOT include in future open commands
g = ioT.open(oFile1, "w", ioT.iofileFlag, logFile=logName)
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
print "Writing all vars to ioapi file: %s" %oFile2
print "This might take a while..."

## open an output file for writing (CF netCDF format)
## write all the variables
g = ioT.open(oFile2, "w", ioT.iofileFlag)
g.write(varLst)
g.close()


## if you want to write variables to an IOAPI files
## over multiple writes, you need to pre-set the IOAPI
## file's metadata
## here we are going to do two writes

## get metadata on variables that will eventually written to file
newM = ioT.combineMeta(varLst)

## open a file for writing and write the metadata
print "Writing metadata to %s" %(oFile3)
g = ioT.open(oFile3, "w", ioT.iofileFlag, newM)

## Make first write to out file
## these writes can be done in any order
print "First write"
g.write(varLst[-1])

## Make second write to out file
print "second write"
g.write(varLst[:-1])
g.close()


## close the input file
f.close()


