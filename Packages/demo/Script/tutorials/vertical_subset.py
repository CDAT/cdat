#!/usr/bin/env python

##################################################################
"""
Vertically subsets a variable

    A couple of ways of indicating vertical levels
       indices  - 0-based index of level

       sigma    - sigma value 
                  
"""
##################################################################

import ioapiTools as ioT
from mx import DateTime as D
import sys, os

## Setup
## input file
iFile = "CCTM_CONC.D1.001"
iFile = os.path.join (sys.prefix,'sample_data/' + iFile)

## output files
oFile1 = "var_vertsub1.nc"
oFile2 = "var_vertsub2.nc"

## variable of interest
varName = "o3"

## vertical domain, these should fall w/in iFile's sigma range
## note that sigma's run from 1.0 (surface) to 0.0 at top of atm
ind1 = 0
ind2 = 5
sig1 = .994
sig2 = .85

##------------------------------------------------------------##

## open input file for reading
f = ioT.open(iFile)

## read  variable
var = f(varName)
f.close()

## print some diagnostics
print "original variable:"
print var

## get layer
print var.getLevel()
print ""

## Subset the variable using level indices
var_sub1 = var.IOsubset(layerLst = [ind1,ind2])

## print some diagnostics
print "subset 1:"
print var_sub1
print var_sub1.getLevel()
print ""

## Subset the variable using sigma
var_sub2 = var.IOsubset(layerLst = [sig1,sig2], layindexFlag=False)

## print some diagnostics
print "subset 2:"
print var_sub2
print var_sub2.getLevel()
print ""



## write variables to output files
g1 = ioT.open(oFile1, "w")
g2 = ioT.open(oFile2, "w")

##
print "Writing subsets to out files"
g1.write(var_sub1)
g2.write(var_sub2)

## close files
g1.close()
g2.close()

