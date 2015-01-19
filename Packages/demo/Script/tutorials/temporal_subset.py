#!/usr/bin/env python

##################################################################
"""
Temporal subsets a variable

    A couple of ways of indicating time for ioapiTools:
       indices  - 0-based index

       DateTime - mx.DateTime objects

       cdtime   - cdtime objects

       string   - time string of format:
                  YYYY-MM-DD HH:MM:SS.SS

                  eg. '1996-06-25 12:00'
                  
"""
##################################################################

import ioapiTools as ioT
from mx import DateTime as D
import sys, os, cdat_info

## Setup
## input file
iFile = "CCTM_ACONC.D1.001"
iFile = os.path.join (cdat_info.get_prefix(),'sample_data/' + iFile)

## output files
oFile1 = "var_tempsub1.nc"
oFile2 = "var_tempsub2.nc"

## variable of interest
varName = "o3"

## temporal domain, these should fall w/in iFile's date range
date1 = D.DateTime(2001, 6, 1, 2)
date2 = date1 + 12*D.oneHour   ## adding 12 hrs

ind1 = 2
ind2 = 18

##------------------------------------------------------------##

## open input file for reading
f = ioT.open(iFile)

## read  variable
var = f(varName)
f.close()

## print some diagnostics
print "original variable:"
print var

## get time axis
print var.getTime()
print ""

## Subset the variable using dates
var_sub1 = var.IOsubset(timeLst = [date1,date2])

## print some diagnostics
print "subset 1:"
print var_sub1
print var_sub1.getTime()
## returns a list of cdtime objects
print var_sub1.getTime().asComponentTime()  
print ""

## Subset the variable using indices
var_sub2 = var.IOsubset(timeLst = [ind1,ind2], indexFlag=True)

## print some diagnostics
print "subset 2:"
print var_sub2
print var_sub2.getTime()
## returns a list of cdtime objects
print var_sub2.getTime().asComponentTime()  
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

