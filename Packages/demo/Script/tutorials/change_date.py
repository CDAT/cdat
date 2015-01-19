#!/usr/bin/env python

##################################################################
"""
Change the starting date of variable


"""
##################################################################

import ioapiTools as ioT
from mx import DateTime as D
import sys, os
import cdat_info

## Setup
## input file
iFile = "CCTM_ACONC.D1.001"
iFile = os.path.join (cdat_info.get_prefix(),'sample_data/' + iFile)

oFile1 = "var_newdate.ioapi"

## variable of interest
varName = "o3"

## new date
dateNew = D.DateTime(2006,7,10,12)


##------------------------------------------------------------##

## open input file for reading
f = ioT.open(iFile)

## read  variable
var = f(varName)
f.close()

## Make a copy of the variable so that you don't change
## original variables dates
var2 = var.IOclone()

## change the date of var2, could use DateTime object, cdtime object
## or a date string
var2.IOchangeDate(dateNew)


## print some diagnostics
print "original date values:"
print var.getTime()  ## time axis
print var.getTime().asComponentTime()  ## list of cdtime values
print ""

print "new date values:"
print var2.getTime()  ## time axis
print var2.getTime().asComponentTime()  ## list of cdtime values

## write variable to new file
print "writing new date variable to a file:", oFile1
g = ioT.open(oFile1, "w", ioT.iofileFlag)
g.write(var2)
g.close()
