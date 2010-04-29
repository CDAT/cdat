#!/usr/bin/env python

##################################################################
"""
Reads in data from multiple files and writes out a single variable.

"""
##################################################################

import ioapiTools as ioT
from mx import DateTime as D
import sys, os

## Setup

## search string -- should uniquely identify
## potential IOAPI files  of interest,
## can use regular expression syntex
searchStr = "CCTM_ACONC.D1.*"
searchStr = os.path.join (sys.prefix,'sample_data/' + searchStr)

## dates of interest: (year, month, day, hour)
date1 = D.DateTime(2001,6,1,6)
date2 = D.DateTime(2001,6,4,18)

## output files
oFile1 = "var_multiday.nc"

## variable of interest
varName = "o3"

##------------------------------------------------------------##


## scan the files from the search string for date range
## returns an iofilescan object
fs = ioT.scan(searchStr, date1, date2)

## print some diagnostics
print fs

## Extract the variable
print "Extracting %s ..." %varName
var = fs(varName)

## write to a file
g = ioT.open(oFile1, "w")
g.write(var)
g.close()

## close iofilescan object
fs.close()



