#!/usr/bin/env python

##################################################################
"""
Reads in data from multiple files and gets a daily average.

Note: Writing data w/ a daily time axis does not work w/ IOAPI
       format. You can only write with an hourly time step. Future
       ioapiTool versions will have this functionality.
       
"""
##################################################################

import ioapiTools as ioT
from mx import DateTime as D
import cdutil
import cdat_info
import sys, os

## Setup

## search string -- should uniquely identify
## potential IOAPI files  of interest,
## can use regular expression syntex
searchStr = "CCTM_ACONC.D1.*"
searchStr = os.path.join (cdat_info.get_prefix(),'sample_data/' + searchStr)

## dates of interest: (year, month, day, hour)
## should be have #hrs as a multiple of 24
date1 = D.DateTime(2001,6,1)
date2 = D.DateTime(2001,6,4,23)

## output files
oFile1 = "var_aver.nc"

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


## To average over time, you need to setup
## the time axis' bounds
cdutil.setTimeBoundsDaily(var.getTime(), frequency=24)

## average over the 0th axis, ie. time
var_average = ioT.binAverager(var, 0)

## change metadata
varDesc = "%s variable -- daily average" %varName
var_average.IOmodVar(desc=varDesc)

## print time axes
print "original data time axis:"
print var.getTime()

print "average data time axis:"
print var_average.getTime()


## write to a netCDF file
print "writing average variable to %s" %oFile1
g = ioT.open(oFile1,"w")
g.write(var_average)
g.close()
