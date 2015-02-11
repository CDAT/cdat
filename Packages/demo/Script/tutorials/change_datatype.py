#!/usr/bin/env python
# Adapted for numpy/ma/cdms2 by convertcdms.py

##################################################################
"""
Change the data type.

For many modules, they will accept iovar objects as if they were
Numeric arrays.  If this does not work, you can first convert the data
to either a cdms object or a simple Numeric array.

"""
##################################################################

import ioapiTools as ioT
import numpy
import cdat_info
import sys, os

## Setup
## input file
iFile = "CCTM_ACONC.D1.001"
iFile = os.path.join (cdat_info.get_prefix(),'sample_data/' + iFile)

## variable of interest
varName = "o3"

##------------------------------------------------------------##

## open input file for reading
f = ioT.open(iFile)

## read  variable
var = f(varName)
f.close()

## diagnostics
print "original type:"
print type(var)
print ""

## convert to cdms transient variable
varCdms = var.IO2cdms()
print "Converted to cdms:"
print type(varCdms)
print ""

## convert to Numeric array
varArray = var.getValue()
print "Converted to Numeric:"
print type(varArray)
print ""

## At time you might have to get a flat Numeric array
varFlat = numpy.ravel(var)
print "Flattened array:"
print "\tarray length: %d" %(varFlat.shape[0])
