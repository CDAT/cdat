#!/usr/bin/env python

import sys
import getopt
import cdms2
from cdms2.grid import lookupArray
from cdms2.axis import calendarToTag, tagToCalendar
from cdms2.cdmsobj import CdFromObject,CdString,CdScalar,CdFloat,CdDouble,CdShort,CdInt,CdLong,CdInt64
import numpy
import string
import cdtime
import os.path
import pprint
import copy
import types
from cdms2 import cdmsNode
import re

usage = """Usage:
    cdscan [options] <files>

    Scan a list of files producing a CDMS dataset in XML representation. See Notes below
    for a more complete explanation.

Arguments:

    <files> is a list of file paths to scan. The files can be listed in any order, and may
    be in multiple directories.  A file may also be a CDML dataset (.xml or .cdml), in
    which case the dataset(s) and files are combined into a new dataset.

Options:

    -a alias_file: change variable names to the aliases defined in an alias file.
                   Each line of the alias file consists of two blank separated
                   fields: variable_id alias. 'variable_id' is the ID of the variable
                   in the file, and 'alias' is the name that will be substituted for
                   it in the output dataset. Only variables with entries in the alias_file
                   are renamed.

    -c calendar:   either "gregorian", "proleptic_gregorian", "julian", "noleap", or "360_day". Default:
                   "gregorian". This option should be used with caution, as it will
                   override any calendar information in the files.

    -d dataset_id: dataset identifier. Default: "none"

    -e newattr:	   Add or modify attributes of a file, variable, or
		   axis. The form of 'newattr' is either:

		   'var.attr = value' to modify a variable or attribute, or
		   '.attr = value' to modify a global (file) attribute.

		   In either case, 'value' may be quoted to preserve spaces
		   or force the attribute to be treated as a string. If
		   'value' is not quoted and the first character is a
		   digit, it is converted to integer or
		   floating-point. This option does not modify the input
		   datafiles. See notes and examples below.

    --exclude var,var,...
                   Exclude specified variables. The argument
                   is a comma-separated list of variables containing no blanks.
                   In contrast to --exclude-file, this skips the variables regardless
                   of the file(s) in which they are contained, but processes other
                   variables in the files.
                   Also see --include.

    --exclude-file pattern
                   Exclude files with a basename matching the regular expression pattern.
                   In contrast to --exclude, this skips the file entirely. Multiple patterns
                   may be listed by separating with vertical bars (e.g. abc|def ). Note
                   that the match is to the initial part of the basename. For example, the
                   pattern 'st' matches any basename starting with 'st'.

    -f file_list:  file containing a list of absolute data file names, one per
                   line. <files> arguments are ignored.

    --forecast     generate a description of a forecast dataset.
                   This is not compatible with the -i, -r, -t, or -l options.
                   A file can contain data for exactly one forecast; its
                   forecast_reference_time (aka run time, analysis time, starting time,
                   generating time, tau=0 time) is specified by the nbdate,nbsec variables.
                   Each file's time axis will be interpreted as the forecast_period (aka
                   tau, the interval from the forecast_reference_time to the current time)
                   regardless of its units, standard_name, or other attributes.

    -h:            print a help message.

    -i time_delta: scan time as a 'linear' dimension. This is useful if the time dimension
                   is very long. The argument is the time delta, a float or integer.  For
                   example, if the time delta is 6 hours, and the reference units are
                   "hours since xxxx", set the interval delta to 6.  The default value is
                   the difference of the first two timepoints.

    --ignore-open-error:
                   Ignore open errors. Print a warning and continue.

    --include var,var,...
                   Only include specified variables in the output. The argument
                   is a comma-separated list of variables containing no blanks.
                   Also see --exclude.

    --include-file pattern
                   Only include files with a basename matching the regular expression pattern.
                   In contrast to --include, this skips files entirely if they do not
                   match the pattern. Multiple patterns
                   may be listed by separating with vertical bars (e.g. abc|def ). Note
                   that the match is to the initial part of the basename. For example, the
                   pattern 'st' matches any basename starting with 'st'.

    -j:		   scan time as a vector dimension. Time values are listed
		   individually. Turns off the -i option.
<<
    -l levels:     list of levels, comma-separated. Only specify if files are partitioned by
                   levels.

    -m levelid:    name of the vertical level dimension. The default is the name of the
                   vertical level dimension

    --notrim-lat:  Don't trim latitude values (in degrees) to the range [-90..90]. By default
		   latitude values are trimmed.

    -p template:   Compatibility with pre-V3.0 datasets. 'cdimport -h' describes template strings.

    -q:            quiet mode

    -r time_units: time units of the form "<units> since yyyy-mm-dd hh:mi:ss", where
                   <units> is one of "year", "month", "day", "hour", "minute", "second".
                   Trailing fields may be omitted. The default is the units of the first
                   time dimension found.

    -s suffix_file: Append a suffix to variable names, depending on the directory
                   containing the data file.  This can be used to distinguish variables
                   having the same name but generated by different models or ensemble
                   runs. 'suffix_file' is the name of a file describing a mapping between
                   directories and suffixes.  Each line consists of two blank-separated
                   fields: 'directory' 'suffix'. Each file path is compared to the
                   directories in the suffix file. If the file path is in that directory
                   or a subdirectory, the corresponding suffix is appended to the variable
                   IDs in the file. If more than one such directory is found, the first
                   directory found is used. If no match is made, the variable ids are not
                   altered.  Regular expressions can be used: see the example in the Notes
                   section.

    -t timeid:     id of the partitioned time dimension. The default is the name of the time
                   dimension.

    --time-linear tzero,delta,units[,calendar]
                   Override the time dimensions(s) with a linear time dimension. The arguments are
                   a comma-separated list:
                   
                   tzero is the initial time point, a floating-point value.
                   delta is the time delta, floating-point.
                   units are time units as specified in the [-r] option.
                   calendar is optional, and is specified as in the [-c] option. If omitted, it
                     defaults to the value specified by [-c], otherwise as specified in the file.

                   Example: --time-linear '0,1,months since 1980,noleap'

                   Note (6) compares this option with [-i] and [-r]

    --var-locate 'var,file_pattern':
                   Only scan a variable if the basename of the file matches the pattern. This
                   may be used to resolve duplicate variable errors. var and file_pattern are
                   separated by a comma, with no blanks.
                   
                   var is the name of the variable
                   file_pattern is a regular expression following the Python re module syntax.e

                   Example: to scan variable ps from files starting with the string 'ps_':
                     --var-locate 'ps,ps_.*'

    -x xmlfile:    XML filename. By default, output is written to standard output.

Example:

    cdscan -c noleap -d test -x test.xml [uv]*.nc
    cdscan -d pcmdi_6h -i 0.25 -r 'days since 1979-1-1' *6h*.ctl

Notes:

    (1) The files can be in netCDF, GrADS/GRIB, HDF, or DRS format, and can be listed in
    any order. Most commonly, the files are the result of a single experiment, and the
    'partitioned' dimension is time. The time dimension of a variable is the coordinate
    variable having a name that starts with 'time' or having an attribute "axis='T'". If
    this is not the case, specify the time dimension with the -t option. The time
    dimension should be in the form supported by cdtime. If this is not the case (or to
    override them) use the -r option.

    (2) The basic form of the command is 'cdscan <files>'. By default, the time values are
    listed explicitly in the output XML. This can cause a problem if the time dimension is
    very long, say for 6-hourly data. To handle this the form 'cdscan -i delta <files>'
    may be used. This generates a compact time representation of the form <start, length,
    delta>. An exception is raised if the time dimension for a given file is not linear.

    (3) Another form of the command is 'cdscan -l lev1,lev2,..,levn <files>'. This asserts
    that the dataset is partitioned in both time and vertical level dimensions. The level
    dimension of a variable is the dimension having a name that starts with "lev", or
    having an attribute "axis=Z". If this is not the case, set the level name with the -m
    option.

    (4) An example of a suffix file:

    /exp/pr/ncar-a  _ncar-a
    /exp/pr/ecm-a   _ecm-a
    /exp/ta/ncar-a  _ncar-a
    /exp/ta/ecm-a   _ecm-a

    For all files in directory /exp/pr/ncar-a or a subdirectory, the corresponding
    variable ids will be appended with the suffix '_ncar-a'.  Regular expressions can be
    used, as defined in the Python 're' module. For example, The previous example can be
    replaced with the single line:

    /exp/[^/]*/([^/]*) _\g<1>

    Note the use of parentheses to delimit a group. The syntax \g<n> refers to the n-th
    group matched in the regular expression, with the first group being n=1. The string
    [^/]* matches any sequence of characters other than a forward slash.

    (5) Adding or modifying attributes with the -e option:

    time.units = "days since 1979-1-1"

    sets the units of all variables/axes to "Days since 1979-1-1". Note
    that since this is done before any other processing is done, it allows
    overriding of non-COARDS time units.

    .newattr=newvalue

    Set the global file attribute 'newattr' to 'newvalue'.

    (6) The [--time-linear] option overrides the time values in the file(s). The resulting
    dimension does not have any gaps. In contrast, the [-i], [-r] options use the specified
    time units (from [-r]), and calendar from [-c] if specified, to convert the file times
    to the new units. The resulting linear dimension may have gaps.

    In either case, the files are ordered by the time values in the files.

    The [--time-linear] option should be used with caution, as it is applied to all the time
    dimensions found.
"""

# Ensure that arrays are fully printed to XML files
numpy.set_printoptions(threshold=numpy.inf)

calendarMap = tagToCalendar

reverseCalendarMap = calendarToTag

attrPattern = re.compile(r'\s*(\w*)\.(\w+)\s*=\s*(.*)$')
def timestamp():
    "Generate a timestamp."
    import time
    y,m,d,h,mi,s,w,dy,ds = time.gmtime(time.time())
    return "%d-%d-%d %d:%d:%d"%(y,m,d,h,mi,s)

def timeindex(value, units, basetime, delta, calendar):
    """ Calculate (t - basetime)/delu
    where t = reltime(value, units)
    and delu is the time interval (delta, delunits) (e.g., 1 month).
    """
    if string.find(units," as ")==-1:
        tval = cdtime.reltime(value, units)
    else:
        tval = cdtime.abstime(value, units)
    newval = tval.torel(basetime, calendar)
    if delta is None:
        return newval.value
    else:
        return newval.value/delta

def combineKeys(dict, typedict, timeIsLinear=0, referenceDelta = None, forecast=None):
    """ Combine dictionary keys into an axis.
    dict: (i,j) => (path, axisname)
    typedict is either timedict or levdict or fcdict.
    timeIsLinear is true iff time has a linear representation.
    referenceDelta is only used for error checks if timeIsLinear is true.
    """

    global verbose

    # Sort the projected time, level indices
    keys = dict.keys()
    keys.sort()

    axislist = []
    prevend = None
    prevpath = None
    name0 = None
    compressPart = []
    partition = []
    previ = 0
    firstunits = None
    prevvals = None
    coordToInd = {(None,None):(None,None)}
    linCoordToInd = {(None,None):(None,None)}
    iadj = None
    errorOccurred = 0
    for i0, i1 in keys:
        path, name = dict[(i0, i1)]
        if name0 is None:
            name0 = name
        values, units, dummy = typedict[(path,name)]
        if firstunits is None:
            firstunits = units
        if prevend is not None and prevend>=i0:
            if prevend>=i1:
                if verbose:
                    print 'Warning, file %s, dimension %s contains values in file %s'%(prevpath,name,path)
                if timeIsLinear:
                    iind = lookupArray(prevvals, values[0])
                    jind = lookupArray(prevvals, values[-1])
                else:
                    iind = lookupArray(prevvals, i0)
                    jind = lookupArray(prevvals, i1)
                if len(values)!=(jind-iind+1):
                    raise RuntimeError, 'Dimension %s in files %s [len(%s)=%d], %s [len(%s)=%d], is inconsistent'%(name, prevpath, name, (jind-iind+1), path, name, len(values))
                coordToInd[(i0,i1)] = (iind, jind)
                
                prevspart, prevepart = partition[-1]
                linCoordToInd[(i0,i1)] = (prevspart+iind, prevspart+jind+1)
                continue
            else:                       # Fix partial overlap
                if timeIsLinear:
                    jind = lookupArray(prevvals, values[0])
                else:
                    jind = lookupArray(prevvals, i0)
                if verbose:
                    print 'Warning, file %s, dimension %s overlaps file %s, value=%f'%(prevpath,name,path,prevvals[jind])
                previ, prevj = compressPart[-1]
                prevj = previ + jind
                axislist[-1] = prevvals[0:jind]
                compressPart[-1] = (previ, prevj)
                coordToInd[(prevvals[0], prevvals[-1])] = (previ, prevj)
                previ = prevj

                prevspart, prevepart = partition[-1]
                prevepart = prevspart + jind
                partition[-1] = (prevspart, prevepart)
                linCoordToInd[(prevvals[0], prevvals[-1])] = (prevspart, prevepart)

        axislist.append(values)
        prevend = i1
        prevpath = path
        prevj = previ+len(values)
        compressPart.append((previ, prevj))
        coordToInd[(i0,i1)] = (previ, prevj)

        if iadj is None:                # partition has to start with 0
            iadj = int(i0)
        spart = int(i0) - iadj
        epart = int(i1) + 1 - iadj
        partition.append((spart, epart))
        linCoordToInd[(i0,i1)] = (spart, epart)
        if timeIsLinear and len(values)!=(epart-spart):
            # Find the bad values
            diffs = values[1:]-values[:-1]
            badindices = numpy.compress(numpy.not_equal(diffs,referenceDelta),range(len(values)))
            badvalues = numpy.take(values, badindices)
            if verbose:
                print "Error: Missing values in %s after times: %s. Set delta with the -i option or turn off linear mode with the -j option."%(path,str(badvalues))
            errorOccurred = 1

        prevvals = values
        previ = prevj
        
    fullaxis = numpy.ma.concatenate(axislist)
    return fullaxis, name0, compressPart, coordToInd, firstunits, partition, linCoordToInd, errorOccurred

def useKeys(dict, typedict, timeIsLinear=0, referenceDelta = None, forecast=None):
    """ Use dictionary keys for an axis.  This is like combineKeys (same arguments, same return values,
    was written by simplifying combineKeys), but this doesn't do nearly so much because this is
    for an axis where there is no splitting across files, hence partitions are not needed.
    dict: (i,j) => (path, axisname)
    typedict is either timedict or levdict or fcdict.
    timeIsLinear is true iff time has a linear representation.
    referenceDelta is only used for error checks if timeIsLinear is true.
    """
    global verbose

    # Sort the projected time, level indices
    keys = dict.keys()
    keys.sort()

    axislist = []
    name0 = None
#    compressPart = []
    compressPart = None
#    partition = []
    partition = None
    previ = 0
    firstunits = None
#    coordToInd = {(None,None):(None,None)}
#    linCoordToInd = {(None,None):(None,None)}
    coordToInd = None
    linCoordToInd = None
    errorOccurred = 0
    for i0, i1 in keys:
        path, name = dict[(i0, i1)]
        if name0 is None:
            name0 = name
        values, units, dummy = typedict[(path,name)]
        if firstunits is None:
            firstunits = units
        axislist.append(values)
        prevj = previ+len(values)
#        coordToInd[(i0,i1)] = (previ, prevj)

    fullaxis = numpy.ma.concatenate(axislist)
    return fullaxis, name0, compressPart, coordToInd, firstunits, partition, linCoordToInd, errorOccurred

def copyDict(dict):
    """Copy a dictionary-like object dict to a true dictionary"""
    result = {}
    for key in dict.keys():
        result[key] = dict[key]

    return result

def disambig(name, dict, num, comparator, value):
    """ Make an unique name from name, wrt to the keys in dictionary dict.
    Try using num first. comparator(value,dict[name]) returns 0 if equal, 1 if not.
    """
    if not dict.has_key(name) or not comparator(value, dict[name]):
        uniqname = name
    else:
        uniqname = '%s_%d'%(name,num)
        if dict.has_key(uniqname) and comparator(value, dict[uniqname]):
            trial_name = uniqname
            for letter in string.lowercase:
                uniqname = '%s_%s'%(trial_name,letter)
                if not dict.has_key(uniqname) or not comparator(value, dict[uniqname]):
                    break
            else:
                raise 'Cannot make axis name unique: ',name

    return uniqname

def compareaxes(axis1, axis2):
    """Return 0 if equal, 1 if not"""
    return ((len(axis1)!=len(axis2)) or not numpy.ma.allclose(axis1[:],axis2[:]))

def comparedomains(domain1, domain2):
    """Return 0 if equal, 1 if not"""
    if len(domain1)!=len(domain2):
        return 1
    for i in range(len(domain1)):
        item1 = domain1[i]
        item2 = domain2[i]
        if type(item1)!=type(item2):
            return 1
        if type(item1)==types.StringType:
            return item1!=item2
        elif compareaxes(item1, item2):
            return 1
    return 0

def compareVarDictValues(val1, val2):
    return comparedomains(val1[0], val2[0])

def cleanupAttrs(attrs):
    for attname in attrs.keys():
        attval = attrs[attname]
        if type(attval) is numpy.ndarray:
            if len(attval)==1:
                attrs[attname] = attval[0]
            else:
                attrs[attname] = str(attval)
    if attrs.has_key('missing_value') and attrs['missing_value'] is None:
        del attrs['missing_value']

def validateAttrs(node):
    """Compare attributes against DTD."""

    global verbose

    if hasattr(node,'datatype'):
        parenttype = node.datatype
    else:
        parenttype = None
    atts = node.getExternalDict()
    for attname in atts.keys():
        (attval,datatype)=atts[attname] # (XML value, datatype)
        constraint = node.extra.get(attname)
        if constraint is not None:
            (scaletype,reqtype)=constraint # (CdScalar|CdArray, required type)
            if reqtype==CdFromObject:
                reqtype = parenttype
            if reqtype!=datatype and datatype==CdString and scaletype==CdScalar:
                if reqtype in (CdFloat,CdDouble) and type(attval)!=types.FloatType:
                    try:
                        attval = string.atof(attval)
                    except:
                        if verbose:
                            print "Warning: %s=%s should be a float, id=%s"%(attname,attval,node.id),
                        try:
                            attval = string.atoi(attval)
                            attval = float(attval)
                            if verbose:
                                print "(Recasting)"
                            node.setExternalAttr(attname,attval)
                        except:
                            if attname in ['modulo', 'add_offset', 'scale_factor']:
                                if verbose:
                                    print "(Removing)"
                                attdict = node.getExternalDict()
                                del attdict[attname]
                            else:
                                if verbose:
                                    print ""
                elif reqtype in (CdShort,CdInt,CdLong) and type(attval)!=types.IntType:
                    try:
                        attval = string.atoi(attval)
                    except:
                        if verbose:
                            print "Warning: %s=%s should be an integer, id=%s"%(attname,attval,node.id),
                        try:
                            attval = string.atof(attval)
                            attval = int(attval)
                            if verbose:
                                print "(Recasting)"
                            node.setExternalAttr(attname,attval)
                        except:
                            if verbose:
                                print ""

def cloneWithLatCheck(axis):
    """Clone an axis, ensuring that latitudes (in degrees) are in the range [-90:90]"""

    global verbose
    global notrimlat

    axisvals = origvals = axis[:]
    if axis.isLatitude() and hasattr(axis,"units") and string.lower(axis.units[0:6])=="degree":
        if notrimlat==0:
            axisvals = numpy.maximum(-90.0, numpy.minimum(90.0,axisvals))
        if not numpy.ma.allclose(axisvals, origvals) and verbose:
            print "Warning: resetting latitude values: ",origvals," to: ",axisvals

    b = axis.getBounds()
    mycopy = cdms2.createAxis(copy.copy(axisvals))
    mycopy.id = axis.id
    try:
        mycopy.setBounds(b)
    except CDMSError:
        b = mycopy.genGenericBounds()
        mycopy.setBounds(b)
    for k, v in axis.attributes.items():
       setattr(mycopy, k, v)
    return mycopy

def addAttrs(fobj, eattrs):
    """Add extra attributes to file/dataset fobj.
    eattrs has the form [(varid,attr,value), (varid,attr,value), ...]
    where if varid is '', set the global attribute."""
    for evar,eattr,evalue in eattrs:
        if evar=='':
            fobj.__dict__[eattr] = evalue
        else:
            varobj = fobj[evar]
            if varobj is not None:
                varobj.__dict__[eattr] = evalue

def setNodeDict(node, dict):
    for key in dict.keys():
        value = dict[key]
        if (isinstance(value, numpy.integer) or isinstance(value, types.IntType)):
            datatype = CdLong
        elif (isinstance(value, numpy.floating) or isinstance(value, types.FloatType)):
            datatype = CdDouble
        else:
            datatype = CdString
        node.attribute[key]=(value,datatype)

def initialize_filemap( filemap, timedict, levdict, timeid, extendDset, splitOnTime, \
                        referenceTime, timeIsLinear, referenceDelta, splitOnLevel, \
                        dirlen, overrideCalendar ):
    # This function was formerly part of the body of "main".
        # Initialize filemap : varid => (tc0, tc1, lc0, lc1, path, timeid, levid)
        # where tc0 is the first time index relative to the reference time, tc1 the last,
        # lc0 is the first level, lc1 the last, path is the filename, timeid is the id
        # of the time dimension of the variable, levid is the id of the level dimension
        # 
        # timedict : (path, timeid) => (timearray, timeunits, calendar)
        #
        # levdict : (path, levelid) => (levelarray, levelunits, None)
        #
    initfilemap = cdms2.dataset.parseFileMap(extendDset.cdms_filemap)
    dsetdirec = extendDset.directory
    for namelist, slicelist in initfilemap:
        for name in namelist:
            var  = extendDset[name]
            timeaxis = var.getTime()
            if timeaxis is not None and not overrideCalendar:
                calendar = timeaxis.getCalendar()
            if splitOnTime and timeaxis is not None:
                if hasattr(timeaxis, 'name_in_file'):
                    timeid = timeaxis.name_in_file
                else:
                    timeid = timeaxis.id
                if referenceTime is None:
                    referenceTime = timeaxis.units
                if timeIsLinear in [None,1]:
                    timeIsLinear = timeaxis.isLinear()
                    if timeIsLinear:
                        if len(timeaxis)>1:
                            referenceDelta = timeaxis[1]-timeaxis[0]
                        else:
                            referenceDelta = 1.0
                    else:
                        referenceDelta = None
            else:
                timeid = None
            levelaxis = var.getLevel()
            if splitOnLevel and levelaxis is not None:
                if hasattr(levelaxis, 'name_in_file'):
                    levid = levelaxis.name_in_file
                else:
                    levid = levelaxis.id
            else:
                levid = None

            varmaplist = []
            for t0, t1, lev0, lev1, path in slicelist:
                fullpath = os.path.join(dsetdirec,path)
                basepath = fullpath[dirlen:]
                if t0 is not None:
                    tc0 = timeindex(timeaxis[t0], timeaxis.units, referenceTime, referenceDelta, calendar)
                    tc1 = timeindex(timeaxis[t1-1], timeaxis.units, referenceTime, referenceDelta, calendar)
                    if not timedict.has_key((basepath, timeid, calendar)):
                        values = timeaxis[t0:t1]
                        timedict[(basepath, timeid)] = (values, timeaxis.units, calendar)
                else:
                    tc0 = tc1 = None
                if lev0 is not None:
                    lc0 = levelaxis[lev0]
                    lc1 = levelaxis[lev1-1]
                    if not levdict.has_key((basepath, levid, None)):
                        values = levelaxis[lev0:lev1]
                        levdict[(basepath, levid)] = (values, levelaxis.units, None)
                else:
                    lc0 = lc1 = None
                varmaplist.append((tc0, tc1, lc0, lc1, basepath, timeid, levid, calendar))
            if filemap.has_key(name):
                filemap[name].extend(varmaplist)
            else:
                filemap[name] = varmaplist

#---------------------------------------------------------------------------------------------

verbose = 1

def main(argv):

    global verbose
    global notrimlat

    try:
        args, lastargs = getopt.getopt( \
            argv[1:], "a:c:d:e:f:hi:jl:m:p:qr:s:t:x:", \
            ["include=","include-file=","exclude=","exclude-file=","forecast","time-linear=", \
             "notrim-lat","var-locate=","ignore-open-error" ] )
    except getopt.error:
        print sys.exc_value
        print usage
        sys.exit(0)

    calendar = None
    calenkey = None
    timeid = None
    levelid = None
    notrimlat = 0
    referenceTime = None
    referenceDelta = None
    readFromFile = 0
    splitOnTime = 1
    splitOnLevel = 0
    datasetid = "none"
    timeIsLinear = None
    writeToStdout = 1
    templatestr = None
    timeIsVector = None
    modelMapFile = None
    aliasMapFile = None
    overrideCalendar = 0
    extraAttrs = []
    extraDict = {}
    includeList = None
    excludeList = None
    overrideTimeLinear = None
    varLocate = None
    ignoreOpenError = False
    excludePattern = None
    includePattern = None
    forecast = False
    for flag, arg in args:
        if flag=='-a':
            aliasMapFile = arg
        elif flag=='-c':
            calenkey = string.lower(arg)
            calendar = calendarMap[calenkey]
            overrideCalendar = 1
        elif flag=='-d':
            datasetid = arg
        elif flag=='-e':
            matchObj = attrPattern.match(arg)
            if matchObj is None:
                raise RuntimeError, "Expression must have form '[var].attr=value': %s"%arg
            matchGroups = matchObj.groups()
            if len(matchGroups)!=3:
                raise RuntimeError, "Expression must have form '[var].attr=value': %s"%arg
            matchValue = matchGroups[2]
            if len(matchValue)>0 and (matchValue[0].isdigit() or matchValue[0] in ['"',"'","-","+"]): #"
                matcheval = eval(matchValue)
            else:
                matcheval = str(matchValue)
            extraAttrs.append((matchGroups[0], matchGroups[1], matcheval))
        elif flag=='--exclude':
            if arg[0]=='-':
                raise RuntimeError, "--exclude option requires an argument"
            excludeList = string.split(arg,',')
        elif flag=='--exclude-file':
            excludePattern = arg
        elif flag=='-f':
            readFromFile = 1
            filelistpath = arg
        elif flag=='--forecast':  # experimental forecast mode
            forecast = True
            splitOnTime = 0
            splitOnLevel = 0
        elif flag=='-h':
            print usage
            sys.exit(0)
        elif flag=='-i':
            splitOnTime = 1
            referenceDelta = string.atof(arg)
            timeIsLinear = 1
            timeIsVector = None
        elif flag=='--ignore-open-error':
            ignoreOpenError = True
        elif flag=='--include':
            if arg[0]=='-':
                raise RuntimeError, "--include option requires an argument"
            includeList = string.split(arg,',')
        elif flag=='--include-file':
            includePattern = arg
        elif flag=='-j':
            timeIsVector = 1
            timeIsLinear = None
        elif flag=='-l':
            splitOnLevel = 1
            levelstr = string.split(arg,',')
            levellist = map(string.atof, levelstr)
            levels = numpy.array(levellist)
            levels = numpy.sort(levels)
        elif flag=='-m':
            levelid = arg
            args.append(('-e','%s.axis=Z'%levelid)) # Add axis=Z attribute
        elif flag=='--notrim-lat':
            notrimlat = 1
        elif flag=='-p':
            templatestr = arg
        elif flag=='-q':
            verbose = 0
        elif flag=='-r':
            splitOnTime = 1
            referenceTime = arg
        elif flag=='-s':
            modelMapFile = arg
        elif flag=='-t':
            splitOnTime = 1
            timeid = arg
            args.append(('-e','%s.axis=T'%timeid)) # Add axis=T attribute
        elif flag=='--time-linear':
            targlist = string.split(arg,',')
            ttzero = string.atof(targlist[0])
            tdelta = string.atof(targlist[1])
            tunits = string.strip(targlist[2])
            if len(targlist)==4:
                tcalendar = string.strip(targlist[3])
            else:
                tcalendar = None
            overrideTimeLinear = [ttzero,tdelta,tunits,tcalendar]
        elif flag=='--var-locate':
            if varLocate is None:
                varLocate = {}
            vname, pattern = string.split(arg,',')
            varLocate[vname]=pattern
        elif flag=='-x':
            writeToStdout = 0
            xmlpath = arg

    # If overriding time, process time as vector so that no gaps result
    if overrideTimeLinear is not None:
        timeIsVector = 1
        timeIsLinear = None
        if overrideCalendar==1:
            overrideTimeLinear[3]=calenkey

    if verbose:
        print 'Finding common directory ...'
    if readFromFile:
        f = open(filelistpath)
        lastargs = f.readlines()
        f.close()

    # Ignore blank paths
    realargs = []
    for arg in lastargs:
        sarg = string.strip(arg)
        if len(sarg)>0:
            realargs.append(sarg)
    lastargs = realargs

    # Split lastargs into files and datasets
    fileargs = []
    dsetargs = []
    for arg in lastargs:
        base, suffix = os.path.splitext(arg)
        if string.lower(suffix) in ['.xml','.cdml']:
            dsetargs.append(arg)
        else:
            fileargs.append(arg)

    # Generate a list of pathnames for datasets
    dsetfiles = []
    for path in dsetargs:
        dset = cdms2.open(path)
        if not hasattr(dset, 'cdms_filemap'):
            raise RuntimeError,'Dataset must have a cdms_filemap attribute: '+path
        if not hasattr(dset, 'directory'):
            raise RuntimeError,'Dataset must have a directory attribute: '+path
        dsetdirec = dset.directory
        initfilemap = cdms2.dataset.parseFileMap(dset.cdms_filemap)
        for namelist, slicelist in initfilemap:
            for t0, t1, lev0, lev1, path in slicelist:
                dsetfiles.append(os.path.join(dsetdirec, path))
    augmentedArgs = fileargs + dsetfiles

    # Find the common directory
    directory = os.path.commonprefix(augmentedArgs)
    firstpath = augmentedArgs[0][len(directory):]
    if not os.path.isfile(os.path.join(directory,firstpath)):
        dnew = os.path.dirname(directory)
        if len(dnew)>0 and directory[len(dnew)]=='/':
            directory = dnew+'/'
        else:
            directory = dnew
    if verbose:
        print 'Common directory:',directory

    dirlen = len(directory)

    if templatestr is not None:
        if os.path.isabs(templatestr):
            templatestr = templatestr[dirlen:]
        templatere, ignore = cdms2.cdmsobj.templateToRegex(templatestr)
        template = re.compile(templatere+'$')
    else:
        template = None

    axisdict = {}
    vardict = {}
    filemap = {}
    timedict = {}
    levdict = {}
    fcdict = {}
    global_attrs = None
    fctau0 = None

    if modelMapFile is not None:
        mfile = open(modelMapFile)
        modelMap = {}
        modelDirs = []
        for line in mfile.readlines():
            mdirec, model = string.split(line)
            modelMap[mdirec] = model
            modelDirs.append(mdirec)
        mfile.close()

    if aliasMapFile is not None:
        afile = open(aliasMapFile)
        aliasMap = {}
        for line in afile.readlines():
            if line[0] not in ["'",'"']: #"
                varid, alias = string.split(line)
            else:
                dummy, varid, alias = string.split(line,line[0])
                alias = string.strip(alias)
            aliasMap[varid] = alias
        afile.close()

    # Save extra attribute information for new axes
    for evar, eattr, evalue in extraAttrs:
        if evar=='':
            continue
        if extraDict.has_key(evar):
            curval = extraDict[evar]
            curval.append((eattr,evalue))
        else:
            extraDict[evar] = [(eattr,evalue)]

    #---------------------------------------------------------------------------------------------
    # Initialize dictionaries if adding to an existing dataset
    if verbose and len(dsetargs)>0:
        print 'Scanning datasets ...'
    for extendPath in dsetargs:
        if verbose:
            print extendPath
        extendDset = cdms2.open(extendPath)

        # Add/modify attributes
        addAttrs(extendDset, extraAttrs)

        # Copy the global attribute dictionary if necessary. Note that copy.copy
        # can't be used here, since .attributes is now a 'fake' dictionary.
        if global_attrs is None:
            global_attrs = copyDict(extendDset.attributes)

        # Initialize filemap : varid => (tc0, tc1, lc0, lc1, path, timeid, levid)
        # where tc0 is the first time index relative to the reference time, tc1 the last,
        # lc0 is the first level, lc1 the last, path is the filename, timeid is the id
        # of the time dimension of the variable, levid is the id of the level dimension
        # 
        # timedict : (path, timeid) => (timearray, timeunits, calendar)
        #
        # levdict : (path, levelid) => (levelarray, levelunits, None)
        #
        initialize_filemap( filemap, timedict, levdict, timeid, extendDset, splitOnTime, \
                            referenceTime, timeIsLinear, referenceDelta, splitOnLevel, \
                            dirlen, overrideCalendar )

        # axisdict : id => transient_axis
        #   for non-partitioned axes only
        #
        tempmap = {}
        for axis in extendDset.axes.values():
            if not ( (splitOnTime and (axis.isTime() or axis.id==timeid)) or \
                     (splitOnLevel and (axis.isLevel() or axis.id==levelid)) ):
                axis = cloneWithLatCheck(axis)
                if axisdict.has_key(axis.id):
                    currentaxis = axisdict[axis.id]

                    # Check that the axis has the same length and values as the saved value. If not,
                    # create an unambiguous name in the axis dictionary.
                    if compareaxes(axis, currentaxis):
                        sepname = disambig(axis.id, axisdict, len(axis), compareaxes, axis)
                        axis.name_in_file = axis.id
                        oldid = axis.id
                        axis.id = sepname
                        axisdict[sepname] = axis
                        tempmap[oldid] = sepname
                else:
                    axisdict[axis.id] = axis

        # vardict : varid => [domain, attributeDict, typecode]
        #   where domain = [axis_or_id, axis_or_id,...]
        #   and axis_or_id is the id of a partitioned dimension, or
        #   the transient axis object associated with a non-partitioned dimension
        #
        for var in extendDset.variables.values():
            tempdomain = []
            for id in var.getAxisIds():
                if tempmap.has_key(id):
                    id = tempmap[id]
                if axisdict.has_key(id):
                    tempdomain.append(axisdict[id])
                else:
                    axis = extendDset[id]
                    if hasattr(axis,'name_in_file'):
                        id = axis.name_in_file
                    tempdomain.append(id)
            varattrs = copyDict(var.attributes)
            vardict[var.id] = [tempdomain, varattrs, var.typecode()]

        extendDset.close()

        # end of loop "for extendPath in dsetargs"

    #---------------------------------------------------------------------------------------------
    if verbose:
        print 'Scanning files ...'

    boundsmap = {}                      # boundsmap : varid => timebounds_id
    boundsdict = {}                     # Same as vardict for time bounds
    for path in fileargs:
        path = string.strip(path)

        # Check if the path is included
        if includePattern is not None:
            base = os.path.basename(path)
            mobj = re.match(includePattern, base)
            if mobj is None:
                continue

        # Check if the path is excluded
        if excludePattern is not None:
            base = os.path.basename(path)
            mobj = re.match(excludePattern, base)
            if mobj is not None:
                continue

        if verbose:
            print path
        try:
            f = cdms2.open(path)
        except:
            if not ignoreOpenError:
                raise RuntimeError,'Error opening file '+path
            else:
                print 'Warning: cannot open file, skipping: %s'%path
                continue

        # Add/modify attributes
        addAttrs(f, extraAttrs)

        # Determine the variable ID suffix, if any
        varsuffix = None
        if modelMapFile is not None:
            for direc in modelDirs:
                mo = re.match(direc, path)
                if mo is not None:
                    suffixPattern = modelMap[direc]
                    def gensuffix(m, mo=mo):
                        i = string.atoi(m.group(1))
                        return mo.group(i)
                    varsuffix = re.sub(r'\\g<(\d)>', gensuffix, suffixPattern)
                    break

        # Copy the global attribute dictionary if necessary. Note that copy.copy
        # can't be used here, since .attributes is now a 'fake' dictionary.
        if global_attrs is None:
            global_attrs = copyDict(f.attributes)

        basepath = path[dirlen:]
        if template is not None and template.match(basepath) is None:
            if verbose:
                print 'Warning: path %s does not match template %s'%(basepath, templatestr)

        # Find time boundary variables
        boundsids = []
        if splitOnTime:
            tmpdict = {}
            for axisname in f.axes.keys():
                axis = f[axisname]
                if axis.isTime() and hasattr(axis, 'bounds'):
                    tmpdict[axis.bounds] = 1
            boundsids = tmpdict.keys()

        # For forecasts, get the time at which the forecast begins (tau=0) which
        # is nbdate,nbsec
        if forecast:
            nbdate = numpy.int( f('nbdate') )  # f('nbdate') is numpy.int32 which gets truncated
            nbsec = f('nbsec')
            fctau0 = nbdate*100000 + nbsec  # hopefully nbsec<(seconds per day)=86400<100000
            fctau0time = cdtime.abstime( nbdate,"day as %Y%m%d" )
            fctau0time = fctau0time.add( nbsec, cdtime.Seconds )  # fctau0 as type comptime
            fc_time_attrs = []

        varnames = f.variables.keys()
        varnames.sort()
        for varname in varnames:

            # If --var-locate is specified for the variable, match the basename before processing
            if varLocate is not None and varLocate.has_key(varname):
                varpattern = varLocate[varname]
                base = os.path.basename(path)
                mobj = re.match(varpattern, base)
                if mobj is None:
                    continue

            var = f.variables[varname]

            # Reset the variable ID to any specified alias
            if aliasMapFile is not None:
                varalias = aliasMap.get(var.id)
                if varalias is not None:
                    var.name_in_file = var.id
                    var.id = varalias
                    varname = varalias

            # Append a suffix to the variable ID, if applicable
            if varsuffix is not None:
                if not hasattr(var, 'name_in_file'):
                    var.name_in_file = var.id
                var.id += varsuffix
                varname += varsuffix

            varentry = [None]*9         # [timestart, timeend, levstart, levend, path, timeid, levid, calendar, fctau0]
            varentry[4] = basepath
            varentry[8] = fctau0

            # Generate a temporary domain entry, and
            # create axis dictionary entries.
            domain = var.getDomain()
            if forecast:
                tempdomain = ['fctau0']
            else:
                tempdomain = []         # List of axis names and/or objects (if not partitioned)
            for axis, start, length, truelen in domain:
                if (splitOnTime and (axis.isTime() or axis.id==timeid)) or \
                   (splitOnLevel and (axis.isLevel() or axis.id==levelid)):
                    tempdomain.append(axis.id)
                elif forecast and  (axis.isTime() or axis.id==timeid):
                    # time axis isn't split but needs special treatment for forecasts
                    tempdomain.append(axis.id)
                    fc_time_attrs.append(axis.attributes)
                else:
                    axis = cloneWithLatCheck(axis) # Transient copy
                    if axisdict.has_key(axis.id):
                        currentaxis = axisdict[axis.id]

                        # Check that the axis has the same length and values as the saved value. If not,
                        # create an unambiguous name in the axis dictionary.
                        if compareaxes(axis, currentaxis):
                            sepname = disambig(axis.id, axisdict, len(axis), compareaxes, axis)
                            axis.name_in_file = axis.id
                            axis.id = sepname

                            # Fix boundary variable names if using suffixes.
                            if varsuffix is not None and hasattr(axis, 'bounds'):
                                axis.bounds += varsuffix
                            axisdict[sepname] = axis
                        else:
                            axis = currentaxis
                    else:
                        # Fix boundary variable names if using suffixes.
                        if varsuffix is not None and hasattr(axis, 'bounds'):
                            axis.bounds += varsuffix
                        axisdict[axis.id] = axis
                    tempdomain.append(axis)

            # Create a dictionary entry for the variable if not already there.
            if var.id in boundsids:
                boundsattrs = copyDict(var.attributes)
                boundsdict[var.id] = [tempdomain, boundsattrs, var.typecode()]
                continue                # Don't set a filemap entry until axes are sorted out
            elif not vardict.has_key(var.id):
                varattrs = copyDict(var.attributes)
                if varsuffix is not None or aliasMapFile is not None:
                    varattrs['name_in_file'] = var.name_in_file
                vardict[var.id] = [tempdomain, varattrs, var.typecode()]
            else:
                currentdomain, attrs, tcode = vardict[var.id]
                if comparedomains(currentdomain, tempdomain):
                    sepname = disambig(var.id, vardict, var.size(), compareVarDictValues, (tempdomain, None))
                    saveid = var.id
                    varname  = var.id = sepname
                    varattrs = copyDict(var.attributes)
                    var.name_in_file = varattrs['name_in_file']  = saveid
                    vardict[sepname] = [tempdomain, varattrs, var.typecode()]

            # Create a filemap entry for this variable/file, if split on time or forecast
            axisids = map(lambda x: x[0].id, var.getDomain())
            if splitOnTime or forecast:
                vartime = None
                if timeid is not None:
                    if timeid in axisids:
                        vartime = f.axes.get(timeid)
                    else:
                        if verbose:
                            print 'Warning, time axis %s not found, -t option ignored'%timeid
                if vartime is None:
                    vartime = var.getTime()
                if vartime is not None:
                    if not overrideCalendar:
                        calendar = vartime.getCalendar()
                    if referenceTime is None:
                        referenceTime = vartime.units
                    if verbose and not forecast:
                        print 'Setting reference time units to', referenceTime
                    if timeIsLinear is None and timeIsVector is None:
                        timeIsLinear = (string.lower(string.split(referenceTime)[0]) in ['hour','hours','minute','minutes','second','seconds'])
                        if timeIsLinear and verbose:
                            print 'Setting time representation to "linear"' #'
                    if timeIsLinear and referenceDelta is None:
                        if len(vartime)>1:
                            time1 = timeindex(vartime[1], vartime.units, referenceTime, None, calendar)
                            time0 = timeindex(vartime[0], vartime.units, referenceTime, None, calendar)
                            referenceDelta = time1 - time0
                        else:
                            referenceDelta = 1
                        if verbose:
                            print 'Setting time delta to', referenceDelta

                    starttime = vartime[0]
                    endtime = vartime[-1]
                    startindex = timeindex(vartime[0], vartime.units, referenceTime, referenceDelta, calendar)
                    endindex = timeindex(vartime[-1], vartime.units, referenceTime, referenceDelta, calendar)
                    if forecast:
                        # split on forecast, hence no split on time 
                        varentry[0] = None
                        varentry[1] = None
                        referenceTime = None
                    else:
                        varentry[0] = startindex
                        varentry[1] = endindex
                    varentry[5] = vartime.id
                    varentry[7] = calendar

                    if not timedict.has_key((basepath,vartime.id)):
                        values = vartime[:]
                        timedict[(basepath,vartime.id)] = (values, vartime.units, calendar)

            if splitOnLevel:
                varlev = None
                if (levelid is not None) and (levelid in axisids):
                    varlev = f.axes.get(levelid)
                if varlev is None:
                    varlev = var.getLevel()
                if varlev is not None:
                    startlev = varlev[0]
                    if type(startlev) is numpy.ndarray:
                        startlev = startlev[0]
                    endlev = varlev[-1]
                    if type(endlev) is numpy.ndarray:
                        endlev = endlev[0]
                    varentry[2] = startlev
                    varentry[3] = endlev
                    varentry[6] = varlev.id

                    if not levdict.has_key((basepath, varlev.id, None)):
                        values = varlev[:]
                        levdict[(basepath,varlev.id)] = (values, varlev.units, None)

            if forecast:
                if not fcdict.has_key((basepath, 'fctau0')):
                    fcdict[(basepath, 'fctau0')] = ( [fctau0], None, None )

            if filemap.has_key(varname):
                filemap[varname].append(tuple(varentry))
            else:
                filemap[varname] = [tuple(varentry)]

            # Set boundsmap : varid => timebounds_id
            if splitOnTime and vartime is not None and hasattr(vartime, "bounds") and not boundsmap.has_key(varname):
                boundsmap[varname] = vartime.bounds

            # End of loop "for varname in varnames"

        f.close()
        # End of loop "for path in fileargs"

    #---------------------------------------------------------------------------------------------

    # Generate varindex, by combining variable names with
    # identical varentry values.
    varindex = []
    varnames = filemap.keys()
    varnames.sort()
    for varname in varnames:
        varentry = filemap[varname]
        varentry.sort()

        for varindexname, varindexvalue in varindex:
            if varentry == varindexvalue:
                varindexname.append(varname)
                break
        else:
            varindex.append(([varname],varentry))

    # If a variable is not a function of one of the partitioned dimensions,
    # no indexing is necessary: just read from the first file containing it.
    for varlist, slicelist in varindex:
        slice0 = slicelist[0]
        a,b,c,d,path0,timename,levname,calen,fctau0 = slice0
        if (a,b,c,d,fctau0)==(None,None,None,None,None):
            del slicelist[1:]

    # Change times to constant units
    sameCalendars = 1                   # True iff all time calendars are the same
    prevcal = None
    if forecast:
        # The data files' time axis is interpreted to be tau time, i.e. the forecast_period.
        # Find the axis, and remember it in timedict.
        for key in timedict.keys():
            values, units, calendar = timedict[key]
            if prevcal is not None and calendar != prevcal:
                sameCalendars = 0
            prevcal = calendar
            if string.find(units," as ")==-1:
                time0 = cdtime.reltime(values[0],units)
            else:
                time0 = cdtime.abstime(values[0],units)
            offset = time0.torel( units, calendar ).value  # normally will be 0
            values = values+offset-values[0]
            # Switch units from "normal" time such as "days since 2001-06-01"
            # to "basic" time such as "days", which makes sense for a forecast_period.
            baslen = time0.units.find(' since ')
            basic_units = time0.units[0:baslen]  # e.g. 'days'
            fc_units = basic_units
            timedict[key] = (values, fc_units, calendar)
    else:       # splitOnTime is true
        for key in timedict.keys():
            values, units, calendar = timedict[key]
            if prevcal is not None and calendar != prevcal:
                sameCalendars = 0
            prevcal = calendar
            if string.find(units," as ")==-1:
                time0 = cdtime.reltime(values[0],units)
            else:
                time0 = cdtime.abstime(values[0],units)
            offset = time0.torel(referenceTime, calendar).value
            values = values+offset-values[0]
            timedict[key] = (values, referenceTime, calendar)

    if sameCalendars and prevcal is not None:
        calenkey = reverseCalendarMap[calendar]
        
    if forecast:
        # For forecasts, make sure that the above has made all timedict values the same.
        # >>> It's conceivable that different forecasts will have different time (really, tau)
        # >>> axes.  If so, at this point we'll want to merge and mask all the time values, so
        # >>> that all variables can have the same time axis..  For now, just raise an error
        # >>> if there are time axis differences at this point.
        values0,units0,calendar0 = timedict[ timedict.keys()[0] ]
        timedict_same = all( [ ((values0==values).all() and units0==units and calendar0==calendar) \
                               for (values,units,calendar) in timedict.values() ] )
        if not timedict_same:
            raise CDMSError, 'cdscan is confused about times for a forecast set'
        # Earlier we had saved all the time axis attributes.  Keep whatever they have in common.
        fc_time_attr = fc_time_attrs[0]
        for fcta in fc_time_attrs:             # go through all time attributes (each a dictionary)
            for attrn in fc_time_attr.keys():
                if not fcta.has_key(attrn):
                    del fc_time_attr[attrn]    # key attrn isn't in all time attributes
                elif fcta[attrn]!=fc_time_attr[attrn]:
                    del fc_time_attr[attrn]    # not all time attributes have the same value for attrn
        # At this point fc_time_attr is the dictionary of those time attributes which are common to
        # all time axes encountered (in the context of a forecast dataset).
        # Finally, add the appropriate standard_name to it, if we haven't already gotten one from
        # the data file.  If the file has anything other than 'forecast_period', it's wrong, but
        # we'll stick with it anyway.
        if not 'standard_name' in fc_time_attr.keys():
            fc_time_attr['standard_name'] = 'forecast_period'
        
    # Create partitioned axes
    axes = []
    masterCoordToInd = {}               # varkey => (timeCoordToInd, levCoordToInd)
    errorOccurred = 0
    for varlist, varentry in varindex:

        # Project time, level indices
        timeproj = {}
        levproj = {}
        fctproj = {}
        for time0, time1, lev0, lev1, path, timename, levname, calendar, fctau0 in varentry:
            if timename is not None:
                timeproj[(time0, time1)] = (path, timename)
            if levname is not None:
                try:
                    levproj[(lev0, lev1)] = (path, levname)
                except:
                    print 'Cannot hash level %s range (%f,%f)'%(levname,lev0,lev1)
                    print type(lev0)
                    raise
            if fctau0 is not None:
                fctproj[(fctau0,fctau0)] = (path, 'fctau0')

        # and combine the projected indices into axes
        timeCoordToInd = None
        timelinCoordToInd = None
        if splitOnTime and timename is not None:
            fullaxis, name, partition, timeCoordToInd, units, opartition, timelinCoordToInd, errflag = \
                      combineKeys(timeproj, timedict, timeIsLinear, referenceDelta)
            axes.append( ( varlist,fullaxis,name,partition,timeCoordToInd,units,opartition, \
                           timelinCoordToInd, calendar ) )
            if errflag: errorOccurred = 1
        levCoordToInd = None
        if splitOnLevel and levname is not None:
            fullaxis, name, partition, levCoordToInd, units, opartition, levlinCoordToInd, errflag = \
                      combineKeys(levproj, levdict)
            axes.append((varlist,fullaxis,name,partition,levCoordToInd,units,opartition,levlinCoordToInd, None))
            if errflag: errorOccurred = 1
        fcCoordToInd = None
        if forecast:
            fullaxis, name, partition, fcCoordToInd, units, opartition, fclinCoordToInd, errflag = \
                      combineKeys(fctproj, fcdict, forecast=forecast)
            axes.append((varlist,fullaxis,name,partition,fcCoordToInd,units,opartition,fclinCoordToInd, None))
            if errflag: errorOccurred = 1
            if len(timeproj)>0:     # i.e., if time is in this variable's domain.
                # The useKeys call is like combineKeys, except that it's for a variable not partitioned
                # among files.  It just sets up axis data and (in the context of this variable loop)
                # propagates what's in timedict to every variable with time in its domain.
                fullaxis, name, partition, timeCoordToInd, units, opartition, timelinCoordToInd, errflag = \
                          useKeys(timeproj, timedict, timeIsLinear, referenceDelta)
                axes.append( (varlist,fullaxis,name,partition,timeCoordToInd,units,opartition, \
                              timelinCoordToInd, calendar) )
                if errflag: errorOccurred = 1
            

        masterCoordToInd[varlist[0]] = (timeCoordToInd, levCoordToInd, timelinCoordToInd, fcCoordToInd)

    if errorOccurred:
        raise RuntimeError, 'Error(s) determining axis values - see previous message(s)'
    
    # Eliminate duplicate axes
    axes2 = []
    for vlist1, axis1, name1, partition1, coordToInd1, units1, opartition1, linCoordToInd1, calen1 in axes:
        for vlist2, axis2, name2, partition2, coordToInd2, units2, opartition2, linCoordToInd2, calen2 in axes2:
            if len(axis1)==len(axis2) and name1==name2 and partition1==partition2 and units1==units2 and \
                   numpy.ma.allclose(axis1,axis2)==1 and calen1==calen2:
                vlist2.extend(vlist1)
                break
        else:
            axes2.append((copy.copy(vlist1),axis1, name1, partition1, coordToInd1, units1, opartition1, \
                          linCoordToInd1, calen1))

    # For each axis described by axis2, disambiguate its name, create the axis object, etc.
    assignedBounds = {}
    for vlist, axis, name, partition, coordToInd, units, opartition, linCoordToInd, calendar in axes2:
        # print vlist, coordToInd
        uniqname = disambig(name, axisdict, len(axis), compareaxes, axis)
        axisobj = cdms2.createAxis(axis)
        axisobj.name_in_file = name
        axisobj.id = uniqname
        axisobj.units = units
        if forecast and axisobj.isTime():   # For forecasts, give the time axis some saved attributes.
            for attr in fc_time_attr.keys():
                if not hasattr(axisobj,attr):
                    setattr(axisobj,attr,fc_time_attr[attr])
        if timeIsLinear and axisobj.isTime():
            axisobj.partition = numpy.ma.ravel(numpy.ma.array(opartition))
            axisobj.length = axisobj.partition[-1]-axisobj.partition[0]
            mopartition = numpy.array(opartition)
            partition_length = numpy.sum(mopartition[:,1]-mopartition[:,0])
            if partition_length<axisobj.length:
                axisobj.partition_length = partition_length
        elif partition is not None:
            axisobj.partition = numpy.ma.ravel(numpy.ma.array(partition))
        if axisobj.isTime():
            axisobj.calendar = reverseCalendarMap[calendar]
        # axisobj.reference_partition = str(numpy.ma.ravel(numpy.ma.array(opartition)))
        if not axisdict.has_key(uniqname):
            axisdict[uniqname] = axisobj
        for varname in vlist:
            domain, attributes, tcode = vardict[varname]
            for i in range(len(domain)):
                item = domain[i]
                if type(item)==types.StringType and item==name:
                    domain[i] = axisobj

        # Add bounds variables to vardict, varindex
        if axisobj.isTime():
            reprVar = vlist[0]              # 'Representative' variable having this time axis
            if boundsmap.has_key(reprVar):
                boundsname = boundsmap[reprVar]
                boundsinfo = boundsdict[boundsname]
                boundsattrs = boundsinfo[1]
                if uniqname!=name:
                    boundsattrs['name_in_file'] = boundsname
                    boundsname = uniqname+'_bnds'
                if not assignedBounds.has_key(boundsname):
                    axisobj.bounds = boundsname
                    for varids, ranges in varindex:
                        if reprVar in varids:
                            varids.append(boundsname)
                    tmpdom = boundsinfo[0]
                    if type(tmpdom[1])==types.StringType:
                        bndsobj = tmpdom[0]
                        boundsdomain = (bndsobj, axisobj)
                    else:
                        bndsobj = tmpdom[1]
                        boundsdomain = (axisobj, bndsobj)
                    vardict[boundsname] = (boundsdomain, boundsinfo[1], boundsinfo[2])
                    assignedBounds[boundsname] = 1

    # Collapse like indices in filemap. For example, transform
    # [x,[[0,10,-,-,file1], [0,10,-,-,file2]]] into
    # [x,[[0,10,-,-,file1]]]
    # This occurs for variables such as time boundaries, which are
    # often duplicated in different files.
    cdms_filemap_list = []
    duplicatevars = {}
    for varindexname, varindexvalue in varindex:
        timeCoordToInd, levCoordToInd, linCoordToInd, fcCoordToInd = masterCoordToInd[varindexname[0]]
        newslicedict = {}
        for time0, time1, lev0, lev1, path, timename, levname, calendar, fctau0 in varindexvalue:
            if timeCoordToInd is not None:
                if timeIsLinear:
                    i0, i1 = linCoordToInd[(time0, time1)]
                else:
                    i0, i1 = timeCoordToInd[(time0, time1)]
            else:
                i0 = i1 = None
            if levCoordToInd is not None:
                j0, j1 = levCoordToInd[(lev0, lev1)]
            else:
                j0 = j1 = None
            if newslicedict.has_key((i0,i1,j0,j1,fctau0)):
                currentpath = newslicedict[(i0,i1,j0,j1,fctau0)]
                if not duplicatevars.has_key(tuple(varindexname)):
                    duplicatevars[tuple(varindexname)] = (currentpath, path)
            else:
                newslicedict[(i0,i1,j0,j1,fctau0)] = path
        keys = newslicedict.keys()
        keys.sort()
        newslicelist = []
        for i0,i1,j0,j1,fctau0 in keys:
            path = newslicedict[(i0,i1,j0,j1,fctau0)]
            newslicelist.append([i0, i1, j0, j1, fctau0, path])
        cdms_filemap_list.append([varindexname, newslicelist])

    # Check if any duplicated variables are a function of longitude or latitude.
    # Raise an exception if so.
    illegalvars = []
    for varlist in duplicatevars.keys():
        for varname in varlist:
            if (excludeList is not None) and (varname in excludeList):
                continue
            dom, attrs, tcode = vardict[varname]
            for axisobj in dom:
                if axisobj.isLatitude() or axisobj.isLongitude():
                    path1, path2 = duplicatevars[varlist]
                    illegalvars.append((varname, path1, path2))
    if len(illegalvars)>0:
        raise RuntimeError, "Variable '%s' is duplicated, and is a function of lat or lon: files %s, %s"%illegalvars[0]
        
    if verbose and len(duplicatevars.values())>0:
        print 'Duplicate variables:'
        for varlist in duplicatevars.keys():
            path1, path2 = duplicatevars[varlist]
            print '\t',varlist,'\t',path1,'\t',path2

    # Generate the cdms_filemap attribute
    cdms_filemap = str(cdms_filemap_list)
    cdms_filemap = string.replace(cdms_filemap, ' ', '')
    cdms_filemap = string.replace(cdms_filemap, 'None', '-')
    cdms_filemap = string.replace(cdms_filemap, '"', '') #"
    cdms_filemap = string.replace(cdms_filemap, "'", '')

    # Dump to XML
    datasetnode = cdmsNode.DatasetNode(datasetid)
    global_attrs['cdms_filemap'] = cdms_filemap
    global_attrs['directory'] = directory
    if sameCalendars and calenkey is not None:
        global_attrs['calendar'] = calenkey
    elif global_attrs.has_key('calendar'):
        del global_attrs['calendar']
    cleanupAttrs(global_attrs)
    # datasetnode.setExternalDict(global_attrs)
    setNodeDict(datasetnode, global_attrs)
    validateAttrs(datasetnode)

    timeWasOverridden = 0
    keys = axisdict.keys()
    keys.sort()
    for key in keys:
        axis = axisdict[key]
        tcode = axis.typecode()
        if tcode in [numpy.float32, numpy.float, numpy.int16, numpy.int32, numpy.int, numpy.intc, numpy.int8]:
            tcode = numpy.sctype2char(tcode)
        cdtype = cdmsNode.NumericToCdType[tcode]
        node = cdmsNode.AxisNode(axis.id, len(axis), cdtype)

        # Override the time axis as a linear axis
        if axis.isTime() and (overrideTimeLinear is not None):
            ttzero = overrideTimeLinear[0]
            ttdelta = overrideTimeLinear[1]
            axis.units = overrideTimeLinear[2]
            if overrideTimeLinear[3] is None:
                axis.calendar = reverseCalendarMap[axis.getCalendar()]
            else:
                axis.calendar = overrideTimeLinear[3]
            linearnode = cdmsNode.LinearDataNode(ttzero, ttdelta, len(axis))
            node.setLinearData(linearnode)
            if verbose:
                if timeWasOverridden==0:
                    print "Overriding values for axis '%s'"%axis.id
                else:
                    print 'Warning, overriding more than one time axis (%s)'%axis.id
            timeWasOverridden = 1

        # Represent time as linear axis using time values in the file
        elif axis.isTime() and timeIsLinear:
            reference_length = axis.partition[-1]-axis.partition[0]
            linearnode = cdmsNode.LinearDataNode(axis[0], referenceDelta, reference_length)
            node.setLinearData(linearnode)
        else:
            try:
                node.setData(axis[:])
            except cdms2.cdmsNode.NotMonotonicError:
                if verbose:
                    print 'Warning: Axis values for axis %s are not monotonic:'%axis.id,axis[:]
                    print 'Warning: Resetting axis %s values to:'%axis.id, numpy.arange(len(axis))
                node.setData(numpy.arange(len(axis)))
        axisattrs = copyDict(axis.attributes)

        # Make sure that new axes have attribute mods
        if extraDict.has_key(key):
            for eattr, evalue in extraDict[key]:
                axisattrs[eattr] = evalue
        cleanupAttrs(axisattrs)
        # node.setExternalDict(axisattrs)
        setNodeDict(node, axisattrs)
        validateAttrs(node)
        datasetnode.addId(axis.id, node)

    keys = vardict.keys()
    keys.sort()
    for key in keys:
        if (includeList is not None) and (key not in includeList):
            continue
        if (excludeList is not None) and (key in excludeList):
            continue
        domain, attrs, tcode = vardict[key]
        if tcode in [numpy.float32, numpy.float, numpy.int16, numpy.int32, numpy.int, numpy.intc, numpy.int8]:
            tcode = numpy.sctype2char(tcode)
        domainNode = cdmsNode.DomainNode()
        cdtype = cdmsNode.NumericToCdType[tcode]
        node = cdmsNode.VariableNode(key, cdtype, domainNode)
        cleanupAttrs(attrs)
        # node.setExternalDict(attrs)
        setNodeDict(node, attrs)
        validateAttrs(node)
        for axis in domain:
            if hasattr(axis,'length'):
                length = axis.length
            else:
                length = len(axis)
            try:
                elemnode = cdmsNode.DomElemNode(axis.id, 0, length)
            except AttributeError:
                print 'Axis %s for variable %s does not have attribute "id"'%(`axis`, key)
            if hasattr(axis, 'partition_length'):
                elemnode.setExternalAttr('partition_length',axis.partition_length)
            domainNode.add(elemnode)
        datasetnode.addId(key, node)

    # Add the Conventions attribute if not present
    conventions = datasetnode.getExternalAttr('Conventions')
    if conventions is None: datasetnode.setExternalAttr('Conventions','')
    if templatestr is not None:
        datasetnode.setExternalAttr('template',templatestr)

    # Add/modify history
    history = datasetnode.getExternalAttr('history')
    if history is None:
        history = ""
    stringargv = reduce(lambda x,y: x+' '+y, argv)
    stringtime = "\n[%s] "%timestamp()
    if len(stringargv)<=256:
        history += stringtime+stringargv
    else:
        history += stringtime+stringargv[:256]+" ..."
    datasetnode.setExternalAttr('history',history)

    ## datasetnode.validate()
    if writeToStdout:
        datasetnode.dump()
    else:
        datasetnode.dump(xmlpath)
        if verbose:
            print xmlpath,'written'

#--------------------------------------------------------------------------------------------------------------------------
if __name__ == '__main__':
    main(sys.argv)
