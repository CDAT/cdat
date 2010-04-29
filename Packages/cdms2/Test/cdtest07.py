## Automatically adapted for numpy.oldnumeric Aug 01, 2007 by 

#!/usr/bin/env python

import sys
import getopt
import cdms2
from cdms2.grid import lookupArray
from cdms2.cdmsobj import CdFromObject,CdString,CdScalar,CdFloat,CdDouble,CdShort,CdInt,CdLong
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

    Scan a list of files producing a CDMS dataset in XML
    representation. See Notes below for a more complete explanation.

Arguments:

    <files> is a list of file paths to scan. The files can be listed in any order, and may be in multiple directories.
      A file may also be a CDML dataset (.xml or .cdml), in which case the dataset(s) and files are combined into a new dataset.

Options:

    -c calendar:   either "gregorian", "julian", "noleap", or "360". Default: "gregorian".
    -d dataset_id: dataset identifier. Default: "none"
    -f file_list:  file containing a list of absolute data file names, one per line. <files> arguments are ignored.
    -h:            print a help message.
    -i time_delta: scan time as a 'linear' dimension. This is useful if the time dimension is very
                   long. The argument is the time delta, a float or integer.
                   For example, if the time delta is 6 hours, and
                   the reference units are "hours since xxxx", set the interval delta to 6.
                   The default value is the difference of the first two timepoints.
    -j:		   scan time as a vector dimension. Time values are listed
		   individually. Turns off the -i option.
    -l levels:     list of levels, comma-separated. Only specify if files are partitioned by levels.
    -m levelid:    name of the vertical level dimension. The default is the name of the vertical level dimension
    -p template:   Compatibility with pre-V3.0 datasets. 'cdimport -h' describes template strings.
    -q:            quiet mode
    -r time_units: time units of the form "<units> since yyyy-mm-dd hh:mi:ss",
                   where <units> is one of "year", "month", "day", "hour", "minute", "second".
                   Trailing fields may be omitted. The default is the units of the first time dimension found.
    -t timeid:     id of the partitioned time dimension. The default is the name of the time dimension.
    -x xmlfile:    XML filename. By default, output is written to standard output.

Example:

    cdscan -c noleap -d test -x test.xml [uv]*.nc
    cdscan -d pcmdi_6h -i 0.25 -r 'days since 1979-1-1' *6h*.ctl

Notes:

    The files can be in netCDF, GrADS/GRIB, HDF, or DRS format, and
    can be listed in any order. Most commonly, the files are the
    result of a single experiment, and the 'partitioned' dimension is
    time. The time dimension of a variable is the coordinate variable
    having a name that starts with 'time' or having an attribute
    "axis='T'". If this is not the case, specify the time dimension
    with the -t option. The time dimension should be in the form
    supported by cdtime. If this is not the case (or to override them)
    use the -r option.

    The basic form of the command is 'cdscan <files>'. By default, the
    time values are listed explicitly in the output XML. This can
    cause a problem if the time dimension is very long, say for
    6-hourly data. To handle this the form 'cdscan -i delta <files>'
    may be used. This generates a compact time representation of the
    form <start, length, delta>. An exception is raised if the time
    dimension for a given file is not linear.

    Another form of the command is 'cdscan -l lev1,lev2,..,levn
    <files>'. This asserts that the dataset is partitioned in both
    time and vertical level dimensions. The level dimension of a
    variable is the dimension having a name that starts with "lev", or
    having an attribute "axis=Z". If this is not the case, set the
    level name with the -m option.


"""

calendarMap = {'gregorian' : cdtime.GregorianCalendar,
               'standard'  : cdtime.GregorianCalendar,
               'julian'    : cdtime.JulianCalendar,
               'noleap'    : cdtime.NoLeapCalendar,
               '360'       : cdtime.Calendar360}

reverseCalendarMap = {cdtime.GregorianCalendar : 'gregorian',
                      cdtime.GregorianCalendar : 'standard',
                      cdtime.JulianCalendar : 'julian',
                      cdtime.NoLeapCalendar : 'noleap',
                      cdtime.Calendar360 : '360'}

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
    tval = cdtime.reltime(value, units)
    newval = tval.torel(basetime, calendar)
    if delta is None:
        return newval.value
    else:
        return newval.value/delta

def combineKeys(dict, typedict, timeIsLinear=0, referenceDelta = None):
    """ Combine dictionary keys into an axis.
    dict: (i,j) => (path, axisname)
    typedict is either timedict or levdict.
    timeIsLinear is true iff time has a linear representation.
    referenceDelta is only used for error checks if timeIsLinear is true.
    """

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
        values, units = typedict[(path,name)]
        if firstunits is None:
            firstunits = units
        if prevend is not None and prevend>=i0:
            if prevend>=i1:
                print 'Warning, file %s, dimension %s contains values in file %s'%(prevpath,name,path)
                if timeIsLinear:
                    iind = lookupArray(prevvals, values[0])
                    jind = lookupArray(prevvals, values[-1])
                else:
                    iind = lookupArray(prevvals, i0)
                    jind = lookupArray(prevvals, i1)
                if len(values)!=(jind-iind+1):
                    raise RuntimeError, 'Dimension %s in files %s, %s, is inconsistent'%(name, prevpath, path)
                coordToInd[(i0,i1)] = (iind, jind)
                
                prevspart, prevepart = partition[-1]
                linCoordToInd[(i0,i1)] = (prevspart+iind, prevspart+jind+1)
                continue
            else:                       # Fix partial overlap
                if timeIsLinear:
                    jind = lookupArray(prevvals, values[0])
                else:
                    jind = lookupArray(prevvals, i0)
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
            print "Error: Missing values in %s after times: %s. Set delta with the -i option or turn off linear mode with the -j option."%(path,str(badvalues))
            errorOccurred = 1

        prevvals = values
        previ = prevj
        
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
        if isinstance(attval, numpy.ndarray):
            if len(attval)==1:
                attrs[attname] = attval[0]
            else:
                attrs[attname] = str(attval)
    if attrs.has_key('missing_value') and attrs['missing_value'] is None:
        del attrs['missing_value']

def validateAttrs(node):
    """Compare attributes against DTD."""
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
                        print "Warning: %s=%s should be a float, id=%s"%(attname,attval,node.id),
                        try:
                            attval = string.atoi(attval)
                            attval = float(attval)
                            print "(Recasting)"
                            node.setExternalAttr(attname,attval)
                        except:
                            print ""
                elif reqtype in (CdShort,CdInt,CdLong) and type(attval)!=types.IntType:
                    try:
                        attval = string.atoi(attval)
                    except:
                        print "Warning: %s=%s should be an integer, id=%s"%(attname,attval,node.id),
                        try:
                            attval = string.atof(attval)
                            attval = int(attval)
                            print "(Recasting)"
                            node.setExternalAttr(attname,attval)
                        except:
                            print ""

def cloneWithLatCheck(axis):
    """Clone an axis, ensuring that latitudes (in degrees) are in the range [-90:90]"""

    axisvals = origvals = axis[:]
    if axis.isLatitude() and hasattr(axis,"units") and string.lower(axis.units[0:6])=="degree":
        axisvals = numpy.maximum(-90.0, numpy.minimum(90.0,axisvals))
        if not numpy.ma.allclose(axisvals, origvals):
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
    

#---------------------------------------------------------------------------------------------

def main(argv):
    try:
        args, lastargs = getopt.getopt(argv[1:],"c:d:f:hi:jl:m:p:qr:st:x:")
    except getopt.error:
        print sys.exc_value
        print usage
        sys.exit(0)

    calendar = None
    timeid = None
    levelid = None
    verbose = 1
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
    for flag, arg in args:
        if flag=='-c':
            calenkey = string.lower(arg)
            calendar = calendarMap[calenkey]
        elif flag=='-d':
            datasetid = arg
        elif flag=='-f':
            readFromFile = 1
            filelistpath = arg
        elif flag=='-h':
            print usage
            sys.exit(0)
        elif flag=='-i':
            splitOnTime = 1
            referenceDelta = string.atof(arg)
            timeIsLinear = 1
            timeIsVector = None
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
        elif flag=='-p':
            templatestr = arg
        elif flag=='-q':
            verbose = 0
        elif flag=='-r':
            splitOnTime = 1
            referenceTime = arg
        elif flag=='-s':
            timeIsLinear = 1
            timeIsVector = None
        elif flag=='-t':
            splitOnTime = 1
            timeid = arg
        elif flag=='-x':
            writeToStdout = 0
            xmlpath = arg

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
    global_attrs = None

    #---------------------------------------------------------------------------------------------
    # Initialize dictionaries if adding to an existing dataset
    if verbose and len(dsetargs)>0:
        print 'Scanning datasets ...'
    for extendPath in dsetargs:
        if verbose:
            print extendPath
        extendDset = cdms2.open(extendPath)

        # Copy the global attribute dictionary if necessary. Note that copy.copy
        # can't be used here, since .attributes is now a 'fake' dictionary.
        if global_attrs is None:
            global_attrs = copyDict(extendDset.attributes)

        # Initialize filemap : varid => (tc0, tc1, lc0, lc1, path, timeid, levid)
        # where tc0 is the first time index relative to the reference time, tc1 the last,
        # lc0 is the first level, lc1 the last, path is the filename, timeid is the id
        # of the time dimension of the variable, levid is the id of the level dimension
        # 
        # timedict : (path, timeid) => (timearray, timeunits)
        #
        # levdict : (path, levelid) => (levelarray, levelunits)
        # 
        initfilemap = cdms2.dataset.parseFileMap(extendDset.cdms_filemap)
        dsetdirec = extendDset.directory
        for namelist, slicelist in initfilemap:
            for name in namelist:
                var  = extendDset[name]
                timeaxis = var.getTime()
                if splitOnTime and timeaxis is not None:
                    if hasattr(timeaxis, 'name_in_file'):
                        timeid = timeaxis.name_in_file
                    else:
                        timeid = timeaxis.id
                    if referenceTime is None:
                        referenceTime = timeaxis.units
                    if calendar is None:
                        calendar = timeaxis.getCalendar()
                        calenkey = reverseCalendarMap[calendar]
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
                        if not timedict.has_key((basepath, timeid)):
                            values = timeaxis[t0:t1]
                            timedict[(basepath, timeid)] = (values, timeaxis.units)
                    else:
                        tc0 = tc1 = None
                    if lev0 is not None:
                        lc0 = levelaxis[lev0]
                        lc1 = levelaxis[lev1-1]
                        if not levdict.has_key((basepath, levid)):
                            values = levelaxis[lev0:lev1]
                            levdict[(basepath, levid)] = (values, levelaxis.units)
                    else:
                        lc0 = lc1 = None
                    varmaplist.append((tc0, tc1, lc0, lc1, basepath, timeid, levid))
                if filemap.has_key(name):
                    filemap[name].extend(varmaplist)
                else:
                    filemap[name] = varmaplist

        # axisdict : id => transient_axis
        #   for non-partitioned axes only
        #
        tempmap = {}
        for axis in extendDset.axes.values():
            if not ((splitOnTime and axis.isTime()) or (splitOnLevel and axis.isLevel())):
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

    #---------------------------------------------------------------------------------------------
    if verbose:
        print 'Scanning files ...'

    for path in fileargs:
        path = string.strip(path)
        if verbose:
            print path
        try:
            f = cdms2.open(path)
        except Exception,err:
            raise RuntimeError,'Error opening file '+path

        # Copy the global attribute dictionary if necessary. Note that copy.copy
        # can't be used here, since .attributes is now a 'fake' dictionary.
        if global_attrs is None:
            global_attrs = copyDict(f.attributes)

        if calendar is None:
            calenkey = global_attrs.get('calendar')
            if calenkey is None or not calendarMap.has_key(string.lower(calenkey)):
                calenkey = 'gregorian'
            else:
                calenkey = string.lower(calenkey)
            calendar = calendarMap[calenkey]
            if verbose:
                print 'Setting calendar to',calenkey
                
        basepath = path[dirlen:]
        if template is not None and template.match(basepath) is None:
            print 'Warning: path %s does not match template %s'%(basepath, templatestr)

        varnames = f.variables.keys()
        varnames.sort()
        for varname in varnames:
            var = f.variables[varname]
            varentry = [None]*7         # [timestart, timeend, levstart, levend, path, timeid, levid]
            varentry[4] = basepath

            # Generate a temporary domain entry, and
            # create axis dictionary entries.
            domain = var.getDomain()
            tempdomain = []         # List of axis names and/or objects (if not partitioned)
            for axis, start, length, truelen in domain:
                if (splitOnTime and axis.isTime()) or (splitOnLevel and axis.isLevel()):
                    tempdomain.append(axis.id)
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
                            axisdict[sepname] = axis
                        else:
                            axis = currentaxis
                    else:
                        axisdict[axis.id] = axis
                    tempdomain.append(axis)

            # Create a dictionary entry for the variable if not already there.
            if not vardict.has_key(var.id):
                varattrs = copyDict(var.attributes)
                vardict[var.id] = [tempdomain, varattrs, var.typecode()]
            else:
                currentdomain, attrs, tcode = vardict[var.id]
                if comparedomains(currentdomain, tempdomain):
                    sepname = disambig(var.id, vardict, var.size(), compareVarDictValues, (tempdomain, None))
                    var.name_in_file = var.id
                    var.id = sepname
                    varattrs = copyDict(var.attributes)
                    vardict[sepname] = [tempdomain, varattrs, var.typecode()]

            # Create a filemap entry for this variable/file, if split on time
            axisids = map(lambda x: x[0].id, var.getDomain())
            if splitOnTime:
                vartime = None
                if timeid is not None:
                    if timeid in axisids:
                        vartime = f.axes.get(timeid)
                    else:
                        print 'Warning, time axis %s not found, -t option ignored'%timeid
                if vartime is None:
                    vartime = var.getTime()
                if vartime is not None:
                    if referenceTime is None:
                        referenceTime = vartime.units
                        if verbose:
                            print 'Setting reference time units to', referenceTime
                    if timeIsLinear is None and timeIsVector is None:
                        timeIsLinear = (string.lower(string.split(referenceTime)[0]) in ['hour','hours','minute','minutes','second','seconds'])
                        if timeIsLinear:
                            print 'Setting time representation to "linear"'
                    if timeIsLinear and referenceDelta is None:
                        if len(vartime)>1:
                            time1 = timeindex(vartime[1], vartime.units, referenceTime, None, calendar)
                            time0 = timeindex(vartime[0], vartime.units, referenceTime, None, calendar)
                            referenceDelta = time1 - time0
                        else:
                            referenceDelta = 1
                        if verbose:
                            print 'Setting time delta to', referenceDelta
                    if vartime is not None:
                        starttime = vartime[0]
                        endtime = vartime[-1]
                        startindex = timeindex(vartime[0], vartime.units, referenceTime, referenceDelta, calendar)
                        endindex = timeindex(vartime[-1], vartime.units, referenceTime, referenceDelta, calendar)
                        varentry[0] = startindex
                        varentry[1] = endindex
                        varentry[5] = vartime.id

                        if not timedict.has_key((basepath,vartime.id)):
                            values = vartime[:]
                            timedict[(basepath,vartime.id)] = (values, vartime.units)

            if splitOnLevel:
                varlev = None
                if (levelid is not None) and (levelid in axisids):
                    varlev = f.axes.get(levelid)
                if varlev is None:
                    varlev = var.getLevel()
                if varlev is not None:
                    startlev = varlev[0]
                    if isinstance(startlev, numpy.ndarray):
                        startlev = startlev[0]
                    endlev = varlev[-1]
                    if isinstance(endlev, numpy.ndarray):
                        endlev = endlev[0]
                    varentry[2] = startlev
                    varentry[3] = endlev
                    varentry[6] = varlev.id

                    if not levdict.has_key((basepath, varlev.id)):
                        values = varlev[:]
                        levdict[(basepath,varlev.id)] = (values, varlev.units)

            if filemap.has_key(varname):
                filemap[varname].append(tuple(varentry))
            else:
                filemap[varname] = [tuple(varentry)]
        f.close()

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
        a,b,c,d,path0,timename,levname = slice0
        if (a,b,c,d)==(None,None,None,None):
            del slicelist[1:]

    # Change times to constant units
    for key in timedict.keys():
        values, units = timedict[key]
        time0 = cdtime.reltime(values[0],units)
        offset = time0.torel(referenceTime, calendar).value
        values = values+offset-values[0]
        timedict[key] = (values, referenceTime)
        
    # Create partitioned axes
    axes = []
    masterCoordToInd = {}               # varkey => (timeCoordToInd, levCoordToInd)
    errorOccurred = 0
    for varlist, varentry in varindex:

        # Project time, level indices
        timeproj = {}
        levproj = {}
        for time0, time1, lev0, lev1, path, timename, levname in varentry:
            if timename is not None:
                timeproj[(time0, time1)] = (path, timename)
            if levname is not None:
                try:
                    levproj[(lev0, lev1)] = (path, levname)
                except:
                    print 'Cannot hash level %s range (%f,%f)'%(levname,lev0,lev1)
                    print type(lev0)
                    raise

        # and combine the projected indices into axes
        timeCoordToInd = None
        timelinCoordToInd = None
        if splitOnTime and timename is not None:
            fullaxis, name, partition, timeCoordToInd, units, opartition, timelinCoordToInd, errflag = combineKeys(timeproj, timedict, timeIsLinear, referenceDelta)
            axes.append((varlist,fullaxis,name,partition,timeCoordToInd,units,opartition,timelinCoordToInd))
            if errflag: errorOccurred = 1
        levCoordToInd = None
        if splitOnLevel and levname is not None:
            fullaxis, name, partition, levCoordToInd, units, opartition, levlinCoordToInd, errflag = combineKeys(levproj, levdict)
            axes.append((varlist,fullaxis,name,partition,levCoordToInd,units,opartition,levlinCoordToInd))
            if errflag: errorOccurred = 1

        masterCoordToInd[varlist[0]] = (timeCoordToInd, levCoordToInd, timelinCoordToInd)

    if errorOccurred:
        raise RuntimeError, 'Error(s) determining axis values - see previous message(s)'
    
    # Eliminate duplicate axes
    axes2 = []
    for vlist1, axis1, name1, partition1, coordToInd1, units1, opartition1, linCoordToInd1 in axes:
        for vlist2, axis2, name2, partition2, coordToInd2, units2, opartition2, linCoordToInd2 in axes2:
            if len(axis1)==len(axis2) and name1==name2 and partition1==partition2 and units1==units2 and numpy.ma.allclose(axis1,axis2)==1:
                vlist2.extend(vlist1)
                break
        else:
            axes2.append((copy.copy(vlist1),axis1, name1, partition1, coordToInd1, units1, opartition1, linCoordToInd1))

    # Disambiguate axis names
    for vlist, axis, name, partition, coordToInd, units, opartition, linCoordToInd in axes2:
        # print vlist, coordToInd
        uniqname = disambig(name, axisdict, len(axis), compareaxes, axis)
        axisobj = cdms2.createAxis(axis)
        axisobj.name_in_file = name
        axisobj.id = uniqname
        axisobj.units = units
        if timeIsLinear and axisobj.isTime():
            axisobj.partition = numpy.ma.ravel(numpy.ma.array(opartition))
            axisobj.length = axisobj.partition[-1]-axisobj.partition[0]
            mopartition = numpy.array(opartition)
            partition_length = numpy.sum(mopartition[:,1]-mopartition[:,0])
            if partition_length<axisobj.length:
                axisobj.partition_length = partition_length
        else:
            axisobj.partition = numpy.ma.ravel(numpy.ma.array(partition))
        # axisobj.reference_partition = str(numpy.ma.ravel(numpy.ma.array(opartition)))
        axisdict[uniqname] = axisobj
        for varname in vlist:
            domain, attributes, tcode = vardict[varname]
            for i in range(len(domain)):
                item = domain[i]
                if type(item)==types.StringType and item==name:
                    domain[i] = axisobj

    # Collapse like indices in filemap. For example, transform
    # [x,[[0,10,-,-,file1], [0,10,-,-,file2]]] into
    # [x,[[0,10,-,-,file1]]]
    # This occurs for variables such as time boundaries, which are
    # often duplicated in different files.
    cdms_filemap_list = []
    duplicatevars = {}
    for varindexname, varindexvalue in varindex:
        timeCoordToInd, levCoordToInd, linCoordToInd = masterCoordToInd[varindexname[0]]
        newslicedict = {}
        for time0, time1, lev0, lev1, path, timename, levname in varindexvalue:
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
            if newslicedict.has_key((i0,i1,j0,j1)):
                currentpath = newslicedict[(i0,i1,j0,j1)]
                if not duplicatevars.has_key(tuple(varindexname)):
                    duplicatevars[tuple(varindexname)] = (currentpath, path)
            else:
                newslicedict[(i0,i1,j0,j1)] = path
        keys = newslicedict.keys()
        keys.sort()
        newslicelist = []
        for i0,i1,j0,j1 in keys:
            path = newslicedict[(i0,i1,j0,j1)]
            newslicelist.append([i0, i1, j0, j1, path])
        cdms_filemap_list.append([varindexname, newslicelist])

    # Check if any duplicated variables are a function of longitude or latitude.
    # Raise an exception if so.
    illegalvars = []
    for varlist in duplicatevars.keys():
        for varname in varlist:
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
    cleanupAttrs(global_attrs)
    datasetnode.setExternalDict(global_attrs)
    validateAttrs(datasetnode)

    keys = axisdict.keys()
    keys.sort()
    for key in keys:
        axis = axisdict[key]
        cdtype = cdmsNode.NumericToCdType[axis.typecode()]
        node = cdmsNode.AxisNode(axis.id, len(axis), cdtype)
        if axis.isTime() and timeIsLinear:
            reference_length = axis.partition[-1]-axis.partition[0]
            linearnode = cdmsNode.LinearDataNode(axis[0], referenceDelta, reference_length)
            node.setLinearData(linearnode)
        else:
            try:
                node.setData(axis[:])
            except cdms2.cdmsNode.NotMonotonicError:
                print 'Warning: Axis values for axis %s are not monotonic:'%axis.id,axis[:]
                print 'Warning: Resetting axis %s values to:'%axis.id, numpy.arange(len(axis))
                node.setData(numpy.arange(len(axis)))
        axisattrs = copyDict(axis.attributes)
        cleanupAttrs(axisattrs)
        node.setExternalDict(axisattrs)
        validateAttrs(node)
        datasetnode.addId(axis.id, node)

    keys = vardict.keys()
    keys.sort()
    for key in keys:
        domain, attrs, tcode = vardict[key]
        domainNode = cdmsNode.DomainNode()
        cdtype = cdmsNode.NumericToCdType[tcode]
        node = cdmsNode.VariableNode(key, cdtype, domainNode)
        cleanupAttrs(attrs)
        node.setExternalDict(attrs)
        validateAttrs(node)
        for axis in domain:
            if hasattr(axis,'length'):
                length = axis.length
            else:
                length = len(axis)
            elemnode = cdmsNode.DomElemNode(axis.id, 0, length)
            if hasattr(axis, 'partition_length'):
                elemnode.setExternalAttr('partition_length',axis.partition_length)
            domainNode.add(elemnode)
        datasetnode.addId(key, node)

    # Add the Conventions attribute if not present
    conventions = datasetnode.getExternalAttr('Conventions')
    if conventions is None: datasetnode.setExternalAttr('Conventions','')
    datasetnode.setExternalAttr('calendar',calenkey)
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
    ## datasetnode.setExternalAttr('history',history)

    ## datasetnode.validate()
    if writeToStdout:
        datasetnode.dump()
    else:
        datasetnode.dump(xmlpath)
        if verbose:
            print xmlpath,'written'

#--------------------------------------------------------------------------------------------------------------------------
if __name__ == '__main__':

    from cdms2 import CDMSError
    print 'Test 7: Database import ...',
    from markError import clearError,markError,reportError
    clearError()
    import os,sys

    argv = string.split('cdtest07.py -q -d test -x junk.xml u_2000.nc u_2001.nc u_2002.nc v_2000.nc v_2001.nc v_2002.nc')

    os.chdir(os.path.join(sys.prefix,'sample_data'))
    
    try:
        main(argv)
    except CDMSError,e:
        markError(str(e))
    except Exception,err:
        markError("Importing dataset")


    err = os.system("diff junk.xml test.xml")

    if err!=0: markError("Comparison with benchmark test.xml")
    reportError()
