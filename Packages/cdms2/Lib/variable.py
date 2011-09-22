## Automatically adapted for numpy.oldnumeric Aug 01, 2007 by 

"""
DatasetVariable: Dataset-based variables
"""
from cdms2 import Cdunif
import numpy
import cdmsNode
import cdtime
import copy
import os
import string
import sys
import types
import cdmsobj
from cdmsobj import CdmsObj, getPathFromTemplate, Max32int
from avariable import AbstractVariable
from sliceut import *
from error import CDMSError

InvalidGridElement = "Grid domain elements are not yet implemented: "
InvalidRegion = "Invalid region: "
NoSuchAxisOrGrid = "No such axis or grid: "
OutOfRange = "Coordinate interval is out of range: "
TooManyPartitions = "Variable has too many partitioned axes, max is two: "
WriteNotImplemented = "Dataset write operation not implemented"
FileClosed = "Cannot read from closed file or dataset, variable: "

def timeindex(value, units, basetime, delta, delunits, calendar):
    """ Calculate (t - basetime)/delu
    where t = reltime(value, units)
    and delu is the time interval (delta, delunits) (e.g., 1 month).
    """
    tval = cdtime.reltime(value, units)
    tounits = "%s since %s"%(delunits, basetime)
    newval = tval.torel(tounits, calendar)
    return int(newval.value/delta)

class DatasetVariable(AbstractVariable):

    def __init__(self,parent,id, variableNode=None):
        """ "Variable (parent, variableNode=None)"
           variableNode is the variable tree node, if any.
           parent is the containing dataset instance.
        """
        AbstractVariable.__init__ (self, parent, variableNode)
        val = self.__cdms_internals__ + ['domain','name_in_file']
        self.___cdms_internals__ = val
        self.id = id
        self.domain = []
        # Get self.name_in_file from the .xml file if present
        if not hasattr(self, 'name_in_file'):
            self.name_in_file = id
            
        # if self.attributes.has_key('name_in_file'):
        #     self.name_in_file = self.attributes['name_in_file']
        if variableNode is not None:          
            self._numericType_ = cdmsNode.CdToNumericType.get(variableNode.datatype)
        else:
            self._numericType_ = numpy.float
        assert self.id is not None
        
    def __len__ (self):
        "Length of first dimension"
        if len(self.domain)>0:
            (axis,start,length,true_length) = self.domain[0]
        else:
            length = 0

        return length

#    def __repr__(self):
#        if self.parent is not None:
#            parentid = self.parent.id
#        else:
#            parentid = "**CLOSED**"
#        return "<Variable: %s, dataset: %s, shape: %s>"%(self.id, parentid, `self.shape`)

    def __getitem__(self, key):
        if self.parent is None:
            raise CDMSError, FileClosed+str(self.id)
        return AbstractVariable.__getitem__(self, key)
        
    def getValue(self, squeeze=1):
        """Return the entire set of values."""
        if self.parent is None:
            raise CDMSError, FileClosed+self.id
        return self.getSlice(Ellipsis, squeeze=squeeze)
    
    def __getslice__(self, low, high):
        if self.parent is None:
            raise CDMSError, FileClosed+self.id

        # Hack to prevent netCDF overflow error on 64-bit architectures
        high = min(Max32int, high)
        
        return AbstractVariable.__getslice__(self, low, high)

    def __setitem__(self, index, value):
        raise CDMSError, WriteNotImplemented

    def __setslice__(self, low, high, value):
        raise CDMSError, WriteNotImplemented

    def _getShape(self):
        return self.getShape()

    def _getdtype(self):
        tc = self.typecode()
        return numpy.dtype(tc)

    def getShape(self):
        shape=[]
        for (axis,start,length,true_length) in self.domain:
            shape.append(length)
        return tuple(shape)

    def typecode (self):
	return self._numericType_

    def size(self):
        "Number of elements."
        n = 1
        for k in self.shape:
            n = k*n
        return n

    def initDomain(self, axisdict, griddict):
        "Must be called by whoever made this Variable to set up axes, grids."
        self.domain = []
        domnode = self._node_.getDomain()
        for denode in domnode.children():
            dename = denode.getName()
            domelem = axisdict.get(dename)
            if domelem is None:
                domelem = griddict.get(dename)
                if grid is None:
                    raise CDMSError, NoSuchAxisOrGrid + dename
                else:
                    raise CDMSError, InvalidGridElement + dename
            partlenstr = denode.getExternalAttr('partition_length')
            if partlenstr is not None:
                truelen = string.atoi(partlenstr)
            else:
                truelen = denode.length
            self.domain.append((domelem, denode.start, denode.length, truelen))

    # Get the template
    def getTemplate(self):
        if hasattr(self,'template'):
            template = self.template
        elif hasattr(self.parent,'template'): 
            template = self.parent.template
        else:
            template = None
        return template

    def getAxis (self, n):
        if n < 0: n = n + self.rank()
        return self.domain[n][0]

    def getDomain (self):
        return self.domain

    # Get the paths associated with the interval region specified
    # by 'intervals'. This incorporates most of the logic of __getitem__,
    # without actually reading the data.
    # 
    # 'specs' is a list of interval range specifications as defined 
    # for getSlice.
    #
    # The function returns a list of tuples of the form (path,slicelist),
    # where path is the path of a file, and slicetuple is a tuple of
    # slices, of the same length as the rank of the variable, representing the
    # region of the variable which is contained in the file. The following
    # would retrieve the data for that file:
    #
    #   f = Cdunif.CdunifFile(path,'r')
    #   var = f.variables[self.name_in_file]
    #   data = apply(var.getitem,slicelist)
    #   
    def getPaths(self, *specs, **keys):

        # Create an equivalent list of slices
        speclist = self._process_specs (specs, keys)
        slicelist = self.specs2slices(speclist)

        # Generate the filelist
        npart, idims, partitionSlices = self.expertPaths(slicelist)

        # Flatten the list
        result = []
        if partitionSlices is None:
            pass
        elif npart==0:
            filename, slicelist = partitionSlices
            if filename is not None:
                result.append((filename, tuple(slicelist)))
        elif npart==1:
            for filename, slicelist in partitionSlices:
                if filename is not None:
                    result.append((filename, tuple(slicelist)))
        elif npart==2:
            for filelist in partitionSlices:
                for filename, slicelist in filelist:
                    if filename is not None:
                        result.append((filename, tuple(slicelist)))
                
        return result

    def genMatch(self, axis, interval, matchnames):
        """Helper function for expertPaths.
        axis is a partitioned axis, either time or vertical level or forecast.
        interval is an index interval (istart, iend).
        matchnames is a partially filled list [id, timestart, timeend, levstart, levend, fc]
          If a filemap is used, matchnames has indices, otherwise has coordinates.

        Function modifies matchnames based on axis and interval,
        returns the modified matchnames tuple.
        """
        if axis.isTime():
            if hasattr(self.parent,'cdms_filemap'):
                start = interval[0]
                end = interval[1]
            else:                       # Use template method
                time0 = axis[interval[0]]
                time1 = axis[interval[1]-1]
                isabs = (string.find(axis.units," as ")!=-1)
                if isabs:
                    start = cdtime.abstime(time0,axis.units)
                    end = cdtime.abstime(time1,axis.units)
                else:
                    cal = axis.getCalendar()
                    start = cdtime.reltime(time0,axis.units).tocomp(cal)
                    end = cdtime.reltime(time1,axis.units).tocomp(cal)
            matchnames[1] = start
            matchnames[2] = end
        elif axis.isForecast():
            start = axis.getValue()[interval[0]]
            end   = axis.getValue()[interval[1]-1]
            matchnames[5] = start
            matchnames[6] = end
        else:
            if hasattr(self.parent,'cdms_filemap'):
                start = interval[0]
                end = interval[1]
            else:
                start = int(axis[interval[0]])
                end = int(axis[interval[1]-1])
            matchnames[3] = start
            matchnames[4] = end

        return matchnames

    def getFilePath(self, matchnames, template):
        """Lookup or generate the file path, depending on whether a filemap
        or template is present.
        """
        if hasattr(self.parent,'cdms_filemap'):
            id, tstart, tend, levstart, levend, fcstart, fcend = matchnames
            filename = self.parent._filemap_[(self.id, tstart, levstart, fcstart)]
            # ... filemap uses dataset IDs
        else:
            filename = getPathFromTemplate(template,matchnames)
        return filename

    def getPartition(self, axis):
        """Get the partition attribute for this variable, axis.
        axis is either a time or level axis. If cdms_filemap is being used,
        get the partition from the _varpart_ attribute, otherwise (for templating) use
        axis.partition.
        """
        if hasattr(self.parent,'cdms_filemap'):
            if axis.isTime():
                partition = self._varpart_[0]
            elif axis.isForecast():
                partition = axis.partition
            else:                  # level
                partition = self._varpart_[1]
        else:                           # Template method
            partition = axis.partition
        return partition

    def expertPaths (self, slist):
        """ expertPaths(self, slicelist)
        takes a list of slices,
        returns a 3-tuple: (npart, dimensionlist, partitionSlices) where:
        npart is the number of partitioned dimensions: 0, 1, or 2;
        dimensionlist is a tuple of length npart, having the dimension
          numbers of the partitioned dimensions;
        partitionSlices is the list of file-specific (filename, slice)
          corresponding to the paths and slices within the files to be read.
          The exact form of partitionSlices depends on the value of npart:
          npart     partitionSlices
          0         (filename,slicelist)
          1         [(filename,slicelist),...,(filename,slicelist)]
          2         [[(filename,slicelist),...,(filename,slicelist)]
                     [(filename,slicelist),...,(filename,slicelist)]
                     ...
                     [(filename,slicelist),...,(filename,slicelist)]]

        Note:
          - A filename of None indicates that no file was found with data
        corresponding to the slicelist.
          - If partitionSlices is None, the slicelist does not intersect the domain.
          - An empty partitionSlices [] means that the variable is zero-dimensional.
        """

        # slicelist gets modified, slist doesn't
        slicelist = copy.copy(slist)
        template = self.getTemplate()

        # Use the name_in_file attribute to access files
        if hasattr(self, 'name_in_file'):
            realid = self.name_in_file
        else:
            realid = self.id

        # Handle rank-0 variables separately
        if self.rank() == 0:
            matchnames = [realid,None,None,None,None,None,None]
            filename = self.getFilePath(matchnames, template)

            result = (0, (), (filename, []))
            return result

        # Find the number of partitioned axes
        npart = 0
        ndim = 0
        for (axis,start,length,true_length) in self.domain:
            if hasattr(axis,'partition'):
                npart = npart+1
                if npart==1:
                    part1 = axis
                    npart1 = ndim
                elif npart==2:
                    part2 = axis
                    npart2 = ndim
                else:
                    raise CDMSError,  TooManyPartitions + variable.id
            ndim = ndim+1

        # If no partitioned axes, just read the data
        if npart==0:
            matchnames = [realid,None,None,None,None,None,None]
            filename = self.getFilePath(matchnames, template)
            result = (0, (), (filename, slicelist))

        # If one partitioned axes:
        elif npart==1:

            # intersect the slice and partition for that axis
            slice1 = slicelist[npart1]
            (axis,startelem,length,true_length) = self.domain[npart1]
            partition = slicePartition(slice1, self.getPartition(axis))
            if partition==[]:
                return (1, (npart1,), None)

            # For each (interval, partslice) in the partition:
            resultlist = []
            (firstinterval, firstslice) = partition[0]
            prevhigh = firstinterval[0]
            for (interval,partslice) in partition:

                # If the previous interval high is less than
                # the current interval low value, interpose
                # missing data.
                low = interval[0]
                if prevhigh<low:
                    missing_interval = (prevhigh,low)
                    missing_slice = sliceIntersect(slice1, missing_interval)

                    # Note: if the slice has a stride>1, it might not intersect,
                    # so don't interpose missing data in this case.
                    if missing_slice is not None:
                        slicelist[npart1] = missing_slice
                        resultlist.append((None,copy.copy(slicelist)))
                prevhigh = interval[1]

                # generate the filename
                matchnames = [realid, None, None, None, None,None,None]
                matchnames = self.genMatch(axis, interval, matchnames)
                filename = self.getFilePath(matchnames, template)

                # adjust the partslice for the interval offset
                # and replace in the slice list
                filestart = partslice.start-interval[0]
                filestop = partslice.stop-interval[0]
                fileslice = slice(filestart,filestop,partslice.step)
                slicelist[npart1] = fileslice

                resultlist.append((filename,copy.copy(slicelist)))

            result = (1,(npart1,),resultlist)

        # If two partitioned axes, 2-D version of previous case
        if npart==2:
            slice1 = slicelist[npart1]
            slice2 = slicelist[npart2]
            (axis1,startelem1,length1,true_length1) = self.domain[npart1]
            (axis2,startelem2,length2,true_length2) = self.domain[npart2]
            partition1 = slicePartition(slice1, self.getPartition(axis1))
            partition2 = slicePartition(slice2, self.getPartition(axis2))
            if partition1==[] or partition2==[]:
                return (2, (npart1,npart2), None)

            # For each (interval, partslice) in the partition:
            resultlist = []
            (firstinterval1, firstslice1) = partition1[0]
            prevhigh1 = firstinterval1[0]
            for (interval1,partslice1) in partition1:

                # If the previous interval high is less than
                # the current interval low value, interpose
                # missing data.
                low = interval1[0]
                if prevhigh1<low:
                    missing_interval = (prevhigh1,low)
                    missing_slice = sliceIntersect(slice1, missing_interval)
                    if missing_slice is not None:
                        slicelist[npart1] = missing_slice
                        resultlist.append( [(None,copy.copy(slicelist))] )
                prevhigh1 = interval1[1]

                # generate matchnames
                matchnames = [realid, None, None, None, None,None,None]
                matchnames = self.genMatch(axis1, interval1, matchnames)

                # adjust the partslice for the interval offset
                # and replace in the slice list
                filestart = partslice1.start-interval1[0]
                filestop = partslice1.stop-interval1[0]
                fileslice = slice(filestart,filestop,partslice1.step)
                slicelist[npart1] = fileslice

                chunklist = []
                (firstinterval2, firstslice2) = partition2[0]
                prevhigh2 = firstinterval2[0]
                for (interval2,partslice2) in partition2:

                    # If the previous interval high is less than
                    # the current interval low value, interpose
                    # missing data.
                    low = interval2[0]
                    if prevhigh2<low:
                        missing_interval = (prevhigh2,low)
                        missing_slice = sliceIntersect(slice1, missing_interval)
                        if missing_slice is not None:
                            slicelist[npart2] = missing_slice
                            chunklist.append((None,copy.copy(slicelist)))
                    prevhigh2 = interval2[1]

                    # generate the filename
                    matchnames = self.genMatch(axis2, interval2, matchnames)
                    filename = self.getFilePath(matchnames, template)

                    filestart = partslice2.start-interval2[0]
                    filestop = partslice2.stop-interval2[0]
                    fileslice = slice(filestart,filestop,partslice2.step)
                    slicelist[npart2] = fileslice

                    chunklist.append((filename,copy.copy(slicelist)))

                resultlist.append(chunklist)

            result = (2,(npart1,npart2),resultlist)

        return result

    def expertSlice (self, initslist):

        # Handle negative slices
        revlist = []                    # Slices to apply to result if reversals needed
        slist = []                      # Slices with positive strides
        haveReversals = 0               # True iff result array needs reversing
        i=0
        for s in initslist:
            if s.step<0:
                axislen = self.shape[i]
                slist.append(reverseSlice(s,axislen))
                revlist.append(slice(None,None,-1))
                haveReversals = 1
            else:
                slist.append(s)
                revlist.append(slice(None,None,1))
            i += 1

        # This does most of the work
        npart, idims, partitionSlices = self.expertPaths(slist)

        # If the dataset includes a forecast axis, find it now, as well
        # as this slice's corresponding index in that direction.
        fci = None
        for i in range(len(self.domain)):
            if self.domain[i][0].isForecast():
                fci = i
                fcv = initslist[i].start
                break

        # If no intersection, return an 'empty' array.
        if partitionSlices is None:
            return numpy.ma.zeros((0,),self._numericType_)

        # Handle rank-0 variables separately
        if self.rank() == 0:
            filename, dumlist = partitionSlices

            f = self.parent.openFile(filename,'r')
            try:
                var = f.variables[self.name_in_file]
                result = var.getValue()
            finally:
                f.close()
            return result

        # If no partitioned axes, just read the data
        if npart==0:
            filename, slicelist = partitionSlices

            f = self.parent.openFile(filename,'r')
            try:
                var = f.variables[self.name_in_file]
                if fci==None:
                    result = self._returnArray(apply(var.getitem,tuple(slicelist)),0)
                else:
                    # If there's a forecast axis, the file doesn't know about it so
                    # don't use it in slicing data out of the file.
                    result = self._returnArray( apply( var.getitem, \
                                   tuple( slicelist[0:fci]+slicelist[fci+1:] ) ), \
                                                0 )
                    # But the result still needs an index in the forecast direction,
                    # which is simple to do because there is only one forecast per file:
                    result.resize( map(lenSlice,slicelist) )

            finally:
                f.close()
            sh = result.shape
            if 0 in sh:
                raise CDMSError, IndexError + 'Coordinates out of Domain'

        # If one partitioned axes:
        elif npart==1:

            npart1 = idims[0]
            resultlist = []
            for filename, slicelist in partitionSlices:

                # If the slice is missing, interpose missing data
                if filename is None:
                    shapelist = map(lenSlice, slicelist)
                    chunk = numpy.ma.zeros(tuple(shapelist),self._numericType_)
                    chunk[...] = numpy.ma.masked

                # else read the data and close the file
                else:
                    f = self.parent.openFile(filename,'r')
                    try:
                        var = f.variables[self.name_in_file]
                        if fci==None:
                            chunk = apply(var.getitem,tuple(slicelist))
                        else:
                            # If there's a forecast axis, the file doesn't know about it so
                            # don't use it in slicing data out of the file.
                            chunk = apply( var.getitem, \
                                           tuple( slicelist[0:fci]+slicelist[fci+1:] ) )
                            # But the chunk still needs an index in the forecast direction,
                            # which is simple to do because there is only one forecast per file:
                            chunk.resize( map(lenSlice,slicelist) )

                    finally:
                        f.close()
                    sh = chunk.shape
                    if 0 in sh:
                        raise CDMSError, 'Coordinates out of Domain'

                resultlist.append(self._returnArray(chunk,0))

            # Combine the chunks into a single array
            # Note: This works because slicelist is the same length
            # as the domain, and var.getitem returns a chunk
            # with singleton dimensions included. This means that
            # npart1 corresponds to the correct dimension of chunk.
            result = numpy.ma.concatenate(resultlist,axis=npart1)
            for chunk in resultlist:
                del(chunk)

        # If two partitioned axes, 2-D version of previous case
        if npart==2:
            npart1, npart2 = idims

            resultlist = []
            for filelist in partitionSlices:
                chunklist = []
                for filename, slicelist in filelist:

                    # If the slice is missing, interpose missing data
                    if filename is None:
                        shapelist = map(lenSlice, slicelist)
                        chunk = numpy.ma.zeros(tuple(shapelist),self._numericType_)
                        chunk[...] = numpy.ma.masked

                    # else read the data and close the file
                    else:
                        f = self.parent.openFile(filename,'r')
                        try:
                            var = f.variables[self.name_in_file]
                            if fci==None:
                                chunk = apply(var.getitem,tuple(slicelist))
                            else:
                                # If there's a forecast axis, the file doesn't know about it so
                                # don't use it in slicing data out of the file.
                                chunk = apply( var.getitem, \
                                               tuple( slicelist[0:fci]+slicelist[fci+1:] ) )
                                # But the chunk still needs an index in the forecast direction,
                                # which is simple to do because there is only one forecast per file:
                                chunk.resize( map(lenSlice,slicelist) )

                        finally:
                            f.close()
                        sh = chunk.shape
                        if 0 in sh:
                            raise CDMSError, 'Coordinates out of Domain'
                    chunklist.append(self._returnArray(chunk,0))

                # Note: This works because slicelist is the same length
                # as the domain, and var.getitem returns a chunk
                # with singleton dimensions included. This means that
                # npart1 corresponds to the correct dimension of chunk.
                bigchunk = numpy.ma.concatenate(chunklist,axis=npart2)
                for chunk in chunklist:
                    del(chunk)
                resultlist.append(bigchunk)

            result = numpy.ma.concatenate(resultlist,axis=npart1)
            for bigchunk in resultlist:
                del(bigchunk)

        # If slices with negative strides were input, apply the appropriate
        # reversals.
        if haveReversals:
            result = result[revlist]

        return result


    shape = property(_getShape,None)
##     shape = _getShape
    dtype = property(_getdtype,None)
    
## PropertiedClasses.set_property (DatasetVariable, 'shape', 
##                                   DatasetVariable._getShape, nowrite=1,
##                                   nodelete=1)
## PropertiedClasses.set_property (DatasetVariable, 'dtype', 
##                                   DatasetVariable._getdtype, nowrite=1,
##                                   nodelete=1)

## internattr.add_internal_attribute(DatasetVariable, 'domain')
