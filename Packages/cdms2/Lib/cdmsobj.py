"""
CDMS module-level functions and definitions
"""

import cdmsNode
import cdtime
import glob
import os
import re
import string
import sys
import types
import AutoAPI
#import internattr

# Data types

CdChar = cdmsNode.CdChar
CdByte = cdmsNode.CdByte
CdShort = cdmsNode.CdShort
CdInt = cdmsNode.CdInt
CdLong = cdmsNode.CdLong
CdInt64 = cdmsNode.CdInt64
CdFloat = cdmsNode.CdFloat
CdDouble = cdmsNode.CdDouble
CdString = cdmsNode.CdString
CdFromObject = cdmsNode.CdFromObject
CdScalar = cdmsNode.CdScalar
CdArray = cdmsNode.CdArray

Unlimited = 1                           # Unlimited axis designator

Max32int = 2**31-1                      # Maximum 32-bit integer

# Regular expressions for each template specifier
_Daynum = '[0-3][0-9]'
_Hour = '[0-5][0-9]'
_Level = '[0-9]{1,4}'
_Minute = '[0-5][0-9]'
_Month = 'jan|feb|mar|apr|may|jun|jul|aug|sep|oct|nov|dec'
_Monthnum = '[0-1]?[0-9]'
_Monthupper = 'JAN|FEB|MAR|APR|MAY|JUN|JUL|AUG|SEP|OCT|NOV|DEC'
_Name = '[a-zA-Z_:][-a-zA-Z0-9_:]*'
_Percent = '%'
_Second = '[0-5][0-9]'
_Year2 = '[0-9][0-9]'
_Year4 = '[0-9]{4,4}'
_Zulu = _Hour+'[z|Z]'+_Year4+_Monthnum+_Daynum

# Positions for time lists
_yr = 0
_mo = 1
_dy = 2
_hr = 3
_mi = 4
_se = 5

# Positions for dimension type lists
_var = 0
_time = 1
_etime = 2
_level = 3
_elevel = 4

# Map template specifier to (regex,name,dimensionType,position)
#   name is used as a unique regex group id
#   dimensionType is one of ('var','time','etime','level','elevel')
#   position is an index into a time tuple, or None if not applicable
_specifierMap = {
    '%%': (_Percent, 'percent', None, None),
    '%G': (_Monthupper, 'monthupper', 'time', _mo),
    '%H': (_Hour, 'hour', 'time', _hr),
    '%L': (_Level, 'level', 'level', None),
    '%M': (_Minute, 'minute', 'time', _mi),
    '%S': (_Second, 'second', 'time', _se),
    '%Y': (_Year4, 'year4', 'time', _yr),
    '%d': (_Daynum, 'day', 'time', _dy),
    '%eG': (_Monthupper, 'emonthupper', 'etime', _mo),
    '%eH': (_Hour, 'ehour', 'etime', _hr),
    '%eL': (_Level, 'elevel', 'elevel', None),
    '%eM': (_Minute, 'eminute', 'etime', _mi),
    '%eS': (_Second, 'esecond', 'etime', _se),
    '%eY': (_Year4, 'eyear4', 'etime', _yr),
    '%ed': (_Daynum, 'eday', 'etime', _dy),
    '%ef': (_Daynum, 'eday', 'etime', _dy),
    '%eg': (_Month, 'emonth', 'etime', _mo),
    '%eh': (_Hour, 'ehour2', 'etime', _hr),
    '%em': (_Monthnum, 'emonthnum', 'etime', _mo),
    '%en': (_Monthnum, 'emonthnum', 'etime', _mo),
    '%ey': (_Year2, 'eyear2', 'etime', _yr),
    '%ez': (_Zulu, 'ezulu', 'etime', None),
    '%f': (_Daynum, 'day', 'time', _dy),
    '%g': (_Month, 'month', 'time', _mo),
    '%h': (_Hour, 'hour2', 'time', _hr),
    '%m': (_Monthnum, 'monthnum', 'time', _mo),
    '%n': (_Monthnum, 'monthnum', 'time', _mo),
    '%v': (_Name, 'name', 'var', None),
    '%y': (_Year2, 'year2', 'time', _yr),
    '%z': (_Zulu, 'zulu', 'time', None),
    }

_monthListUpper = ['JAN','FEB','MAR','APR','MAY','JUN','JUL','AUG','SEP','OCT','NOV','DEC']
_monthMapUpper = {
    'JAN':1,
    'FEB':2,
    'MAR':3,
    'APR':4,
    'MAY':5,
    'JUN':6,
    'JUL':7,
    'AUG':8,
    'SEP':9,
    'OCT':10,
    'NOV':11,
    'DEC':12,
    }

_monthListLower = ['jan','feb','mar','apr','may','jun','jul','aug','sep','oct','nov','dec']
_monthMapLower = {
    'jan':1,
    'feb':2,
    'mar':3,
    'apr':4,
    'may':5,
    'jun':6,
    'jul':7,
    'aug':8,
    'sep':9,
    'oct':10,
    'nov':11,
    'dec':12,
    }

_specre = re.compile('(%%|%G|%H|%L|%M|%S|%Y|%d|%eG|%eH|%eL|%eM|%eS|%eY|%ed|%ef|%eg|%eh|%em|%en|%ey|%ez|%f|%g|%h|%m|%n|%v|%y|%z)')
_filere = re.compile('[^'+os.sep+']+')

_debug = 0                              # Print debug info

AbsoluteTemplate = "Template must be a relative path: "

# Module-level functions

# Set debug mode, to 'on' or 'off'
def setDebugMode(mode):
    global _debug
    if mode=='on':
        _debug=1
    elif mode=='off':
        _debug=0

# Map a template to a regular expression
# Returns (regex,dimtypes), where regex is the regular expression
# corresponding to template, and dimtypes is a dictionary
# such that dimtypes[dimtype] = specStrings
# where specStrings is the specifier associated with the dimension type,
# or for time, the list of specifiers in the order (yr,mo,dy,hr,mi,se)
# where each element is the specifier for that time element
def templateToRegex(template):

    matchspec = {}
    dimtypes = {'var':None,
                'time':[None,None,None,None,None,None],
                'etime':[None,None,None,None,None,None],
                'level':None,
                'elevel':None,
                }

    # Map spec to regex
    # (The default arg bring matchspec and dimtypes into the local scope)
    def retspec(matchobj, matchspec=matchspec, dimtypes=dimtypes):
        spec = matchobj.group(0)
        pat,name,dimtype,pos = _specifierMap[spec]

        if dimtype=='var':
            dimtypes['var']=spec
        elif dimtype in ('time','etime'):
            if pos is not None:
                dimtypes[dimtype][pos]=spec
            elif name in ('zulu','ezulu'):
                pass # Crack Zulu time
        elif dimtype in ('level','elevel'):
            dimtypes[dimtype]=spec

        if matchspec.has_key(spec):
            return '(?P='+name+')'
        else:
            matchspec[spec] = 1
            return '(?P<'+name+'>'+pat+')'
        
    templatere = _specre.sub(retspec,template)
    return (templatere,dimtypes)

def retglob(matchobj):
    return '*'

# Generate a component time from a matchobj and list of specs
def generateTime(matchobj,timespecs):
    iyr = 0
    imo = 1
    idy = 1
    ihr = 0
    imi = 0
    ise = 0
    yrspec,mospec,dyspec,hrspec,mispec,sespec = timespecs
    if yrspec:
        pat,name,dimtype,pos = _specifierMap[yrspec]
        yrstr = matchobj.group(name)
        iyr = string.atoi(yrstr)

        # Map 2-digit year to [1950,2049)
        if yrspec in ('%y','%ey'):
            if iyr<50:
                iyr = iyr+2000
            else:
                iyr = iyr+1900
    if mospec:
        pat,name,dimtype,pos = _specifierMap[mospec]
        mostr = matchobj.group(name)
        if mospec in ('%G','%eG'):
            imo = _monthMapUpper[mostr]
        elif mospec in ('%g','%eg'):
            imo = _monthMapLower[mostr]
        elif mospec in ('%m','%em','%n','%en'):
            imo = string.atoi(mostr)
    if dyspec:
        pat,name,dimtype,pos = _specifierMap[dyspec]
        dystr = matchobj.group(name)
        idy = string.atoi(dystr)
    if hrspec:
        pat,name,dimtype,pos = _specifierMap[hrspec]
        hrstr = matchobj.group(name)
        ihr = string.atoi(hrstr)
    if mispec:
        pat,name,dimtype,pos = _specifierMap[mispec]
        mistr = matchobj.group(name)
        imi = string.atoi(mistr)
    if sespec:
        pat,name,dimtype,pos = _specifierMap[sespec]
        sestr = matchobj.group(name)
        ise = string.atoi(sestr)
    return cdtime.comptime(iyr,imo,idy,ihr,imi,ise)   

# Find all files in 'direc' which match 'template'.
# template is a relative path, and may contain specifiers
# in directory names. Returns a list [(f,m),..,(f,m)] where
# f is a matching file name, and m is a list [var,time,etime,level,elevel]
# of matching values in f. Any or all elems of the list may be None.
def matchingFiles(direc,template):

    if os.path.isabs(template):
        raise AbsoluteTemplate, template

    # Generate a glob pattern
    normTemplate = os.path.normpath(template)
    globPattern = _filere.sub(retglob,normTemplate)

    # Map the template to a regular expression
    templatere,dimtypes = templateToRegex(template)
    ctre = re.compile(templatere)

    # Get a list of candidate files
    try:
        os.chdir(direc)
    except os.error:
        raise IOError,'%s: %s'%(sys.exc_value,direc)
        
    candlist = glob.glob(globPattern)

    # Look for matches 
    matchfiles = []
    for candfile in candlist:
        matchobj = ctre.match(candfile)

        # Create matching values
        if matchobj is None: continue
        matchnames = [None,None,None,None,None]
        if dimtypes['var'] is not None:
            matchnames[_var] = matchobj.group('name')
        if dimtypes['time'] != [None,None,None,None,None,None]:
            matchnames[_time] = generateTime(matchobj,dimtypes['time'])
        if dimtypes['etime'] != [None,None,None,None,None,None]:
            matchnames[_etime] = generateTime(matchobj,dimtypes['etime'])
        if dimtypes['level'] is not None:
            matchnames[_level] = string.atoi(matchobj.group('level'))
        if dimtypes['elevel'] is not None:
            matchnames[_elevel] = string.atoi(matchobj.group('elevel'))
        matchfiles.append((candfile,matchnames))

    return matchfiles

# Get a string time component from a spec and a component time
def getTimeAsString(spec,time):

    if spec in ('%G','%eG'):
        imo = time.month
        specstr = _monthListUpper[imo-1]
    elif spec in ('%H','%eH'):
        specstr = str(time.hour)
    elif spec in ('%M','%eM'):
        specstr = str(time.minute)
    elif spec in ('%S','%eS'):
        specstr = str(int(time.second))
    elif spec in ('%Y','%eY'):
        specstr = string.zfill(str(time.year),4)
    elif spec in ('%d','%ed'):
        specstr = str(time.day)
    elif spec in ('%f','%ef'):
        specstr = string.zfill(str(time.day),2)
    elif spec in ('%g','%eg'):
        imo = time.month
        specstr = _monthListLower[imo-1]
    elif spec in ('%h','%eh'):
        specstr = string.zfill(str(time.hour),2)
    elif spec in ('%m','%em'):
        specstr = str(time.month)
    elif spec in ('%n','%en'):
        specstr = string.zfill(str(time.month),2)
    elif spec in ('%y','%ey'):
        specstr = string.zfill(str(time.year%100),2)
    elif spec in ('%z','%ez'):
        specstr = getTimeAsString('%H',time)+'Z'+getTimeAsString('%Y',time)+getTimeAsString('%n',time)+getTimeAsString('%d',time)
    return specstr

# Generate a file path, given a template and matchname list.
# matchnames is a list [varname,time,etime,level,elevel], where
# any or all elems may be None.  If matchnames be a longer list,
# it is not an error but the additional elements are ignored.
def getPathFromTemplate(template,matchnames):

    # Map spec to value string
    # (Default arg brings matchnames into the local scope)
    def retpath(matchobj, matchnames=matchnames):
        spec = matchobj.group(0)
        pat,name,dimtype,pos = _specifierMap[spec]
        var,time,etime,level,elevel = matchnames[0:5]

        if dimtype=='var':
            if var is None:
                specstr = spec
            else:
                specstr = var
        elif dimtype=='time':
            if time is None:
                specstr = spec
            else:
                specstr = getTimeAsString(spec,time)
        elif dimtype=='etime':
            if etime is None:
                specstr = spec
            else:
                specstr = getTimeAsString(spec,etime)
        elif dimtype=='level':
            if level is None:
                specstr = spec
            else:
                specstr = str(level)
        elif dimtype=='elevel':
            if elevel is None:
                specstr = spec
            else:
                specstr = str(elevel)

        return specstr

    path = _specre.sub(retpath,template)
    return path

# Search an object or list of objects for a string attribute which
# contains a pattern.  If the object is a dataset, all objects contained in
# it are searched as well.  The function returns a list of objects with
# matching attribute, or the empty list if there are no matches.
# tag is one of 'dataset', 'variable', 'axis',
# 'rectGrid','xlink', or None.  If tag is None, all object classes are
# searched.  If attribute is None, all attributes are searched.

def searchPattern(objlist, pattern, attribute=None, tag=None):
    if tag is not None:
        tag = string.lower(tag)
    regexp = re.compile(pattern)
    if type(objlist) is not types.ListType:
        objlist = [objlist]

    returnlist = []
    for obj in objlist:
        returnlist = returnlist + obj.searchPattern(regexp,attribute,tag)

    return returnlist

# Search an object or list of objects for a string attribute which
# matches a pattern.  If the object is a dataset, all objects contained in
# it are searched as well.  The function returns a list of objects with
# matching attribute, or the empty list if there are no matches.
# tag is one of 'dataset', 'variable', 'axis',
# 'rectGrid','xlink', or None.  If tag is None, all object classes are
# searched.  If attribute is None, all attributes are searched.

def matchPattern(objlist, pattern, attribute=None, tag=None):
    if tag is not None:
        tag = string.lower(tag)
    regexp = re.compile(pattern)
    if type(objlist) is not types.ListType:
        objlist = [objlist]

    returnlist = []
    for obj in objlist:
        returnlist = returnlist + obj.matchPattern(regexp,attribute,tag)

    return returnlist

# Search an object or list of objects for those objects which satisfy a
# predicate condition. If the object is a dataset, all objects contained in
# it are searched as well. The predicate is a function which takes a single
# object (dataset, variable, axis, etc.) and returns true or false. A list of
# objects for which the predicate is true, is returned. If there are no objects
# which satisfy the predicate, If tag is None, all object classes are
# searched, otherwise only those objects of class matching tag (e.g., "dataset",
# "variable", "axis", etc.) are tested.
#
# Note that Attribute errors are ignored. This simplifies building predicate
# functions, as it is not necessary to test whether the object has a particular attribute.
# For example, if searching for axes with the partition length greater than 1000
#
#   lambda obj: obj.partition_length > 1000
#
# is sufficient, it is not necessary to test for the existence of the attribute.

def searchPredicate(objlist, predicate, tag=None):
    if tag is not None:
        tag = string.lower(tag)
    if type(objlist) is not types.ListType:
        objlist = [objlist]

    returnlist = []
    for obj in objlist:
        returnlist = returnlist + obj.searchPredicate(predicate,tag)

    return returnlist

#------------------------------------------------------------------------
# Classes

# Generic CDMS object has a tree node, attributes
class CdmsObj (object,AutoAPI.AutoAPI):
##     def __setattr__(self,name,value):
##         object.__setattr__(self,name,value)
##         if not name in self.__cdms_internals__ and not name[0]=='_':
##             self.attributes[name]=value
## ##             if name == 'shape' :
## ##                 print self.__class__,name,value

    def _listatts(self):
        dic={}
        for nm,val in self.__dict__.items():
            if (nm[0]!='_' and not nm in self.__cdms_internals__) or nm in ['_FillValue']:
                dic[nm]=val
            if nm == '_units':
                dic['units']=val
        return dic
    def _setatts(self,value):
        return

    attributes = property(_listatts,_setatts)
    
        
    def __init__(self, node = None):
        if not hasattr(self,'___cdms_internals__'):
            self.__dict__['___cdms_internals__']=['__cdms_internals__','___cdms_internals__','_node_','parent','attributes','shape','autoApiInfo']
        self.attributes={}
        self._node_ = node
        if node is not None:
            # Build an attribute dictionary from the node, 
            # CDML datatype constraints

            if hasattr(node,'datatype'):
                parenttype = node.datatype
            else:
                parenttype = None
            atts = node.getExternalDict()
            adict = self.__dict__
            for attname in atts.keys():
                (attval,datatype)=atts[attname] # (XML value, datatype)
                constraint = node.extra.get(attname)
                if constraint is not None:
                    (scaletype,reqtype)=constraint # (CdScalar|CdArray, required type)
                    if reqtype==CdFromObject:
                        reqtype = parenttype
                    if reqtype!=datatype and datatype==CdString and scaletype==CdScalar:
                        if reqtype in (CdFloat,CdDouble):
                            try:
                                attval = string.atof(attval)
                            except:
                                raise RuntimeError,"%s=%s must be a float"%(attname,attval)
                        elif reqtype in (CdShort,CdInt,CdLong,CdInt64):
                            try:
                                attval = string.atoi(attval)
                            except:
                                raise RuntimeError,"%s=%s must be an integer"%(attname,attval)
                adict[attname] = attval
                self.attributes[attname] = attval
        self.autoApiInfo = AutoAPI.Info(self)
        self.autoApiInfo.expose=set(["dump","searchone","matchone","searchPattern","matchPattern","searchPredicate"])


    def searchone(self, pattern, attname):
        """Return true if the attribute with name attname is a string
        attribute which contains the compiled regular expression pattern, or
        if attname is None and pattern matches at least one string
        attribute. Return false if the attribute is not found or is not 
        a string.
        :::
        Input:::
        pattern :: (str) (0) pattern
        attname :: (str/None) (1) attribute name
        :::
        Output:::
        result :: (int/True/False) (0) True if the attribute with name attname is a string attribute which contains the compiled regular expression pattern, or if attname is None and pattern matches at least one string attribute, False if the attribute is not found or is not a string
        :::
        """
        if attname is None:
            for attval in self.attributes.values():
                if type(attval) is types.StringType and pattern.search(attval) is not None:
                    return 1
            return 0
        elif self.attributes.has_key(attname):
            attval = self.attributes[attname]
            return (type(attval) is types.StringType and pattern.search(attval) is not None)
        else:
            return 0

    # Return true iff the attribute with name attname is a string
    # attribute which matches the compiled regular expression pattern, or
    # if attname is None and pattern matches at least one string
    # attribute. Return false if the attribute is not found or is not a string
    def matchone(self, pattern, attname):
        """
        Return true if the attribute with name attname is a string
        attribute which matches the compiled regular expression pattern, or
        if attname is None and pattern matches at least one string
        attribute. Return false if the attribute is not found or is not a string
        :::
        Input:::
        pattern :: (str) (0) pattern
        attname :: (str/None) (1) attribute name
        :::
        Output:::
        result :: (int/True/False) (0) True if the attribute with name attname is a string attribute which matches the compiled regular expression pattern, or if attname is None and pattern matches at least one string attribute, False if the attribute is not found or is not a string
        :::
        """
        if attname is None:
            for attval in self.attributes.values():
                if type(attval) is types.StringType and pattern.match(attval) is not None:
                    return 1
            return 0
        elif self.attributes.has_key(attname):
            attval = self.attributes[attname]
            return (type(attval) is types.StringType and pattern.match(attval) is not None)
        else:
            return 0

    # Search for a pattern in a string-valued attribute. If attribute is None,
    # search all string attributes. If tag is not None, it must match the internal node tag.
    def searchPattern(self,pattern,attribute,tag):
        """
        Search for a pattern in a string-valued attribute. If attribute is None, search all string attributes. If tag is not None, it must match the internal node tag.
        :::
        Input:::
        pattern :: (str) (0) pattern
        attribute :: (str/None) (1) attribute name
        tag :: (str/None) (2) node tag
        :::
        Output:::
        result :: (list) (0) 
        :::
        """
        if tag is None or string.lower(tag)==self._node_.tag:
            if self.searchone(pattern,attribute):
                return [self]
            else:
                return []
        else:
            return []

    # Match a pattern in a string-valued attribute. If attribute is None,
    # search all string attributes. If tag is not None, it must match the internal node tag.
    def matchPattern(self,pattern,attribute,tag):
        """
        Match for a pattern in a string-valued attribute. If attribute is None, search all string attributes. If tag is not None, it must match the internal node tag.
        :::
        Input:::
        pattern :: (str) (0) pattern
        attribute :: (str/None) (1) attribute name
        tag :: (str/None) (2) node tag
        :::
        Output:::
        result :: (list) (0) 
        :::
        """
        if tag is None or string.lower(tag)==self._node_.tag:
            if self.matchone(pattern,attribute):
                return [self]
            else:
                return []
        else:
            return []

    # Apply a truth-valued predicate. Return a list containing a single instance: [self]
    # if the predicate is true and either tag is None or matches the object node tag.
    # If the predicate returns false, return an empty list
    def searchPredicate(self,predicate,tag):
        """
        Apply a truth-valued predicate. Return a list containing a single instance: [self] if the predicate is true and either tag is None or matches the object node tag. If the predicate returns false, return an empty list
        :::
        Input:::
        predicate :: (function) (0) predicate
        tag :: (str/None) (1) node tag
        :::
        Output:::
        result :: (list) (0) 
        :::
        """
        if tag is None or string.lower(tag)==self._node_.tag:
            try:
                if apply(predicate,(self,))==1:
                    result = [self]
            except:
                result = []
        else:
            result = []
        return result

    def dump(self,path=None,format=1):
        """ dump(self,path=None,format=1)
        Dump an XML representation of this object to a file.
        'path' is the result file name, None for standard output.
        'format'==1 if the file is formatted with newlines for readability
        :::
        Input:::
          path :: (None) (0) result file name, None for standard output
          format :: (int) (1) 1 if the file is formatted with newlines for readability
        :::
        Output:::
        None :: (None) (0) nothing returned
        :::
        """
        if self._node_ is None:
            raise CDMSError, "No tree node found"
        self._node_.dump(path,format)

    def _getinternals(self):
        return self.___cdms_internals__
    def _setinternals(self,value):
        self.___cdms_internals__ = value
    __cdms_internals__ = property(_getinternals,_setinternals)
#internattr.add_internal_attribute(CdmsObj)

if __name__ == '__main__':
    x = CdmsObj(None)
    x.someatt = 1
    assert x.attributes['someatt'] == x.someatt
    assert not x.attributes.has_key('_node')
    # need tests for the search routines...
    print "Test passed."




