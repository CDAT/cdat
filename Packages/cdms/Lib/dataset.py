""" CDMS dataset and file objects"""
from error import CDMSError
import MA
import Cdunif
import Numeric
import cdmsNode
import os, sys
import string
import urllib
import cdmsURLopener                    # Import after urllib, to handle errors
import urlparse
import internattr
import cdmsobj
import re
from CDMLParser import CDMLParser
from cdmsobj import CdmsObj
from axis import Axis, FileAxis, FileVirtualAxis, isOverlapVector
from coord import FileAxis2D, DatasetAxis2D
from auxcoord import FileAuxAxis1D, DatasetAuxAxis1D
from grid import RectGrid, FileRectGrid
from hgrid import FileCurveGrid, DatasetCurveGrid
from gengrid import FileGenericGrid, DatasetGenericGrid
from variable import DatasetVariable
from fvariable import FileVariable
from tvariable import asVariable
from cdmsNode import CdDatatypes
import convention
try:
    import cache
except ImportError:
    pass

DuplicateAxis = "Axis already defined: "
class DuplicateAxisError(CDMSError):
    pass
DuplicateGrid = "Grid already defined: "
DuplicateVariable = "Variable already defined: "
FileNotFound = "File not found: "
FileWasClosed = "File was closed: "
InvalidDomain = "Domain elements must be axes or grids"
ModeNotSupported = "Mode not supported: "
SchemeNotSupported = "Scheme not supported: "

# Regular expressions for parsing the file map.
_Name = re.compile(r'[a-zA-Z_:][-a-zA-Z0-9._:]*')
_ListStartPat = r'\[\s*'
_ListStart = re.compile(_ListStartPat)
_ListEndPat = r'\s*\]'
_ListEnd = re.compile(_ListEndPat)
_ListSepPat = r'\s*,\s*'
_ListSep = re.compile(_ListSepPat)
_IndexPat = r'(\d+|-)'
_FilePath = r"([^\s\]\',]+)"
_IndexList = re.compile(_ListStartPat+_IndexPat+_ListSepPat+_IndexPat+_ListSepPat+_IndexPat+_ListSepPat+_IndexPat+_ListSepPat+_FilePath+_ListEndPat)

_NPRINT = 20

# Create a tree from a file path.
# Returns the parse tree root node.
def load(path):
    fd = open(path)
    text = fd.read()
    fd.close()
    p=CDMLParser()
    p.feed(text)
    p.close()
    return p.getRoot()

# Create a tree from a URI
# URI is of the form scheme://netloc/path;parameters?query#fragment
# where fragment may be an XPointer.
# Returns the parse tree root node.
def loadURI(uri):
    (scheme,netloc,path,parameters,query,fragment)=urlparse.urlparse(uri)
    uripath = urlparse.urlunparse((scheme,netloc,path,'','',''))
    fd = urllib.urlopen(uripath)
    text = fd.read()
    fd.close()
    p=CDMLParser()
    p.feed(text)
    p.close()
    return p.getRoot()

# Create a dataset
# 'path' is the XML file name, or netCDF filename for simple file create
# 'template' is a string template for the datafile(s), for dataset creation
def createDataset(path,template=None):
    return openDataset(path,'w',template)

# Open an existing dataset
# 'uri' is a Uniform Resource Identifier, referring to a cdunif file, XML file,
#   or LDAP URL of a catalog dataset entry.
# 'mode' is 'r', 'r+', 'a', or 'w'
def openDataset(uri,mode='r',template=None,dods=1):
    uri = string.strip(uri)
    (scheme,netloc,path,parameters,query,fragment)=urlparse.urlparse(uri)
    if scheme in ('','file'):
        path = os.path.expanduser(path)
        root,ext = os.path.splitext(path)
        if ext in ['.xml','.cdml']:
            if mode!='r': raise ModeNotSupported,mode
            datanode = load(path)
        else:
            file = CdmsFile(path,mode)
            return file
    elif scheme in ['http', 'gridftp']:
        
        if (dods):
            if mode!='r': raise ModeNotSupported,mode
            # DODS file?
            try:
                file = CdmsFile(uri,mode)
                return file
            except:
                raise "Error in DODS open of: ",uri
        else:
            try:
                datanode = loadURI(uri)
                return datanode
            except:
                datanode = loadURI(uri)
                raise "Error in loadURI of: ",uri
            
    else:
        raise SchemeNotSupported, scheme

    # Determine dpath, the absolute path to data files:
    # dpath =
    # (1) head + node.directory, if .directory is relative
    # (2) node.directory, if absolute
    # (3) head, if no directory entry found (assume XML file is
    #       at top level of data directory)
    #
    # Note: In general, dset.datapath is relative to the URL of the
    #   enclosing database, but here the database is null, so the
    #   datapath should be absolute.
    direc = datanode.getExternalAttr('directory')
    head = os.path.dirname(path)
    if direc and os.path.isabs(direc):
        dpath = direc
    elif direc:
        dpath = os.path.join(head,direc)
    else:
        dpath = head

    dataset = Dataset(uri, mode, datanode, None, dpath)
    return dataset

# Functions for parsing the file map.
def parselist(text, f):
    """Parse a string of the form [A, A, ...].
    f is a function which parses A and returns (A, nconsumed)
    """

    n = 0
    m = _ListStart.match(text)
    if m is None:
        raise CDMSError, "Parsing cdms_filemap near "+text[0:_NPRINT]
    result = []
    n += m.end()
    s, nconsume = f(text[n:])
    result.append(s)
    n += nconsume
    while 1:
        m = _ListSep.match(text[n:])
        if m is None:
            break
        else:
            n += m.end()
        s, nconsume = f(text[n:])
        result.append(s)
        n += nconsume
    m = _ListEnd.match(text[n:])
    if m is None:
        raise CDMSError, "Parsing cdms_filemap near "+text[n:n+_NPRINT]
    n += m.end()
    return result, n

def parseIndexList(text):
    """Parse a string of the form [i,j,k,l,path] where
    i,j,k,l are indices or '-', and path is a filename.
    Coerce the indices to integers, return (result, nconsumed).
    """
    m = _IndexList.match(text)
    if m is None:
        raise CDMSError, "Parsing cdms_filemap near "+text[0:_NPRINT]
    result = [None]*5
    for i in range(4):
        s = m.group(i+1)
        if s!='-':
            result[i] = string.atoi(s)
    result[4] = m.group(5)
    return result, m.end()

def parseName(text):
    m = _Name.match(text)
    if m is None:
        raise CDMSError, "Parsing cdms_filemap near "+text[0:_NPRINT]
    return m.group(), m.end()

def parseVarMap(text):
    """Parse a string of the form [ namelist, slicelist ]"""
    n = 0
    m = _ListStart.match(text)
    if m is None:
        raise CDMSError, "Parsing cdms_filemap near "+text[0:_NPRINT]
    result = []
    n += m.end()
    s, nconsume = parselist(text[n:],parseName)
    result.append(s)
    n += nconsume
    m = _ListSep.match(text[n:])
    if m is None:
        raise CDMSError, "Parsing cdms_filemap near "+text[n:n+_NPRINT]
    n += m.end()
    s, nconsume = parselist(text[n:], parseIndexList)
    result.append(s)
    n += nconsume
    m = _ListEnd.match(text[n:])
    if m is None:
        raise CDMSError, "Parsing cdms_filemap near "+text[n:n+_NPRINT]
    n += m.end()
    return result, n

def parseFileMap(text):
    """Parse a CDMS filemap. having the form:
    filemap :== [ varmap, varmap, ...]
    varmap :== [ namelist, slicelist ]
    namelist :== [name, name, ...]
    slicelist :== [indexlist, indexlist, ,,,]
    indexlist :== [i,j,k,l,path]
    """
    result, n = parselist(text, parseVarMap)
    if n<len(text):
        raise CDMSError, "Parsing cdms_filemap near "+text[n:n+_NPRINT]
    return result


# A CDMS dataset consists of a CDML/XML file and one or more data files
from cudsinterface import cuDataset
class Dataset(CdmsObj, cuDataset):
    def __init__(self, uri, mode, datasetNode=None, parent=None, datapath=None):
        if datasetNode is not None and datasetNode.tag !='dataset':
            raise CDMSError, 'Node is not a dataset node'
        CdmsObj.__init__(self,datasetNode)
        cuDataset.__init__(self)
        self.parent = parent
        self.uri = uri
        self.mode = mode
            # Path of data files relative to parent db.
            # Note: .directory is the location of data relative to the location of the XML file
        self.datapath = datapath
        self.variables = {}
        self.axes = {}
        self.grids = {}
        self.xlinks = {}
        self._gridmap_ = {}
            # Gridmap:(latname,lonname,order,maskname,gridclass) => grid
        (scheme,netloc,xmlpath,parameters,query,fragment)=urlparse.urlparse(uri)
        self._xmlpath_ = xmlpath
        # Dictionary of dictionaries, keyed on node tags
        self.dictdict = {'variable':self.variables, 
                         'axis':self.axes, 
                         'rectGrid':self.grids, 
                         'curveGrid':self.grids,
                         'genericGrid':self.grids,
                         'xlink':self.xlinks
                        }
        # Dataset IDs are external, so may not have been defined yet.
        if not hasattr(self,'id'):
            self.id='<None>'
        self._status_ = 'open'
        self._convention_ = convention.getDatasetConvention(self)

        # Collect named children (having attribute 'id') into dictionaries
        if datasetNode is not None:
            coordsaux = self._convention_.getDsetnodeAuxAxisIds(datasetNode)

            for node in datasetNode.getIdDict().values():
                if node.tag=='variable':
                    if node.id in coordsaux:
                        if node.getDomain().getChildCount() == 1:
                            obj = DatasetAuxAxis1D(self, node.id, node)
                        else:
                            obj = DatasetAxis2D(self, node.id, node)
                    else:
                        obj = DatasetVariable(self, node.id, node)
                    self.variables[node.id]=obj
                elif node.tag=='axis':
                    obj = Axis(self,node)
                    self.axes[node.id]=obj
                elif node.tag=='rectGrid':
                    obj = RectGrid(self,node)
                    self.grids[node.id]=obj
                elif node.tag=='xlink':
                    obj = Xlink(node)
                    self.xlinks[node.id]=obj
                else:
                    dict = self.dictdict.get(node.tag)
                    if dict is not None:
                        dict[node.id]=node
                    else:
                        self.dictdict[node.tag] = {node.id:node}

            # Initialize grid domains
            for grid in self.grids.values():
                grid.initDomain(self.axes, self.variables)
                latname = grid.getLatitude().id
                lonname = grid.getLongitude().id
                mask = grid.getMaskVar()
                if mask is None:
                    maskname = ""
                else:
                    maskname = mask.id
                self._gridmap_[(latname, lonname, grid.getOrder(), maskname)] = grid

            # Initialize variable domains.
            for var in self.variables.values():
                var.initDomain(self.axes, self.grids)

            for var in self.variables.values():

                # Get grid information for the variable. gridkey has the form
                # (latname,lonname,order,maskname,abstract_class).
                gridkey, lat, lon = var.generateGridkey(self._convention_, self.variables)

                # If the variable is gridded, lookup the grid. If no such grid exists,
                # create a unique gridname, create the grid, and add to the gridmap.
                if gridkey is None:
                    grid = None
                else:
                    grid = self._gridmap_.get(gridkey)
                    if grid is None:
                        if hasattr(var,'grid_type'):
                            gridtype = var.grid_type
                        else:
                            gridtype = "generic"

                        candidateBasename = None
                        if gridkey[4] == 'rectGrid':
                            gridshape = (len(lat),len(lon))
                        elif gridkey[4] == 'curveGrid':
                            gridshape = lat.shape
                        elif gridkey[4] == 'genericGrid':
                            gridshape = lat.shape
                            candidateBasename = 'grid_%d'%gridshape
                        else:
                            gridshape = (len(lat),len(lon))

                        if candidateBasename is None:
                            candidateBasename = 'grid_%dx%d'%gridshape
                        if not self.grids.has_key(candidateBasename):
                            gridname = candidateBasename
                        else:
                            foundname = 0
                            for i in range(97,123): # Lower-case letters
                                candidateName = candidateBasename+'_'+chr(i)
                                if not self.grids.has_key(candidateName):
                                    gridname = candidateName
                                    foundname = 1
                                    break

                            if not foundname:
                                print 'Warning: cannot generate a grid for variable', var.id
                                continue
                            
                        # Create the grid
                        if gridkey[4] == 'rectGrid':
                            node = cdmsNode.RectGridNode(gridname, lat.id, lon.id, gridtype, gridkey[2])
                            grid = RectGrid(self,node)
                            grid.initDomain(self.axes, self.variables)
                        elif gridkey[4] == 'curveGrid':
                            grid = DatasetCurveGrid(lat, lon, gridname, self)
                        else:
                            grid = DatasetGenericGrid(lat, lon, gridname, self)
                        self.grids[grid.id] = grid
                        self._gridmap_[gridkey] = grid

                # Set the variable grid
                var.setGrid(grid)

            # Attach boundary variables
            for name in coordsaux:
                var = self.variables[name]
                bounds = self._convention_.getVariableBounds(self, var)
                var.setBounds(bounds)

        # Create the internal filemap, if attribute 'cdms_filemap' is present.
        # _filemap_ is a dictionary, mapping (varname, timestart, levstart) => path
        #
        # Also, for each partitioned variable, set attribute '_varpart_' to [timepart, levpart]
        # where timepart is the partition for time (or None if not time-dependent)
        # and levpart is the partition in the level dimension, or None if not applicable.
        #
        # For variables partitioned in both time and level dimension, it is assumed that
        # for a given variable the partitions are orthogonal. That is, for a given
        # variable, at any timeslice the level partition is the same.
        if hasattr(self, 'cdms_filemap'):
            self._filemap_ = {}
            filemap = parseFileMap(self.cdms_filemap)
            for varlist, varmap in filemap:
                for varname in varlist:
                    timemap = {}
                    levmap = {}
                    for tstart, tend, levstart, levend, path in varmap:
                        self._filemap_[(varname, tstart, levstart)] = path
                        if tstart is not None:
                            timemap[(tstart, tend)] = 1 # Collect unique (tstart, tend) tuples
                        if levstart is not None:
                            levmap[(levstart, levend)] = 1
                    tkeys = timemap.keys()
                    if len(tkeys)>0:
                        tkeys.sort()
                        tpart = map(lambda x: list(x), tkeys)
                    else:
                        tpart = None
                    levkeys = levmap.keys()
                    if len(levkeys)>0:
                        levkeys.sort()
                        levpart = map(lambda x: list(x), levkeys)
                    else:
                        levpart = None
                    if self.variables.has_key(varname):
                        self.variables[varname]._varpart_ = [tpart, levpart]

    def getConvention(self):
        """Get the metadata convention associated with this dataset or file."""
        return self._convention_

    # Get a dictionary of objects with the given tag
    def getDictionary(self,tag):
        return self.dictdict[tag]

    # Synchronize writes with data/metadata files
    def sync(self):
        pass

    # Close all files
    def close(self):
        for dict in self.dictdict.values():
            for obj in dict.values():
                obj.parent = None
                del obj
        self.dictdict = {}
        self.variables = {}
        self.axes = {}
        self.grids = {}
        self.xlinks = {}
        self.parent = None
        self._status_ = 'closed'

##  Note: Removed to allow garbage collection of reference cycles
##     def __del__(self):
##         if cdmsobj._debug==1:
##             print 'Deleting dataset',self.id
##         self.close()
    
    # Create an axis
    # 'name' is the string name of the Axis
    # 'ar' is the 1-D data array, or None for an unlimited axis
    # Return an axis object.
    def createAxis(self,name,ar):
        pass

    # Create an implicit rectilinear grid. lat, lon, and mask are objects.
    # order and type are strings
    def createRectGrid(id, lat, lon, order, type="generic", mask=None):
        node = cdmsNode.RectGridNode(id, lat.id, lon.id, type, order, mask.id)
        grid = RectGrid(self,node)
        grid.initDomain(self.axes, self.variables)
        self.grids[grid.id] = grid
        self._gridmap_[gridkey] = grid

    # Create a variable
    # 'name' is the string name of the Variable
    # 'datatype' is a CDMS datatype
    # 'axisnames' is a list of axes or grids
    # Return a variable object.
    def createVariable(self,name,datatype,axisnames):
        pass

    # Search for a pattern in a string-valued attribute. If attribute is None,
    # search all string attributes. If tag is 'dataset', just check the dataset,
    # else check all nodes in the dataset of class type matching the tag. If tag
    # is None, search the dataset and all objects contained in it.
    def searchPattern(self,pattern,attribute,tag):
        resultlist = []
        if tag is not None:
            tag = string.lower(tag)
        if tag in ('dataset',None):
            if self.searchone(pattern,attribute)==1:
                resultlist = [self]
            else:
                resultlist = []
        if tag is None:
            for dict in self.dictdict.values():
                for obj in dict.values():
                    if obj.searchone(pattern,attribute):
                        resultlist.append(obj)
        elif tag!='dataset':
            dict = self.dictdict[tag]
            for obj in dict.values():
                if obj.searchone(pattern,attribute):
                    resultlist.append(obj)
        return resultlist

    # Match a pattern in a string-valued attribute. If attribute is None,
    # search all string attributes. If tag is 'dataset', just check the dataset,
    # else check all nodes in the dataset of class type matching the tag. If tag
    # is None, search the dataset and all objects contained in it.
    def matchPattern(self,pattern,attribute,tag):
        resultlist = []
        if tag is not None:
            tag = string.lower(tag)
        if tag in ('dataset',None):
            if self.matchone(pattern,attribute)==1:
                resultlist = [self]
            else:
                resultlist = []
        if tag is None:
            for dict in self.dictdict.values():
                for obj in dict.values():
                    if obj.matchone(pattern,attribute):
                        resultlist.append(obj)
        elif tag!='dataset':
            dict = self.dictdict[tag]
            for obj in dict.values():
                if obj.matchone(pattern,attribute):
                    resultlist.append(obj)
        return resultlist

    # Apply a predicate, returning a list of all objects in the dataset
    # for which the predicate is true. The predicate is a function which
    # takes a dataset as an argument, and returns true or false. If the
    # tag is 'dataset', the predicate is applied to the dataset only.
    # If 'variable', 'axis', etc., it is applied only to that type of object
    # in the dataset. If None, it is applied to all objects, including
    # the dataset itself.
    def searchPredicate(self,predicate,tag):
        resultlist = []
        if tag is not None:
            tag = string.lower(tag)
        if tag in ('dataset',None):
            try:
                if apply(predicate,(self,))==1:
                    resultlist.append(self)
            except AttributeError:
                pass
        if tag is None:
            for dict in self.dictdict.values():
                for obj in dict.values():
                    try:
                        if apply(predicate,(obj,))==1:
                            resultlist.append(obj)
                    except AttributeError:
                        pass
        elif tag!="dataset":
            dict = self.dictdict[tag]
            for obj in dict.values():
                try:
                    if apply(predicate,(obj,))==1:
                        resultlist.append(obj)
                except:
                    pass
        return resultlist

    # Return a sorted list of all data files associated with the dataset
    def getPaths(self):
        pathdict = {}
        for var in self.variables.values():
            for path, stuple in var.getPaths():
                pathdict[path] = 1
        result = pathdict.keys()
        result.sort()
        return result

    # Open a data file associated with this dataset.
    # <filename> is relative to the self.datapath
    # <mode> is the open mode.
    def openFile(self, filename, mode):

        # Opened via a local XML file?
        if self.parent is None:
            path = os.path.join(self.datapath,filename)
            if cdmsobj._debug==1:
                sys.stdout.write(path+'\n'); sys.stdout.flush()
            f = Cdunif.CdunifFile(path,mode)
            return f

        # Opened via a database
        else:
            dburls = self.parent.url
            if type(dburls)!=type([]):
                dburls = [dburls]

            # Try first to open as a local file
            for dburl in dburls:
                if os.path.isabs(self.directory):
                    fileurl = os.path.join(self.directory,filename)
                else:
                    try:
                        fileurl = os.path.join(dburl,self.datapath,filename)
                    except:
                        print 'Error joining',`dburl`,self.datapath,filename
                        raise
                (scheme,netloc,path,parameters,query,fragment)=urlparse.urlparse(fileurl)
                if scheme in ['file',''] and os.path.isfile(path):
                    if cdmsobj._debug==1:
                        sys.stdout.write(fileurl+'\n'); sys.stdout.flush()
                    f = Cdunif.CdunifFile(path,mode)
                    return f

            # See if request manager is being used for file transfer
            db = self.parent
            if db.usingRequestManager():
                cache = db.enableCache()
                lcbase = db.lcBaseDN
                lcpath = self.getLogicalCollectionDN(lcbase)

                # File location is logical collection path combined with relative filename
                fileDN = (self.uri, filename)
                path = cache.getFile(filename, fileDN, lcpath=lcpath, userid=db.userid, useReplica=db.useReplica)
                try:
                    f = Cdunif.CdunifFile(path,mode)
                except:
                    # Try again, in case another process clobbered this file
                    path = cache.getFile(fileurl,fileDN)
                    f = Cdunif.CdunifFile(path,mode)
                return f

            # Try to read via FTP:

            for dburl in dburls:
                fileurl = os.path.join(dburl,self.datapath,filename)
                (scheme,netloc,path,parameters,query,fragment)=urlparse.urlparse(fileurl)
                if scheme=='ftp':
                    cache = self.parent.enableCache()
                    fileDN = (self.uri, filename) # Global file name
                    path = cache.getFile(fileurl,fileDN)
                    try:
                        f = Cdunif.CdunifFile(path,mode)
                    except:
                        # Try again, in case another process clobbered this file
                        path = cache.getFile(fileurl,fileDN)
                        f = Cdunif.CdunifFile(path,mode)
                    return f

            # File not found
            raise FileNotFound, filename

    def getLogicalCollectionDN(self, base=None):
        """Return the logical collection distinguished name of this dataset.
        If <base> is defined, append it to the lc name.
        """
        if hasattr(self, "lc"):
            dn = self.lc
        else:
            dn = "lc=%s"%self.id
        if base is not None:
            dn = "%s,%s"%(dn,base)
        return dn

    def getVariable(self, id):
        "Get the variable object with the given id. Returns None if not found."
        return self.variables.get(id)

    def getVariables(self, spatial=0):
        """Get a list of variable objects. If spatial=1, only return those
        axes defined on latitude or longitude, excluding weights and bounds."""
        retval = self.variables.values()
        if spatial:
            retval = filter(lambda x: x.id[0:7]!="bounds_" and x.id[0:8]!="weights_" and ((x.getLatitude() is not None) or (x.getLongitude() is not None) or (x.getLevel() is not None)), retval)
        return retval

    def getAxis(self, id):
        "Get the axis object with the given id. Returns None if not found."
        return self.axes.get(id)

    def getGrid(self, id):
        "Get the grid object with the given id. Returns None if not found."
        return self.grids.get(id)

    def __repr__(self):
        return "<Dataset: '%s', URI: '%s', mode: '%s', status: %s>"%(self.id, self.uri, self.mode, self._status_)

internattr.add_internal_attribute (Dataset, 'datapath',
                                            'variables',
                                            'axes',
                                            'grids',
                                            'xlinks',
                                            'dictdict',
                                            'default_variable_name',
                                            'parent',
                                            'uri',
                                            'mode')

class CdmsFile(CdmsObj, cuDataset):

    def __init__(self, path, mode):
        CdmsObj.__init__(self, None)
        cuDataset.__init__(self)

        self.id = path
        self._mode_ = mode
        try:
            _fileobj_ = Cdunif.CdunifFile (path, mode)
        except:
            raise CDMSError, 'Cannot open file %s'%path
        self._file_ = _fileobj_   # Cdunif file object
        self.variables = {}
        self.axes = {}
        self.grids = {}
        self.xlinks = {}
        self._gridmap_ = {}

        # self.attributes returns the Cdunif file dictionary. 
        self.replace_external_attributes(self._file_.__dict__)
        self._boundAxis_ = None         # Boundary axis for cell vertices
        if self._mode_=='w':
            self.Conventions = convention.CFConvention.current
        self._status_ = 'open'
        self._convention_ = convention.getDatasetConvention(self)

        # Get lists of 1D and auxiliary coordinate axes
        coords1d = self._convention_.getAxisIds(self._file_.variables)
        coordsaux = self._convention_.getAxisAuxIds(self._file_.variables, coords1d)

        # Build variable list
        for name in self._file_.variables.keys():
            if name not in coords1d:
                cdunifvar = self._file_.variables[name]
                if name in coordsaux:
                    # Put auxiliary coordinate axes with variables, since there may be
                    # a dimension with the same name.
                    if len(cdunifvar.shape)==2:
                        self.variables[name] = FileAxis2D(self, name, cdunifvar)
                    else:
                        self.variables[name] = FileAuxAxis1D(self, name, cdunifvar)
                else:
                    self.variables[name] = FileVariable(self,name,cdunifvar)

        # Build axis list
        for name in self._file_.dimensions.keys():
            if name in coords1d:
                cdunifvar = self._file_.variables[name]
            else:
                cdunifvar = None
            self.axes[name] = FileAxis(self,name,cdunifvar)

        # Attach boundary variables
        for name in coordsaux:
            var = self.variables[name]
            bounds = self._convention_.getVariableBounds(self, var)
            var.setBounds(bounds)

        self.dictdict = {'variable':self.variables, 'axis':self.axes, 'rectGrid':self.grids, 'curveGrid':self.grids, 'genericGrid':self.grids}

        # Initialize variable domains
        for var in self.variables.values():
            var.initDomain(self.axes)

        # Build grids
        for var in self.variables.values():
            # Get grid information for the variable. gridkey has the form
            # (latname,lonname,order,maskname, abstract_class).
            gridkey, lat, lon = var.generateGridkey(self._convention_, self.variables)

            # If the variable is gridded, lookup the grid. If no such grid exists,
            # create a unique gridname, create the grid, and add to the gridmap.
            if gridkey is None:
                grid = None
            else:
                grid = self._gridmap_.get(gridkey)
                if grid is None:

                    if hasattr(var,'grid_type'):
                        gridtype = var.grid_type
                    else:
                        gridtype = "generic"

                    candidateBasename = None
                    if gridkey[4] == 'rectGrid':
                        gridshape = (len(lat),len(lon))
                    elif gridkey[4] == 'curveGrid':
                        gridshape = lat.shape
                    elif gridkey[4] == 'genericGrid':
                        gridshape = lat.shape
                        candidateBasename = 'grid_%d'%gridshape
                    else:
                        gridshape = (len(lat),len(lon))

                    if candidateBasename is None:
                        candidateBasename = 'grid_%dx%d'%gridshape
                    if not self.grids.has_key(candidateBasename):
                        gridname = candidateBasename
                    else:
                        foundname = 0
                        for i in range(97,123): # Lower-case letters
                            candidateName = candidateBasename+'_'+chr(i)
                            if not self.grids.has_key(candidateName):
                                gridname = candidateName
                                foundname = 1
                                break

                        if not foundname:
                            print 'Warning: cannot generate a grid for variable', var.id
                            continue

                    # Create the grid
                    if gridkey[4] == 'rectGrid':
                        grid = FileRectGrid(self, gridname, lat, lon, gridkey[2], gridtype)
                    else:
                        if gridkey[3]!='':
                            if self.variables.has_key(gridkey[3]):
                                maskvar = self.variables[gridkey[3]]
                            else:
                                print 'Warning: mask variable %s not found'%gridkey[3]
                                maskvar = None
                        else:
                            maskvar = None
                        if gridkey[4] == 'curveGrid':
                            grid = FileCurveGrid(lat, lon, gridname, parent=self, maskvar=maskvar)
                        else:
                            grid = FileGenericGrid(lat, lon, gridname, parent=self, maskvar=maskvar)
                    self.grids[grid.id] = grid
                    self._gridmap_[gridkey] = grid

            # Set the variable grid
            var.setGrid(grid)

    # setattr writes external global attributes to the file
    def __setattr__ (self, name, value):
        s = self.get_property_s(name)
        if s is not None:
            print '....handler'
            s(self, name, value)
            return
        self.__dict__[name] =  value #attributes kept in sync w/file
        if not self.is_internal_attribute(name):
            setattr(self._file_, name, value)

    # getattr reads external global attributes from the file
    def __getattr__ (self, name):
        g = self.get_property_g(name)
        if g is not None:
            return g(self, name)
        if self.is_internal_attribute(name):
            try:
                return self.__dict__[name]
            except KeyError:
                raise AttributeError, "%s instance has no attribute %s." % \
                           (self.__class__.__name__, name)
        else:
            return getattr(self._file_,name)

    # delattr deletes external global attributes in the file
    def __delattr__(self, name):
        d = self.get_property_d(name)
        if d is not None:
            d(self, name)
            return
        try:
            del self.__dict__[name]
        except KeyError:
            raise AttributeError, "%s instance has no attribute %s." % \
                  (self.__class__.__name__, name)
        if not self.is_internal_attribute(name):
            delattr(self._file_, name)

    def sync(self):
        if self._status_=="closed":
            raise CDMSError, FileWasClosed + self.id
        self._file_.sync()

    def close(self):
        if self._status_=="closed":
            return
        for dict in self.dictdict.values():
            for obj in dict.values():
                obj.parent = None
                del obj
        self.dictdict = self.variables = self.axes = {}
        self._file_.close()
        self._status_ = 'closed'

##  Note: Removed to allow garbage collection of reference cycles
##     def __del__(self):
##         if cdmsobj._debug==1:
##             print 'Deleting file',self.id
##         # If the object has been deallocated due to open error,
##         # it will not have an attribute .dictdict
##         if hasattr(self,"dictdict") and self.dictdict != {}:
##             self.close()

    # Create an axis
    # 'name' is the string name of the Axis
    # 'ar' is the 1-D data array, or None for an unlimited axis
    # Set unlimited to true to designate the axis as unlimited
    # Return an axis object.
    def createAxis(self,name,ar,unlimited=0):
        if self._status_=="closed":
            raise CDMSError, FileWasClosed + self.id
        cufile = self._file_
        if ar is None or unlimited==1:
            cufile.createDimension(name,None)
            if ar is None:
                typecode = Numeric.Float
            else:
                typecode = ar.typecode()
        else:
            cufile.createDimension(name,len(ar))
            typecode = ar.typecode()
        cuvar = cufile.createVariable(name,typecode,(name,))

        # Cdunif should really create this extra dimension info:
        #   (units,typecode,filename,varname_local,dimension_type,ncid)
        cufile.dimensioninfo[name] = ('',typecode,name,'','global',-1)
        if ar is not None:
            cuvar[0:len(ar)] = MA.filled(ar)
        axis = FileAxis(self,name,cuvar)
        self.axes[name] = axis
        return axis

    def createVirtualAxis(self, name, axislen):
        """Create an axis without any associated coordinate array. This
        axis is read-only. This is useful for the 'bound' axis.
        <name> is the string name of the axis.
        <axislen> is the integer length of the axis.

        Note: for netCDF output, this just creates a dimension without
        the associated coordinate array. On reads the axis will look like
        an axis of type float with values [0.0, 1.0, ..., float(axislen-1)].
        On write attempts an exception is raised.
        """
        if self._status_=="closed":
            raise CDMSError, FileWasClosed + self.id
        cufile = self._file_
        cufile.createDimension(name, axislen)
        cufile.dimensioninfo[name] = ('','f',name,'','global',-1)
        axis = FileVirtualAxis(self, name, axislen)
        self.axes[name] = axis
        return axis

    # Copy axis description and data from another axis
    def copyAxis(self, axis, newname=None, unlimited=0, index=None, extbounds=None):
        if newname is None: newname=axis.id

        # If the axis already exists and has the same values, return existing
        if self.axes.has_key(newname):
            newaxis = self.axes[newname]
            if newaxis.isVirtual():
                if len(axis)!=len(newaxis):
                    raise DuplicateAxisError, DuplicateAxis+newname
            elif unlimited==0:
                if len(axis)!=len(newaxis) or Numeric.alltrue(Numeric.less(Numeric.absolute(newaxis[:]-axis[:]),1.e-5))==0:
                    raise DuplicateAxisError, DuplicateAxis+newname
            else:
                if index is None:
                    isoverlap, index = isOverlapVector(axis[:],newaxis[:])
                else:
                    isoverlap = 1
                if isoverlap:
                    newaxis[index:index+len(axis)] = axis[:]
                    if extbounds is None:
                        axisBounds = axis.getBounds()
                    else:
                        axisBounds = extbounds
                    if axisBounds is not None:
                        newaxis.setBounds(axisBounds)
                else:
                    raise DuplicateAxisError, DuplicateAxis+newname

        elif axis.isVirtual():
            newaxis = self.createVirtualAxis(newname, len(axis))

        # Else create the new axis and copy its bounds and metadata
        else:
            newaxis = self.createAxis(newname, axis[:], unlimited)
            bounds = axis.getBounds()
            if bounds is not None:
                if hasattr(axis, 'bounds'):
                    boundsid = axis.bounds
                else:
                    boundsid = None
                newaxis.setBounds(bounds, persistent=1, boundsid=boundsid)
            for attname,attval in axis.attributes.items():
                if attname not in ["datatype", "id","length","isvar","name_in_file","partition"]:
                    setattr(newaxis, attname, attval)

        return newaxis

    # Create an implicit rectilinear grid. lat, lon, and mask are objects.
    # order and type are strings
    def createRectGrid(self, id, lat, lon, order, type="generic", mask=None):
        grid = FileRectGrid(self, id, lat, lon, order, type, mask)
        self.grids[grid.id] = grid
        gridkey = (lat.id, lon.id, order, None)
        self._gridmap_[gridkey] = grid
        return grid

    # Copy grid
    def copyGrid(self, grid, newname=None):
        if newname is None:
            if hasattr(grid,'id'):
                newname = grid.id
            else:
                newname = 'Grid'

        oldlat = grid.getLatitude()
        if not hasattr(oldlat, 'id'):
            oldlat.id = 'latitude'
        oldlon = grid.getLongitude()
        if not hasattr(oldlon, 'id'):
            oldlon.id = 'longitude'
        lat = self.copyAxis(oldlat)
        lat.designateLatitude(persistent=1)
        lon = self.copyAxis(oldlon)
        lon.designateLongitude(persistent=1)

        # If the grid name already exists, and is the same, just return it
        if self.grids.has_key(newname):
            newgrid = self.grids[newname]
            newlat = newgrid.getLatitude()
            newlon = newgrid.getLongitude()
            if ((newlat is not lat) or
                (newlon is not lon) or
                (newgrid.getOrder() != grid.getOrder()) or
                (newgrid.getType() != grid.getType())):
                raise DuplicateGrid, newname

        # else create a new grid and copy metadata
        else:
            newmask = grid.getMask()    # Get the mask array
            newgrid = self.createRectGrid(newname, lat, lon, grid.getOrder(), grid.getType(), None)
            newgrid.setMask(newmask)    # Set the mask array, non-persistently
            for attname in grid.attributes.keys():
                setattr(newgrid, attname, getattr(grid, attname))

        return newgrid

    # Create a variable
    # 'name' is the string name of the Variable
    # 'datatype' is a CDMS datatype or Numeric typecode
    # 'axesOrGrids' is a list of axes, grids. (Note: this should be
    #   generalized to allow subintervals of axes and/or grids)
    # Return a variable object.
    def createVariable(self,name,datatype,axesOrGrids,fill_value=None):
        if self._status_=="closed":
            raise CDMSError, FileWasClosed + self.id
        cufile = self._file_
        if datatype in CdDatatypes:
            numericType = cdmsNode.CdToNumericType.get(datatype)
        else:
            numericType = datatype

        #dimensions = map(lambda x: x.id, axes)
        # Make a list of names of axes for _Cdunif
        dimensions = []
        for obj in axesOrGrids:
            if isinstance(obj, FileAxis):
                dimensions.append(obj.id)
            elif isinstance(obj, FileRectGrid):
                dimensions = dimensions + [obj.getAxis(0).id, obj.getAxis(1).id]
            else:
                raise InvalidDomain

        try:
            cuvar = cufile.createVariable(name,numericType,tuple(dimensions))
        except:
            raise CDMSError, "Creating variable "+name
        var = FileVariable(self,name,cuvar)
        var.initDomain(self.axes)
        self.variables[name] = var
        if fill_value is not None: var.setMissing(fill_value)
        return var

    # Create a variable from an existing variable, and copy the metadata
##     def createVariableCopy(self, var, newname=None):

##         if newname is None: newname=var.id
##         if self.variables.has_key(newname):
##             raise DuplicateVariable, newname


##         # Create axes if necessary
##         axislist = []
##         for (axis,start,length,true_length) in var.getDomain():
##             try:
##                 newaxis = self.copyAxis(axis)
##             except DuplicateAxisError:

##                 # Create a unique axis name
##                 setit = 0
##                 for i in range(97,123): # Lower-case letters
##                     try:
##                         newaxis = self.copyAxis(axis,axis.id+'_'+chr(i))
##                         setit = 1
##                         break
##                     except DuplicateAxisError:
##                         continue

##                 if setit==0: raise DuplicateAxisError, DuplicateAxis+axis.id

##             axislist.append(newaxis)

##         # Create the new variable
##         datatype = cdmsNode.NumericToCdType.get(var.typecode())
##         newvar = self.createVariable(newname, datatype, axislist)

##         # Copy variable metadata
##         for attname in var.attributes.keys():
##             if attname not in ["id", "datatype"]:
##                 setattr(newvar, attname, getattr(var, attname))

##         return newvar

    # Search for a pattern in a string-valued attribute. If attribute is None,
    # search all string attributes. If tag is 'cdmsFile', just check the dataset,
    # else check all nodes in the dataset of class type matching the tag. If tag
    # is None, search the dataset and all objects contained in it.
    def searchPattern(self,pattern,attribute,tag):
        resultlist = []
        if tag is not None:
            tag = string.lower(tag)
        if tag in ('cdmsFile',None,'dataset'):
            if self.searchone(pattern,attribute)==1:
                resultlist = [self]
            else:
                resultlist = []
        if tag is None:
            for dict in self.dictdict.values():
                for obj in dict.values():
                    if obj.searchone(pattern,attribute):
                        resultlist.append(obj)
        elif tag not in ('cdmsFile','dataset'):
            dict = self.dictdict[tag]
            for obj in dict.values():
                if obj.searchone(pattern,attribute):
                    resultlist.append(obj)
        return resultlist

    # Match a pattern in a string-valued attribute. If attribute is None,
    # search all string attributes. If tag is 'cdmsFile', just check the dataset,
    # else check all nodes in the dataset of class type matching the tag. If tag
    # is None, search the dataset and all objects contained in it.
    def matchPattern(self,pattern,attribute,tag):
        resultlist = []
        if tag is not None:
            tag = string.lower(tag)
        if tag in ('cdmsFile',None,'dataset'):
            if self.matchone(pattern,attribute)==1:
                resultlist = [self]
            else:
                resultlist = []
        if tag is None:
            for dict in self.dictdict.values():
                for obj in dict.values():
                    if obj.matchone(pattern,attribute):
                        resultlist.append(obj)
        elif tag not in ('cdmsFile','dataset'):
            dict = self.dictdict[tag]
            for obj in dict.values():
                if obj.matchone(pattern,attribute):
                    resultlist.append(obj)
        return resultlist

    # Apply a predicate, returning a list of all objects in the dataset
    # for which the predicate is true. The predicate is a function which
    # takes a dataset as an argument, and returns true or false. If the
    # tag is 'cdmsFile', the predicate is applied to the dataset only.
    # If 'variable', 'axis', etc., it is applied only to that type of object
    # in the dataset. If None, it is applied to all objects, including
    # the dataset itself.
    def searchPredicate(self,predicate,tag):
        resultlist = []
        if tag is not None:
            tag = string.lower(tag)
        if tag in ('cdmsFile',None,'dataset'):
            try:
                if apply(predicate,(self,))==1:
                    resultlist.append(self)
            except AttributeError:
                pass
        if tag is None:
            for dict in self.dictdict.values():
                for obj in dict.values():
                    try:
                        if apply(predicate,(obj,))==1:
                            resultlist.append(obj)
                    except AttributeError:
                        pass
        elif tag not in ('dataset','cdmsFile'):
            dict = self.dictdict[tag]
            for obj in dict.values():
                try:
                    if apply(predicate,(obj,))==1:
                        resultlist.append(obj)
                except:
                    pass
        return resultlist

    def createVariableCopy(self, var, id=None, attributes=None, axes=None, extbounds=None,
                              extend=0, fill_value=None, index=None, newname=None, grid=None):
        """Define a new variable, with the same axes and attributes as in <var>.
        This does not copy the data itself.
        Keywords:
        attributes: A dictionary of attributes. Default is var.attributes.
        axes: The list of axis objects. Default is var.getAxisList()
        extbounds: Bounds of the (portion of) the extended dimension being written.
        id or newname: String identifier of the new variable.
        extend: If 1, define the first dimension as the unlimited dimension. If 0, do not define
          an unlimited dimension. The default is the define the first dimension as unlimited
          only if it is a time dimension.
        - fill_value is the missing value flag.
        - index is the extended dimension index to write to. The default index is determined
          by lookup relative to the existing extended dimension.
        grid is the variable grid. If none, the value of var.getGrid() is used.
        """
        if newname is None:
            newname=var.id
        if id is not None:
            newname = id
        if self.variables.has_key(newname):
            raise DuplicateVariable, newname

        # Determine the extended axis name if any
        if axes is None:
            sourceAxislist = var.getAxisList()
        else:
            sourceAxislist = axes

        if var.rank()==0:      # scalars are not extensible
            extend = 0
            
        if extend in (1,None):
            firstAxis = sourceAxislist[0]
            if firstAxis is not None and (extend==1 or firstAxis.isTime()):
                extendedAxis = firstAxis.id
            else:
                extendedAxis = None
        else:
            extendedAxis = None

        # Create axes if necessary
        axislist = []
        for axis in sourceAxislist:
            if extendedAxis is None or axis.id!=extendedAxis:
                try:
                    newaxis = self.copyAxis(axis)
                except DuplicateAxisError:

                    # Create a unique axis name
                    setit = 0
                    for i in range(97,123): # Lower-case letters
                        try:
                            newaxis = self.copyAxis(axis,axis.id+'_'+chr(i))
                            setit = 1
                            break
                        except DuplicateAxisError:
                            continue

                    if setit==0: raise DuplicateAxisError, DuplicateAxis+axis.id
            else:
                newaxis = self.copyAxis(axis, unlimited=1, index=index, extbounds=extbounds)

            axislist.append(newaxis)

        # Create grid as necessary
        if grid is None:
            grid = var.getGrid()
        if grid is not None:
            coords = grid.writeToFile(self)
            if coords is not None:
                coordattr = "%s %s"%(coords[0].id, coords[1].id)
                if attributes is None:
                    attributes = {'coordinates': coordattr}
                else:
                    attributes['coordinates'] = coordattr

        # Create the new variable
        datatype = cdmsNode.NumericToCdType.get(var.typecode())
        newvar = self.createVariable(newname, datatype, axislist)

        # Copy variable metadata
        if attributes is None:
            attributes = var.attributes
        for attname,attval in attributes.items():
            if attname not in ["id", "datatype", "parent"]:
                setattr(newvar, attname, attval)
        if fill_value is not None:
            newvar.setMissing(fill_value)

        return newvar

    def write(self, var, attributes=None, axes=None, extbounds=None, id=None, \
              extend=None, fill_value=None, index=None, typecode=None):
        """Write var to the file. If the variable is not yet defined in the file,
        a definition is created. By default, the time dimension of the variable is defined as the
        'extended dimension' of the file. The function returns the corresponding file variable.

        Keywords:
          - attributes is the attribute dictionary for the variable. The default is var.attributes.
          - axes is the list of file axes comprising the domain of the variable. The default is to
            copy var.getAxisList().
          - extbounds is the extended dimension bounds. Defaults to var.getAxis(0).getBounds()
          - id is the variable name in the file. Default is var.id.
          - extend=1 causes the first dimension to be 'extensible': iteratively writeable.
            The default is None, in which case the first dimension is extensible if it is time.
            Set to 0 to turn off this behaviour.
          - fill_value is the missing value flag.
          - index is the extended dimension index to write to. The default index is determined
            by lookup relative to the existing extended dimension.
        """

        # Make var an AbstractVariable
        if typecode is not None and var.typecode()!=typecode:
            var = var.astype(typecode)
        var = asVariable(var, writeable=0)

        # Define the variable if necessary.
        if id is None:
            varid = var.id
        else:
            varid = id
        if self.variables.has_key(varid):
            v = self.variables[varid]
        else:
            v = self.createVariableCopy(var, attributes=attributes, axes=axes, extbounds=extbounds,
                                           id=varid, extend=extend, fill_value=fill_value, index=index)

        # If var has typecode Numeric.Int, and v is created from var, then v will have
        # typecode Numeric.Int32. (This is a Cdunif 'feature'). This causes a downcast error
        # for Numeric versions 23+, so make the downcast explicit.
        if var.typecode()==Numeric.Int and v.typecode()==Numeric.Int32:
            var = var.astype(Numeric.Int32)

        # Write
        if axes is None:
            sourceAxislist = var.getAxisList()
        else:
            sourceAxislist = axes

        vrank = var.rank()
        if vrank==0:      # scalars are not extensible
            extend = 0
        else:
            vec1 = sourceAxislist[0]
            
        if extend==0 or (extend is None and not vec1.isTime()):
            if vrank>0:
                v[:] = var
            else:
                v.assignValue(var.getValue())
        else:
            # Determine if the first dimension of var overlaps the first dimension of v
            vec2 = v.getAxis(0)
            if extbounds is None:
                bounds1 = vec1.getBounds()
            else:
                bounds1 = extbounds
            if index is None:
                isoverlap, index = isOverlapVector(vec1[:], vec2[:])
            else:
                isoverlap = 1
            if isoverlap==1:
                v[index:index+len(vec1)] = var
                vec2[index:index+len(vec1)] = vec1[:]
                if bounds1 is not None:
                    vec2.setBounds(bounds1, persistent=1, index=index)
            else:
                raise CDMSError,'Cannot write variable %s: the values of dimension %s=%s, do not overlap the extended dimension %s values: %s'%(varid, vec1.id,`vec1[:]`,vec2.id,`vec2[:]`)

        return v

    def getVariable(self, id):
        "Get the variable object with the given id. Returns None if not found."
        return self.variables.get(id)

    def getVariables(self, spatial=0):
        """Get a list of variable objects. If spatial=1, only return those
        axes defined on latitude or longitude, excluding weights and bounds."""
        retval = self.variables.values()
        if spatial:
            retval = filter(lambda x: x.id[0:7]!="bounds_" and x.id[0:8]!="weights_" and ((x.getLatitude() is not None) or (x.getLongitude() is not None) or (x.getLevel() is not None)), retval)
        return retval

    def getAxis(self, id):
        "Get the axis object with the given id. Returns None if not found."
        return self.axes.get(id)

    def getGrid(self, id):
        "Get the grid object with the given id. Returns None if not found."
        return self.grids.get(id)

    def getBoundsAxis(self, n):
        "Get a bounds axis of length n. Create the bounds axis if necessary."
        if n==2:
            boundid = "bound"
        else:
            boundid = "bound_%d"%n
        if self.axes.has_key(boundid):
            boundaxis = self.axes[boundid]
        else:
            boundaxis = self.createVirtualAxis(boundid, n)
        return boundaxis

    def __repr__(self):
        filerep = `self._file_`
        loc = string.find(filerep,"file")
        if loc==-1: loc=0
        return "<CDMS "+filerep[loc:-1]+", status: %s>"%self._status_

internattr.add_internal_attribute (CdmsFile, 'datapath',
                                            'variables',
                                            'axes',
                                            'grids',
                                            'xlinks',
                                            'dictdict',
                                            'default_variable_name',
                                            'id',
                                            'parent',
                                            'mode')
