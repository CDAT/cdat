"""CDMS database objects"""

from error import CDMSError
import cdmsobj
import cdurlparse
## import internattr
import copy
import os
import re
import string
import sys
import types
from CDMLParser import CDMLParser
from cdmsobj import CdmsObj
from dataset import Dataset

try:
    import ldap
    Subtree = ldap.SCOPE_SUBTREE            # Search object and tree below
    Onelevel = ldap.SCOPE_ONELEVEL          # Search object and one level below
    Base = ldap.SCOPE_BASE                  # Search object only
except ImportError:
    Base = 0
    Onelevel = 1
    Subtree = 2

AuthenticationError = "Error authenticating to database"
CannotOpenDataset = "Cannot open dataset"
ConnectError = "Error connecting to database"
DatabaseNotFound = "Database not found"
InvalidEntryName = "Invalid entry name"
MethodNotImplemented = "Method not yet implemented"
PermissionError = "No permission to access"
SchemeNotSupported = "Scheme not supported"

_Att = re.compile('([a-zA-Z_:][-a-zA-Z0-9._:]*)=(.*)',re.DOTALL)

# Open a database connection
def connect(uri=None, user="", password=""):
    """
    Method:

      connect(uri=None, user="", password="")

    Description:

      Open a CDMS database connection.

    Arguments:

      uri: Universal Resource Identifier. If unspecified, defaults to the environment variable CDMSROOT.
      user: user id
      password: password

    Returns:

      Database instance.

    Example:

      db = cdms.connect("ldap://dbhost.llnl.gov/database=CDMS,ou=PCMDI,o=LLNL,c=US")
    """
    if uri is None:
        try:
            uri = os.environ['CDMSROOT']
        except KeyError:
            raise CDMSError, ConnectError + '%s\nSet environment variable CDMSROOT to default database location'%uri
    (scheme,netloc,path,parameters,query,fragment)=cdurlparse.urlparse(uri)

    if scheme in ['','ldap']:
        try:
            ldapdb = ldap.open(netloc)
        except:
            raise CDMSError, ConnectError +"%s\n%s"%(uri,sys.exc_value)

        try:
            ldapdb.simple_bind_s(user,password)
        except:
            raise CDMSError, AuthenticationError + "%s\n%s"%(uri,sys.exc_value)

        try:
            result = ldapdb.search_s(path[1:], ldap.SCOPE_SUBTREE, "objectclass=database")
        except:
            raise CDMSError, DatabaseNotFound + "%s\n%s"%(uri,sys.exc_value)

        try:
            dn, attrs = result[0]
        except:
            raise CDMSError, PermissionError + uri
        newuri = "ldap://%s/%s"%(netloc,dn)
        db = LDAPDatabase(newuri, ldapdb)
        db.setExternalDict(attrs)
        return db

    else:
        raise CDMSError, SchemeNotSupported +  scheme

def loadString(text, uri, parent=None, datapath=None):
    """ Create a dataset from a text string. <text> is the string in CDML format.
        <uri> is the URL of the dataset in a catalog or file.
        <parent> is the containing database object, if any.
        <datapath> is the location of data files relative to the parent database URL.
    """
    p=CDMLParser()
    p.feed(text)
    p.close()
    return Dataset(uri,'r',p.getRoot(),parent,datapath)
    

class AbstractDatabase(CdmsObj):
    """AbstractDatabase defines the common database interface. Concrete database classes are 
       derived from this class.
    """

    def __init__(self, uri, path):
        CdmsObj.__init__(self,None)
        self.uri = uri
        self.path = path
        self._cache_ = {}
        self._cdmlcache_ = {}
        self._datacache_ = None # datasetdn: obj # Remote file data cache
        self.lcBaseDN = None            # Logical Collection base distinguished name
        self.useReplica = None          # Use replica catalog if true (request manager transfers only)
        self.userid = None              # User ID for request manager transfers

    def close(self):
        raise CDMSError, MethodNotImplemented

    def cachecdml(self, name, cdml):
        raise CDMSError, MethodNotImplemented

    def getDataset(self, name):
        raise CDMSError, MethodNotImplemented

    def getObjFromDataset(self, name):
        raise CDMSError, MethodNotImplemented

    def openDataset(self, dsetid, mode='r'):
        raise CDMSError, MethodNotImplemented

    def searchFilter(self, filter, classtag=None, relbase=None, scope=Subtree, attnames=[]):
        raise CDMSError, MethodNotImplemented

    def enableCache(self):
        if self._datacache_ is None:
            import cache
            self._datacache_ = cache.Cache()
        return self._datacache_

    def disableCache(self):
        if self._datacache_ != None:
            self._datacache_.delete()
            self._datacache_ = None

    def useRequestManager(self, lcBaseDN, useReplica=1, userid = "anonymous"):
        import cache
        self.enableCache()
        cache.useRequestManagerTransfer()
        self.lcBaseDN = lcBaseDN
        self.useReplica = useReplica
        self.userid = userid

    def usingRequestManager(self):
        import cache
        return (cache._transferMethod==cache._requestManagerTransfer)

    def __repr__(self):
        return "<Database '%s'>"%(self.uri)

## internattr.add_internal_attribute(AbstractDatabase, 'uri', 'path')

# Database implemented via LDAP (Lightweight Directory Access Protocol)
class LDAPDatabase(AbstractDatabase):

    def __init__(self, uri, db):
        (scheme,netloc,path,parameters,query,fragment)=cdurlparse.urlparse(uri)
        AbstractDatabase.__init__(self,uri,path[1:])
        self.netloc = netloc
        self.db = db

    def close(self):
        """
        Method:

          close()

        Description:

          Close a database connection.

        Returns:

          None

        """
        if self.db != None:
            self.db.unbind()
        self.db = None
        self.disableCache()
        
    def __del__(self):
        # if cdmsobj._debug==1:
        #    print 'Deleting object',self
        self.close()
    
    def normalizedn(self, dn):
        explodeddn = ldap.explode_dn(dn)
        return string.join(explodeddn,',')

    def cachecdml(self, name, cdml, datapath):
        normaldn = self.normalizedn(name)
        self._cdmlcache_[normaldn] = (cdml,datapath)

    def getDataset(self, dn):
        normaldn = self.normalizedn(dn)
        if self._cache_.has_key(normaldn):
            dataset = self._cache_[normaldn]
        elif self._cdmlcache_.has_key(normaldn):
            (text,datapath) = self._cdmlcache_[normaldn]
            uri = "ldap://%s/%s"%(self.netloc,normaldn)
            if cdmsobj._debug==1:
                print 'Loading %s from cached CDML'%uri
            dataset = loadString(text,uri,self,datapath)
            self._cache_[normaldn] = dataset
        else:
            if cdmsobj._debug==1:
                print 'Search filter: (objectclass=dataset), scope: base, base: "%s", attributes=["cdml"]'%(dn,)
            result = self.db.search_s(dn, ldap.SCOPE_BASE, "objectclass=dataset",["cdml","datapath"])
            resultdn,attrs = result[0]
            text = attrs["cdml"][0]
            uri = "ldap://%s/%s"%(self.netloc,normaldn)
            datapath = attrs.get("datapath")
            if datapath: datapath = datapath[0]
            dataset = loadString(text,uri,self,datapath)
            self._cache_[normaldn] = dataset
        return dataset
 
    def getObjFromDataset(self, dn):

        # Get the parent dataset
        explodeddn = ldap.explode_dn(dn)
        dsetdn = string.join(explodeddn[1:],',') # Dataset node is parent of variable
        dset = self.getDataset(dsetdn)
        rdn = explodeddn[0]
        matchobj = _Att.match(rdn)
        if matchobj is None:
            raise CDMSError, InvalidEntryName +  dn
        tag, id = matchobj.groups()

        # Get the correct dictionary for this tag
        dict = dset.dictdict[tag]
        obj = dict[id]
        return obj

    def openDataset(self, dsetid, mode='r'):
        """
        Method:

          openDataset(dsetid, mode='r')

        Description:

          Open a dataset.

        Arguments:

          dsetid: string dataset identifier
          mode: open mode ('r' - read-only, 'r+' - read-write, 'w' - create)

        Returns:

          Dataset instance.

        Example:

          dset = db.openDataset('ncep_reanalysis_mo')
        """
        dn = "dataset=%s,%s"%(dsetid,self.path)
        dset = self.getDataset(dn)
        return dset

    open = openDataset

    # Set the database attributes from an LDAP search result.
    # ldapattrs is a dictionary, keyed on attribute name.
    # Values are lists of attribute values.
    def setExternalDict(self, ldapattrs):
        for attname in ldapattrs.keys():
            attvals = ldapattrs[attname]
            if attname=='objectclass':
                continue
            elif attname=='attr':       # Handle attr: name=value
                for attval in attvals:
                    matchobj = _Att.match(attval)
                    if matchobj is not None:
                        newname,newval = matchobj.groups()
                        self.attributes[newname] = newval

            # If the attribute value is a multi-valued list, keep it as a list
            # otherwise copy the single value from the list.
            if len(attvals)==1:
                self.attributes[attname] = attvals[0]
            else:
                self.attributes[attname] = attvals

    def searchFilter(self, filter=None, tag=None, relbase=None, scope=Subtree, attnames=None, timeout=None):
        """
        Method:

          searchFilter(filter=None, tag=None, relbase=None, scope=Subtree, attnames=None, timeout=None)

        Description:

          Search a CDMS database.

        Arguments:

          filter: string search filter
            Simple filters have the form "tag = value". Simple filters can be combined using
            logical operators '&', '|', '!' in prefix notation. For example,
            the filter '(&(objectclass=variable)(id=cli))' finds all variables named cli.

            More formally:

              filter     ::= "(" filtercomp ")"
              filtercomp ::= "&" filterlist | # and
                             "|" filterlist | # or
                             "!" filterlist | # not
                             simple
              filterlist ::= filter | filter filterlist
              simple     ::= tag op value
              op         ::= "=" |      # equality
                             "~=" |     # approximate equality
                             "<=" |     # lexicographically less than or equal to
                             ">="       # lexicographically greater than or equal to
              value      ::= string, may include '*' as a wild card
                             
          tag: string class tag ("dataset" | "variable" | "database" | "axis" | "grid").
            Restricts the search to a class of objects
          relbase: string search base, relative to the database path
          scope: search scope (Subtree | Onelevel | Base). Subtree searches the base object and its descendants.
            Onelevel searches the base object and its immediate descendants. Base searches the base object alone.
            Default is Subtree.
          attnames: list of attribute names. Restricts the attributes returned.
          timeout: integer number of seconds before timeout.

        Returns:

          SearchResult instance. Entries can be accessed sequentially. For each entry, entry.name is the
          name of the entry, entry.attributes is a dictionary of the attributes returned by the search,
          entry.getObject() returns the CDMS object associated with the entry:

          for entry in result:
            print entry.name, entry.attributes["id"]

          Entries can be refined with searchPredicate().

        Example:

        (1) Find all variables named "cli":

          result = db.searchFilter(filter="id=cli",tag="variable")

        (2) Find all objects in dataset "ncep_reanalysis_mo":

          result = db.searchFilter(relbase="dataset=ncep_reanalysis_mo"), scope=cdms.Onelevel)

        """
        if tag is None: tag='*'
        newfilter = "(objectclass=%s)"%tag
        if filter is not None:
            if filter[0]!='(':
                filter = "(%s)"%filter
            newfilter = "(&%s%s)"%(newfilter,filter)

        if relbase is None:
            base = self.path
        else:
            base = "%s,%s"%(relbase,self.path)

        if attnames is None:
            atts = None
        else:
            atts = ["objectclass","cdml","id"]+attnames
            
        if cdmsobj._debug==1:
            print 'Search filter:%s, scope %s, base: "%s", attributes=%s'%(newfilter,`scope`,base,`atts`)
        if timeout is None:
            result = self.db.search_s(base, scope, newfilter, atts)
        else:
            result = self.db.search_s(base, scope, newfilter, atts, 0, timeout)
        searchResult = LDAPSearchResult(self, result)
        return searchResult

    def listDatasets(self):
        """ Return a list of the dataset IDs in this database."""
        entries = self.searchFilter(tag='dataset', scope=Onelevel )
        result = map(lambda x: x.attributes['id'][0], entries)
        return result

## internattr.add_internal_attribute(LDAPDatabase, 'netloc', 'db')

class AbstractSearchResult:

    def __getitem__(self, key):
        MethodNotImplemented = "Method not yet implemented"

    def __len__(self):
        MethodNotImplemented = "Method not yet implemented"

    def searchPredicate(self, predicate, tag=None):
        MethodNotImplemented = "Method not yet implemented"

class LDAPSearchResult(AbstractSearchResult):

    def __init__(self, db, LDAPresult):
        self.db = db
        self.result = LDAPresult

        # Scan the result for CDML attributes, cache them in the database
        for dn, attrs in self.result:
            if attrs.has_key('cdml') and attrs.has_key('datapath'):
                cdml = attrs['cdml'][0]
                datapath = attrs['datapath'][0]
                self.db.cachecdml(dn,cdml,datapath)
                del attrs['cdml']

    def __getitem__(self, key):
        if key>=len(self):
            raise IndexError, 'index out of bounds'

        dn, attributes = self.result[key]

        return LDAPResultEntry(self.db, dn, attributes)

    def searchPredicate(self, predicate, tag=None):
        """
        Method:

          searchPredicate(predicate, tag=None)

        Description:

          Refine a search result, with a predicate search.

        Arguments:

          predicate: Function name or lambda function. The function takes a single CDMS object,
            and returns true (1) if the object satisfies the predicate, 0 if not.
          tag: Restrict the search to objects in one class.

        Returns:

          SearchResult instance. Entries can be accessed sequentially. For each entry, entry.name is the
          name of the entry, entry.attributes is a dictionary of the attributes returned by the search,
          entry.getObject() returns the CDMS object associated with the entry:

          for entry in result:
            print entry.name, entry.attributes["id"]

          Entries can be refined with searchPredicate().

        Example:

        (1) Find all variables on a 73x96 grid

          newresult = result.searchPredicate(lambda obj: obj.getGrid().shape==(73,96),"variable")

        """
        if tag is not None:
            tag = string.lower(tag)

        resultlist = []
        for entry in self:
            obj = entry.getObject()
            if tag is None or tag==entry.tag:
                try:
                    if apply(predicate,(obj,))==1:
                        resultlist.append((entry.name,entry.attributes))
                except:
                    pass

        return LDAPSearchResult(self.db, resultlist)

    def __len__(self):
        return len(self.result)

class AbstractResultEntry:

    def __init__(self, db):
        self.db = db

    def getObject(self):
        """
        Method:

          getObject()

        Description:

          Get the CDMS object associated with this entry.

        Returns:

          Instance of a CDMS object.

        """

        if self.tag=="database":
            obj = self.db
        elif self.tag=="dataset":
            obj = self.db.getDataset(self.name)
        else:
            obj = self.db.getObjFromDataset(self.name)

        return obj

class LDAPResultEntry(AbstractResultEntry):

    def __init__(self, db, dn, attributes):
        AbstractResultEntry.__init__(self, db)
        self.name = dn
        self.attributes = attributes

        # Get the tag
        explodeddn = ldap.explode_dn(dn)
        rdn = explodeddn[0]
        matchobj = _Att.match(rdn)
        if matchobj is None:
            raise IndexError, InvalidEntryName + dn

        self.tag = matchobj.group(1)



