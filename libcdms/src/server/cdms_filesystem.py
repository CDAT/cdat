# CDMS filesystem

from filesys import abstract_filesystem
import cdms, cdms_templates
import os, os.path
import string

class string_file:
    "a string that looks like a file"

    def __init__(self, s):
        self.string = s
        self.position = 0

    def read(self, nbytes):
        if self.position>=len(self.string):
            data = ''
        else:
            newposition = self.position+nbytes
            data = self.string[self.position : newposition]
            self.position = newposition
        return data

    def close(self):
        pass

class cdms_filesystem(abstract_filesystem):
    "CDMS filesystem abstraction"

    def __init__ (self, server, root):
        self.server = server
        self.root = root
        self.objcache = {}
        self.stringcache = {}           # Cache responses between stat and open

    def open (self, path, mode='r'):
        "Return an open file object"

        template = self.generateResponse(path)
        # Delete the cached object so that the cache doesn't fill up.
        # This is a hack: it works because open is called only once
        # after some stat calls.
        if self.stringcache.has_key(path):
            del self.stringcache[path]
        return string_file(template)

    def stat (self, path):
        import stat, time
        "Return the equivalent of os.stat() on the given path."

        result = [0,0,0,0,0,0,0,0,0,0]

        template = self.generateResponse(path)
        result[stat.ST_SIZE] = len(template)
        result[stat.ST_MTIME] = int(time.time()) # Now
        return result

    def isdir (self, path):
        "Does the path represent a directory?"

        return 0                        # Always return a file

    def isfile (self, path):
        "Does the path represent a plain file?"

        return 1                        # Everything is a file

    def generateResponse(self, path):

        if self.stringcache.has_key(path):
            template = self.stringcache[path]
        else:
            method, obj = self.pathMethod(path)
            if method=='_cdmsOverview':
                template = obj.overview()
            elif method=='_cdmsIndex':
                template = obj.index()
            elif method=='_cdmsDisplay':
                template = obj.display()
            elif method=='_cdmsFilelist':
                template = obj.filelist()
            elif method=='_cdmsPlot.gif':
                template = obj.plot()
            elif method=='_cdmsDJF.gif':
                import averagers
                template = obj.plot_one_average(averagers.djf_average)
            elif method=='_cdmsMAM.gif':
                import averagers
                template = obj.plot_one_average(averagers.mam_average)
            elif method=='_cdmsJJA.gif':
                import averagers
                template = obj.plot_one_average(averagers.jja_average)
            elif method=='_cdmsSON.gif':
                import averagers
                template = obj.plot_one_average(averagers.son_average)
            elif method=='_cdmsSEASONS.gif':
                import averagers
                template = obj.plot_four_views (averagers.djf_average, averagers.mam_average,
                                                averagers.jja_average, averagers.son_average)
            elif method=='_cdmsAnimation.gif':
                template = obj.animation()
            self.stringcache[path] = template
        return template

    def pathMethod(self, path):

        methods = ['_cdmsOverview', '_cdmsIndex', '_cdmsDisplay', '_cdmsFilelist','_cdmsPlot.gif','_cdmsAnimation.gif', '_cdmsDJF.gif','_cdmsMAM.gif','_cdmsJJA.gif','_cdmsSON.gif','_cdmsSEASONS.gif']

        fields = string.split(string.strip(os.path.normpath(path)),os.sep)
        if fields[0]=='':
            fields = fields[1:]

        method = fields[-1]
        if method in methods:
            fields = fields[:-1]
        else:
            method = '_cdmsOverview'

        metapath = apply(os.path.join,[os.sep]+fields)
        obj = self.objcache.get(metapath)

        if obj is None:
            nfields = len(fields)
            if nfields==2:
                dbid = fields[1]
                obj = self.getfsdb(metapath,dbid)
            elif nfields==3:
                dbid = fields[1]
                dsetid = fields[2]
                obj = self.getfsdset(metapath, dbid, dsetid)
            elif nfields==4:
                dbid = fields[1]
                dsetid = fields[2]
                varid = fields[3]
                obj = self.getfsvar(metapath, dbid, dsetid, varid)

        return method, obj

    def getfsdb(self, metapath, dbid):
        dbname = "database=%s,%s"%(dbid,self.root)
        dbpath = os.path.join(self.server, dbname)
        print 'Connecting to',dbpath
        db = cdms.connect(dbpath)
        obj = cdms_templates.FSDatabase(db, metapath, dbid)
        self.objcache[metapath] = obj
        return obj

    def getfsdset(self, metapath, dbid, dsetid):
        head, tail = os.path.split(metapath)
        dbobj = self.objcache.get(head)
        if dbobj is None:
            dbobj = self.getfsdb(metapath, dbid)
        db = dbobj.db
        print 'Opening',metapath
        dset = db.openDataset(dsetid)
        obj = cdms_templates.FSDataset(dset, metapath)
        self.objcache[metapath] = obj
        return obj

    def getfsvar(self, metapath, dbid, dsetid, varid):
        head, tail = os.path.split(metapath)
        dsetobj = self.objcache.get(head)
        if dsetobj is None:
            dsetobj = self.getfsdset(metapath, dbid, dsetid)
        dset = dsetobj.dset
        var = dset.variables[varid]
        obj = cdms_templates.FSVariable(var, metapath)
        self.objcache[metapath] = obj
        return obj
