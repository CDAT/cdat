import cdms2
import urllib2
import AutoAPI
import xml.etree.ElementTree
import genutil
import os
#import bz2

class esgfConnectionException(Exception):
    pass
class esgfDatasetException(Exception):
    pass
class esgfFilesException(Exception):
    pass
    ## def __init__(self,value):
    ##     self.value=value
    ## def __repr__(self):
    ##     msg =  "rest API error: %s" % repr(value)
    ##     print msg
    ##     return msg
validSearchTypes =  ["Dataset","File"]#"ById","ByTimeStamp"]
class esgfConnection(object,AutoAPI.AutoAPI):
    def __init__(self,host,port=80,timeout=15,limit=1000,offset=0,mapping=None,datasetids=None,fileids=None,restPath=None):
        self.autoApiInfo = AutoAPI.Info(self)
        self.port=port
        url=str(host).replace("://","^^^---^^^")
        sp= url.split("/")
        host = sp[0].replace("^^^---^^^","://")
        if restPath is None:
            restPath = "/".join(sp[1:])
            if len(restPath)==0:
                self.restPath="/esg-search/search"
            else:
                self.restPath=restPath
        else:
            self.restPath=restPath
        self.host=host
        self.defaultSearchType = "Dataset"
        self.EsgfObjectException = esgfConnectionException
        self.validSearchTypes=validSearchTypes
        self.validSearchTypes=["Dataset",]
        all = self._search("cf_variable=crap",searchType=None)
        ## Now figure out the facet fields
        self.serverOrder = []
        for e in all:
            if e.tag=="lst" and "name" in e.keys() and e.get("name")=="responseHeader":
                ## ok found the Header
                for s in e:
                    if s.get("name")=="params":
                        params=s
                        break
                self.params={"text":None,"limit":limit,"offset":offset}
                self.searchableKeys=set(["text","limit","offset"])
                for p in params:
                    if p.get("name")=="facet.field":
                        for f in p:
                            self.serverOrder.append(f.text)
                            self.params[f.text]=None
                            self.searchableKeys.add(f.text)

        self.keys = self.params.keys
        self.items = self.params.items
        self.values = self.params.values

        if datasetids is not None:
            self.datasetids=genutil.StringConstructor(datasetids)
        else:
            self.datasetids=None
        if fileids is not None:
            self.fileids=genutil.StringConstructor(fileids)
            if datasetids is not None:
                self.fileids.template=self.fileids.template.replace("%(datasetid)",self.datasetids.template)
        elif self.datasetids is not None:
            self.fileids=genutil.StringConstructor("%s.%%(filename)" % self.datasetids.template)
        else:
            self.fileids=None
        #self.setMapping(mapping)
        self.mapping=mapping                    
            
    ## def setUserOrder(self,value):
    ##     self.userOrder=value
    ## def getUserOrder(self):
    ##     return self.userOrder
    ## order=property(getUserOrder,setUserOrder)
    def __getitem__(self,key):
        try:
            val = self.params[key]
        except:
            raise self.EsgfObjectException("Invalid key: %s" % repr(key))
        return val
    def __setitem__(self,key,value):
        if not key in self.params.keys():
            raise self.EsgfObjectException("Invalid key: %s, valid keys are: %s" % (repr(key),repr(self.params.keys())))
        self.params[key]=value
        return

                            
    def _search(self,search="",searchType=None,stringType=False):
        if searchType is None:
            searchType=self.defaultSearchType
        if not searchType in self.validSearchTypes:
            raise self.EsgfObjectException("Valid Search types are: %s" % repr(self.validSearchTypes))
        while search[0]=="&":
            search=search[1:]
        rqst = "%s/?type=%s&%s" % (self.restPath,searchType,search)
        #print "REQUEST: %s%s" % (self.host,rqst)
        if self.host.find("://")>-1:
            urltype=""
        else:
            urltype="http://"
        try:
            rqst="%s%s:%s/%s" % (urltype,self.host,self.port,rqst)
            tmp=rqst[6:].replace("//","/")
            rqst=rqst[:6]+tmp
            #print "Request:%s"%rqst
            url = urllib2.urlopen(rqst)
        except Exception,msg:
             raise self.EsgfObjectException(msg)
        r = url.read()
        if stringType:
            return r
        else:
            try:
                e = xml.etree.ElementTree.fromstring(r)
                return e
            except Exception,err:
                raise self.EsgfObjectException("Could not interpret server's results: %s" % err)

    def generateRequest(self,stringType=False,**keys):
        search = ""
        params={"limit":self["limit"],"offset":self["offset"]}

        ## for k in self.keys():
        ##     if self[k] is not None and k in self.searchableKeys and k!="type":
        ##         params[k]=self[k]

        
        for k in keys.keys():
            if k == "stringType":
                stringType=keys[k]
                continue
            elif k == "type":
                continue
            ## elif not k in self.searchableKeys:
            ##     raise self.EsgfObjectException("Invalid key: %s, valid keys are: %s" % (repr(k),repr(self.params.keys())))
            if keys[k] is not None:
                params[k]=keys[k]

        search = ""
        for k in params.keys():
            if isinstance(params[k],list):
                for v in params[k]:
                    if isinstance(v,str):
                        v=v.strip()
                    search+="&%s=%s" % (k,v)
            else:
                v = params[k]
                if isinstance(v,str):
                    v=v.strip()
                search+="&%s=%s" % (k,v)

#        search = "&".join(map(lambda x : "%s=%s" % (x[0],x[1]), params.items()))
        search=search.replace(" ","%20")
        return search
    
    def request(self,**keys):
        search = self.generateRequest(**keys)
        stringType=keys.get("stringType",False)
        return self._search(search,stringType=stringType)
    
    def extractTag(self,f):
        out=None
        if f.tag=="str":
            out=f.text
        elif f.tag=="arr":
            out=[]
            for sub in f[:]:
                out.append(self.extractTag(sub))
            
        elif f.tag=="float":
            out = float(f.text)
        elif f.tag=="int":
            out = int(f.text)
        elif f.tag=="date":
            ## Convert to cdtime?
            out =f.text
        else:
            out=f
        if isinstance(out,list) and len(out)==1:
            out=out[0]
        return out
        
    def searchDatasets(self,**keys):
        resp = self.request(**keys)
        stringType=keys.get("stringType",False)
        if stringType:
            return resp
        datasets = []
        for r in resp[:]:
            if r.tag=="result":
                ##Ok let's go thru these datasets
                for d in r[:]:
                    #print "************************************************"
                    tmpkeys={}
                    for f in d[:]:
                        k = f.get("name")
                        tmpkeys[k]=self.extractTag(f)
                    if tmpkeys["type"]=="Dataset":
                        datasetid = tmpkeys["id"]
                        #print datasetid,self.restPath
                        #print "KEYS FOR DATASET",keys.keys()
                        datasets.append(esgfDataset(host=self.host,port=self.port,limit=self["limit"],offset=self["offset"],mapping=self.mapping,datasetids=self.datasetids,fileids=self.fileids,keys=tmpkeys,originalKeys=keys,restPath=self.restPath))
        return datasets

class esgfDataset(esgfConnection):
    def __init__(self,host=None,port=80,limit=1000,offset=0,mapping=None,datasetids=None,fileids=None,_http=None,restPath=None,keys={},originalKeys={}):
        if host is None:
            raise esgfDatasetException("You need to pass url")
        self.host=host
        self.port=port
        self.defaultSearchType="File"
        if restPath is None:
            self.restPath="/esg-search/search"
        else:
            self.restPath=restPath
        if datasetids is None:
            if "dataset_id_template_" in keys:
                tmp=keys["dataset_id_template_"]
                if tmp[:5]=="cmip5":
                    tmp = tmp.replace("valid_institute","institute")
                    tmp="%(project)"+tmp[5:]
                self.datasetids = genutil.StringConstructor(tmp.replace(")s",")"))
            elif "project" in keys and keys["project"]=="cmip5":
                self.datasetids = genutil.StringConstructor("%(project).%(product).%(institute).%(model).%(experiment).%(time_frequency).%(realm).%(cmor_table).%(ensemble)")
            else:
                self.datasetids=None
        if isinstance(datasetids,genutil.StringConstructor):
            self.datasetids=datasetids
        elif isinstance(datasetids,str):
            self.datasetids=genutil.StringConstructor(datasetids)
        if fileids is not None:
            if isinstance(fileids,genutil.StringConstructor):
                self.fileids=fileids
            else:
                self.fileids=genutil.StringConstructor(fileids)
            if self.datasetids is not None:
                self.fileids.template=self.fileids.template.replace("%(datasetid)",self.datasetids.template)
        elif self.datasetids is not None:
            self.fileids=genutil.StringConstructor("%s.%%(filename)" % self.datasetids.template)
        else:
            self.fileids=None
        self.originalKeys=originalKeys
        self.validSearchTypes=validSearchTypes
        self.validSearchTypes=["File",]
        self.EsgfObjectException = esgfDatasetException
        self.params=keys
        self.keys = self.params.keys
        self.items = self.params.items
        self.values = self.params.values
        self.id=self["id"]
        self.params["limit"]=limit
        self.params["offset"]=offset
        self.mapping=mapping
        #print "SEARCHING DS:",originalKeys
        self.resp=None
        self.cacheTime = None
#        self.search()
#        self.remap()
        
        ## Ok now we need to "map" this according to the user wishes

           

    ## def mappedItems():
    ##     mapped=[]
    ##     mapppoint=self.mapped
    ##     for k in self.mapping.keys():
    ##         keys=[]
    ##         level=[k,mappoint.keys()]
    ##         mappoint
    def _extractFiles(self,resp,**inKeys):
        ## We need to stick in there the bit from Luca to fill in the matching key from facet for now it's empty
        files=[]
        skipped = ["type","title","timestamp","service","id","score","file_url","service_type"]
        for r in resp[:]:
            if r.tag=="result":
                for d in r[:][:]:
                    keys={}
                    for f in d[:]:
                        k = f.get("name")
                        keys[k]=self.extractTag(f)
                    if keys["type"]=="File":
                        ## if self["id"]=="obs4MIPs.NASA-JPL.AIRS.mon":
                        ##     verbose=True
                        ## else:
                        ##     verbose=False
                        ## #verbose=True
                        ## if verbose: print "OK",keys["variable"],keys["file_id"],self["id"]
                        ## if verbose: print "FILEIDS:",self.fileids
                        ## if verbose: print "Fileids:",self.fileids.template
                        ## if verbose: print "keys:",keys
                        ## if self.fileids is not None:
                        ##     try:
                        ##         if verbose: print "file:",keys["file_id"],self.fileids.template
                        ##         k2 = self.fileids.reverse(keys["file_id"])
                        ##         if verbose: print "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@",k2
                        ##         for k in k2.keys():
                        ##             keys[k]=k2[k]
                        ##     except:
                        ##         if verbose: print "Failed:",ids[i].text,self.fileids.template
                        ##         pass
                        ## if verbose: print "KEYS FOR FILE:",keys.keys()
                        ## if verbose: print "INKEYS:",inKeys.keys()
                        ## matched = True
                        ## matchWithKeys = {}
                        ## for k in self.keys():
                        ##     if k in self.originalKeys.keys():
                        ##         matchWithKeys[k]=self.originalKeys[k]
                        ##     else:
                        ##         matchWithKeys[k]=self[k]
                        ## for s in skipped:
                        ##     try:
                        ##         matchWithKeys.pop(s)
                        ##     except:
                        ##         pass
                        ## for k in inKeys.keys():
                        ##     matchWithKeys[k]=inKeys[k]
                        ## if verbose: print "matching:",matchWithKeys.keys()
                        ## for k in keys.keys():
                        ##     if k in matchWithKeys.keys():
                        ##         if verbose: print "Testing:",k,keys[k]
                        ##         v = matchWithKeys[k]
                        ##         if isinstance(v,(str,int,float)):
                        ##             if verbose: print "\tComparing with:",v
                        ##             if v != keys[k]:
                        ##                 matched = False
                        ##                 if verbose: print "\t\tNOPE"
                        ##                 break
                        ##         elif isinstance(v,list):
                        ##             if verbose: print "\tComparing with (and %i more):%s"%(len(v),v[0]),v
                        ##             if not keys[k] in v:
                        ##                 matched = False
                        ##                 if verbose: print "\t\tNOPE"
                        ##                 break
                        ##         else:
                        ##             print "\twould compare %s with type: %s if I knew how to" % (str(v),type(v))
                        ## if verbose: print keys["file_id"],matched
                        ## if matched :
                        ##     for k in self.keys():
                        ##         if not k in keys.keys():
                        ##             keys[k]=self[k]
                        ##     print "KEYS:",keys
                            files.append(esgfFile(**keys))
        return files
            
    def info(self):
        print self

    def __str__(self):
        st = "Dataset Information\nid: %s\nKeys:\n" % self.id
        for k in self.keys():
            st+="\t%s : %s\n" % (k,self[k])
        return st
        
    def clearWebCache(self):
        self.resp = None

    def saveCache(self,target="."):
        if self.resp is None:
            return
        if os.path.isdir(target):
            target = os.path.join(target,"esgfDatasetsCache.pckl")
        if os.path.exists(target):
            f=open(source)
            #dict=eval(bz2.decompress(f.read()))
	    dict=eval(f.read())
            f.close()
        else:
            dict={}
        dict[self.id]=[self["timestamp"],xml.etree.ElementTree.tostring(self.resp),self.originalKeys]
        f=open(target,"w")
        #f.write(bz2.compress(repr(self.cache)))
        f.write(repr(self.cache))
        f.close()
        
    def loadCache(self,source):
        if isinstance(source,dict):
            dict=source
        else:
            if os.path.isdir(source):
                source = os.path.join(source,"esgfDatasetsCache.pckl")
            if os.path.exists(source):
                f=open(source)
                #dict=eval(bz2.decompress(f.read()))
                dict=eval(f.read())
                f.close()
            else:
                dict={}
        vals = dict.get(self.id,["",None,{}])
        if vals[1] is not None:
            self.cacheTime=vals[0]
            self.resp=xml.etree.ElementTree.fromstring(vals[0])
            self.originalKeys=vals[1]
        
    def clearOriginalQueryCache(self):
        self.originalKeys={}

    def clear(self):
        self.clearWebCache()
        self.clearOriginalQueryCache()
    
    def search(self,**keys):
        #search = self.generateRequest(**keys)
        stringType=keys.get("stringType",False)
        keys.update(self.originalKeys)
        st=""
        for k in keys.keys():
            if k in ["searchString","stringType",]:
                continue
            st+="&%s=%s" % (k,keys[k])
        if self.resp is None:
            self.resp = self._search("dataset_id=%s&limit=%s&offset=%s%s" % (self["id"],self["limit"],self["offset"],st),stringType=stringType)
        if stringType:
            return self.resp
        return esgfFiles(self._extractFiles(self.resp,**keys),self)


class esgfFiles(object,AutoAPI.AutoAPI):
    def __init__(self,files,parent,mapping=None,datasetids=None,fileids=None):
        self._files=files
        if not isinstance(parent,esgfDataset):
            raise esgfFilesException("parent must be an esgfDataset instance")
        self.parent=parent
        self.EsgfObjectException = esgfFilesException
        if datasetids is None:
            datasetids=parent.datasetids
        if isinstance(datasetids,genutil.StringConstructor):
            self.datasetids=datasetids
        elif isinstance(datasetids,str):
            self.datasetids=genutil.StringConstructor(datasetids)
        else:
            self.datasetids=None
        if fileids is not None:
            if isinstance(fileids,genutil.StringConstructor):
                self.fileids=fileids
            else:
                self.fileids=genutil.StringConstructor(fileids)
            if self.datasetids is not None:
                self.fileids.template=self.fileids.template.replace("%(datasetid)",self.datasetids.template)
        elif self.datasetids is not None:
            self.fileids=genutil.StringConstructor("%s.%%(filename)" % self.datasetids.template)
        else:
            self.fileids=parent.fileids
        if mapping is None:
            mapping=parent.mapping
        self.setMapping(mapping)
        self.remap()
        
    def __getitem__(self,item):
        if isinstance(item,int):
            return self._files[item]
        elif isinstance(item,str):
            for f in self._files:
                if f["id"]==item:
                    return f
        elif isinstance(item,slice):
            return self._files[item]
        else:
            raise esgfFilesException("unknown item type: %s" % type(item))
    def __setitem__(self,item):
        raise esgfFilesException("You cannot set items")
    def __len__(self):
        return len(self._files)
    def setMapping(self,mapping):
        if mapping is None:
            self.mapping=""
            if self.datasetids is not None:
                self.mapping=self.datasetids
            else:
                for k in self.parent.keys():
                    self.mapping+="%%(%s)" % k
        else:
            self.mapping=mapping
        #print "Stage 1 mapping:",self.mapping
        if not isinstance(self.mapping,genutil.StringConstructor):
            if self.datasetids is not None:
                self.mapping=self.mapping.replace("%(datasetid)",self.datasetids.template)
            self.mapping = genutil.StringConstructor(self.mapping)
        #print "Stage 2:",self.mapping.template,self.keys()

        vk = self.parent.keys()
        for k in self.mapping.keys():
            ok = False
            if self.datasetids is not None:
                vk += self.datasetids.keys()
                if k in self.datasetids.keys():
                    ok = True
            if self.fileids is not None:
                vk+=self.fileids.keys()
                if k in self.fileids.keys():
                    ok = True
            if k in self.parent.keys():
                ok=True
            ## Ok second to last hope... Matching to datasetids
            if isinstance(self.datasetids,genutil.StringConstructor) and ok is False:
                try:
                    mapid = self.datasetids.reverse(self.parent.id)
                    vk+=mapid.keys()
                    if k in mapid.keys():
                        ok = True
                        
                except:
                    #print "Couldn't map: %s to %s" % (self.parent.id,self.datasetids.template)
                    pass
            if ok is False:
                vk = set(vk)
                raise self.EsgfObjectException("Invalid mapping key: %s, valid keys are: %s" % (k,sorted(vk)))
            
    def remap(self,mapping=None,verbose=False):
        if mapping is None:
            thismapping = self.mapping
        self.mapped={}
        ## if verbose: print "################ REMAPPING: %s: %s #############################" % (thismapping.template,repr(thismapping.keys()))
        for f in self._files:
            mappoint=self.mapped
            tabs=""
            nok=0
            nlevels = len(thismapping.keys())
            for k in thismapping.keys():
                ## if verbose: print tabs,"keys:",k,"File keys:",f.keys()
                ## if k == self.mapping.keys()[0]:
                ##     f.matched.keys()
                ## else:
                ##     ## if verbose: print
                if k in f.keys():
                    ## if verbose: print tabs,k,f[k]
                    nok+=1
                    cont = f[k]
                    if not isinstance(cont,(str,int,float)):
                        break
                    if not cont in mappoint.keys():
                        mappoint[cont]={}
                elif k in self.parent.keys():
                    ## if verbose: print tabs,k,f[k]
                    nok+=1
                    cont = self[k]
                    if not cont in mappoint.keys():
                        mappoint[cont]={}
                elif isinstance(self.fileids,genutil.StringConstructor):
                    try:
                        mapid = self.fileids.reverse(self.parent.id)
                        ## if verbose:
                            ## print "MAPID:",k,mapid
                        if k in mapid.keys():
                            ## if verbose: print tabs,k,mapid[k]
                            nok+=1
                            cont = mapid[k]
                            if not cont in mappoint.keys():
                                mappoint[cont]={}
                    except:
                        break
                else:
                    break
                mappoint=mappoint[cont]
                tabs+="\t"
            tmp = mappoint.get("files",[])
            tmp.append(f)
            mappoint["files"] = tmp
        ## if verbose: print "################ REMAPPED: %s #############################" % (thismapping,)

class esgfFile(object,AutoAPI.AutoAPI):
    def __init__(self,**keys):
        self.autoApiInfo = AutoAPI.Info(self)
        self.__items__=keys
        self.keys = self.__items__.keys
        self.items = self.__items__.items
        self.values = self.__items__.values

        services=[]
        #print "Keys:",self.keys()
        #print self["url"]
        S=self["url"]
        if isinstance(S,str):
            S=[S,]
        for service in S:
            url,s2,s1 = service.split("|")
            setattr(self,s1,url)
            services.append(s1)
        self.services=services
        self.id=self["id"]

    def __getitem__(self,key):
        val = self.__items__[key]
        return val
    
    def __setitem__(self,key,value):
        self.__items__[key]=value
        return

    def __str__(self):
        st = "File Information\nid: %s\nParent Dataset: %s" % (self["id"],self["dataset_id"])
        st+="Matched keys: %s\n" % (repr(self.__items__))
        for service in self.services:
            st+="service: %s @ %s\n" % (service,getattr(self,service))
        return st[:-1]
