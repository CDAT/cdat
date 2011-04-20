import cdms2
import httplib
import AutoAPI
import xml.etree.ElementTree
import genutil

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
    def __init__(self,host,port=80,timeout=15,limit=100000000,offset=0,mapping=None,datasetids=None,fileids=None,restPath=None):
        self.autoApiInfo = AutoAPI.Info(self)
        self._http=httplib.HTTPConnection(host,port,timeout=timeout)
        if restPath is None:
            self.restPath="/esg-search/ws/rest/search"
        else:
            self.restPath=restPath
        self.defaultSearchType = "Dataset"
        self.ESGObjectException = esgfConnectionException
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
        if fileids is not None:
            self.fileids=genutil.StringConstructor(fileids)
            if datasetids is not None:
                self.fileids.template=self.fileids.template.replace("%(datasetid)",self.datasetids.template)
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
            raise self.ESGObjectException("Invalid key: %s" % repr(key))
        return val
    def __setitem__(self,key,value):
        if not key in self.params.keys():
            raise self.ESGObjectException("Invalid key: %s, valid keys are: %s" % (repr(key),repr(self.params.keys())))
        self.params[key]=value
        return

                            
    def _search(self,search="",searchType=None,stringType=False):
        if searchType is None:
            searchType=self.defaultSearchType
        if not searchType in self.validSearchTypes:
            raise self.ESGObjectException("Valid Search types are: %s" % repr(self.validSearchTypes))
        while search[0]=="&":
            search=search[1:]
        if self._http.host[:7]=="uv-cdat":
            rqst = "/search.xml"
        if self._http.host[:7]=="uv-cdat":
            rqst = "/search.xml"
        else:
            rqst = "%s/?type=%s&%s" % (self.restPath,searchType,search)
        #print "REQUEST: %s%s" % (self._http.host,rqst)
        self._http.request("GET",rqst)
        response = self._http.getresponse()
        r= response.read()
        if not response.status == httplib.OK:
            i = r.find("<body>")
            r=r[i+6:]
            i = r.find("<b>description</b>")
            r=r[i+18:]
            j=r.find("</p>")
            r=r[:j]
            msg = "receive http error (%i:%s), Tomcat message: %s" % (response.status,httplib.responses[response.status],r)
            #print msg
            raise self.ESGObjectException(msg)
        if stringType:
            return r
        else:
            return xml.etree.ElementTree.fromstring(r)

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
            ##     raise self.ESGObjectException("Invalid key: %s, valid keys are: %s" % (repr(k),repr(self.params.keys())))
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
        
    def search(self,**keys):
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
                        #print datasetid
                        #print "KEYS FOR DATASET",keys.keys()
                        datasets.append(esgfDataset(_http=self._http,limit=self["limit"],offset=self["offset"],mapping=self.mapping,datasetids=self.datasetids,fileids=self.fileids,keys=tmpkeys,originalKeys=keys,restPath=self.restPath))
        return datasets

class esgfDataset(esgfConnection):
    def __init__(self,host=None,port=80,limit=1000000,offset=0,mapping=None,datasetids=None,fileids=None,_http=None,restPath=None,keys={},originalKeys={}):
        if host is None and _http is None:
            raise esgfDatasetException("You need to pass url of an http connection")
        if _http is not None:
            self._http=_http
        else:
            self._http=httplib.HTTPConnection(host,port)
        self.defaultSearchType="File"
        if restPath is None:
            self.restPath="/esg-search/ws/rest/search"
        else:
            self.restPath=restPath
        if datasetids is None:
            datasetids=""
        if isinstance(datasetids,genutil.StringConstructor):
            self.datasetids=datasetids
        else:
            self.datasetids=genutil.StringConstructor(datasetids)
        if fileids is not None:
            if isinstance(fileids,genutil.StringConstructor):
                self.fileids=fileids
            else:
                self.fileids=genutil.StringConstructor(fileids)
            if self.datasetids is not None:
                self.fileids.template=self.fileids.template.replace("%(datasetid)",self.datasetids.template)

        self.originalKeys=originalKeys
        self.validSearchTypes=validSearchTypes
        self.validSearchTypes=["File",]
        self.ESGObjectException = esgfDatasetException
        self.params=keys
        self.keys = self.params.keys
        self.items = self.params.items
        self.values = self.params.values
        self.setMapping(mapping)
        self.id=self["id"]
        self.params["limit"]=limit
        self.params["offset"]=offset
        #print "SEARCHING DS:",originalKeys
        self.resp=None
        self.search()
#        self.remap()
        
        ## Ok now we need to "map" this according to the user wishes

    def setMapping(self,mapping):
        if mapping is None:
            self.mapping=""
            for k in self.keys():
                self.mapping+="%%(%s)" % k
        else:
            self.mapping=mapping
        #print "Stage 1 mapping:",self.mapping
        if not isinstance(self.mapping,genutil.StringConstructor):
            if self.datasetids is not None:
                self.mapping=self.mapping.replace("%(datasetid)",self.datasetids.template)
            self.mapping = genutil.StringConstructor(self.mapping)
        #print "Stage 2:",self.mapping.template,self.keys()

        for k in self.mapping.keys():
            ok = False
            if self.datasetids is not None:
                if k in self.datasetids.keys():
                    ok = True
            if self.fileids is not None:
                if k in self.fileids.keys():
                    ok = True
            if k in self.keys():
                ok=True
            if ok is False:
                raise self.ESGObjectException("Invalid mapping key: %s" % k)
            
    def remap(self,mapping=None):
        if mapping is None:
            thismapping = self.mapping
        self.mapped={}
        #print "################ REMAPPING: %s: %s #############################" % (thismapping.template,repr(thismapping.keys()))
        for f in self.files:
            mappoint=self.mapped
            tabs=""
            nok=0
            nlevels = len(thismapping.keys())
            for k in thismapping.keys():
                #print tabs,"keys:",k,"File keys:",f.keys()
                ## if k == self.mapping.keys()[0]:
                ##     f.matched.keys()
                ## else:
                ##     print
                if k in f.keys():
                    #print tabs,k,f[k]
                    nok+=1
                    cont = f[k]
                    if not f[k] in mappoint.keys():
                        mappoint[f[k]]={}
                elif k in self.keys():
                    #print tabs,k,f[k]
                    nok+=1
                    cont = self[k]
                    if not self[k] in mappoint.keys():
                        mappoint[self[k]]={}
                else:
                    break
                mappoint=mappoint[cont]
                tabs+="\t"
            tmp = mappoint.get("files",[])
            tmp.append(f)
            mappoint["files"] = tmp
        #print "################ REMAPPED: %s #############################" % (thismapping,)
           

    def mappedItems():
        mapped=[]
        mapppoint=self.mapped
        for k in self.mapping.keys():
            keys=[]
            level=[k,mappoint.keys()]
            mappoint
    def _extractFiles(self,resp,**inKeys):
        ## We need to stick in there the bit from Luca to fill in the matching key from facet for now it's empty
        files=[]
        skipped = ["type","title","timestamp","service","id","score","file_url","service_type"]
        nfiles = 0
        nta = 0
        for r in resp[:]:
            if r.tag=="result":
                for d in r[:][:]:
                    keys={}
                    for f in d[:]:
                        k = f.get("name")
                        keys[k]=self.extractTag(f)
                    if keys["type"]=="File":
                        nfiles+=1
                        ## if keys["variable"]=="ta":
                        ##     verbose=False
                        ##     nta+=1
                        ## else:
                        ##     verbose=False
                        ## if verbose: print "OK",keys["variable"],keys["file_id"],self["id"]
                        #if verbose: print "FILEIDS:",self.fileids
                        if self.fileids is not None:
                            try:
                                #if verbose: print "file:",keys["file_id"],self.fileids.template
                                k2 = self.fileids.reverse(keys["file_id"])
                                #if verbose: print "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@",k2
                                for k in k2.keys():
                                    keys[k]=k2[k]
                            except:
                                #if verbose: print "Failed:",ids[i].text,self.fileids.template
                                pass
                        ## if verbose: print "KEYS FOR FILE:",keys.keys()
                        ## if verbose: print "INKEYS:",inKeys.keys()
                        matched = True
                        matchWithKeys = {}
                        for k in self.keys():
                            if k in self.originalKeys.keys():
                                matchWithKeys[k]=self.originalKeys[k]
                            else:
                                matchWithKeys[k]=self[k]
                        for s in skipped:
                            matchWithKeys.pop(s)
                        for k in inKeys.keys():
                            matchWithKeys[k]=inKeys[k]
                        for k in keys.keys():
                            if k in matchWithKeys.keys():
                                #if verbose: print "Testing:",k,keys[k]
                                v = matchWithKeys[k]
                                if isinstance(v,(str,int,float)):
                                    ## if verbose: print "\tComparing with:",v
                                    if v != keys[k]:
                                        matched = False
                                        ## if verbose: print "\t\tNOPE"
                                        break
                                elif isinstance(v,list):
                                    #if verbose: print "\tComparing with (and %i more):%s"%(len(v),v[0])
                                    if not keys[k] in v:
                                        matched = False
                                        #if verbose: print "\t\tNOPE"
                                        break
                                else:
                                    print "\twould compare %s with type: %s if I knew how to" % (str(v),type(v))
                        #if verbose: print keys["file_id"],matched
                        if matched :
                            for k in self.keys():
                                if not k in keys.keys():
                                    keys[k]=self[k]
                            files.append(esgfFile(**keys))
        #print "We found:",nfiles,"files on dataset",self["id"],nta,"of them have ta"
        ##         break
        ## fileId=None
        ## files=[]
        ## for i in range(len(services)):
        ##     if ids[i].text!=fileId:
        ##         if fileId is not None:
        ##             files.append(esgfFile(fileId,furls,fservices,keys))
        ##         fileId=ids[i].text
        ##         furls=[urls[i].text,]
        ##         fservices=[services[i].text,]
        ##         keys={}
        ##         if self.fileids is not None:
        ##             try:
        ##                 keys = self.fileids.reverse(ids[i].text)
        ##             except:
        ##                 #print "Failed:",ids[i].text,self.fileids.template
        ##                 pass
        ##     else:
        ##         furls.append(urls[i].text)
        ##         fservices.append(services[i].text)
        ## files.append(esgfFile(fileId,furls,fservices,keys))
        return files
            
    def info(self):
        print self

    def __str__(self):
        st = "Dataset Information\nid: %s\nKeys:\n" % self.id
        for k in self.keys():
            st+="\t%s : %s\n" % (k,self[k])
        return st
        

    
    def search(self,**keys):
        #search = self.generateRequest(**keys)
        if self.resp is None:
            self.resp = self._search("parent_id=%s&limit=%s&offset=%s" % (self["id"],self["limit"],self["offset"]))
        stringType=keys.get("stringType",False)
        if stringType:
            return self.resp
        self.files = esgfFiles(self._extractFiles(self.resp,**keys))
        self.remap()


class esgfFiles(object,AutoAPI.AutoAPI):
    def __init__(self,files):
        self._files=files
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

class esgfFile(object,AutoAPI.AutoAPI):
    def __init__(self,**keys):
        self.autoApiInfo = AutoAPI.Info(self)
        self.__items__=keys
        self.keys = self.__items__.keys
        self.items = self.__items__.items
        self.values = self.__items__.values

        services=[]
        S=self["service"]
        if isinstance(S,str):
            S=[S,]
        for service in S:
            s1,s2,url = service.split("|")
            setattr(self,s1,url)
            services.append(s1)
        self.services=services
        self.id=self["file_id"]

    def __getitem__(self,key):
        val = self.__items__[key]
        return val
    
    def __setitem__(self,key,value):
        self.__items__[key]=value
        return

    def __str__(self):
        st = "File Information\nid: %s\nParent Dataset: %s" % (self["file_id"],self["parent_id"])
        st+="Matched keys: %s\n" % (repr(self.__items__))
        for service in self.services:
            st+="service: %s @ %s\n" % (service,getattr(self,service))
        return st[:-1]
