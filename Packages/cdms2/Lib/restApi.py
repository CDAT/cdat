import cdms2
import httplib
import AutoAPI
import xml.etree.ElementTree

class esgNodeConnectionException(Exception):
    pass
class esgNodeDatasetException(Exception):
    pass
    ## def __init__(self,value):
    ##     self.value=value
    ## def __repr__(self):
    ##     msg =  "rest API error: %s" % repr(value)
    ##     print msg
    ##     return msg
validSearchTypes =  ["","ById","ByTimeStamp"]
class esgNodeConnection(object,AutoAPI.AutoAPI):
    def __init__(self,host,port=80,limit=10,offset=0,drs="%(project).%(institute).%(obs_project).%(instrument).%(time_frequency)"):
        self.autoApiInfo = AutoAPI.Info(self)
        self._http=httplib.HTTPConnection(host,port)
        self.defaultSearchType = ""
        self.ESGObjectException = esgNodeConnectionException
        self.validSearchTypes=validSearchTypes
        all = self._search("cf_variable=crap",searchType="")
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
                for p in params:
                    if p.get("name")=="facet.field":
                        for f in p:
                            self.serverOrder.append(f.text)
                            self.params[f.text]=None

        # For now HARD CODING the server output structure
        self.drs=drs
        ## if order is None:
        ##     self.userOrder = self.serverOrder
        ## else:
        ##     self.userOrder=order
        self.keys = self.params.keys
        self.items = self.params.items
        self.values = self.params.values
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
        self._http.request("GET","/esg-search/ws/rest/search%s/?%s" % (searchType,search))
#        self._http.request("GET","/esg-search/ws/rest/search/?%s" % search)
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
            print msg
            raise self.ESGObjectException(msg)
        if stringType:
            return r
        else:
            return xml.etree.ElementTree.fromstring(r)

    def request(self,stringType=False,**keys):
        search = ""
        params={}

        for k in self.keys():
            if self[k] is not None:
                params[k]=self[k]

        for k in keys.keys():
            if k == "stringType":
                stringType=keys[k]
                continue
            if not k in self.keys():
                raise self.ESGObjectException("Invalid key: %s, valid keys are: %s" % (repr(k),repr(self.params.keys())))
            if keys[k] is not None:
                params[k]=keys[k]

        search = "&".join(map(lambda x : "%s=%s" % (x[0],x[1]), params.items()))
        search=search.replace(" ","%20")
        return self._search(search,stringType=stringType)
    
    def search(self,stringType=False,**keys):
        resp = self.request(stringType,**keys)
        datasets = []
        for r in resp[:]:
            if r.tag=="result":
                ##Ok let's go thru these datasets
                for d in r[:]:
                    for f in d[:]:
                        if f.get("name")=="id":
                            datasets.append(esgNodeDataset(f.text,_http=self._http,limit=self["limit"],offset=self["offset"],drs=self.drs))
                break
        return datasets
            
class esgNodeDataset(esgNodeConnection):
    def __init__(self,myid,host=None,port=80,limit=10,offset=0,drs="%(project).%(institute).%(obs_project).%(instrument).%(time_frequency)",_http=None):
        if host is None and _http is None:
            raise esgNodeDatasetException("You need to pass url of an http connection")
        if _http is not None:
            self._http=_http
        else:
            self._http=httplib.HTTPConnection(host,port)
        self.defaultSearchType="ById"
        self.drs=drs
        self.validSearchTypes=validSearchTypes
        self.ESGObjectException = esgNodeDatasetException
        self.params={"id":myid}

        self.keys = self.params.keys
        self.items = self.params.items
        self.values = self.params.values
        self.files = self.search()
        resp = self.request()
        tags = ["title","url","version","timestamp","score","description","id"]
        for r in resp[:]:
            if r.tag=="result":
                for d in r[0][:]:
                    if d.get("name") in tags:
                        if d.tag=="str":
                            setattr(self,d.get("name"),d.text)
                        elif d.tag=="arr":
                            setattr(self,d.get("name"),d[0].text)
                        elif d.tag=="float":
                            setattr(self,d.get("name"),float(d.text))
                        elif d.tag=="date":
                            ## Convert to cdtime?
                            setattr(self,d.get("name"),d.text)
                        else:
                            setattr(self,d.get("name"),d)

    def search(self,stringType=False,**keys):
        resp = self.request(stringType,**keys)
        if stringType:
            return resp
        files = []
        for r in resp[:]:
            if r.tag=="result":
                for d in r[0][:]:
                    if d.get("name")=="url":
                        ##Ok open the url to figure out the files
                        url = httplib.urlsplit(d.text)
                        tmphttp = httplib.HTTPConnection(url.netloc)
                        tmphttp.request("GET",url.path)
                        r=tmphttp.getresponse().read()
                        files.append(r)

                break
        return files

class esgNodeFile():
    def __init__(self):
        pass
