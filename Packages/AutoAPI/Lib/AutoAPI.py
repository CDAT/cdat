import xml.dom.minidom
import types
import inspect
import optparse
import sys
import cdat_info

class Info:
##     name='Template'
    def __init__(self,parent):
        self.url='http://cdat.sf.net'
        self.doc="""This diagnostic is not documented yet"""
        self.type='class'
        self.programminglanguage="Python"
        self.author="PCMDI's software team"
        self.version= '.'.join(map(str,cdat_info.version()))
        self.codepath = str(type(parent)).split("'")[1]
        self.hide=['go','fromXml','toXml','toDOM','printXml','scanDocString','stripSectionsFromDoc','hide']

class Results:
    doc="""Results from Diagnostics are stored here"""

class Requirements:
    doc="""Requirements informations"""

class Input:
    doc="""Input informations"""
    requirements=Requirements()
    
class AutoAPI:
        
    ## Template for Diagnostic classes to be used for diagnostics
    ## Reusable, implemeted for automod
    def __init__(self,**kw):
        self.autoApiInfo=Info(self)
        self.autoApiInput=Input()
        self.autoApiResults=Results()
        for k in kw.keys():
            setattr(self,k,kw[k])

    def fromXml(self,xmlfile):
        """
        Load an xml info file. Overwrite doc string appropriately
        """
        ## Parse the xml file
        XML=xml.dom.minidom.parse(xmlfile)
        ## Recovers the "diagnostic" 
        d=XML.getElementsByTagName("diagnostic")
        ## Parse the diagnostic infos
        ## Loop and get the corresponding diagnosis
        found=False
        for D in d:
            info=D.getElementsByTagName("info")
            if info==[]:
                continue
            for i in info:
                try:
                    if i.hasAttribute("name"):
                        nm=i.getAttribute("name")
                    else:
                        nm=i.getElementsByTagName("name")[0]
                        nm=self._processXMLValue(nm) # Name must be string...
                        found=True
                        break
                except Exception,err:
                    continue
            if self.autoApiInfo.name==nm:
                break
        info=D.getElementsByTagName("info")
        if info==[]:
            raise "Error XML Diag description does not contain the <info> tag"
        for i in info:
            for c in i.childNodes:
                if c.localName is not None:
                    setattr(self.autoApiInfo,c.localName,self._processXmlValue(c))
        optdict={}
        if hasattr(self,'autoApiInfo'):
            if hasattr(self.autoApiInfo,'doc'):
                optdict=self.scanDocString(self.autoApiInfo.doc,"options")
        ## Parse the Diagnostic Options
        options=D.getElementsByTagName("option")
        for o in options:
            nm=o.getAttribute("name")
            if nm in optdict.keys():
                defval,docst=optdict[nm]
                if defval!='':
                    setattr(self,nm,defval)                    
            else:
                defval=''
                docst=''
            try:
                value = self._processXmlValue(o.getElementsByTagName("defaultValue")[0])
                setattr(self,nm,value)
                defval=value
            except Exception,err:
                pass
            try:
                value = self._processXmlValue(o.getElementsByTagName("value")[0])
                setattr(self,nm,value)
            except Exception,err:
                pass
            try:
                docst = self._processXmlValue(o.getElementsByTagName("doc")[0])
            except Exception,err:
                pass
            optdict[nm]=(defval,docst)
            

            ## Ok now replace the options part
            start=self.autoApiInfo.doc.lower().find('options:::')
            if start!=-1:
                beg=self.autoApiInfo.doc[:start]
                doc=self.autoApiInfo.doc[start+11:]
                end=doc.find(':::')
                if end!=-1:
                    end=doc[end+3:]
                else:
                    end=''
            else:
                beg=self.autoApiInfo.doc
                end=''
            doc='\nOptions:::\n'
            for k in optdict.keys():
                o=optdict[k]
                lns=o[1].split('\n')
                docst=''
                for l in lns:
                    docst+='\t\t'+l.strip()
                doc+='\t%s:: (%s)\n%s\n' % (k,o[0],docst)
            self.autoApiInfo.doc=beg+doc+end

    def _getSections(self,doc):
        """ Try to figure out sections in a doc string so we can create the tags"""
        sections=[]
        sp=doc.split(':::')
        if len(sp)==1:
            return []
        for s in sp:
            if s[-1].strip()!='':
                sections.append(s.strip())
        return sections
    
    def stripSectionsFromDoc(self,doc):
        sp=doc.split(":::")
        dels=[]
        for i in range(len(sp)):
            s=sp[i]
            if s[-1].strip()!='':
                dels.append(sp[i])
                dels.append(sp[i+1])
        for d in dels:
            sp.remove(d)
        return ''.join(sp).strip()
                
        
    def scanDocString(self,doc,section="options"):
        """
        Scan a doc string for a section (followed by 3 ":")
        The section defining the keyword is suposed to end at the next :::
        This function returns a dictionary with keyword, the keyword are taken within the
        delimited section. A keyword is followed by "::" its class inbetween paranthesis ,
        default value (or position if section is "output/input") then come between paranthesis: '(default/pos)', then some documentation
        Input:::
          doc :: (str) (0) Doc string to parse
        :::
        Options:::
          section:: (str) ('options') Section of the doc string to scan/parse
        :::
        Output:::
          dictionary :: (dict) (0) A dictionary containing the keywords as keys and a tuple (default_value, documentation)
        :::
        """
        dict={}
        start=doc.lower().find("%s:::" % section)
        if start==-1:
            return {}
        sp=doc[start:].split('\n')
        ln=sp.pop(0)
        while len(sp)>0 and ln.strip()!=':::': ## loop until end of "section"
            ln=sp.pop(0)
            while len(sp)!=0 and ln.find('::')==-1: ## Look for a "section"
                ln=sp.pop(0)
                continue
            spl=ln.split('::')
            kw=spl[0].strip() # ok the found a "section"
            spl[1]=spl[1].strip()
            if spl[1][0]=='(': ## instance is defined
                end=spl[1][1:].find(')')
                inst=spl[1][1:end+1]
                spl[1]=spl[1][end+2:].strip()
            else:
                inst =''
            if spl[1][0]=='(': ## default val is defined
                end=spl[1][1:].find(')')
                defval=spl[1][1:end+1]
                spl[1]=spl[1][end+2:].strip()
            else:
                defval =''
            doc=spl[1]
            if len(doc)>0 and doc[-1]!='\n':
                doc+='\n'
            ln=sp.pop(0)
            while len(sp)>0 and ln.find('::')==-1:
                doc+=ln+'\n'
                ln=sp.pop(0)
            if ln.find(':::')==-1 and len(sp)!=0:
                sp.insert(0,ln)
            dict[kw]=(inst.strip(),defval.strip(),doc)
        return dict
    
    def _toDOMElement(self,tagname,value):
        st='<tmp>'
        st='<tmp><%s>%s</%s></tmp>' % (tagname,repr(value),tagname)
        return xml.dom.minidom.parseString(st).childNodes[0].childNodes[0]
    def _toDOMElements(self,obj,doc,element,wrap_options=False,used=[]):
        att=inspect.getmembers(obj)
        optdict={}
        outdict={}
        explist=[]
        hidelist=[]
        if hasattr(obj,'autoApiInfo'):
            if hasattr(obj.autoApiInfo,'doc'):
                optdict=self.scanDocString(obj.autoApiInfo.doc,"options")
                outdict=self.scanDocString(obj.autoApiInfo.doc,"output")
            if hasattr(obj.autoApiInfo,'expose'):
                explist=obj.autoApiInfo.expose
            if hasattr(obj.autoApiInfo,'hide'):
                hidelist=obj.autoApiInfo.hide
            for attr in dir(obj.autoApiInfo):
                if attr[0]!='_' and attr not in ['expose','doc','hide']:
                    element.setAttribute(attr,str(getattr(obj.autoApiInfo,attr)))
        try:
            docst=obj.__call__.__doc__
##             print 'found:',docst
        except Exception,err:
            docst=self.__doc__
        if docst is not None:
            tmpdict=self.scanDocString(docst,"options")
            for k in tmpdict.keys():
                optdict[k]=tmpdict[k]
            tmpdict=self.scanDocString(docst,"output")
            for k in tmpdict.keys():
                outdict[k]=tmpdict[k]
        for o in outdict.keys():
            d=doc.createElement('output')
            d.setAttribute('name',o)
            try:
                d.setAttribute('type',repr(eval(outdict[o][0])))
            except:
                d.setAttribute('type',repr(outdict[o][0]))
            outst=outdict[o][1]
            t=doc.createTextNode(outst)
            d.appendChild(t)
            element.appendChild(d)
            
        for o in optdict.keys():
            tmp=doc.createElement('option')
            tmp.setAttribute('name',o)
            tmp.setAttribute('defaultvalue',optdict[o][1])
            tmp.setAttribute('instance',optdict[o][0])
            tmp.setAttribute('doc',optdict[o][2].strip())
            tmp.setAttribute('value',repr(getattr(self,o)))
            element.appendChild(tmp)


        for m in used:
            while m in used:
                att.remove(m)
        delatt=[]
        for m in att:
            if (not m[0] in explist) and (explist!=['ALL']) or (m[0] in hidelist):
                delatt.append(m)
        for m in delatt:
            att.remove(m)
        for m in att:
            used.append(m)
##             counter+=1
##             print m[0],counter,'--------------------'
            if m[0][0]=='_':
                continue
            tmp=doc.createElement(m[0]) # Creates the element
            if isinstance(m[1],(types.MethodType)):
                tmp=doc.createElement('action')
                tmp.setAttribute('name',m[0])
                tmp.setAttribute('type','function')
                docst=m[1].__doc__
                if docst is not None:
                    sections = self._getSections(docst)
                    for s in sections:
                        sdict = self.scanDocString(docst,s.lower())
                        d=doc.createElement(s.lower())
                        for k in sdict.keys():
                            o=sdict[k]
                            tmp2=doc.createElement(k)
                            tmp2.setAttribute("doc",o[2].strip())
                            if o[0]!='':
                                tmp2.setAttribute("instance",str(o[0]).strip())
                            if o[1]!='':
                                if s.lower() in ["output","input"]:
                                    tmp2.setAttribute("position",str(o[1]).strip())
                                else:
                                    tmp2.setAttribute("default",str(o[1]).strip())
                            d.appendChild(tmp2)
                        tmp.appendChild(d)
                    d=doc.createElement('doc')
                    t=doc.createTextNode(self.stripSectionsFromDoc(docst))
                    d.appendChild(t)
                    tmp.appendChild(d)
                    ## Now the output section
                    for o in outdict.keys():
                        d=doc.createElement('output')
                        d.setAttribute('name',o)
                        d.setAttribute('type',outdict[o][0])
                        outst=outdict[o][1]
                        t=doc.createTextNode(outst)
                        d.appendChild(t)
                        tmp.appendChild(d)
                element.appendChild(tmp)
            elif isinstance(m[1],(str,dict,list,tuple,int,float,unicode)) and not m[0] in optdict.keys():
                txt = doc.createTextNode(str(m[1]))
                if wrap_options:
                    tmp=doc.createElement('option')
                    tmp.setAttribute('name',str(m[0]).strip())
                    if m[0] in optdict.keys():
                        o=optdict[m[0]]
                        d=doc.createElement('doc')
                        t=doc.createTextNode(o[1])
                        d.appendChild(t)
                        tmp.appendChild(d)
                        if o[0]!='':
                            tmp.setAttribute('defaultValue',o[0])
##                             d=doc.createElement('defaultValue')
##                             t=doc.createTextNode(o[0])
##                             d.appendChild(t)
##                             tmp.appendChild(d)
##                     d=doc.createElement('value')
##                     d.appendChild(txt)
##                     tmp.appendChild(d)
##                 else:
##                     tmp.appendChild(txt)
                tmp.setAttribute('value',repr(m[1]))
                element.appendChild(tmp)
            else:
                try:
                    tmp = self._toDOMElements(m[1],doc,tmp,used=used)
                    element.appendChild(tmp)
                except Exception,err:
##                     print err
                    pass
        return element
        
    def toDOM(self):
        """
        Parse the class for option and functions and returns a DOM object describing the class
        """
        impl = xml.dom.minidom.getDOMImplementation()
        doc = impl.createDocument(None, "diagnostic", None)
        diag = doc.documentElement
        diag = self._toDOMElements(self,doc,diag,wrap_options=True,used=[])
        return diag

    def toXml(self):
        """
        Parse the class for option and functions and returns a XML string describing the class
        """
        return self.toDOM().toprettyxml()

    def printXml(self,*args,**kargs):
        """
        Simply print to the screen the result of toXml() function
        """
        print self.toXml()
        return
        
    def _processXmlValue(self,Node):
        try:
            type=Node.attributes.get('type','string').value.lower()
        except Exception, err:
            type='string'
        node=None
        for n in Node.childNodes:
            if not self._isEmptyNode(n):
                if node is None: # We could think of exiting here and ignoring further defs
                    node=n
                else:
                    raise "Error Multiple Value Definition within Value Tag"
        
        if node is None:
            return None
        if node.nodeType==node.TEXT_NODE:
            result=node.data.strip()
            ## Now process the eventual "type" definition
            if type=='list':
                sp=result.split()
                result=[]
                for s in sp:
                    if s[0] not in ['"',"'"]:
                        r=s.replace(',','')
                    try:
                        result.append(eval(r))
                    except:
                        result.append(r)
            else:
                try:
                    result=eval(result)
                except:
                    pass
            
        elif node.nodeType==node.ELEMENT_NODE:
            if node.localName=='enum':
                node.setAttribute('type','list')
                result=self._processXmlValue(node)
        else:
            raise "Error: At the moment we do not not how to deal with nodes of type:"+str(node.nodeType)
        return result

    def _isEmptyNode(self,node):
        if not node.nodeType==node.TEXT_NODE:
            return False
        elif len(node.data.strip())==0:
            return True
        return False

    def _makeOptParser(self):
        """
        Automagically generates an option parser for your class, so you can run your script
        with --option where option is any attribute your class has. (excluding function and element
        starting with "-"
        simply add the following lines at the end of the script:
        and run the script with --help option to see availaible option (with doc if classe is correctly documented.
if __name__=='__main__':
    D=Diagnosis() # Or whatever your inherited class name is
    parser=D._makeOptParser()
    (options, args) = parser.parse_args()
        """
        try:
            DOM=self.toDOM()
            DOM.normalize()
            opts=DOM.getElementsByTagName('option')
        except Exception,err:
            docst=self.__doc__
        op=optparse.OptionParser()
        op.add_option('--toXml',action='callback',callback=self.printXml,help=self.toXml.__doc__)
        for k in opts:
            nm=k.getAttribute("name")
            defval=k.getAttribute("value")
            try:
                defval=eval(defval)
            except:
                pass
            o=k.getElementsByTagName("doc")
            if len(o)>0:
                doc=o[0].childNodes[0].data
            else:
                doc=''
            op.add_option("--"+str(nm),dest=str(nm),help=str(doc).strip(),default=defval)
        return op

    def go(self):
        """
        Parse the command line arguments, sets the option and execute default function
        pass --help option to see available options
        Simply add the following lines at the end of the script:
        and run the script with --help option to see availaible option (with doc if classe is correctly documented. The result from parsing is stored in self.autoApiInput.[keyword] if not default, the parser is stored in self.autoApiInfo.autoApiInput._parser in case you need it again
if __name__=='__main__':
    D=Diagnosis() # Or whatever your inherited class name is
    D.go()       
        """
        self.autoApiInput._parser=self._makeOptParser()
        opts,args = self.autoApiInput._parser.parse_args()
##         print opts.tonetcdf
##         sys.exit()
        for option in self.autoApiInput._parser.defaults.keys():
##             print option,
            try:
                setattr(self,option,eval(getattr(opts,option)))
            except:
                setattr(self,option,getattr(opts,option))
##             print ' - ',getattr(self,option)
        self()
        return        

## if __name__=='__main__':
##     import sys
##     D=Diagnostic2()
##     D.fromXml('Diagnosis/test.xml')
##     if '--toXml' in sys.argv:
##         print D.toXml()
##     op=D._makeOptParser()
##     (options, args) = op.parse_args()
