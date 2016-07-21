"""
Parse a CDML/XML file
"""

from cdxmllib import XMLParser
import CDML
import re
import cdmsNode
import string

# Error constants
InvalidAttribute = "Invalid attribute"

# Regular expressions
_S = re.compile('[ \t\r\n]+$')
_opS = '[ \t\r\n]*'
_Integer = re.compile(_opS+'[0-9]+$'+_opS)

class CDMLParser(XMLParser):

    def __init__(self, verbose=0):
	XMLParser.__init__(self)
        self.root = None
        self.currentPath = []         # Current path, a stack
        self.dtd = CDML.CDML().dtd
        self.verbose = verbose

    # Push current node on the stack
    def pushCurrentNode(self,node):
        self.currentPath.append(node)
        if not self.root: self.root = node

    # Pop the current node off the stack
    def popCurrentNode(self):
        node = self.currentPath[-1]
        self.currentPath = self.currentPath[:-1]
        return node

    # Get the current parent node
    def getCurrentNode(self):
        return self.currentPath[-1]

    # Get the root node
    def getRoot(self):
        return self.root

    # Handle content
    # discard data which is just whitespace,
    # and strip other data
    def handle_data(self, data):
        matchObj = _S.match(data)
        if not matchObj:
            if self.verbose: print 'data:',data
            if self.root:
                self.getCurrentNode().setContentFromString(string.strip(data))

    def handle_cdata(self, data):
        if self.verbose: print 'cdata:', `data`

    def handle_proc(self, name, data):
        if self.verbose: print 'processing:',name,`data`

    def handle_special(self, data):
        if self.verbose: print 'special:',`data`

    def handle_starttag(self, tag, method, attrs):

        if self.dtd.has_key(tag):

            # Check that attributes are valid
	    validDict = self.dtd[tag]
            validAttrs = validDict.keys()
            attrnames = attrs.keys()
            for attrname in attrnames:
                if attrname not in validAttrs:
                    self.cdml_syntax_error(self.lineno,
                                      'unknown attribute %s of element %s' %
                                      (attrname, tag))
                else:
                    (atttype,attdefault)=validDict[attrname]
                    if type(atttype)==type((0,)):
                        attrval = attrs[attrname]
                        if attrval not in atttype:
                            self.cdml_syntax_error(self.lineno,
                                              'invalid attribute value %s=%s of element %s, must be one of %s' %
                                              (attrname,attrval,tag,atttype))

            # Check that required attributes are present,
            # and add default values
            for attrname in validAttrs:
                (atttype,attdefault)=validDict[attrname]
                if attdefault==CDML.Required and attrname not in attrnames:
                    self.cdml_syntax_error(self.lineno,
                                      'element %s requires an attribute %s' %
                                      (tag,attrname))
                if type(attdefault)==type("") and attrname not in attrnames:
                    attrs[attrname]=attdefault
            
	method(attrs)

    #------------------------------------------------------------------------
    # CDML tags

    def start_attr(self,attrs):
        if self.verbose: print 'attr:',attrs
        name = attrs['name']
        datatype = attrs['datatype']
        attr = cdmsNode.AttrNode(name,None)
        attr.datatype = datatype
        self.pushCurrentNode(attr)

    # Now the value if any as stored in self.content
    def end_attr(self):
        attr = self.popCurrentNode()
        var = self.getCurrentNode()
        attr.setValueFromString(attr.getContent(),attr.datatype)
        var.setExternalAttrFromAttr(attr)

    #------------------------------------------------------------------------

    def start_axis(self,attrs):
        if self.verbose: print 'axis:',attrs
        id = attrs['id']
        length_s = attrs['length']
        datatype = attrs.get('datatype')
        if _Integer.match(length_s) is None:
            raise InvalidAttribute, 'length='+length_s
        length = string.atoi(length_s)
        axis = cdmsNode.AxisNode(id,length,datatype)
        partstring = attrs.get('partition')
        if partstring is not None:
            axis.setPartitionFromString(partstring)
        axis.setExternalDict(attrs)
        self.getCurrentNode().addId(id,axis)
        self.pushCurrentNode(axis)
                
    def end_axis(self):
        self.popCurrentNode()

    #------------------------------------------------------------------------
    def start_cdml(self, attrs):
        if self.verbose: print 'cdml:',attrs

    def end_cdml(self):
        pass

    #------------------------------------------------------------------------

    def start_component(self, attrs):
        if self.verbose: print 'component:',attrs

    def end_component(self):
        pass

    #------------------------------------------------------------------------
    def start_compoundAxis(self, attrs):
        if self.verbose: print 'compoundAxis:',attrs

    def end_compoundAxis(self):
        pass

    #------------------------------------------------------------------------
    def start_data(self, attrs):
        if self.verbose: print 'data:',attrs

    def end_data(self):
        pass

    #------------------------------------------------------------------------

    def start_dataset(self,attrs):
        if self.verbose: print 'dataset:',attrs
        id = attrs['id']
        dataset = cdmsNode.DatasetNode(id)
        dataset.setExternalDict(attrs)
        if self.root:
            self.getCurrentNode().addId(id,dataset)
        self.pushCurrentNode(dataset)
                
    def end_dataset(self):
        dataset = self.popCurrentNode()
        dataset.validate()

    #------------------------------------------------------------------------

    def start_doclink(self, attrs):
        if self.verbose: print 'docLink:',attrs
        uri = attrs['href']
        doclink = cdmsNode.DocLinkNode(uri)
        doclink.setExternalDict(attrs)
        self.getCurrentNode().add(doclink)
        self.pushCurrentNode(doclink)

    def end_doclink(self):
        self.popCurrentNode()

    #------------------------------------------------------------------------

    def start_domElem(self, attrs):
        if self.verbose: print 'domElem:',attrs
        name = attrs['name']
        start_s = attrs.get('start')
        length_s = attrs.get('length')
        if start_s is not None:
            start = string.atoi(start_s)
        else:
            start = None
        if length_s is not None:
            length = string.atoi(length_s)
        else:
            length = None
        domElem = cdmsNode.DomElemNode(name,start,length)
        domElem.setExternalDict(attrs)
        self.getCurrentNode().add(domElem)

    def end_domElem(self):
        pass

    #------------------------------------------------------------------------
    def start_domain(self, attrs):
        if self.verbose: print 'domain:',attrs
        domain = cdmsNode.DomainNode()
        self.getCurrentNode().setDomain(domain)
        self.pushCurrentNode(domain)

    def end_domain(self):
        self.popCurrentNode()

    #------------------------------------------------------------------------

    def start_rectGrid(self,attrs):
        if self.verbose: print 'rectGrid:',attrs
        id = attrs['id']
        gridtype = attrs['type']
        latitude = attrs['latitude']
        longitude = attrs['longitude']
        grid = cdmsNode.RectGridNode(id,latitude,longitude,gridtype)
        grid.setExternalDict(attrs)
        self.getCurrentNode().addId(id,grid)
        self.pushCurrentNode(grid)
                
    def end_rectGrid(self):
        self.popCurrentNode()

    #------------------------------------------------------------------------

    def start_linear(self, attrs):
        if self.verbose: print 'linear:',attrs
        start_s = attrs['start']
        delta_s = attrs['delta']
        length_s = attrs['length']
        try:
            start=string.atof(start_s)
        except ValueError:
            raise InvalidAttribute, 'start='+start_s
        try:
            delta=string.atof(delta_s)
        except ValueError:
            raise InvalidAttribute, 'delta='+delta_s
        try:
            length=string.atoi(length_s)
        except ValueError:
            raise InvalidAttribute, 'length='+length_s
        linear = cdmsNode.LinearDataNode(start,delta,length)
        self.getCurrentNode().setLinearData(linear)

    def end_linear(self):
        pass

    #------------------------------------------------------------------------

    def start_variable(self,attrs):
        if self.verbose: print 'variable:',attrs
        id = attrs['id']
        datatype = attrs['datatype']
        variable = cdmsNode.VariableNode(id,datatype,None)
        variable.setExternalDict(attrs)
        self.getCurrentNode().addId(id,variable)
        self.pushCurrentNode(variable)
                
    def end_variable(self):
        self.popCurrentNode()

    #------------------------------------------------------------------------

    def start_xlink(self, attrs):
        if self.verbose: print 'xlink:',attrs
        id = attrs['id']
        uri = attrs['href']
        contentRole = attrs['content-role']
        xlink = cdmsNode.XLinkNode(id,uri,contentRole)
        xlink.setExternalDict(attrs)
        self.getCurrentNode().addId(id,xlink)
        self.pushCurrentNode(xlink)

    def end_xlink(self):
        self.popCurrentNode()

    #------------------------------------------------------------------------

    def cdml_syntax_error(self, lineno, message):
        print 'error near line %d:' % lineno, message

    def unknown_starttag(self, tag, attrs):
        if self.verbose: print '**'+tag+'**:',attrs

    def unknown_endtag(self, tag):
        pass

    def unknown_entityref(self, ref):
	self.flush()
        if self.verbose: print '*** unknown entity ref: &' + ref + ';'

    def unknown_charref(self, ref):
	self.flush()
        if self.verbose: print '*** unknown char ref: &#' + ref + ';'

    def close(self):
	XMLParser.close(self)

if __name__ == '__main__':
    import sys
    
    sampfile = open(sys.argv[1])
    text = sampfile.read()
    sampfile.close()

    if len(sys.argv)==2:
        verbose = 0
    else:
        verbose = 1
    p = CDMLParser(verbose)
    p.feed(text)
    p.close()
    p.root.dump()
    
