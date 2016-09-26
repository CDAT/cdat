#!/usr/bin/env python
"""
ESG markup parsing. See http://www.earthsystemgrid.org
"""

import esg
from types import StringType, ListType, DictionaryType
from xml.sax import make_parser, saxutils, ContentHandler, ErrorHandler, SAXParseException
from xml.sax.handler import feature_namespaces

class ESGParseException(SAXParseException):

    def __str__(self):
        "Create a string representation of the exception."
        sysid = self.getSystemId()
        if sysid is None:
            sysid = "<unknown>"
        return "%s, in file %s, line %d" % (self._msg, sysid, self.getLineNumber())

def load(path):
    """Create a tree of ESG nodes from a file path.
    Returns the parse tree root node.
    """
    parser = make_parser()
    parser.setFeature(feature_namespaces,1)
    ch = esgContentHandler()
    eh = esgErrorHandler()
    parser.setContentHandler(ch)
    parser.setErrorHandler(eh)
    parser.parse(path)
    return ch.root

def _getatt(attrs, key, ns=None, default=None):
    """Get the value of attribute 'key' from attrs object,
    an AttributesNS object. Return default value if not found."""
    keytuple = (ns, unicode(key))
    if attrs.has_key(keytuple):
        result = attrs[keytuple]
    else:
        result = default
    return result

class esgContentHandler(ContentHandler):
    """Handle ESG content. An instance of this class is associated
    with the parser using setContentHandler()."""

    __dispatch = {
        'Activity':('startActivity','endActivity'),
        'Analysis':('startAnalysis','endAnalysis'),
        'Campaign':('startCampaign','endCampaign'),
        'Dataset':('startDataset','endDataset'),
        'Ensemble':('startEnsemble','endEnsemble'),
        'Experiment':('startExperiment','endExperiment'),
        'File':('startFile','endFile'),
        'Institution':('startInstitution','endInstitution'),
        'Investigation':('startInvestigation','endInvestigation'),
        'Observation':('startObservation','endObservation'),
        'Person':('startPerson','endPerson'),
        'Project':('startProject','endProject'),
        'Parameter':('startParameter','endParameter'),
        'ParameterList':('startParameterList','endParameterList'),
        'Service':('startService','endService'),
        'Simulation':('startSimulation','endSimulation'),
        'activityRef':('startObjRef',None,('activityId','activityRef')),
        'city':('startString','endString',('city',StringType)),
        'contact':('startContact','endContact'),
        'convention':('startString','endString',('convention',StringType)),
        # 'conventions':('startString','endString',('convention',StringType)),
        'country':('startString','endString',('country',StringType)),
        'date':('startDate','endDate'),
        'desc':('startString','endString',('description',StringType)),
        'description':('startString','endString',('description',StringType)),
        'email':('startString','endString',('email',StringType)),
        'fax':('startString','endString',('fax',StringType)),
        'firstName':('startString','endString',('firstName',StringType)),
        'format':('startformat',None),
        'funding':('startString','endString',('funding',StringType)),
        'generatedBy':('startObjRef',None,('generatedBy','generatedBy')),
        'hasChild':('starthasChild','endhasChild'),
        'hasParent':('starthasParent','endhasParent'),
        'hasParameters':('starthasParameters',None),
        'hasParameter':('starthasParameter',None),
        'hasSibling':('starthasSibling','endhasSibling'),
        'isDerivedFrom':('startObjRef',None,('isDerivedFrom','isDerivedFrom')),
        'isPartOf':('startObjRef',None,('isPartOf','isPartOf')),
        'lastName':('startString','endString',('lastName',StringType)),
        'mapping':('startmapping','endmapping'),
        'metadata':('startmetadata','endmetadata'),
        'name':('startString','endString',('name',StringType)),
        'note':('startString','endString',('notes',ListType)),
        'participant':('startParticipant',None),
        'phone':('startString','endString',('telephone',StringType)),
        'postalCode':('startString','endString',('postcode',StringType)),
        'province':('startString','endString',('province',StringType)),
        'reference':('startReference','endReference'),
        'region':('startString','endString',('region',StringType)),
        'rights':('startString','endString',('rights',StringType)),
        'simulationHardware':('startString','endString',('hardware',ListType)),
        'simulationInput':('startsimulationInput','endsimulationInput'),
        'spaceCoverage':('startspaceCoverage',None),
        'state':('startString','endString',('state',StringType)),
        'street':('startString','endString',('street',StringType)),
        'timeCoverage':('starttimeCoverage',None),
        'topic':('startQualified','endQualified', 'topic'),
        'type':('startString','endString',('ofType',StringType)),
        'url':('startString','endString',('url',StringType)),
        'worksFor':('startObjRef',None,('worksFor','worksFor'))
        }

    def __init__(self):
        self.stack = []                 # Context stack
        self.root = None                # Result node
        self.content = ""               # content of current node

    def setDocumentLocator(self, locator):
        self._locator = locator

    def push(self, item):
        self.stack = [item] + self.stack

    def pop(self):
        top, self.stack = self.stack[0], self.stack[1:]
        return top

    def peek(self):
        return self.stack[0]

    def startElement(self, name, attrs):
        raise ESGParseException('No namespace found.'%elocal, None, self._locator)

    def endElement(self, name):
        raise ESGParseException('No namespace found.'%elocal, None, self._locator)

    def startElementNS(self, name, qname, attrs):
        uri, localname = name
        elocal = localname.encode()
        dtuple = self.__dispatch.get(elocal, None)
        if dtuple is not None and dtuple[0] is not None:
            startfunc = getattr(self.__class__, dtuple[0])
            if len(dtuple)==2:
                startfunc(self, attrs)
            else:
                startfunc(self, attrs, dtuple[2])
        else:
            raise ESGParseException('Invalid element tag: %s'%elocal, None, self._locator)

    def endElementNS(self, name, qname):
        uri, localname = name
        elocal = localname.encode()
        dtuple = self.__dispatch.get(elocal, None)
        if dtuple is not None and dtuple[1] is not None:
            endfunc = getattr(self.__class__, dtuple[1])
            if len(dtuple)==2:
                endfunc(self)
            else:
                endfunc(self, dtuple[2])

    def characters(self, ch):
        self.content += ch

    def startActivity(self, attrs, clas):
        parent = self.peek()
        id = _getatt(attrs, 'id')
        name = _getatt(attrs, 'name')
        a = clas(id, name=name)
        self.push(a)
        parent.setActivity(a)
        return a

    def endActivity(self):
        self.pop()

    def startAnalysis(self, attrs):
        p = self.startActivity(attrs, esg.Analysis)

    def endAnalysis(self):
        self.endActivity()

    def startCampaign(self, attrs):
        p = self.startActivity(attrs, esg.Campaign)

    def endCampaign(self):
        self.endActivity()

    def startContact(self, attrs):
        contact = esg.Contact()
        self.peek().setContact(contact)
        self.push(contact)

    def endContact(self):
        self.pop()

    def startDataset(self, attrs):
        id = _getatt(attrs, 'id')
        name = _getatt(attrs, 'name')
        dsetnode = esg.Dataset(id, name=name)
        self.peek().setDataset(dsetnode)
        self.push(dsetnode)

    def endDataset(self):
        self.pop()

    def startEnsemble(self, attrs):
        p = self.startActivity(attrs, esg.Ensemble)

    def endEnsemble(self):
        self.endActivity()

    def startExperiment(self, attrs):
        p = self.startActivity(attrs, esg.Experiment)

    def endExperiment(self):
        self.endActivity()

    def startFile(self, attrs):
        id = _getatt(attrs, 'id')
        name = _getatt(attrs, 'name')
        fileobj = esg.File(id, name=name)
        self.peek().setFile(fileobj)
        self.push(fileobj)

    def endFile(self):
        self.pop()

    def startInstitution(self, attrs):
        id = _getatt(attrs, 'id')
        inst = esg.Institution(id)
        self.peek().setInstitution(inst)
        self.push(inst)

    def endInstitution(self):
        self.pop()

    def startObservation(self, attrs):
        p = self.startActivity(attrs, esg.Observation)

    def endObservation(self):
        self.endActivity()

    def startPerson(self, attrs):
        id = _getatt(attrs, 'id')
        person = esg.Person(id)
        self.peek().setPerson(person)
        self.push(person)

    def endPerson(self):
        self.pop()

    def startProject(self, attrs):
        p = self.startActivity(attrs, esg.Project)

    def endProject(self):
        self.endActivity()

    def startParameter(self, attrs):
        id = _getatt(attrs, 'id')
        name = _getatt(attrs, 'name')
        param = esg.Parameter(id, name)
        self.peek().setParameter(param)
        self.push(param)

    def endParameter(self):
        self.pop()

    def startParameterList(self, attrs):
        id = _getatt(attrs, 'id')
        qlist = esg.ParameterList(id)
        self.peek().setParameterList(qlist)
        self.push(qlist)

    def endParameterList(self):
        self.pop()

    def startService(self, attrs):
        pass

    def endService(self):
        pass

    def startSimulation(self, attrs):
        p = self.startActivity(attrs, esg.Simulation)

    def endSimulation(self):
        self.endActivity()

    def startmetadata(self, attrs):
        m = esg.Metadata()
        self.push(m)

    def endmetadata(self):
        self.root = self.pop()
    
    #---------------------------------------------------------------------------

    def startString(self, attrs, arg):
        self.content = ''

    def endString(self, arg):
        attr, attrtype = arg
        parent = self.peek()
        if attrtype is StringType:
            setattr(parent, attr, self.content)
        elif attrtype is ListType:
            setattr(parent, attr, getattr(parent, attr) + [self.content])
    
    def startQualified(self, attrs, arg):
        qual = _getatt(attrs, 'type')
        parent = self.peek()
        qualnode = esg.Qualified(qual, arg)
        if arg=='topic':
            parent.setTopic(qualnode)
        self.push(qualnode)
        self.content = ''

    def endQualified(self, arg):
        qualnode = self.pop()
        qualnode.setContent(self.content)
    
    def startObjRef(self, attrs, arg):
        idref = _getatt(attrs, 'idref')
        attr, tag = arg
        parent = self.peek()
        objnode = esg.ObjRef(idref)
        setattr(parent, attr, objnode)
    
    def startParticipant(self, attrs):
        idref = _getatt(attrs, 'idref')
        role = _getatt(attrs, 'role')
        p = esg.Participant(idref, role)
        self.peek().addParticipant(p)

    def startReference(self, attrs):
        uri = _getatt(attrs, 'uri')
        parent = self.peek()
        refnode = esg.Reference(uri)
        parent.addReference(refnode)
        self.push(refnode)
        self.content = ''

    def endReference(self):
        refnode = self.pop()
        refnode.setContent(self.content)
    
    def startsimulationInput(self, attrs):
        ofType = _getatt(attrs, 'type')
        si = esg.SimulationInput(ofType)
        self.peek().setInput(si)
        self.push(si)
        self.content = ''

    def endsimulationInput(self):
        si = self.pop()
        si.setContent(self.content)
    
    def starttimeCoverage(self, attrs):
        name = _getatt(attrs, 'name')
        start = _getatt(attrs, 'start')
        stop = _getatt(attrs, 'stop')
        encoding = _getatt(attrs, 'encoding')
        calendar = _getatt(attrs, 'calendar')
        trnode = esg.TimeRegion(name, start, stop, encoding, calendar)
        self.peek().setTimeCoverage(trnode)
    
    def startspaceCoverage(self, attrs):
        name = _getatt(attrs, 'name')
        northLimit = _getatt(attrs, 'northLimit')
        eastLimit = _getatt(attrs, 'eastLimit')
        southLimit = _getatt(attrs, 'southLimit')
        westLimit = _getatt(attrs, 'westLimit')
        upLimit = _getatt(attrs, 'upLimit')
        downLimit = _getatt(attrs, 'downLimit')
        xunits = _getatt(attrs, 'xunits', default='degrees')
        yunits = _getatt(attrs, 'yunits', default='degrees')
        zunits = _getatt(attrs, 'zunits', default='degrees')
        if westLimit and eastLimit:
            xrange = (westLimit, eastLimit, xunits)
        else:
            xrange = None
        if southLimit and northLimit:
            yrange = (southLimit, northLimit, yunits)
        else:
            yrange = None
        if downLimit and upLimit:
            zrange = (downLimit, upLimit, zunits)
        else:
            zrange = None
        spaceRegion = esg.SpaceRegion(name, xrange, yrange, zrange)
        self.peek().setSpaceCoverage(spaceRegion)

    def startmapping(self, attrs):
        authority = _getatt(attrs, 'authority')
        mapping = esg.Mapping(authority)
        self.push(mapping)
        self.content = ''

    def endmapping(self):
        mapping = self.pop()
        mapping.standardName = self.content
        self.peek().setMapping(mapping)
    
    def starthasParameters(self, attrs):
        idref = _getatt(attrs, 'idref')
        objnode = esg.ObjRef(idref)
        self.peek().addParameterList(objnode)

    def starthasParameter(self, attrs):
        idref = _getatt(attrs, 'idref')
        objnode = esg.ObjRef(idref)
        self.peek().addParamRef(objnode)
    
    def startDate(self, attrs):
        qual = _getatt(attrs, 'type')
        encoding = _getatt(attrs, 'encoding')
        datenode = esg.Date(qual, encoding)
        self.peek().addDate(datenode)
        self.push(datenode)
        self.content = ''

    def endDate(self):
        datenode = self.pop()
        datenode.setContent(self.content)

    def startformat(self, attrs):
        uri = _getatt(attrs, 'uri')
        ofType = _getatt(attrs, 'type')
        formnode = esg.Format(uri, ofType)
        self.peek().format = formnode

    # Ignore these for now.
    def starthasChild(self, attrs):
        pass

    def endhasChild(self):
        pass
    
    def starthasParent(self, attrs):
        pass

    def endhasParent(self):
        pass
    
class esgErrorHandler(ErrorHandler):

    """Handle an ESG parsing error. An object of this type is associated
    with the parser using setErrorHandler()."""

    def __init__(self):
        pass

    def printErrorLine(self, path, linenum):
        f = open(path)
        lines = f.readlines()
        line = lines[linenum-1]
        print line,

    def fatalError(self, exception):
        print 'Got a fatal error'
        print 'Message:',exception.getMessage()
        print 'Column:',exception.getColumnNumber()
        linenum = exception.getLineNumber()
        print 'Line:', linenum
        print 'Public ID:',exception.getPublicId()
        systemid = exception.getSystemId()
        print 'System ID:', systemid
        print 'Error in line:', self.printErrorLine(systemid, linenum)
        raise exception

if __name__=='__main__':
    import sys

    # esgnode = load(sys.argv[1])
    esgnode = load("../Test/pcm_example.xml")
    print '<?xml version="1.0" encoding="ISO-8859-1"?>'
    esgnode.write(sys.stdout)


