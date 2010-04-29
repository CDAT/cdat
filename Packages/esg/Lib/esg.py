#!/usr/bin/env python
"""
ESG markup support. See http://www.earthsystemgrid.org
"""

import string, re, cdtime

class ESGError (Exception):
    def __init__ (self, args="Unspecified error from package esg"):
        self.args = args
    def __str__(self):
        return str(self.args)

nsprefix = "esg"
namespace = "http://www.earthsystemgrid.org/"
schemaLocation = "http://www.ucar.edu/schemas/esg.xsd"
defaultTimeEncoding = r'yyyy(-mm(-dd(\s+hh(:mi(:ss)?)?)?)?)?'
_isoco = re.compile(r'^(?P<year>\d{1,4})(-(?P<month>\d{1,2})(-(?P<day>\d{1,2})(T(?P<hour>\d{1,2})(:(?P<minute>\d{1,2})(:(?P<second>\d{1,2}(\.\d+)?))?)?(Z|(?P<tzhour>(\+|\-)\d{1,2})(:(?P<tzminute>\d{1,2}))?))?)?)?$')
_Illegal = re.compile('([<>&\"\'])|([^\t\r\n -\176\240-\377])')   #" illegal chars in content

_timeComponent = {
    'year': 0,
    'month': 1,
    'day': 2,
    'hour': 3,
    'minute': 4,
    'second': 5,
    'tzhour': 6,
    'tzminute': 7
    }

def parseTime(timeString, fromEncoding=defaultTimeEncoding):

    """
    Parse an encoded time string, returning a tuple of the form
    (year, month, day, hour, minute, second).

    timeString: string representing an absolute time. If None, return value is None.
    fromEncoding: input time encoding, a regular expression (see the re module),
       defaults to "yyyy(-mm(-dd(\s+hh(:mi(:ss)?)?)?)?)?"

    An exception is raised if the time string and encoding don't match.
    """
    
    if timeString is None:
        return None

    if fromEncoding is None:
        fromEncoding = defaultTimeEncoding

    fromStr = string.replace(string.lower(string.strip(fromEncoding)), 'yyyy','(?P<year>\d{1,4})')
    fromStr = string.replace(fromStr, 'mm', '(?P<month>\d{1,2})')
    fromStr = string.replace(fromStr, 'dd', '(?P<day>\d{1,2})')
    fromStr = string.replace(fromStr, 'hh', '(?P<hour>\d{1,2})')
    fromStr = string.replace(fromStr, 'mi', '(?P<minute>\d{1,2})')
    fromStr = string.replace(fromStr, 'ss', '(?P<second>\d{1,2}(\.\d+)?)')

    reobj = re.compile('^'+fromStr+'$')
    matchobj = reobj.match(timeString)
    if matchobj is None:
        raise ESGError, 'time string: %s does not match encoding: %s'%(timeString, fromEncoding)

    result = [None]*6
    dict = matchobj.groupdict()
    for key in dict.keys():
        value = dict[key]
        if value is not None:
            if key!='second':
                result[_timeComponent[key]] = string.atoi(value)
            else:
                result[5] = string.atof(dict[key])
    return tuple(result)

def encodeTime(timeTuple, toEncoding=None):

    """
    Encode a time tuple.

    timeTuple: tuple (year, month, day, hour, minute, second)
    toEncoding: encoding string, such as 'yyyy-mm-dd hh:mi:ss'. If omitted,
       the string is generated based on the non-null values of timeTuple.
    """

    year, month, day, hour, minute, second = timeTuple

    # Create an encoding if not specified.
    if toEncoding is None:
        toEncoding = ''
        if year is not None:
            toEncoding += 'yyyy'
            if month is not None:
                toEncoding += '-mm'
                if day is not None:
                    toEncoding += '-dd'
                    if hour is not None:
                        toEncoding += ' hh'
                        if minute is not None:
                            toEncoding += ':mi'
                            if second is not None:
                                toEncoding += ':ss'

    # Encode the value
    if month is None: month = 1
    if day is None: day = 1
    if hour is None: hour = 0
    if minute is None: minute = 0
    if second is None: second = 0
    toEncoding = string.lower(toEncoding)
    toEncoding = string.replace(toEncoding, 'yyyy', str(year))
    toEncoding = string.replace(toEncoding, 'mm', str(month))
    toEncoding = string.replace(toEncoding, 'dd', str(day))
    toEncoding = string.replace(toEncoding, 'hh', str(hour))
    toEncoding = string.replace(toEncoding, 'mi', str(minute))
    toEncoding = string.replace(toEncoding, 'ss', str(second))

    return toEncoding

def parseIsoTime(timeString):

    """
    Parse an encoded time string in ISO 8601 format, conforming to the W3C profile (http://www.w3.org/TR/NOTE-datetime).

    A tuple of the form (year, month, day, hour, minute, second, timezoneHour, timezoneMinute) is returned.
    
    In short, the encoding is:

    "YYYY(-MM(-DD(Thh(:mi(:ss.s)?)?TZD)?)?)?"

    (e.g. 1997-07-16T19:20:30.45+01:00)

    where:

    YYYY = four-digit year
    MM   = two-digit month (01=January, etc.)
    DD   = two-digit day of month (01 through 31)
    hh   = two digits of hour (00 through 23) (am/pm NOT allowed)
    mi   = two digits of minute (00 through 59)
    ss   = two digits of second (00 through 59)
    s    = one or more digits representing a decimal fraction of a second
    TZD  = time zone designator (Z or +hh:mm or -hh:mm)
    """

    matchobj = _isoco.match(timeString)
    if matchobj is None:
        raise ESGError, 'Invalid ISO time: %s'%timeString

    result = [None]*8
    dict = matchobj.groupdict()
    for key in dict.keys():
        value = dict[key]
        if value is not None:
            if key!='second':
                result[_timeComponent[key]] = string.atoi(value)
            else:
                result[5] = string.atof(dict[key])
    return tuple(result)

def encodeIsoTime(timeTuple, toEncoding=None):

    """
    Encode a time tuple as an ISO 8601 time (see parseIsoTime).

    timeTuple: tuple (year, month, day, hour, minute, second, timezoneHour, timezoneMinute)
    """
    year, month, day, hour, minute, second, timezoneHour, timezoneMinute = timeTuple
    result = '%04d'%year
    if month is not None: result += '-%02d'%month
    if day is not None: result += '-%02d'%day
    if hour is not None: result += 'T%02d'%hour
    if minute is not None: result += ':%02d'%minute
    if second is not None: result += ':%05.2f'%second
    if hour is not None:
        if timezoneHour is None:
            tzcode = 'Z'
        else:
            tzcode = '%+03d'%timezoneHour
            if timezoneMinute is not None:
                tzcode += ':%2d'%timezoneMinute
        result += tzcode
    return result

# Map reserved XML characters to entity references:
# '<' --> &lt;
# '>' --> &gt;
# '&' --> &amp;
# '"' --> &quot;
# "'" --> &apos;
# all other illegal characters are removed #"
def mapIllegalToEntity(matchobj):
    s = matchobj.group(0)
    if s == '<':
        return '&lt;'
    elif s == '>':
        return '&gt;'
    elif s == '&':
        return '&amp;'
    elif s == '"':   #"
        return '&quot;'
    elif s=="'":
        return '&apos;'
    else:
        return ""

def _ent(s):
    """Replace reserved characters in s with predefined entities,
    and remove illegal characters.
    """
    result = _Illegal.sub(mapIllegalToEntity,s)
    return result

def _narrow(timetuple):
    "Strip trailing None entries from a time tuple."
    result = []
    for item in timetuple:
        if item is not None:
            result.append(item)
        else:
            break
    return tuple(result)

def writes(fd, tag, s):
    """Write a string element."""
    qtag = '%s:%s'%(nsprefix, tag)
    try:
        fd.write('<%s>%s</%s>\n'%(qtag, _ent(s), qtag))
    except:
        cleanString = string.join([chr(ord(c)) for c in s],'')
        fd.write('<%s>%s</%s>\n'%(qtag, _ent(cleanString), qtag))

class ESGNode:
    """Abstract ESG node."""

    def __init__(self, id, name=None):
        self.tag = self.__class__.__name__ # ESG element tag
        self.id = id
        self.name = name

    def startwrite(self, fd, nl=1, noname=0):
        if self.name is None or noname==1:
            fd.write('<%s:%s id="%s">'%(nsprefix, self.tag, self.id))
        else:
            fd.write('<%s:%s id="%s" name="%s">'%(nsprefix, self.tag, self.id, self.name))
        if nl: fd.write('\n')

    def endwrite(self, fd):
        fd.write('</%s:%s>'%(nsprefix, self.tag))
        fd.write('\n')

    def write(self, fd):
        """Output to a file. fd is an open file descriptor."""
        raise ESGError, "Method not implemented"

class Activity(ESGNode):

    def __init__(self, id, name=None, description=None, rights=None, isPartOf=None, isDerivedFrom=None):
        ESGNode.__init__(self, id)
        self.name = name
        self.description = description
        self.rights = rights
        self.dates = []                 # list of Qualifieds
        self.notes = []
        self.isPartOf = isPartOf        # instance of ObjRef
        self.isDerivedFrom = isDerivedFrom # instance of ObjRef
        self.participants = []          # list of Participants
        self.references = []            # list of References

    def addDate(self, date):
        self.dates += [date]

    def addNote(self, note):
        self.note += [note]

    def addDerived(seld, derived):
        self.derived[derived.id] = derived

    def addParticipant(self, participant):
        self.participants += [participant]

    def addReference(self, reference):
        self.references += [reference]

    def write(self, fd):
        if self.description: writes(fd, 'description', self.description)
        if self.rights: writes(fd, 'rights', self.rights)
        for date in self.dates:
            date.write(fd)
        for note in self.notes:
            writes(fd, 'note', note)
        if self.isPartOf: self.isPartOf.write(fd, 'isPartOf')
        if self.isDerivedFrom: self.isDerivedFrom.write(fd, 'isDerivedFrom')
        for part in self.participants:
            part.write(fd)
        for ref in self.references:
            ref.write(fd)

class Project(Activity):

    def __init__(self, id, name=None, description=None, rights=None, funding=None):
        Activity.__init__(self, id, name, description, rights)
        self.funding = funding
        self.topics = []                # List of Qualifieds

    def setTopic(self, topic):
        self.topics += [topic]

    def write(self, fd):
        self.startwrite(fd, noname=1)
        Activity.write(self, fd)
        for topic in self.topics:
            topic.write(fd, 'topic')
        if self.funding is not None: writes(fd, 'funding', self.funding)
        self.endwrite(fd)

class Ensemble(Activity):

    def __init__(self, id, name=None, description=None, rights=None):
        Activity.__init__(self, id, name, description, rights)

    def write(self, fd):
        self.startwrite(fd, noname=1)
        Activity.write(self, fd)
        self.endwrite(fd)

class Campaign(Activity):

    def __init__(self, id, name=None, description=None, rights=None):
        Activity.__init__(self, id, name, description, rights)

    def write(self, fd):
        self.startwrite(fd, noname=1)
        Activity.write(self, fd)
        self.endwrite(fd)

class Investigation(Activity):

    def __init__(self, id, name=None, description=None, rights=None):
        Activity.__init__(self, id, name, description, rights)

class Simulation(Investigation):

    def __init__(self, id, name=None, description=None, rights=None):
        Investigation.__init__(self, id, name, description, rights)
        self.inputs = []                # list of SimulationInputs
        self.hardware = []

    def setInput(self, input):
        self.inputs += [input]

    def setHardware(seld, hardware):
        self.hardware += [hardware]

    def write(self, fd):
        self.startwrite(fd, noname=0)
        Activity.write(self, fd)
        for input in self.inputs:
            input.write(fd, 'simulationInput')
        for h in self.hardware:
            writes(fd, 'simulationHardware', h)
        self.endwrite(fd)

class Experiment(Investigation):

    def __init__(self, id, name=None, description=None, rights=None, parent=None):
        Investigation.__init__(self, id, name, description, rights)

    def write(self, fd):
        self.startwrite(fd, noname=1)
        Activity.write(self, fd)
        self.endwrite(fd)

class Observation(Investigation):

    def __init__(self, id, name=None, description=None, rights=None, parent=None):
        Investigation.__init__(self, id, name, description, rights)

    def write(self, fd):
        self.startwrite(fd, noname=1)
        Activity.write(self, fd)
        self.endwrite(fd)

class Analysis(Investigation):

    def __init__(self, id, name=None, description=None, rights=None, parent=None):
        Investigation.__init__(self, id, name, description, rights)

    def write(self, fd):
        self.startwrite(fd, noname=1)
        Activity.write(self, fd)
        self.endwrite(fd)

class Dataset(ESGNode):

    def __init__(self, id, generatedBy=None, ofType=None, isPartOf=None, convention=None, timeCoverage=None, spaceCoverage=None, format=None, name=None):
        """
        Create a Dataset.

        id: Unique string identifier
        generatedBy: Investigation that generated the dataset.
        ofType: String distinguishing characteristic within an investigation, e.g. "monthly means"
        isPartOf: Dataset that contains this dataset, if any.
        convention: String metadata convention ID, e.g. "CF-1.0"
        timeCoverage: instance of TimeRegion
        spaceCoverage: instance of SpaceRegion

        """

        ESGNode.__init__(self, id, name=name)
        self.generatedBy = generatedBy  # instance of ObjRef
        self.ofType = ofType
        self.isPartOf = isPartOf        # instance of ObjRef
        self.convention = convention
        self.timeCoverage = timeCoverage # instance of TimeRegion
        self.spaceCoverage = spaceCoverage # instance of SpaceRegion
        self.dates = []                 # List of Qualifieds
        self.parameterLists = []         # List of ObjRefs
        self.format = format            # instance of Format
        self._parameternames = {}         # Dictionary of parameter names. values are 1

    def addDate(self, date):

        """
        Set an event date.

        ofType: String type of event, e.g., "start" or "stop".
        datetime: String of form "yyyy-mm-dd hh:mi:ss"
        """

        self.dates += [date]

    def addParameterList(self, qlist):

        """
        Add a parameter list.

        qlist: idref of a parameter list.
        """
        self.parameterLists += [qlist]

    def setTimeCoverage(self, tc):
        self.timeCoverage = tc

    def setSpaceCoverage(self, sc):
        self.spaceCoverage = sc

    def write(self, fd):
        self.startwrite(fd)
        if self.ofType: writes(fd, 'type', self.ofType)
        if self.generatedBy: self.generatedBy.write(fd, 'generatedBy')
        if self.convention: writes(fd, 'convention', self.convention)
        for date in self.dates:
            date.write(fd, 'date')
        if self.timeCoverage: self.timeCoverage.write(fd)
        if self.spaceCoverage: self.spaceCoverage.write(fd)
        for param in self.parameterLists:
            param.write(fd, 'hasParameters')
        if self.isPartOf: self.isPartOf.write(fd, 'isPartOf')
        if self.format is not None: self.format.write(fd)
        self.endwrite(fd)

class TimeRegion:

    def __init__(self, name=None, start=None, stop=None, encoding=None, calendar="gregorian"):
    
        self.name = name
        self.start = parseTime(start, encoding) # (year, month, day, hour, minute, second) or None
        self.stop = parseTime(stop, encoding) # ditto
        self.encoding = encoding
        self.calendar = calendar
        self.tag = 'timeCoverage'

    def write(self, fd):
        fd.write('<%s:%s'%(nsprefix, self.tag))
        if self.name: fd.write(' name="%s"'%_ent(self.name))
        if self.start: fd.write(' start="%s"'%encodeTime(self.start, self.encoding))
        if self.stop: fd.write(' stop="%s"'%encodeTime(self.stop, self.encoding))
        if self.encoding: fd.write(' encoding="%s"'%_ent(self.encoding))
        if self.calendar: fd.write(' calendar="%s"'%_ent(self.calendar))
        fd.write('/>\n')

    def merge(self, tr):
        if tr is None:
            return
        if self.start is None:
            self.start = tr.start
        elif tr.start is not None:
            oldstart = apply(cdtime.comptime, _narrow(self.start))
            newstart = apply(cdtime.comptime, _narrow(tr.start))
            if oldstart.cmp(newstart) == 1:  # oldstart > newstart?
                self.start = tr.start

        if self.stop is None:
            self.stop = tr.stop
        elif tr.stop is not None:
            oldstop = apply(cdtime.comptime, _narrow(self.stop))
            newstop = apply(cdtime.comptime, _narrow(tr.stop))
            if oldstop.cmp(newstop) == -1:  # oldstop < newstop?
                self.stop = tr.stop

def mergeRange(ra, rb):
    if ra is None:
        return rb
    elif rb is None:
        return ra
    raw, rae, rau = ra
    rbw, rbe, rbu = rb
    result = (min(raw, rbw), max(rae, rbe), rau)
    return result

class SpaceRegion:

    def __init__(self, name=None, xrange=None, yrange=None, zrange=None):

        self.name = name
        self.xrange = xrange            # (westLimit, eastLimit, units)
        self.yrange = yrange            # (southLimit, northLimit, units)
        self.zrange = zrange            # (downLimit, upLimit, units)
        self.tag = 'spaceCoverage'

    def write(self, fd):
        fd.write('<%s:%s'%(nsprefix, self.tag))
        if self.name: fd.write(' name="%s"'%_ent(self.name))
        if self.xrange: fd.write(' westLimit="%s" eastLimit="%s" xunits="%s"'%self.xrange)
        if self.yrange: fd.write(' southLimit="%s" northLimit="%s" yunits="%s"'%self.yrange)
        if self.zrange: fd.write(' downLimit="%s" upLimit="%s" zunits="%s"'%self.zrange)
        fd.write('/>\n')

    def merge(self, sr):
        self.xrange = mergeRange(self.xrange, sr.xrange)
        self.yrange = mergeRange(self.yrange, sr.yrange)
        self.zrange = mergeRange(self.zrange, sr.zrange)

class Parameter(ESGNode):

    def __init__(self, id, name, description=None, activityRef=None, standardName=None, standardAuthority=None):
        ESGNode.__init__(self, id, name=name)
        self.description = description
        self.activityRef = activityRef
        self.standardName = standardName
        self.standardAuthority = standardAuthority

    def setMapping(self, mapping):
        self.standardName = mapping.standardName
        self.standardAuthority = mapping.authority

    def getMapping(self):
        return Mapping(self.standardAuthority, self.standardName)

    def write(self, fd):
        self.startwrite(fd)
        if self.description: writes(fd, 'desc', self.description)
        if self.activityRef: self.activityRef.write(fd, 'activityRef')
        if self.standardName: self.getMapping().write(fd)
        self.endwrite(fd)

class ParameterList(ESGNode):

    def __init__(self, id, description=None, paramrefs=None, activityRef=None):
        ESGNode.__init__(self, id)
        self.description = description
        self.paramrefs = paramrefs    # Dictionary of ObjRefs
        if paramrefs is None:
            self.paramrefs = {}
        self.activityId = activityRef  # ObjRef

    def addParamRef(self, paramref):
        self.paramrefs[paramref.idref] = paramref

    def write(self, fd):
        self.startwrite(fd)
        if self.activityId is not None:
            self.activityId.write(fd, 'activityRef')
        paramids = self.paramrefs.keys()
        paramids.sort()
        for paramid in paramids:
            self.paramrefs[paramid].write(fd, 'hasParameter')
        self.endwrite(fd)

class Service(ESGNode):

    def __init__(self, id, name=None, description=None):
        ESGNode.__init__(self, id)
        self.name = name
        self.description = description

class Contact:

    def __init__(self, street=None, city=None, state=None, region=None, province=None, postcode=None, country=None, email=None, telephone=None, fax=None, url=None):
        self.tag = 'contact'
        self.street = street
        self.city = city
        self.state = state
        self.region = region
        self.province = province
        self.postcode = postcode
        self.country = country
        self.email = email
        self.telephone = telephone
        self.fax = fax
        self.url = url

    def write(self, fd):
        qtag = '%s:%s'%(nsprefix, self.tag)
        fd.write('<%s>'%qtag)
        if self.street: writes(fd, 'street', self.street)
        if self.city: writes(fd, 'city', self.city)
        if self.state:
            writes(fd, 'state', self.state)
        elif self.province:
            writes(fd, 'province', self.province)
        elif self.region:
            writes(fd, 'region', self.region)
        if self.postcode: writes(fd, 'postalCode', self.postcode)
        if self.country: writes(fd, 'country', self.country)
        if self.telephone: writes(fd, 'phone', self.telephone)
        if self.fax: writes(fd, 'fax', self.fax)
        if self.email: writes(fd, 'email', self.email)
        if self.url: writes(fd, 'url', self.url)
        fd.write('</%s>'%qtag)

class Person(ESGNode):

    def __init__(self, id, firstName=None, lastName=None, contact=None, worksFor=None):
        ESGNode.__init__(self, id)
        self.firstName = firstName
        self.lastName = lastName
        self.contact = contact          # instance of Contact
        self.worksFor = worksFor        # instance of Institution

    def setContact(self, contact):
        self.contact = contact

    def write(self, fd):
        self.startwrite(fd)
        if self.firstName: writes(fd, 'firstName', self.firstName)
        if self.lastName: writes(fd, 'lastName', self.lastName)
        if self.contact: self.contact.write(fd)
        if self.worksFor: self.worksFor.write(fd, 'worksFor')
        self.endwrite(fd)

class Institution(ESGNode):

    def __init__(self, id, name=None, ofType=None, contact=None):
        ESGNode.__init__(self, id)
        self.name = name
        self.ofType = ofType            # e.g. "government"
        self.contact = contact          # instance of Contact

    def setContact(self, c):
        self.contact = c

    def write(self, fd):
        self.startwrite(fd, noname=1)
        if self.name: writes(fd, 'name', self.name)
        if self.ofType: writes(fd, 'type', self.ofType)
        if self.contact: self.contact.write(fd)
        self.endwrite(fd)

class File(ESGNode):

    def __init__(self, id, isPartOf=None, name=None, size=None):
        ESGNode.__init__(self, id)
        self.isPartOf = isPartOf        # Object Ref
        self.name = name
        self.tag = 'File'
        self.size = size

    def write(self, fd):
        self.startwrite(fd)
        self.isPartOf.write(fd, 'isPartOf')
        self.endwrite(fd)

class Metadata(ESGNode):

    """Root node."""

    def __init__(self):
        ESGNode.__init__(self, None)
        self.tag = "metadata"
        self.activities = {}
        self.datasets = {}
        self.services = {}
        self.persons = {}
        self.institutions = {}
        self.parameters = {}
        self.parameterLists = {}
        self.files = {}

    def setActivity(self, activity):
        self.activities[activity.id] = activity
        
    def setDataset(self, dataset):
        self.datasets[dataset.id] = dataset
        
    def setService(self, service):
        self.services[service.id] = service
        
    def setPerson(self, person):
        self.persons[person.id] = person
        
    def setInstitution(self, institution):
        self.institutions[institution.id] = institution
        
    def setParameter(self, parameter):
        self.parameters[parameter.id] = parameter
        
    def setParameterList(self, parameterList):
        self.parameterLists[parameterList.id] = parameterList

    def setFile(self, fileobj):
        self.files[fileobj.id] = fileobj
        
    def write(self, fd):
        fd.write('<%s:%s'%(nsprefix, self.tag))
        fd.write(' xmlns:%s="%s"'%(nsprefix,namespace))
        fd.write(' xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"')
        fd.write(' xsi:schemaLocation="%s %s">\n'%(namespace,schemaLocation))

        for d in [self.activities, self.datasets, self.services, self.persons, self.institutions, self.parameters, self.parameterLists, self.files]:
            keys = d.keys()
            keys.sort()
            for key in keys:
                d[key].write(fd)
        fd.write('</%s:%s>\n'%(nsprefix, self.tag))

    def merge(self, targetdsetid, sourcemd, sourcedsetid, newlistid, resultid=None, include=None, exclude=None, extralists=None, targetParams=None, mustResolve="no" ):
        """
        Merge dataset sourcedsetid in container sourcemd, into dataset targetdsetid in self.

        targetdsetid: string ID of target dataset, in self
        sourcemd    : metadata node containing source dataset
        sourcedsetid: string ID of source dataset
        newlistid   : string ID for the new parameter list generated, if any. If the target dataset has a
                      parameter list with this ID, extra parameters are added to it instead.
        resultid    : string ID of result dataset. Defaults to targetdsetid
        include     : list of parameter names. If specified, only merge variables in the list.
        exclude     : list of parameter names. Do not merge variables in the list.
        extralists  : list of extra parameters lists
        targetParams: list of parameters referenced by the source dataset, but not included in the source metadata.
        mustResolve : ["yes" | "no"] If yes, an error is raised whenever a parameterlist IDREF cannot be resolved
                      to the list object. Default is "no".

        Note: the merge is done in-place. If an exception occurs, the target dataset may be modified.
        """

        oldparamlists = self.parameterLists.copy()
        if extralists is None:
            extralists = []
        for plist in extralists:
            oldparamlists[plist.id] = plist
            
        oldparams = self.parameters.copy()
        if targetParams is None:
            targetParams = []
        for param in targetParams:
            oldparams[param.id] = param

        newparamlists = sourcemd.parameterLists
        newparams = sourcemd.parameters
        olddset = self.datasets[targetdsetid]
        newdset = sourcemd.datasets[sourcedsetid]

        if olddset.generatedBy is None:
            olddset.generatedBy = newdset.generatedBy
        if olddset.ofType is None:
            olddset.ofType = newdset.ofType
        if olddset.convention is None:
            olddset.convention = newdset.convention
        if olddset.format is None:
            olddset.format = newdset.format

        extraparams = []                # parameters in newdset but not in olddset

        # Generate a dictionary of parameter names for olddset
        if olddset._parameternames == {}:
            for paramlistref in olddset.parameterLists:

                # Get the parameter list. If not found - because it's external -
                # just skip it.
                try:
                    paramlist = oldparamlists[paramlistref.idref]
                except KeyError:
                    if mustResolve=="yes":
                        raise ESGError, "Cannot resolve parameterlist %s"%paramlistref.idref
                    else:
                        continue
                for paramref in paramlist.paramrefs.values():

                    try:
                        param = oldparams[paramref.idref]
                        paramname = param.name
                    except KeyError:
                        if mustResolve=="yes":
                            raise ESGError, "Cannot resolve parameter %s"%paramref.idref
                        else:

                            # For now, just get the name from the idref
                            paramname = string.split(paramref.idref, '.')[-1]
                    olddset._parameternames[paramname] = 1
                    
        # For each variable in the new dataset:
        for listref in newdset.parameterLists:
            paramlist = newparamlists[listref.idref]
            for paramref in paramlist.paramrefs.values():

                param = newparams[paramref.idref]

                # If not in the include list, skip.
                if include is not None and paramref.idref not in include:
                    continue

                # If in the exclude list, skip.
                if exclude is not None and paramref.idref in exclude:
                    continue

                # If the parameter is not in olddset, add it to a new parameter list
                if not olddset._parameternames.has_key(param.name):
                    extraparams.append(param)

                # Merge time and space coverage
                if olddset.timeCoverage is None:
                    olddset.setTimeCoverage(newdset.timeCoverage)
                else:
                    olddset.timeCoverage.merge(newdset.timeCoverage)

                if olddset.spaceCoverage is None:
                    olddset.setSpaceCoverage(newdset.spaceCoverage)
                else:
                    olddset.spaceCoverage.merge(newdset.spaceCoverage)

        # If any parameters were added, make a new parameter list and associate
        # it with olddset, or use the existing parameter list if contained in the target dataset.
        if extraparams != []:
            extrarefs = {}
            for param in extraparams:
                paramref = ObjRef(param.id)
                extrarefs[paramref.idref] = paramref
                olddset._parameternames[param.name] = 1
                self.parameters[param.id] = param
            if ObjRef(newlistid) in olddset.parameterLists and self.parameterLists.has_key(newlistid):
                extraparamlist = self.parameterLists[newlistid]
                for ref in extrarefs.values():
                    extraparamlist.addParamRef(ref)
            else:
                extraparamlist = ParameterList(newlistid, paramrefs = extrarefs)
                self.parameterLists[newlistid] = extraparamlist
                olddset.addParameterList(ObjRef(extraparamlist.id))

        # Copy dataset files
        for fileobj in sourcemd.files.values():
            if fileobj.isPartOf.idref == sourcedsetid:
                self.setFile(fileobj)

        # Change the result ID if necessary
        if resultid is not None:
            del self.datasets[targetdsetid]
            olddset.id = resultid
            self.setDataset(olddset)

class Qualified:

    def __init__(self, qual, tag=None):
        self.qual = qual
        self.tag = tag
        self.content = None

    def write(self, fd, tag):
        """Write a qualified type (has an attribute 'type')"""
        qtag = '%s:%s'%(nsprefix, tag)
        fd.write('<%s type="%s">%s</%s>\n'%(qtag, _ent(self.qual), _ent(self.content), qtag))

    def setContent(self, content):
        self.content = content

class ObjRef:

    def __init__(self, idref):
        self.idref = idref

    def write(self, fd, tag):
        """Write an object reference type (has an attribute 'idref')"""
        qtag = '%s:%s'%(nsprefix, tag)
        fd.write('<%s idref="%s" />\n'%(qtag, _ent(self.idref)))

    def __cmp__(self, other):
        return not (self.idref == other.idref)

class Participant(ObjRef):

    def __init__(self, idref, role):
        ObjRef.__init__(self, idref)
        self.tag = 'participant'
        self.role = role

    def write(self, fd):
        """Write a participant type (has a role and idref)."""
        qtag = '%s:%s'%(nsprefix, self.tag)
        fd.write('<%s idref="%s" role="%s"/>\n'%(qtag, _ent(self.idref), _ent(self.role)))

class Reference:

    def __init__(self, uri, reference=None):
        self.uri = uri
        self.reference = reference

    def setContent(self, content):
        self.reference = content

    def write(self, fd):
        """Write a reference type (has a uri)."""
        qtag = '%s:%s'%(nsprefix, 'reference')
        fd.write('<%s uri="%s">%s</%s>\n'%(qtag, _ent(self.uri), _ent(self.reference), qtag))

class SimulationInput(Qualified):

    def __init__(self, ofType, input=None):
        Qualified.__init__(self, ofType)
        self.input = input
        self.tag = 'simulationInput'

class Mapping:

    def __init__(self, authority, standardName=None):
        self.authority = authority
        self.standardName = standardName

    def write(self, fd):
        qtag = '%s:%s'%(nsprefix, 'mapping')
        fd.write('<%s authority="%s">%s</%s>\n'%(qtag, _ent(self.authority), _ent(self.standardName), qtag))

class Date(Qualified):

    def __init__(self, type, encoding=None):
        Qualified.__init__(self, type, tag='date')
        self.encoding = encoding

    def write(self, fd):
        qtag = '%s:%s'%(nsprefix, self.tag)
        fd.write('<%s type="%s" encoding="%s">%s</%s>\n'%(qtag, _ent(self.qual), _ent(self.encoding), _ent(self.content), qtag))

class Format:

    def __init__(self, uri, ofType):
        self.uri = uri
        self.ofType = ofType
        self.tag = 'format'

    def write(self, fd):
        qtag = '%s:%s'%(nsprefix, self.tag)
        fd.write('<%s uri="%s" type="%s" />\n'%(qtag, _ent(self.uri), _ent(self.ofType)))
                 
