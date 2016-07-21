"""
CDML Document Type Definition
This defines the CDML language
"""

# DTD constants
Cdata = 1
Id = 2
Idref = 3
Entity = 4
Entities = 5
Nmtoken = 6
Nmtokens = 7
Notation = 8

Required = 1
Implied = 2
Fixed = 3

# Data types

CdChar = "Char"
CdByte = "Byte"
CdShort = "Short"
CdInt = "Int"
CdLong = "Long"
CdInt64 = "Int64"
CdFloat = "Float"
CdDouble = "Double"
CdString = "String"
CdFromObject = "FromObject"             # Datatype is same as parent object
CdAny = "Any"                           # Unspecified datatype

CdScalar = "Scalar"
CdArray = "Array"

# Note: at some point, this should be created dynamically
# from the XML DTD file. For now, it is built statically.

class CDML:

    cache = {}
    extraCache = {}

    def __init__(self, uri=None):
        self.dtd = self.cache.get(uri)
        self.extra = self.extraCache.get(uri)
        if not self.dtd:
            self.dtd = self.buildDTD(uri)
            self.cache[uri]=self.dtd
            self.extra = self.buildExtra(uri)
            self.extraCache[uri]=self.extra

    def buildDTD(self,uri):
        dtd = {}
        dtd['attr'] = {
            'name': (Cdata,Required),
            'datatype': (("Char","Byte","Short","Int","Long","Int64","Float","Double","String"),Required),
            }
        dtd['axis'] = {
            'id': (Id,Required),
            'associate': (Idref,Implied),
            'axis': (("X","Y","Z","T"),Implied),
            'bounds': (Idref,Implied),
            'calendar': (Cdata,Implied),
            'comment': (Cdata,Implied),
            'component': (Cdata,Implied),
            'compress': (Cdata,Implied),
            'datatype': (("Char","Byte","Short","Int","Long","Int64","Float","Double","String"),Required),
            'expand': (Idref,Implied),
            'interval': (Cdata,Implied),
            'isvar': ( ("true","false"),"true"),
            'length': (Cdata,Required),
            'long_name': (Cdata,Implied),
            'modulo': (Cdata,Implied),
            'name_in_file': (Cdata,Implied),
            'partition': (Cdata,Implied),
            'partition_length': (Cdata,Implied),
            'positive': (("up","down"),Implied),
            'spacing': (("uniform","variable","disjoint"),Implied),
            'topology': (("linear","circular"),Implied),
            'weights': (Idref,Implied),
            'units': (Cdata,Required),
            }
        dtd['component'] = {
            'name':(Idref,Required),
            }
        dtd['dataset'] = {
            'id': (Id,Required),
            'Conventions': (Cdata,Required),
            'appendices': (Cdata,Implied),
            'calendar': (Cdata,Implied),
            'cdms_filemap': (Cdata,Implied),
            'comment': (Cdata,Implied),
            'directory': (Cdata,Implied),
            'frequency': (Cdata,Implied),
            'history': (Cdata,Implied),
            'institution': (Cdata,Implied),
            'production': (Cdata,Implied),
            'project': (Cdata,Implied),
            'template': (Cdata,Implied),
            }
        dtd['doclink'] = {
            'id': (Id,Implied),
            'xml:link': (Cdata,(Fixed,"simple")),
            'href': (Cdata,Required),
            'role':	(Cdata,Implied),
            'title': (Cdata,Implied),
            'show': (("embed","replace","new"),"replace"),
            'actuate': (("auto","user"),Implied),
            'behavior':(Cdata,Implied),
            'content-role': (Cdata,Implied),
            'content-title': (Cdata,Implied),
            'inline':(("true","false"),"true"),
            }
        dtd['domain'] = {}
        dtd['domElem'] = {
            'name':(Idref,Required),
            'length':(Cdata,Implied),
            'partition_length':(Cdata,Implied),
            'start':(Cdata,Implied),
            }
        dtd['rectGrid'] = {
            'id': (Id,Required),
            'type':(("gaussian","uniform","equalarea","unknown"),"unknown"),
            'latitude':(Idref,Required),
            'longitude':(Idref,Required),
            'mask':(Idref,Implied),
            'order':(("xy","yx"),"yx"),
            }
        dtd['linear'] = {
            'delta': (Cdata,Required),
            'length': (Cdata,Required),
            'start': (Cdata,Required),
            }
        dtd['variable'] = {
            'id': (Id,Required),
            'add_offset': (Cdata,Implied),
            'associate': (Cdata,Implied),
            'axis': (Cdata,Implied),
            'comments': (Cdata,Implied),
            'datatype': (("Char","Byte","Short","Int","Long","Int64","Float","Double","String"),Required),
            'grid_name': (Cdata,Implied),
            'grid_type': (Cdata,Implied),
            'long_name': (Cdata,Implied),
            'missing_value': (Cdata, Implied),
            'name_in_file': (Cdata,Implied),
            'scale_factor': (Cdata,Implied),
            'subgrid': (Cdata,Implied),
            'template': (Cdata,Implied),
            'units': (Cdata,Implied),
            'valid_max': (Cdata,Implied),
            'valid_min': (Cdata,Implied),
            'valid_range': (Cdata,Implied),
            }
        dtd['xlink'] = {
            'id': (Id,Implied),
            'xml:link': (Cdata,(Fixed,"simple")),
            'href': (Cdata,Required),
            'role':	(Cdata,Implied),
            'title': (Cdata,Implied),
            'show': (("embed","replace","new"),"embed"),
            'actuate': (("auto","user"),Implied),
            'behavior':(Cdata,Implied),
            'content-role': (("dataset","axis","grid","variable","object"),"object"),
            'content-title': (Cdata,Implied),
            'inline':(("true","false"),"true"),
            }
        return dtd

    # Extra datatype information not included in the formal DTD.
    def buildExtra(self,uri):
        extra = {}
        extra['attr'] = {
            'name': (CdScalar,CdString),
            'datatype': (CdScalar,CdString),
            }
        extra['axis'] = {
            'id': (CdScalar,CdString),
            'associate': (CdScalar,CdString),
            'axis': (CdScalar,CdString),
            'bounds': (CdArray,CdFromObject),
            'calendar': (CdScalar,CdString),
            'comment': (CdScalar,CdString),
            'component': (CdScalar,CdString),
            'compress': (CdScalar,CdString),
            'datatype': (CdScalar,CdString),
            'expand': (CdScalar,CdString),
            'interval': (CdScalar,CdFromObject),
            'isvar': (CdScalar,CdString),
            'length': (CdScalar,CdInt),
            'long_name': (CdScalar,CdString),
            'modulo': (CdScalar,CdAny),
            'name_in_file': (CdScalar,CdString),
            'partition': (CdArray,CdInt),
            'partition_length': (CdScalar,CdInt),
            'positive': (CdScalar,CdString),
            'spacing': (CdScalar,CdFromObject),
            'topology': (CdScalar,CdString),
            'weights': (CdArray,CdDouble),
            'units': (CdScalar,CdString),
            }
        extra['component'] = {
            'name': (CdScalar,CdString),
            }
        extra['dataset'] = {
            'id': (CdScalar,CdString),
            'Conventions': (CdScalar,CdString),
            'appendices': (CdScalar,CdString),
            'calendar': (CdScalar,CdString),
            'cdms_filemap': (CdScalar,CdString),
            'comment': (CdScalar,CdString),
            'directory': (CdScalar,CdString),
            'frequency': (CdScalar,CdString),
            'history': (CdScalar,CdString),
            'institution': (CdScalar,CdString),
            'production': (CdScalar,CdString),
            'project': (CdScalar,CdString),
            'template': (CdScalar,CdString),
            }
        extra['doclink'] = {
            'id': (CdScalar,CdString),
            'xml:link': (CdScalar,CdString),
            'href': (CdScalar,CdString),
            'role': (CdScalar,CdString),
            'title': (CdScalar,CdString),
            'show': (CdScalar,CdString),
            'actuate': (CdScalar,CdString),
            'behavior': (CdScalar,CdString),
            'content-role': (CdScalar,CdString),
            'content-title': (CdScalar,CdString),
            'inline': (CdScalar,CdString),
            }
        extra['domain'] = {}
        extra['domElem'] = {
            'name': (CdScalar,CdString),
            'length': (CdScalar,CdInt),
            'partition_length': (CdScalar,CdInt),
            'start': (CdScalar,CdInt),
            }
        extra['rectGrid'] = {
            'id': (CdScalar,CdString),
            'type': (CdScalar,CdString),
            'latitude': (CdScalar,CdString),
            'longitude': (CdScalar,CdString),
            'mask': (CdScalar,CdString),
            'order': (CdScalar,CdString),
            }
        extra['linear'] = {
            'delta': (CdScalar,CdFromObject),
            'length': (CdScalar,CdInt),
            'start': (CdScalar,CdInt),
            }
        extra['variable'] = {
            'id': (CdScalar,CdString),
            'add_offset': (CdScalar,CdDouble),
            'associate': (CdScalar,CdString),
            'axis': (CdScalar,CdString),
            'comments': (CdScalar,CdString),
            'datatype': (CdScalar,CdString),
            'grid_name': (CdScalar,CdString),
            'grid_type': (CdScalar,CdString),
            'long_name': (CdScalar,CdString),
            'missing_value': (CdScalar,CdFromObject),
            'name_in_file': (CdScalar,CdString),
            'scale_factor': (CdScalar,CdDouble),
            'subgrid': (CdScalar,CdString),
            'template': (CdScalar,CdString),
            'units': (CdScalar,CdString),
            'valid_max': (CdScalar,CdFromObject),
            'valid_min': (CdScalar,CdFromObject),
            'valid_range': (CdArray,CdFromObject),
            }
        extra['xlink'] = {
            'id': (CdScalar,CdString),
            'xml:link': (CdScalar,CdString),
            'href': (CdScalar,CdString),
            'role': (CdScalar,CdString),
            'title': (CdScalar,CdString),
            'show': (CdScalar,CdString),
            'actuate': (CdScalar,CdString),
            'behavior': (CdScalar,CdString),
            'content-role': (CdScalar,CdString),
            'content-title': (CdScalar,CdString),
            'inline': (CdScalar,CdString),
            }
        return extra

if __name__=='__main__':
    cdml = CDML()
    print cdml.extra
    cdml2 = CDML()
    print cdml2.extra
