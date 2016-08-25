"""
# Isofill (Gfi) module
"""
#
#
# Module:       isofill (Gfi) module                                          #
#
# Copyright:    2000, Regents of the University of California                 #
# This software may not be distributed to others without        #
# permission of the author.                                     #
#
# Author:       PCMDI Software Team                                           #
# Lawrence Livermore NationalLaboratory:                        #
# support@pcmdi.llnl.gov                                        #
#
# Description:  Python command wrapper for VCS's isofill graphics method.     #
#
# Version:      5.0                                                           #
#
#
#
#
#
#
#
# Import: VCS C extension module.                                             #
#
#
import vcs
import VCS_validation_functions
import cdtime
import xmldocs


def load(nm, json_dict={}):
    return


def process_src(nm, code):

    # Takes VCS script code (string) as input and generates isofill gm from it
    try:
        g = Gfi(nm)
    except:
        g = vcs.elements["isofill"][nm]
    # process attributes with = as assignement
    for att in ["projection",
                "xticlabels#1", "xticlabels#2",
                "xmtics#1", "xmtics#2",
                "yticlabels#1", "yticlabels#2",
                "ymtics#1", "ymtics#2",
                "xaxisconvert", "yaxisconvert",
                "datawc_tunits",
                "boxfill_type",
                "legend",
                "ext_1", "ext_2",
                "missing",
                "datawc_calendar"]:
        i = code.find(att)
        if i == -1:
            continue
        j = code[i:].find(",") + i
        if j - i == -1:  # last one no comma
            j = None
        scode = code[i:j]
        sp = scode.split("=")
        nm = sp[0].strip()
        nm = nm.replace("#", "")
        if nm == "datawc_tunits":
            nm = "datawc_timeunits"
        if nm == "legend":
            if sp[1] != "()":
                g.legend = sp[1][1:-1]
            continue
        try:
            # int will be converted
            setattr(g, nm, int(sp[1]))
        except:
            try:
                # int and floats will be converted
                setattr(g, nm, eval(sp[1]))
            except:
                # strings
                try:
                    setattr(g, nm, sp[1])
                except:
                    pass  # oh well we stick to default value
    # Datawc
    idwc = code.find(" datawc(")
    if idwc > -1:
        jdwc = code[idwc:].find(")") + idwc
        cd = code[idwc + 8:jdwc]
        vals = cd.split(",")
        g.datawc_x1 = float(vals[0])
        g.datawc_y1 = float(vals[1])
        g.datawc_x2 = float(vals[2])
        g.datawc_y2 = float(vals[3])
    # idatawc
    idwc = code.find("idatawc(")
    if idwc > -1:
        jdwc = code[idwc:].find(")") + idwc
        cd = code[idwc + 8:jdwc]
        vals = cd.split(",")
        if int(vals[0]) == 1:
            g.datawc_x1 = cdtime.reltime(
                g.datawc_x1,
                g.datawc_timeunits).tocomp(
                g.datawc_calendar)
        if int(vals[1]) == 1:
            g.datawc_y1 = cdtime.reltime(
                g.datawc_x2,
                g.datawc_timeunits).tocomp(
                g.datawc_calendar)
        if int(vals[2]) == 1:
            g.datawc_x2 = cdtime.reltime(
                g.datawc_y1,
                g.datawc_timeunits).tocomp(
                g.datawc_calendar)
        if int(vals[3]) == 1:
            g.datawc_y2 = cdtime.reltime(
                g.datawc_y2,
                g.datawc_timeunits).tocomp(
                g.datawc_calendar)

    vcs.utils.process_range_from_old_scr(code, g)


class Gfi(object):

    __doc__ = """
    The Isofill graphics method fills the area between selected isolevels
    (levels of constant value) of a two-dimensional array with a
    user-specified color. The example below shows how to display an isofill
    plot on the VCS Canvas and how to create and remove isofill isolevels.

    This class is used to define an isofill table entry used in VCS, or it
    can be used to change some or all of the isofill attributes in an
    existing isofill table entry.

    .. describe:: Useful Functions:

        .. code-block:: python

            # VCS Canvas Constructor
            a=vcs.init()
            # Show predefined isofill graphics methods
            a.show('isofill')
            # Show predefined fillarea objects
            a.show('fillarea')
            # Show predefined template objects
            a.show('template')
            # Change the VCS color map
            a.setcolormap("AMIP")
            # Create a template
            a.createtemplate('test')
            # Create a fillarea
            a.createfillarea('fill')
            # Get an existing template
            a.gettemplate('AMIP')
            # Get an existing fillarea
            a.getfillarea('def37')
            # Plot array 's' with isofill 'i' and template 't'
            a.isofill(s,i,t)
            # Updates the VCS Canvas at user's request
            a.update()

    .. describe:: Creating an isofill object:

        .. code-block:: python

            #Create a VCS Canvas
            a=vcs.init()
            #Create a new instance of isofill:
            # Copies content of 'quick' to 'new'
            iso=a.createisofill('new','quick')
            # Copies content of 'default' to 'new'
            iso=a.createisofill('new')

    .. describe:: Modifying an existing isofill:

        .. code-block:: python

            iso=a.getisofill('AMIP_psl')

    .. describe:: Overview of isofill attributes:

        * List all isofill attribute values:

            .. code-block:: python

                iso.list()

        * Set isofill attributes:

            .. code-block:: python

                iso.projection='linear'
                lon30={-180:'180W',-150:'150W',0:'Eq'}
                iso.xticlabels1=lon30
                iso.xticlabels2=lon30
                # Will set them both
                iso.xticlabels(lon30, lon30)
                iso.xmtics1=''
                iso.xmtics2=''
                # Will set them both
                iso.xmtics(lon30, lon30)
                iso.yticlabels1=lat10
                iso.yticlabels2=lat10
                # Will set them both
                iso.yticlabels(lat10, lat10)
                iso.ymtics1=''
                iso.ymtics2=''
                # Will set them both
                iso.ymtics(lat10, lat10)
                iso.datawc_y1=-90.0
                iso.datawc_y2=90.0
                iso.datawc_x1=-180.0
                iso.datawc_x2=180.0
                # Will set them all
                iso.datawc(-90, 90, -180, 180)
                iso.xaxisconvert='linear'
                iso.yaxisconvert='linear'
                # Will set them both
                iso.xyscale('linear', 'area_wt')
                # Color index value range 0 to 255
                iso.missing=241
                iso.legend=None
                ext_1='n'
                ext_2='y'
                # Will set them both
                iso.exts('n', 'y' )

        * Setting the isofill levels:

            .. code-block:: python

                # 1) When levels are all contiguous:
                    iso.levels=([0,20,25,30,35,40],)
                    iso.levels=([0,20,25,30,35,40,45,50])
                    iso.levels=[0,20,25,30,35,40]
                    iso.levels=(0.0,20.0,25.0,30.0,35.0,40.0,50.0)

                # 2) When levels are not contiguous:
                    iso.levels=([0,20],[30,40],[50,60])
                    iso.levels=([0,20,25,30,35,40],[30,40],[50,60])

        * Setting the fillarea color indices:

            .. code-block:: python

                iso.fillareacolors=([22,33,44,55,66,77])
                iso.fillareacolors=(16,19,33,44)
                iso.fillareacolors=None

        * Setting the fillarea style:

            .. code-block:: python

                iso.fillareastyle = 'solid'
                iso.fillareastyle = 'hatch'
                iso.fillareastyle = 'pattern'

        * Setting the fillarea hatch or pattern indices:

            .. code-block:: python

                iso.fillareaindices=([1,3,5,6,9,20])
                iso.fillareaindices=(7,1,4,9,6,15)

    .. describe:: Using the fillarea secondary object (Ex):

            * Create a new instance of fillarea:

                .. code-block:: python

                    f=createfillarea('fill1')

            * Create a new isofill:

                .. code-block:: python

                    # Copies 'quick' to 'new'
                    fill=a.createisofill('new','quick')
                    # Copies 'default' to 'new'
                    fill=a.createisofill('new')

            * Modify an existing isofill:

                .. code-block:: python

                    fill=a.getisofill('def37')

            * Set index using fillarea

                .. code-block:: python

                    iso.fillareaindices=(7,fill,4,9,fill,15)
                    # list fillarea attributes
                    fill.list()
                    # change style
                    fill.style='hatch'
                    # change color
                    fill.color=241
                    # change style index
                    fill.index=3

    .. describe:: Attribute descriptions:

        %s
        %s
""" % (xmldocs.graphics_method_core, xmldocs.isofill_doc)
    colormap = VCS_validation_functions.colormap
    __slots__ = [
        '__doc__',
        'colormap',
        '_colormap',
        'name',
        'g_name',
        'xaxisconvert',
        'yaxisconvert',
        'levels',
        'fillareacolors',
        'fillareastyle',
        'fillareaindices',
        'fillareaopacity',
        'ext_1',
        'ext_2',
        'missing',
        'projection',
        'xticlabels1',
        'xticlabels2',
        'yticlabels1',
        'yticlabels2',
        'xmtics1',
        'xmtics2',
        'ymtics1',
        'ymtics2',
        'datawc_x1',
        'datawc_x2',
        'datawc_y1',
        'datawc_y2',
        'legend',
        'datawc_timeunits',
        'datawc_calendar',
        '_name',
        '_xaxisconvert',
        '_yaxisconvert',
        '_levels',
        '_fillareacolors',
        '_fillareastyle',
        '_fillareaindices',
        '_fillareaopacity',
        '_ext_1',
        '_ext_2',
        '_missing',
        '_projection',
        '_xticlabels1',
        '_xticlabels2',
        '_yticlabels1',
        '_yticlabels2',
        '_xmtics1',
        '_xmtics2',
        '_ymtics1',
        '_ymtics2',
        '_datawc_x1',
        '_datawc_x2',
        '_datawc_y1',
        '_datawc_y2',
        '_legend',
        '_datawc_timeunits',
        '_datawc_calendar',
        'info',
    ]

    def _getname(self):
        return self._name

    def _setname(self, value):
        value = VCS_validation_functions.checkname(self, 'name', value)
        self._name = value
    name = property(_getname, _setname)

    def _getcalendar(self):
        return self._datawc_calendar

    def _setcalendar(self, value):
        value = VCS_validation_functions.checkCalendar(
            self,
            'datawc_calendar',
            value)
        self._datawc_calendar = value
    datawc_calendar = property(_getcalendar, _setcalendar)

    def _gettimeunits(self):
        return self._datawc_timeunits

    def _settimeunits(self, value):
        value = VCS_validation_functions.checkTimeUnits(
            self,
            'datawc_timeunits',
            value)
        self._datawc_timeunits = value
    datawc_timeunits = property(_gettimeunits, _settimeunits)

    def _getxaxisconvert(self):
        return self._xaxisconvert

    def _setxaxisconvert(self, value):
        value = VCS_validation_functions.checkAxisConvert(
            self,
            'xaxisconvert',
            value)
        self._xaxisconvert = value
    xaxisconvert = property(_getxaxisconvert, _setxaxisconvert)

    def _getyaxisconvert(self):
        return self._yaxisconvert

    def _setyaxisconvert(self, value):
        value = VCS_validation_functions.checkAxisConvert(self, 'yaxisconvert', value)
        self._yaxisconvert = value
    yaxisconvert = property(_getyaxisconvert, _setyaxisconvert)

    def _getfillareacolors(self):
        return self._fillareacolors

    def _setfillareacolors(self, value):
        if value is not None:
            value = VCS_validation_functions.checkColorList(
                self,
                'fillareacolors',
                value)
            self._fillareacolors = value
    fillareacolors = VCS_validation_functions.fillareacolors

    def _getfillareaindices(self):
        return self._fillareaindices

    def _setfillareaindices(self, value):
        if value is not None:
            value = VCS_validation_functions.checkIndicesList(
                self,
                'fillareaindices',
                value)
            self._fillareaindices = value
    fillareaindices = property(_getfillareaindices, _setfillareaindices)

    def _getfillareastyle(self):
        return self._fillareastyle

    def _setfillareastyle(self, value):
        value = VCS_validation_functions.checkFillAreaStyle(
            self,
            'fillareastyle',
            value)
        self._fillareastyle = value
    fillareastyle = property(_getfillareastyle, _setfillareastyle)

    fillareaopacity = VCS_validation_functions.fillareaopacity

    ext_1 = VCS_validation_functions.ext_1
    ext_2 = VCS_validation_functions.ext_2

    def _getmissing(self):
        return self._missing

    def _setmissing(self, value):
        value = VCS_validation_functions.checkColor(self, 'missing', value)
        self._missing = value
    missing = property(_getmissing, _setmissing)

    legend = VCS_validation_functions.legend

    projection = VCS_validation_functions.projection

    def _getxticlabels1(self):
        return self._xticlabels1

    def _setxticlabels1(self, value):
        value = VCS_validation_functions.checkStringDictionary(
            self,
            'xticlabels1',
            value)
        self._xticlabels1 = value
    xticlabels1 = property(_getxticlabels1, _setxticlabels1)

    def _getxticlabels2(self):
        return self._xticlabels2

    def _setxticlabels2(self, value):
        value = VCS_validation_functions.checkStringDictionary(
            self,
            'xticlabels2',
            value)
        self._xticlabels2 = value
    xticlabels2 = property(_getxticlabels2, _setxticlabels2)

    def _getyticlabels1(self):
        return self._yticlabels1

    def _setyticlabels1(self, value):
        value = VCS_validation_functions.checkStringDictionary(
            self,
            'yticlabels1',
            value)
        self._yticlabels1 = value
    yticlabels1 = property(_getyticlabels1, _setyticlabels1)

    def _getyticlabels2(self):
        return self._yticlabels2

    def _setyticlabels2(self, value):
        value = VCS_validation_functions.checkStringDictionary(
            self,
            'yticlabels2',
            value)
        self._yticlabels2 = value
    yticlabels2 = property(_getyticlabels2, _setyticlabels2)

    def _getxmtics1(self):
        return self._xmtics1

    def _setxmtics1(self, value):
        value = VCS_validation_functions.checkStringDictionary(
            self,
            'xmtics1',
            value)
        self._xmtics1 = value
    xmtics1 = property(_getxmtics1, _setxmtics1)

    def _getxmtics2(self):
        return self._xmtics2

    def _setxmtics2(self, value):
        value = VCS_validation_functions.checkStringDictionary(
            self,
            'xmtics2',
            value)
        self._xmtics2 = value
    xmtics2 = property(_getxmtics2, _setxmtics2)

    def _getymtics1(self):
        return self._ymtics1

    def _setymtics1(self, value):
        value = VCS_validation_functions.checkStringDictionary(
            self,
            'ymtics1',
            value)
        self._ymtics1 = value
    ymtics1 = property(_getymtics1, _setymtics1)

    def _getymtics2(self):
        return self._ymtics2

    def _setymtics2(self, value):
        value = VCS_validation_functions.checkStringDictionary(
            self,
            'ymtics2',
            value)
        self._ymtics2 = value
    ymtics2 = property(_getymtics2, _setymtics2)

    def _getdatawc_x1(self):
        return self._datawc_x1

    def _setdatawc_x1(self, value):
        VCS_validation_functions.checkDatawc(self, 'datawc_x1', value)
        self._datawc_x1 = value
    datawc_x1 = property(_getdatawc_x1, _setdatawc_x1)

    def _getdatawc_x2(self):
        return self._datawc_x2

    def _setdatawc_x2(self, value):
        VCS_validation_functions.checkDatawc(self, 'datawc_x2', value)
        self._datawc_x2 = value
    datawc_x2 = property(_getdatawc_x2, _setdatawc_x2)

    def _getdatawc_y1(self):
        return self._datawc_y1

    def _setdatawc_y1(self, value):
        VCS_validation_functions.checkDatawc(self, 'datawc_y1', value)
        self._datawc_y1 = value
    datawc_y1 = property(_getdatawc_y1, _setdatawc_y1)

    def _getdatawc_y2(self):
        return self._datawc_y2

    def _setdatawc_y2(self, value):
        VCS_validation_functions.checkDatawc(self, 'datawc_y2', value)
        self._datawc_y2 = value
    datawc_y2 = property(_getdatawc_y2, _setdatawc_y2)

    levels = VCS_validation_functions.levels

    def __init__(self, Gfi_name, Gfi_name_src='default'):
                #
                #
                # Initialize the isofill class and its members            #
                # The getGfimember function retrieves the values of the   #
                # isofill members in the C structure and passes back the  #
                # appropriate Python Object.                              #
                #
                #
        if not isinstance(Gfi_name, str):
            raise ValueError("Isofill name must be a string")
        if Gfi_name in vcs.elements["isofill"].keys():
            raise ValueError(
                "isofill graphic method '%s' already exists" %
                Gfi_name)
        self._name = Gfi_name
        self.g_name = 'Gfi'

        if Gfi_name == "default":
            self._projection = "linear"
            self._xticlabels1 = "*"
            self._xticlabels2 = "*"
            self._xmtics1 = ""
            self._xmtics2 = ""
            self._yticlabels1 = "*"
            self._yticlabels2 = "*"
            self._ymtics1 = ""
            self._ymtics2 = ""
            self._datawc_y1 = 1.e20
            self._datawc_y2 = 1.e20
            self._datawc_x1 = 1.e20
            self._datawc_x2 = 1.e20
            self._xaxisconvert = 'linear'
            self._yaxisconvert = 'linear'
            self._missing = 241
            self._ext_1 = False
            self._ext_2 = False
            self._fillareastyle = 'solid'
            self._fillareaindices = [1, ]
            self._fillareacolors = [1, ]
            self._fillareaopacity = []
            self._levels = ([1.0000000200408773e+20, 1.0000000200408773e+20],)
            self._legend = None
            self._datawc_timeunits = "days since 2000"
            self._datawc_calendar = 135441
            self._colormap = None
        else:
            if isinstance(Gfi_name_src, Gfi):
                Gfi_name_src = Gfi_name_src.name
            if Gfi_name_src not in vcs.elements["isofill"].keys():
                raise ValueError(
                    "Isofill method '%s' does not exists" %
                    Gfi_name_src)
            src = vcs.elements["isofill"][Gfi_name_src]
            self._ext_1 = False
            self._ext_2 = False
            for att in ['projection', 'colormap', 'xticlabels1', 'xticlabels2', 'xmtics1', 'xmtics2',
                        'yticlabels1', 'yticlabels2', 'ymtics1', 'ymtics2', 'datawc_y1', 'datawc_y2', 'datawc_x1',
                        'datawc_x2', 'levels', 'xaxisconvert', 'yaxisconvert', 'missing', 'ext_1', 'ext_2',
                        'fillareastyle', 'fillareaindices', 'fillareacolors', 'fillareaopacity', 'legend',
                        'datawc_timeunits', 'datawc_calendar']:
                setattr(self, "_" + att, getattr(src, "_" + att))

        vcs.elements["isofill"][self.name] = self
        #
        #
        # Find and set the isofill structure in VCS C pointer     #
        # list. If the isofill name does not exist, then use      #
        # default isofill.                                        #
        #
        #

    def colors(self, color1=16, color2=239):
        self.fillareacolors = range(color1, color2)
    colors.__doc__ = xmldocs.colorsdoc

    def exts(self, ext1='n', ext2='y'):
        self.ext_1 = ext1
        self.ext_2 = ext2
    exts.__doc__ = xmldocs.extsdoc
#
# Doesn't make sense to inherit. This would mean more coding in C.
# I put this code back.
#

    def xticlabels(self, xtl1='', xtl2=''):
        self.xticlabels1 = xtl1
        self.xticlabels2 = xtl2
    xticlabels.__doc__ = xmldocs.xticlabelsdoc

    def xmtics(self, xmt1='', xmt2=''):
        self.xmtics1 = xmt1
        self.xmtics2 = xmt2
    xmtics.__doc__ = xmldocs.xmticsdoc

    def yticlabels(self, ytl1='', ytl2=''):
        self.yticlabels1 = ytl1
        self.yticlabels2 = ytl2
    yticlabels.__doc__ = xmldocs.yticlabelsdoc

    def ymtics(self, ymt1='', ymt2=''):
        self.ymtics1 = ymt1
        self.ymtics2 = ymt2
    ymtics.__doc__ = xmldocs.ymticsdoc

    def datawc(self, dsp1=1e20, dsp2=1e20, dsp3=1e20, dsp4=1e20):
        self.datawc_y1 = dsp1
        self.datawc_y2 = dsp2
        self.datawc_x1 = dsp3
        self.datawc_x2 = dsp4
    datawc.__doc__ = xmldocs.datawcdoc

    def xyscale(self, xat='', yat=''):
        self.xaxisconvert = xat
        self.yaxisconvert = yat
    xyscale.__doc__ = xmldocs.xyscaledoc

    def list(self):
        print "", "----------Isofill (Gfi) member (attribute) listings ----------"
        print "graphics method =", self.g_name
        print "name =", self.name
        print "projection =", self.projection
        print "xticlabels1 =", self.xticlabels1
        print "xticlabels2 =", self.xticlabels2
        print "xmtics1 =", self.xmtics1
        print "xmtics2 =", self.xmtics2
        print "yticlabels1 =", self.yticlabels1
        print "yticlabels2 =", self.yticlabels2
        print "ymtics1 = ", self.ymtics1
        print "ymtics2 = ", self.ymtics2
        print "datawc_x1 =", self.datawc_x1
        print "datawc_y1 = ", self.datawc_y1
        print "datawc_x2 = ", self.datawc_x2
        print "datawc_y2 = ", self.datawc_y2
        print "datawc_timeunits = ", self.datawc_timeunits
        print "datawc_calendar = ", self.datawc_calendar
        print "xaxisconvert = ", self.xaxisconvert
        print "yaxisconvert = ", self.yaxisconvert
        print "missing = ", self.missing
        print "ext_1 = ", self.ext_1
        print "ext_2 = ", self.ext_2
        print "fillareastyle = ", self.fillareastyle
        print "fillareaindices = ", self.fillareaindices
        print "fillareacolors = ", self.fillareacolors
        print "fillareaopacity = ", self.fillareaopacity
        print "levels = ", self.levels
        print "legend = ", self.legend
    list.__doc__ = xmldocs.listdoc

    #
    #
    # Script out primary isofill graphics method in VCS to a file.            #
    #
    #
    def script(self, script_filename, mode='a'):
        if (script_filename is None):
            raise ValueError(
                'Error - Must provide an output script file name.')

        if (mode is None):
            mode = 'a'
        elif (mode not in ('w', 'a')):
            raise ValueError(
                'Error - Mode can only be "w" for replace or "a" for append.')

        # By default, save file in json
        scr_type = script_filename.split(".")
        if len(scr_type) == 1 or len(scr_type[-1]) > 5:
            scr_type = "json"
            if script_filename != "initial.attributes":
                script_filename += ".json"
        else:
            scr_type = scr_type[-1]
        if scr_type == '.scr':
            raise DeprecationWarning("scr script are no longer generated")
        elif scr_type == "py":
            mode = mode + '+'
            py_type = script_filename[
                len(script_filename) -
                3:len(script_filename)]
            if (py_type != '.py'):
                script_filename = script_filename + '.py'

            # Write to file
            fp = open(script_filename, mode)
            if (fp.tell() == 0):  # Must be a new file, so include below
                fp.write("#####################################\n")
                fp.write("#                                 #\n")
                fp.write("# Import and Initialize VCS     #\n")
                fp.write("#                             #\n")
                fp.write("#############################\n")
                fp.write("import vcs\n")
                fp.write("v=vcs.init()\n\n")

            unique_name = '__Gfi__' + self.name
            fp.write(
                "#----------Isofill (Gfi) member (attribute) listings ----------\n")
            fp.write("gfi_list=v.listelements('isofill')\n")
            fp.write("if ('%s' in gfi_list):\n" % self.name)
            fp.write("   %s = v.getisofill('%s')\n" % (unique_name, self.name))
            fp.write("else:\n")
            fp.write(
                "   %s = v.createisofill('%s')\n" %
                (unique_name, self.name))
            # Common core graphics method attributes
            fp.write("%s.projection = '%s'\n" % (unique_name, self.projection))
            fp.write(
                "%s.xticlabels1 = '%s'\n" %
                (unique_name, self.xticlabels1))
            fp.write(
                "%s.xticlabels2 = '%s'\n" %
                (unique_name, self.xticlabels2))
            fp.write("%s.xmtics1 = '%s'\n" % (unique_name, self.xmtics1))
            fp.write("%s.xmtics2 = '%s'\n" % (unique_name, self.xmtics2))
            fp.write(
                "%s.yticlabels1 = '%s'\n" %
                (unique_name, self.yticlabels1))
            fp.write(
                "%s.yticlabels2 = '%s'\n" %
                (unique_name, self.yticlabels2))
            fp.write("%s.ymtics1 = '%s'\n" % (unique_name, self.ymtics1))
            fp.write("%s.ymtics2 = '%s'\n" % (unique_name, self.ymtics2))
            if isinstance(self.datawc_x1, (int, long, float)):
                fp.write("%s.datawc_x1 = %g\n" % (unique_name, self.datawc_x1))
            else:
                fp.write(
                    "%s.datawc_x1 = '%s'\n" %
                    (unique_name, self.datawc_x1))
            if isinstance(self.datawc_y1, (int, long, float)):
                fp.write("%s.datawc_y1 = %g\n" % (unique_name, self.datawc_y1))
            else:
                fp.write(
                    "%s.datawc_y1 = '%s'\n" %
                    (unique_name, self.datawc_y1))
            if isinstance(self.datawc_x2, (int, long, float)):
                fp.write("%s.datawc_x2 = %g\n" % (unique_name, self.datawc_x2))
            else:
                fp.write(
                    "%s.datawc_x2 = '%s'\n" %
                    (unique_name, self.datawc_x2))
            if isinstance(self.datawc_y2, (int, long, float)):
                fp.write("%s.datawc_y2 = %g\n" % (unique_name, self.datawc_y2))
            else:
                fp.write(
                    "%s.datawc_y2 = '%s'\n" %
                    (unique_name, self.datawc_y2))
            fp.write(
                "%s.datawc_calendar = %g\n" %
                (unique_name, self.datawc_calendar))
            fp.write(
                "%s.datawc_timeunits = '%s'\n\n" %
                (unique_name, self.datawc_timeunits))
            fp.write(
                "%s.xaxisconvert = '%s'\n" %
                (unique_name, self.xaxisconvert))
            fp.write(
                "%s.yaxisconvert = '%s'\n" %
                (unique_name, self.yaxisconvert))
            # Unique attribute for isofill
            fp.write("%s.missing = %g\n" % (unique_name, self.missing))
            fp.write("%s.ext_1 = '%s'\n" % (unique_name, self.ext_1))
            fp.write("%s.ext_2 = '%s'\n" % (unique_name, self.ext_2))
            fp.write(
                "%s.fillareastyle = '%s'\n" %
                (unique_name, self.fillareastyle))
            fp.write(
                "%s.fillareaindices = %s\n" %
                (unique_name, self.fillareaindices))
            fp.write(
                "%s.fillareacolors = %s\n" %
                (unique_name, self.fillareacolors))
            fp.write(
                "%s.fillareaopacity = '%s'\n" %
                (unique_name, self.fillareaopacity))
            fp.write("%s.levels = %s\n" % (unique_name, self.levels))
            fp.write("%s.legend = %s\n" % (unique_name, self.legend))
            fp.write(
                "%s.colormap = '%s'\n\n" %
                (unique_name, repr(
                    self.colormap)))
        else:
            # Json type
            mode += "+"
            f = open(script_filename, mode)
            vcs.utils.dumpToJson(self, f)
            f.close()
    script.__doc__ = xmldocs.isofill_script


#
# END OF FILE						              #
#
