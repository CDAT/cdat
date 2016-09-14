# Adapted for numpy/ma/cdms2 by convertcdms.py
"""
# Fillarea (Tf) module
"""
#
#
# Module:       fillarea (Tf) module                                            #
#
# Copyright:    2000, Regents of the University of California                   #
# This software may not be distributed to others without          #
# permission of the author.                                       #
#
# Author:       PCMDI Software Team                                             #
# Lawrence Livermore NationalLaboratory:                          #
# support@pcmdi.llnl.gov                                          #
#
# Description:  Python command wrapper for VCS's fill area secondary object.    #
#
# Version:      4.0                                                             #
#
#
#
#
#
import VCS_validation_functions
import vcs
import genutil
from xmldocs import fillarea_script


def getmember(self, name):
    return getattr(self, "_%s" % name)


def process_src(nm, code):
    try:
        f = Tf(nm)
    except:
        f = vcs.elements["fillarea"][nm]
    atts = {}
    if code.find("(") > -1:  # ok with have the keywords spelled out
        # faci: fillarea color index
        # fasi: fillarea style index
        # fais: fillarea index style (hatching/pattern/solid)
        # faoi: ???
        # vp: viewport
        # wc: worldcoordinates
        # x: x coordinates
        # y: y coordinates
        for a in ["faci", "fasi", "fais", "faoi", "vp", "wc", "x", "y"]:
            i = code.find(a + "(")
            v = genutil.get_parenthesis_content(code[i:])
            if v != "":
                vals = []
                for V in v.split(","):
                    try:
                        vals.append(int(V))
                    except:
                        vals.append(float(V))
                    if a in ["fais"]:
                        vals[-1] = vals[-1]-1
                atts[a] = vals
    else:
        sp = code.split(",")
        atts["fais"] = int(sp[0]) - 1
        atts["faci"] = int(sp[2])
        atts["fasi"] = int(sp[1])
        atts["vp"] = [10. * float(sp[-4]), 10. * float(sp[-3]), 10. * float(sp[-2]), 10. * float(sp[-1])]

    f.style = atts.get("fais", f.style)
    try:  # in case these are strings?
        I = atts.get("fasi", f.index)
        for i, v in enumerate(I):
            if v == 0:
                I[i] = 1
    except:
        pass
    f.index = I
    f.color = atts.get("faci", f.color)
    f.viewport = atts.get("vp", f.viewport)
    f.worldcoordinate = atts.get("wc", f.worldcoordinate)
    f.x = atts.get('x', f.x)
    f.y = atts.get('y', f.y)
    i = code.find("projection=")
    if i > -1:
        j = code[i:].find(",") + i
        f.projection = code[i + 11:j]
    for b in vcs.elements["boxfill"].values(
    ) + vcs.elements["isofill"].values() + vcs.elements["meshfill"].values():
        if b.fillareaindices is not None:
            for i, v in enumerate(b.fillareaindices):
                if isinstance(v, str) and v == nm:
                    b._fillareaindices[i] = f.index
                    b._fillareacolor[i] = f.color
                    b._fillareastyle = f.style
                    b._fillareaopacity = f.opacity

#
#
# Fillarea (Tm) Class.                                                      #
#
#


class Tf(object):

    """
    The Fillarea class object allows the user to edit fillarea attributes, including
    fillarea interior style, style index, and color index.

    This class is used to define an fillarea table entry used in VCS, or it
    can be used to change some or all of the fillarea attributes in an
    existing fillarea table entry.


    .. describe:: Useful Functions:

        .. code-block:: python

            # VCS Canvas Constructor
            a=vcs.init()
            # Show predefined fillarea objects
            a.show('fillarea')
            # Updates the VCS Canvas at user's request
            a.update()

    .. describe:: Create a fillarea object:

        .. code-block:: python

            #Create a VCS Canvas object
            a=vcs.init()

            # Two ways to create a fillarea:

            # Copies content of 'def37' to 'new'ea:
            fa=a.createfillarea('new','def37')
            # Copies content of 'default' to 'new'
            fa=a.createfillarea('new')

    .. describe::  Modify an existing fillarea:

        .. code-block:: python

            fa=a.getfillarea('red')

    * Overview of fillarea attributes:

        * List all the fillarea attribute values

            .. code-block:: python

                fa.list()

        * There are three possibilities for setting the isofill style:

            .. code-block:: python

                fa.style = 'solid'
                fa.style = 'hatch'
                fa.style = 'pattern'

        * Setting index, color, opacity:

            .. code-block:: python

                # Range from 1 to 20
                fa.index=1
                # Range from 1 to 256
                fa.color=100
                # Range from 0 to 100
                fa.opacity=100

        * Setting the graphics priority viewport, worldcoordinate:

            .. code-block:: python

                fa.priority=1
                # FloatType [0,1]x[0,1]
                fa.viewport=[0, 1.0, 0,1.0]
                # FloatType [#,#]x[#,#]
                fa.worldcoordinate=[0,1.0,0,1.0]

        * Setting x and y values:

            .. code-block:: python

                #List of FloatTypes
                fa.x=[[0,.1,.2], [.3,.4,.5]]
                # List of FloatTypes
                fa.y=[[.5,.4,.3], [.2,.1,0]]

        """
    __slots__ = [
        'name',
        's_name',
        'color',
        'opacity',
        'priority',
        'style',
        'index',
        'viewport',
        'worldcoordinate',
        'x',
        'y',
        'projection',
        'colormap',
        '_name',
        '_color',
        '_priority',
        '_style',
        '_index',
        '_viewport',
        '_worldcoordinate',
        '_x',
        '_y',
        '_projection',
        '_colormap',
        '_opacity'
    ]

    colormap = VCS_validation_functions.colormap

    def _getname(self):
        return self._name

    def _setname(self, value):
        value = VCS_validation_functions.checkname(self, 'name', value)
        if value is not None:
            self._name = value
    name = property(_getname, _setname)

    def _getfillareacolors(self):
        return getmember(self, 'color')

    def _setfillareacolors(self, value):
        if isinstance(value, Tf):
            value = value.color
        if isinstance(value, (str, int)):
            value = [value, ]
        if value is not None:
            value = VCS_validation_functions.checkColorList(
                self,
                'color',
                value)
        else:
            value = [(0.0, 0.0, 0.0, 100.0)]
        self._color = value
    color = property(_getfillareacolors, _setfillareacolors)

    def _getfillareaopacity(self):
        return getmember(self, 'opacity')

    def _setfillareaopacity(self, value):
        if not isinstance(value, (list, tuple)) and value is not None:
            value = [value, ]
        if value is not None:
            value = VCS_validation_functions.checkOpacitiesList(
                self,
                'opacity',
                value)
        self._opacity = value
    opacity = property(_getfillareaopacity, _setfillareaopacity)

    def _getfillareaindices(self):
        return getmember(self, 'index')

    def _setfillareaindices(self, value):
        if not isinstance(value, (list, tuple)) and value is not None:
            value = [value, ]
        if value is not None:
            value = VCS_validation_functions.checkIndicesList(
                self,
                'index',
                value)
        if value in [(), []]:
            raise ValueError("You cannot set fillarea index to an empty list")
        self._index = value
    index = property(_getfillareaindices, _setfillareaindices)

    def _getfillareastyle(self):
        return getmember(self, 'style')

    def _setfillareastyle(self, value):
        if isinstance(value, (str, int, Tf)):
            value = [value, ]
        vals = []
        for v in value:
            v = VCS_validation_functions.checkFillAreaStyle(self, 'style', v)
            vals.append(v)
        if vals == []:
            raise ValueError("fillareastyle cannot be empty list")
        value = vals
        self._style = value
    style = property(_getfillareastyle, _setfillareastyle)

    def _getpriority(self):
        return getmember(self, 'priority')

    def _setpriority(self, value):
        value = VCS_validation_functions.checkInt(
            self,
            'priority',
            value,
            minvalue=0)
        self._priority = value
    priority = property(_getpriority, _setpriority)

    projection = VCS_validation_functions.projection

    worldcoordinate = VCS_validation_functions.worldcoordinate

    viewport = VCS_validation_functions.viewport

    def _getx(self):
        return getmember(self, 'x')

    def _setx(self, value):
        if value is None:
            self._x = value
            return
        if not isinstance(value, (list, tuple)):
            raise ValueError('x must be a tuple or list of values.')
        try:
            # first we'll see if it is simply a list of values
            value = VCS_validation_functions.checkListOfNumbers(
                self,
                'x',
                value)
        except:
            # ok it was not, so it maybe a list of list of numbers ?
            val = []
            for v in value:
                tmp = VCS_validation_functions.checkListOfNumbers(self, 'x', v)
                val.append(tmp)
            value = val
        self._x = value
    x = property(_getx, _setx)

    def _gety(self):
        return getmember(self, 'y')

    def _sety(self, value):
        if value is None:
            self._y = None
            return
        if not isinstance(value, (list, tuple)):
            raise ValueError('y must be a tuple or list of values.')
        try:
            # first we'll see if it is simply a list of values
            value = VCS_validation_functions.checkListOfNumbers(
                self,
                'y',
                value)
        except:
            # ok it was not, so it maybe a list of list of numbers ?
            val = []
            for v in value:
                tmp = VCS_validation_functions.checkListOfNumbers(self, 'y', v)
                val.append(tmp)
            value = val
        self._y = value
    y = property(_gety, _sety)

    #
    #
    # Initialize the fillarea attributes.                                       #
    #
    #
    def __init__(self, Tf_name=None, Tf_name_src='default'):
                    #
                    #
                    # Initialize the fillarea class and its members           #
                    # The getTfmember function retrieves the values of the    #
                    # fillarea members in the C structure and passes back the #
                    # appropriate Python Object.                              #
                    #
                    #
        if isinstance(Tf_name_src, Tf):
            Tf_name_src = Tf_name_src.name
        if Tf_name_src != "default" and Tf_name_src not in vcs.elements[
                "fillarea"].keys():
            raise ValueError("Fillarea '%s' does not exists" % Tf_name_src)
        if (Tf_name is None):
            raise ValueError('Must provide a fillarea name.')
        else:
            if Tf_name in vcs.elements["fillarea"].keys():
                raise ValueError(
                    "The fillarea '%s' already exists, use getfillarea instead" %
                    Tf_name)
        self._name = Tf_name
        self.s_name = 'Tf'

        if Tf_name == "default":
            self._style = ['solid', ]
            self._index = [1, ]
            self._color = [1, ]
            self._opacity = []
            self._priority = 1
            self._viewport = [0., 1., 0., 1.]
            self._worldcoordinate = [0., 1., 0., 1.]
            self._x = None
            self._y = None
            self._projection = "default"
            self._colormap = None
        else:
            src = vcs.elements["fillarea"][Tf_name_src]
            self.style = src.style
            self.index = src.index
            self.color = src.color
            self.opacity = src.opacity
            self.priority = src.priority
            self.viewport = src.viewport
            self.worldcoordinate = src.worldcoordinate
            self.x = src.x
            self.y = src.y
            self.projection = src.projection
            self.colormap = src.colormap

        vcs.elements["fillarea"][Tf_name] = self

    #
    #
    # Fillarea out line members (attributes).                                   #
    #
    #
    def list(self):
        if (self.name == '__removed_from_VCS__'):
            raise ValueError('This instance has been removed from VCS.')
        print "", "----------Fillarea (Tf) member (attribute) listings ----------"
        print "secondary method =", self.s_name
        print "name =", self.name
        print "style =", self.style
        print "index =", self.index
        print "color =", self.color
        print "opacity =", self.opacity
        print "priority =", self.priority
        print "viewport =", self.viewport
        print "worldcoordinate =", self.worldcoordinate
        print "x =", self.x
        print "y =", self.y
        print "projection =", self.projection
        print "colormap =", self.colormap

    #
    #
    # Script out secondary fillarea method in VCS to a file.                    #
    #
    #
    def script(self, script_filename=None, mode=None):
        """
        Documentation moved to xmldocs.py
        """
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

            unique_name = '__Tf__' + self.name
            fp.write(
                "#----------Fillarea (Tf) member (attribute) listings ----------\n")
            fp.write("tf_list=v.listelements('fillarea')\n")
            fp.write("if ('%s' in tf_list):\n" % self.name)
            fp.write(
                "   %s = v.getfillarea('%s')\n" %
                (unique_name, self.name))
            fp.write("else:\n")
            fp.write(
                "   %s = v.createfillarea('%s')\n" %
                (unique_name, self.name))
            fp.write("%s.style = %s\n" % (unique_name, self.style))
            fp.write("%s.index = %s\n" % (unique_name, self.index))
            fp.write("%s.color = %s\n\n" % (unique_name, self.color))
            fp.write("%s.opacity = %s\n\n" % (unique_name, self.opacity))
            fp.write("%s.priority = %d\n" % (unique_name, self.priority))
            fp.write("%s.viewport = %s\n" % (unique_name, self.viewport))
            fp.write(
                "%s.worldcoordinate = %s\n" %
                (unique_name, self.worldcoordinate))
            fp.write("%s.x = %s\n" % (unique_name, self.x))
            fp.write("%s.y = %s\n\n" % (unique_name, self.y))
            fp.write("%s.projection = %s\n\n" % (unique_name, self.projection))
            fp.write(
                "%s.colormap = %s\n\n" %
                (unique_name, repr(
                    self.colormap)))
        else:
            # Json type
            mode += "+"
            f = open(script_filename, mode)
            vcs.utils.dumpToJson(self, f)
            f.close()
    script.__doc__ = fillarea_script


#
# END OF FILE								#
#
