"""
# Text Orientation (To) module
"""
##########################################################################
#                                                                               #
# Module:       textorientation (To) module                                     #
#                                                                               #
# Copyright:    2000, Regents of the University of California                   #
#               This software may not be distributed to others without          #
#               permission of the author.                                       #
#                                                                               #
# Author:       PCMDI Software Team                                             #
#               Lawrence Livermore NationalLaboratory:                          #
#               support@pcmdi.llnl.gov                                          #
#                                                                               #
# Description:  Python command wrapper for VCS's text orientation secondary 	#
#               object.                                                         #
#                                                                               #
# Version:      4.0                                                             #
#                                                                               #
##########################################################################
#
#
#
import VCS_validation_functions
import vcs
from xmldocs import textorientation_script


def process_src(nm, code):

    # Takes VCS script code (string) as input and generates boxfill gm from it
    try:
        to = To(nm)
    except:
        to = vcs.elements["textorientation"][nm]
    # process attributes with = as assignement
    sp = code.split(",")
    to.height = int(float(sp[0]) * 1000)
    to.angle = int(sp[1])
    to.path = ["r", "l", "u", "d"].index(sp[2])
    to.halign = ["l", "c", "r"].index(sp[3])
    to.valign = ["t", "c", "h", "b", "s"].index(sp[4])


#############################################################################
#                                                                           #
# Text Orientation (To) Class.                                              #
#                                                                           #
#############################################################################
class To(object):

    """
    The (To) Text Orientation lists text attribute set names that define the font, spacing,
    expansion, and color index.

    This class is used to define an text orientation table entry used in VCS, or it
    can be used to change some or all of the text orientation attributes in an
    existing text orientation table entry.

    .. describe:: Useful Functions:

        .. code-block:: python

            # VCS Canvas Constructor
            a=vcs.init()
            # Show predefined text orientation objects
            a.show('textorientation')
            # Updates the VCS Canvas at user's request
            a.update()

    .. describe:: Make a canvas object to work with:

        .. code-block:: python

            a=vcs.init()

    .. describe:: Create a new instance of text orientation:

        .. code-block:: python

            # Copies content of '7left' to 'new'
            to=a.createtextorientation('new','7left')
            # Copies content of 'default' to 'new'
            to=a.createtextorientation('new')

    .. describe:: Modify an existing textorientation:

        .. code-block:: python

            to=a.gettextorientation('7left')

    .. describe:: Overview of textorientation attributes:

        * Listing the attributes:

            .. code-block:: python

                # Will list all the textorientation attribute values
                to.list()

        * Specify the text height:

            .. code-block:: python

                # The height value must be an integer
                to.height=20

        * Specify the text angle:

            .. code-block:: python

                # The angle value must be in the range 0 to 360
                to.angle=0

        * Specify the text path:

            .. code-block:: python

                # Same as to.path=0
                to.path='right'
                # Same as to.path=1
                to.path='left'
                # Same as to.path=2
                to.path='up'
                # Same as to.path=3
                to.path='down'

        * Specify the text horizontal alignment:

            .. code-block:: python

                # Same as to.halign=0
                to.halign='right'
                # Same as to.halign=1
                to.halign='center'
                # Same as to.halign=2
                to.halign='right'

        * Specify the text vertical alignment:

            .. code-block:: python

                # Same as tovalign=0
                to.valign='top'
                # Same as tovalign=1
                to.valign='cap'
                # Same as tovalign=2
                to.valign='half'
                # Same as tovalign=3
                to.valign='base'
                # Same as tovalign=4
                to.valign='bottom'
    """
    __slots__ = [
        's_name',
        'name',
        'height',
        'angle',
        'path',
        'halign',
        'valign',
        '_name',
        '_height',
        '_angle',
        '_path',
        '_halign',
        '_valign',
    ]

    def _getname(self):
        return self._name

    def _setname(self, value):
        value = VCS_validation_functions.checkname(self, 'name', value)
        self._name = value
    name = property(_getname, _setname)

    def _getheight(self):
        return self._height

    def _setheight(self, value):
        self._height = VCS_validation_functions.checkNumber(
            self,
            'height',
            value)
    height = property(_getheight, _setheight)

    def _getangle(self):
        return self._angle

    def _setangle(self, value):
        self._angle = VCS_validation_functions.checkInt(
            self,
            'angle',
            value,
            minvalue=-
            360,
            maxvalue=360)
    angle = property(_getangle, _setangle)

    def _getpath(self):
        return self._path

    def _setpath(self, value):
        vals = ["right", "left", "up", "down"]
        self._path = VCS_validation_functions.checkInStringsListInt(
            self,
            'path',
            value,
            vals)
    path = property(_getpath, _setpath)

    def _gethalign(self):
        return self._halign

    def _sethalign(self, value):
        vals = ["left", "center", "right"]
        self._halign = VCS_validation_functions.checkInStringsListInt(
            self,
            'halign',
            value,
            vals)
    halign = property(_gethalign, _sethalign)

    def _getvalign(self):
        return self._valign

    def _setvalign(self, value):
        vals = ["top", "cap", "half", "base", "bottom"]
        self._valign = VCS_validation_functions.checkInStringsListInt(
            self,
            'valign',
            value,
            vals)
    valign = property(_getvalign, _setvalign)

    ##########################################################################
    #                                                                           #
    # Initialize the text orientation attributes.                               #
    #                                                                           #
    ##########################################################################
    def __init__(self, To_name, To_name_src='default'):
        #                                                           #
        #############################################################
        # Initialize the text orientation class and its members     #
        #                                                           #
        # The getTomember function retrieves the values of the      #
        # text orientation members in the C structure and passes    #
        # back the appropriate Python Object.                       #
        #############################################################
        #                                                           #
        if To_name in vcs.elements["textorientation"].keys():
            raise ValueError(
                "textorientation object '%' already exists" %
                To_name)
        self._name = To_name
        if isinstance(To_name_src, To):
            To_name_src = To_name_src.name
        self.s_name = 'To'
        if To_name == "default":
            self._height = 14
            self._angle = 0
            self._path = "right"
            self._halign = "left"
            self._valign = "half"
        else:
            if To_name_src not in vcs.elements["textorientation"].keys():
                raise ValueError(
                    "source textorientation '%s' does not exists" %
                    To_name_src)
            src = vcs.elements["textorientation"][To_name_src]
            self.height = src.height
            self.angle = src.angle
            self.path = src.path
            self.halign = src.halign
            self.valign = src.valign
        vcs.elements["textorientation"][To_name] = self

    ##########################################################################
    #                                                                           #
    # List out text orientation members (attributes).                           #
    #                                                                           #
    ##########################################################################
    def list(self):
        if (self.name == '__removed_from_VCS__'):
            raise ValueError('This instance has been removed from VCS.')
        print "", "----------Text Orientation (To) member (attribute) listings ----------"
        print "secondary method =", self.s_name
        print "name =", self.name
        print "height =", self.height
        print "angle =", self.angle
        print "path =", self.path
        print "halign =", self.halign
        print "valign =", self.valign

    ##########################################################################
    #                                                                           #
    # Script out secondary text orientation method in VCS to a file.            #
    #                                                                           #
    ##########################################################################
    def script(self, script_filename=None, mode=None):
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

            unique_name = '__To__' + self.name
            fp.write(
                "#----------Text Orientation (To) member (attribute) listings ----------\n")
            fp.write("to_list=v.listelements('textorientation')\n")
            fp.write("if ('%s' in to_list):\n" % self.name)
            fp.write(
                "   %s = v.gettextorientation('%s')\n" %
                (unique_name, self.name))
            fp.write("else:\n")
            fp.write(
                "   %s = v.createtextorientation('%s')\n" %
                (unique_name, self.name))
            fp.write("%s.height = %g\n" % (unique_name, self.height))
            fp.write("%s.angle = %g\n" % (unique_name, self.angle))
            fp.write("%s.path = '%s'\n" % (unique_name, self.path))
            fp.write("%s.halign = '%s'\n" % (unique_name, self.halign))
            fp.write("%s.valign = '%s'\n\n" % (unique_name, self.valign))
            fp.close()
        else:
            # Json type
            mode += "+"
            f = open(script_filename, mode)
            vcs.utils.dumpToJson(self, f)
            f.close()
    script.__doc__ = textorientation_script
