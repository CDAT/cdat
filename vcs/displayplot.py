# Automatically adapted for numpy.oldnumeric Jul 06, 2007 by numeric2numpy.py

"""
# Display Plot (Dp) module
"""
###############################################################################
#                                                                             #
# Module:       Display Plot (Dp) module                                      #
#                                                                             #
# Copyright:    2000, Regents of the University of California                 #
#               This software may not be distributed to others without        #
#               permission of the author.                                     #
#                                                                             #
# Authors:      PCMDI Software Team                                           #
#               Lawrence Livermore NationalLaboratory:                        #
#               support@pcmdi.llnl.gov                                        #
#                                                                             #
# Description:  Python command wrapper for VCS's display plot object.         #
#                                                                             #
# Version:      4.0                                                           #
#                                                                             #
###############################################################################
#
#
#
import VCS_validation_functions
import vcs


class Dp(object):

    """
    The Display plot object allows the manipulation of the plot name, off,
    priority, template, graphics type, graphics name, and data array(s).

    This class is used to define a display plot table entry used in VCS, or it
    can be used to change some or all of the display plot attributes in an
    existing display plot table entry.

    .. describe:: Useful Functions:

        .. code-block:: python

            # Canvas constructor
            a=vcs.init()
            # Show display plot objects
            a.show('plot')
            # Updates the VCS Canvas at user's request
            a.update()

    .. describe:: General display plot usage:

            .. code-block:: python

                #Create a VCS Canvas object
                a=vcs.init()
                #To Create a new instance of plot:
                # Create a plot object
                p1=a.plot(s)
                #To Modify an existing plot in use:
                p1=a.getplot('dpy_plot_1')

    .. describe:: Display plot object attributes:

        .. code-block:: python

            # Will list all the display plot attributes
            p1.list()
            # "On" or "Off" status, 1=on, 0=off
            p1.off=1
            # Priority to place plot in front of other objects
            p1.priority=1
            # Name of template object
            p1.template='quick'
            # Graphics method type
            p1.g_type='boxfill'
            # Graphics method name
            p1.g_name='quick'
            # List of all the array names
            p1.array=['a1']
    """
    __slots__ = ["name",
                 "_name",
                 "s_name",
                 "off",
                 "priority",
                 "template",
                 "backend",
                 "_template_origin",
                 "g_type",
                 "g_name",
                 "array",
                 "continents",
                 "extradisplays",
                 "parent",
                 "_parent",
                 "_off",
                 "_priority",
                 "_template",
                 "__template_origin",
                 "_g_type",
                 "_g_name",
                 "_array",
                 "_continents",
                 "_continents_line",
                 "continents_line",
                 "_backend",
                 "ratio",
                 "newelements",
                 "_newelements",
                 ]

    def _repr_png(self):
        import tempfile
        tmp = tempfile.mktemp() + ".png"
        self._parent.png(tmp)
        f = open(tmp)
        st = f.read()
        f.close()
        return st
# TODO: html,json,jpeg,png,svg,latex

    def _getname(self):
        return self._name

    def _setname(self, value):
        value = VCS_validation_functions.checkname(self, 'name', value)
        if value is not None:
            self._name = value
    name = property(_getname, _setname)

    def _setnewelements(self, value):
        if not isinstance(value, dict):
            raise ValueError("newelements attribute must be a dictionary")
        self._newelements = value

    def _getnewelements(self):
        return self._newelements
    newelements = property(_getnewelements, _setnewelements)

    def _getcontinents(self):
        return self._continents

    def _setcontinents(self, value):
        VCS_validation_functions.checkContinents(
            self,
            value)
        self._continents = value
    continents = property(_getcontinents, _setcontinents)

    def _getcontinents_line(self):
        return self._continents_line

    def _setcontinents_line(self, value):
        self._continents_line = VCS_validation_functions.checkLine(
            self, "continents_line", value)
    continents_line = property(_getcontinents_line, _setcontinents_line)

    def _getpriority(self):
        return self._priority

    def _setpriority(self, value):
        self._priority = VCS_validation_functions.checkInt(
            self,
            'priority',
            value,
            minvalue=0)
    priority = property(_getpriority, _setpriority)

    def _getoff(self):
        return self._off

    def _setoff(self, value):
        self._off = VCS_validation_functions.checkInt(
            self,
            'off',
            value,
            minvalue=0,
            maxvalue=1)
        for d in self.extradisplays:
            d.off = self._off
    off = property(_getoff, _setoff)

    def _getg_name(self):
        return self._g_name

    def _setg_name(self, value):
        self._g_name = VCS_validation_functions.checkString(
            self,
            'g_name',
            value)
    g_name = property(_getg_name, _setg_name)

    def _getarray(self):
        return self._array

    def _setarray(self, value):
        if not isinstance(value, list):
            raise ValueError('The array must be contained in a list object.')
        self._array = value
    array = property(_getarray, _setarray)

    def _gettemplate(self):
        return self._template

    def _settemplate(self, value):
        self._template = VCS_validation_functions.checkString(
            self,
            'template',
            value)
    template = property(_gettemplate, _settemplate)

    def _gettemplate_origin(self):
        return self.__template_origin

    def _settemplate_origin(self, value):
        self.__template_origin = VCS_validation_functions.checkString(
            self,
            '_template_origin',
            value)
    _template_origin = property(_gettemplate_origin, _settemplate_origin)

    def _getg_type(self):
        return self._g_type

    def _setg_type(self, value):
        import vcsaddons
        value = VCS_validation_functions.checkString(self, 'g_type', value)
        value = value.lower()
        if value not in vcs.elements and value != "text" and value not in vcsaddons.gms:
            raise ValueError(
                "invalid g_type '%s' must be one of: %s " %
                (value, vcs.elements.keys()))
        self._g_type = value
    g_type = property(_getg_type, _setg_type)

    def _get_backend(self):
        return self._backend

    def _set_backend(self, value):
        if not isinstance(value, (dict, None)):
            raise Exception(
                "The dispaly backend attribute must be a dictionary or None")
        self._backend = value
    backend = property(
        _get_backend,
        _set_backend,
        None,
        "dictionary of things the backend wants to be able to reuse")

    ##########################################################################
    #                                                                           #
    # Initialize the display plot attributes.                                   #
    #                                                                           #
    ##########################################################################
    def __init__(self, Dp_name, Dp_name_src='default', parent=None):
            #                                                                           #
            ###################################################################
            # Initialize the display plot's class and its members                       #
            # The getDpmember function retrieves the values of the                      #
            # display plot members in the C structure and passes back the               #
            # appropriate Python Object.                                                #
            ###################################################################
            #                                                                           #
        self.extradisplays = []
        self._name = Dp_name
        self.s_name = 'Dp'
        self._parent = parent
        if self._name == "default":
            self._off = 0
            self._priority = 0
            self._template = "default"
            self.__template_origin = "default"
            self._g_type = "boxfill"
            self._g_name = "default"
            self._array = []
            self._continents = 1
            self._continents_line = "default"
            self.ratio = None
        else:
            src = vcs.elements["display"][Dp_name_src]
            self.off = src.off
            self.array = src.array
            self.template = src.template
            self._template_origin = src._template_origin
            self.g_type = src.g_type
            self.g_name = src.g_name
            self.continents = src.continents
            self.continents_line = src.continents_line
            self.priority = src.priority
            self.ratio = src.ratio

        vcs.elements["display"][self._name] = self
    ##########################################################################
    #                                                                           #
    # List out display plot members (attributes).                               #
    #                                                                           #
    ##########################################################################

    def list(self):
        if (self.name == '__removed_from_VCS__'):
            raise ValueError('This instance has been removed from VCS.')
        print "", "----------Display Plot (Dp) member (attribute) listings ----------"
        print "Display plot method =", self.s_name
        print "name =", self.name
        print "off =", self.off
        print "priority =", self.priority
        print "template =", self.template
        print "template_origin =", self._template_origin
        print "g_type =", self.g_type
        print "g_name =", self.g_name
        print "array =", self.array
        print "continents =", self.continents
        print "extradisplays =", self.extradisplays
        print "ratio =", self.ratio
