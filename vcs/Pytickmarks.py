# Adapted for numpy/ma/cdms2 by convertcdms.py
"""
# Template Y - Tick Marks (Pyt) module
"""
##########################################################################
#                                                                               #
# Module:       Template Y - Tick Marks (Pyt) module                            #
#                                                                               #
# Copyright:    2000, Regents of the University of California                   #
#               This software may not be distributed to others without          #
#               permission of the author.                                       #
#                                                                               #
# Author:       PCMDI Software Team                                             #
#               Lawrence Livermore NationalLaboratory:                          #
#               support@pcmdi.llnl.gov                                          #
#                                                                               #
# Description:  Python command wrapper for VCS's template Y - Tick Marks object.#
#                                                                               #
# Version:      4.0                                                             #
#                                                                               #
##########################################################################
#
#
#
import VCS_validation_functions
#############################################################################
#                                                                           #
# Template text (Pyt) Class.                                                #
#                                                                           #
#############################################################################


class Pyt(object):

    """
    The Template text object allows the manipulation of line type, width, and color index.

    This class is used to define an line table entry used in VCS, or it
    can be used to change some or all of the line attributes in an
    existing line table entry.

    .. describe:: Useful Functions:

        .. code-block:: python

            # VCS Canvas Constructor
            a=vcs.init()
            # Show predefined line objects
            a.show('line')
            # Updates the VCS Canvas at user's request
            a.update()

    .. describe:: Make a Canvas object to work with:

        .. code-block:: python

            a=vcs.init()

    .. describe:: Create a new instance of line:

        .. code-block:: python

            # Copies content of 'red' to 'new'
            ln=a.createline('new','red')
            # Copies content of 'default' to 'new'
            ln=a.createline('new')

    .. describe:: Modify an existing line:

        .. code-block:: python
            # Get a copy of 'red' line
            ln=a.getline('red')


    .. describe:: Overview of line attributes:

        * Listing line attributes:

            .. code-block:: python

                # Will list all the line attribute values
                ln.list()
                # Range from 1 to 256
                ln.color=100
                # Range from 1 to 300
                ln.width=100

        * Specifying the line type:

            .. code-block:: python

                # Same as ln.type=0
                ln.type='solid'
                # Same as ln.type=1
                ln.type='dash'
                # Same as ln.type=2
                ln.type='dot'
                # Same as ln.type=3
                ln.type='dash-dot'
                # Same as ln.type=4
                ln.type='long-dash'
"""
    ##########################################################################
    #                                                                           #
    # Initialize the line attributes.                                           #
    #                                                                           #
    ##########################################################################
    __slots__ = [
        "line",
        "priority",
        "x1",
        "x2",
        "member",
        "_line",
        "_priority",
        "_x1",
        "_x2"]

    def __init__(self, member):
        #    def __init__(self, template, member=None):
        #                                                         #
        ###########################################################
        # Initialize the line class and its members               #
        # The getPytmember function retrieves the values of the    #
        # line members in the C structure and passes back the     #
        # appropriate Python Object.                              #
        ###########################################################
        #                                                         #
        self.member = member
        self.line = "default"
        self.priority = 1
        if member == "ytic1":
            self.x1 = 0.0500000007451
            self.x2 = 0.0399999991059
        elif member == "ytic2":
            self.x1 = 0.949999988079
            self.x2 = 0.959999978542
        elif member == "ymintic1":
            self.priority = 0
            self.x1 = 0.0500000007451
            self.x2 = 0.0450000017881
        elif member == "ymintic2":
            self.priority = 0
            self.x1 = 0.949999988079
            self.x2 = 0.954999983311
        self.line = "default"

    priority = VCS_validation_functions.priority
    x1 = VCS_validation_functions.x1
    x2 = VCS_validation_functions.x2
    line = VCS_validation_functions.line

    ##########################################################################
    #                                                                           #
    # List out template text members (attributes).                              #
    #                                                                           #
    ##########################################################################
    def list(self):
        print "member = ", self.member
        print "     priority =", self.priority
        print "     x1 =", self.x1
        print "     x2 =", self.x2
        print "     line =", self.line


##########################################################################
#        END OF FILE								#
##########################################################################
