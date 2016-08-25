# Adapted for numpy/ma/cdms2 by convertcdms.py
"""
# Template Y - Labels (Pyl) module
"""
##########################################################################
#                                                                               #
# Module:       Template Y - Labels (Pyl) module                                #
#                                                                               #
# Copyright:    2000, Regents of the University of California                   #
#               This software may not be distributed to others without          #
#               permission of the author.                                       #
#                                                                               #
# Author:       PCMDI Software Team                                             #
#               Lawrence Livermore NationalLaboratory:                          #
#               support@pcmdi.llnl.gov                                          #
#                                                                               #
# Description:  Python command wrapper for VCS's template Y - Labels object.    #
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
# Template text (Pyl) Class.                                                #
#                                                                           #
#############################################################################


class Pyl(object):

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
        "priority",
        "x",
        "texttable",
        "textorientation",
        "member",
        "_priority",
        "_x",
        "_texttable",
        "_textorientation"]

    def __init__(self, member):
        #    def __init__(self, template, member=None):
        #                                                         #
        ###########################################################
        # Initialize the line class and its members               #
        # The getPylmember function retrieves the values of the    #
        # line members in the C structure and passes back the     #
        # appropriate Python Object.                              #
        ###########################################################
        #                                                         #
        self.member = member
        self.priority = 1
        self.texttable = "default"
        self.textorientation = "defright"
        if member == "ylabel1":
            self.x = 0.0399999991059
        elif member == "ylabel2":
            self.x = 0.959999978542
            self.priority = 0
            self.textorientation = "default"

    ##########################################################################
    #                                                                           #
    # Set template text  attributes.                                            #
    #                                                                           #
    ##########################################################################
    priority = VCS_validation_functions.priority
    x = VCS_validation_functions.x
    texttable = VCS_validation_functions.texttable
    textorientation = VCS_validation_functions.textorientation

    ##########################################################################
    #                                                                           #
    # List out template text members (attributes).                              #
    #                                                                           #
    ##########################################################################
    def list(self):
        print "member = ", self.member
        print "     priority =", self.priority
        print "     x =", self.x
        print "     texttable =", self.texttable
        print "     textorientation =", self.textorientation


##########################################################################
#        END OF FILE								#
##########################################################################
