# Adapted for numpy/ma/cdms2 by convertcdms.py
"""
# Template X - Labels (Pxl) module
"""
#
#
# Module:       Template X - Labels (Pxl) module                                #
#
# Copyright:    2000, Regents of the University of California                   #
# This software may not be distributed to others without          #
# permission of the author.                                       #
#
# Author:       PCMDI Software Team                                             #
# Lawrence Livermore NationalLaboratory:                          #
# support@pcmdi.llnl.gov                                          #
#
# Description:  Python command wrapper for VCS's template X - Labels object.    #
#
# Version:      4.0                                                             #
#
#
#
#
#
import VCS_validation_functions
#
#
# Template text (Pxl) Class.                                                #
#
#


class Pxl(object):

    '''
 Class:	Pxl				# Template text

 Description of Pxl Class:
    The Template text object allows the manipulation of line type, width, and color index.

    This class is used to define an line table entry used in VCS, or it
    can be used to change some or all of the line attributes in an
    existing line table entry.

 Other Useful Functions:
             a=vcs.init()		# Constructor
             a.show('line')		# Show predefined line objects
             a.update()               	# Updates the VCS Canvas at user's request
             a.mode=1, or 0           	# If 1, then automatic update, else if
                                          0, then use update function to
                                          update the VCS Canvas.

 Example of Use:
    a=vcs.init()
    To Create a new instance of line use:
     ln=a.createline('new','red') 	# Copies content of 'red' to 'new'
     ln=a.createline('new') 		# Copies content of 'default' to 'new'

    To Modify an existing line use:
     ln=a.getline('red')

    ln.list()  				# Will list all the line attribute values
    ln.color=100			# Range from 1 to 256
    ln.width=100			# Range from 1 to 300

    Specify the line type:
     ln.type='solid'          		# Same as ln.type=0
     ln.type='dash'          		# Same as ln.type=1
     ln.type='dot'          		# Same as ln.type=2
     ln.type='dash-dot'          	# Same as ln.type=3
     ln.type='long-dash'          	# Same as ln.type=4
'''
    #
    #
    # Initialize the line attributes.                                           #
    #
    #
    __slots__ = [
        "priority",
        "y",
        "texttable",
        "textorientation",
        "member",
        "_priority",
        "_y",
        "_texttable",
        "_textorientation"]

    def __init__(self, member):
        #    def __init__(self, template, member=None):
        #
        #
        # Initialize the line class and its members               #
        # The getPxlmember function retrieves the values of the    #
        # line members in the C structure and passes back the     #
        # appropriate Python Object.                              #
        #
        #
        self.member = member
        self.priority = 1
        self.texttable = "default"
        self.textorientation = "defcenter"
        if member == "xlabel1":
            self.y = 0.234999999404
        elif member == "xlabel2":
            self.y = 0.870000004768
            self.priority = 0

    #
    #
    # Set template text  attributes.                                            #
    #
    #
    priority = VCS_validation_functions.priority
    y = VCS_validation_functions.y
    texttable = VCS_validation_functions.texttable
    textorientation = VCS_validation_functions.textorientation

    #
    #
    # List out template text members (attributes).                              #
    #
    #
    def list(self):
        print "member = ", self.member
        print "     priority =", self.priority
        print "     y =", self.y
        print "     texttable =", self.texttable
        print "     textorientation =", self.textorientation


#
# END OF FILE								#
#
