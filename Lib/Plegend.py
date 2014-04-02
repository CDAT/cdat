# Adapted for numpy/ma/cdms2 by convertcdms.py
"""
# Template Legend Space (Pls) module
"""
#################################################################################
#                                                                               #
# Module:       Template Legend Space (Pls) module                              #
#                                                                               #
# Copyright:    2000, Regents of the University of California                   #
#               This software may not be distributed to others without          #
#               permission of the author.                                       #
#                                                                               #
# Author:       PCMDI Software Team                                             #
#               Lawrence Livermore NationalLaboratory:                          #
#               support@pcmdi.llnl.gov                                          #
#                                                                               #
# Description:  Python command wrapper for VCS's template legend space object.  #
#                                                                               #
# Version:      4.0                                                             #
#                                                                               #
#################################################################################
#
#
#
import queries
import VCS_validation_functions

#############################################################################
#                                                                           #
# Template text (Pls) Class.                                                #
#                                                                           #
#############################################################################
class Pls(object):
    '''
 Class:	Pls				# Template text

 Description of Pls Class:
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
    #############################################################################
    #                                                                           #
    # Initialize the line attributes.                                           #
    #                                                                           #
    #############################################################################
    __slots__ = ["priority","x1","x2","y1","y2","line","member","_priority","_x1","_x2","_y1","_y2","_line","_texttable","_textorientation","texttable","textorientation"]
    def __init__(self, member):
#    def __init__(self, template, member=None):
	#                                                         #
        ###########################################################
	# Initialize the line class and its members               #
        #							  #
	# The getPlsmember function retrieves the values of th    #
        # line members in the C structure and passes back the     #
	# appropriate Python Object.                              #
        ###########################################################
	#                                                         #

        self.member = member
        self.priority= 1 
        self.line="default"
        self.texttable="default"
        self.textorientation="defcenter"
        self.x1=0.0500000007451
        self.y1=0.129999995232
        self.x2=0.949999988079
        self.y2=0.159999996424

    #############################################################################
    #                                                                           #
    # Set template text  attributes.                                            #
    #                                                                           #
    #############################################################################
    priority = VCS_validation_functions.priority
    x1 = VCS_validation_functions.x1
    x2 = VCS_validation_functions.x2
    y1 = VCS_validation_functions.y1
    y2 = VCS_validation_functions.y2
    line = VCS_validation_functions.line
    texttable = VCS_validation_functions.texttable
    textorientation = VCS_validation_functions.textorientation

    #############################################################################
    #                                                                           #
    # List out template text members (attributes).                              #
    #                                                                           #
    #############################################################################
    def list(self):
        print "member = ", self.member
        print "     priority =", self.priority
        print "     x1 =", self.x1
        print "     y1 =", self.y1
        print "     x2 =", self.x2
        print "     y2 =", self.y2
        print "     line =", self.line
        print "     texttable =", self.texttable
        print "     textorientation =", self.textorientation


#################################################################################
#        END OF FILE								#
#################################################################################
