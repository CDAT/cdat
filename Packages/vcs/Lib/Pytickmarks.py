# Adapted for numpy/ma/cdms2 by convertcdms.py
"""
# Template Y - Tick Marks (Pyt) module
"""
#################################################################################
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
#################################################################################
#
#
#
import queries
import VCS_validation_functions
from types import *
#############################################################################
#                                                                           #
# Template text (Pyt) Class.                                                #
#                                                                           #
#############################################################################
class Pyt(object):
    '''
 Class:	Pyt				# Template text

 Description of Pyt Class:
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
    __slots__ = ["line","priority","x1","x2","member","_line","_priority","_x1","_x2"]
    def __init__(self, member):
#    def __init__(self, template, member=None):
	#                                                         #
        ###########################################################
	# Initialize the line class and its members               #
        #							  #
	# The getPytmember function retrieves the values of the    #
        # line members in the C structure and passes back the     #
	# appropriate Python Object.                              #
        ###########################################################
	#                                                         #
        self.member=member
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

    #############################################################################
    #                                                                           #
    # List out template text members (attributes).                              #
    #                                                                           #
    #############################################################################
    def list(self):
        print "member = ", self.member
        print "     priority =", self.priority
        print "     x1 =", self.x1
        print "     x2 =", self.x2
        print "     line =", self.line


#################################################################################
#        END OF FILE								#
#################################################################################
