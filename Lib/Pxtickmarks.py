# Adapted for numpy/ma/cdms2 by convertcdms.py
"""
# Template X - Tick Marks (Pxt) module
"""
#################################################################################
#                                                                               #
# Module:       Template X - Tick Marks (Pxt) module                            #
#                                                                               #
# Copyright:    2000, Regents of the University of California                   #
#               This software may not be distributed to others without          #
#               permission of the author.                                       #
#                                                                               #
# Author:       PCMDI Software Team                                             #
#               Lawrence Livermore NationalLaboratory:                          #
#               support@pcmdi.llnl.gov                                          #
#                                                                               #
# Description:  Python command wrapper for VCS's template X - Tick Marks object.#
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
# Template text (Pxt) Class.                                                 #
#                                                                           #
#############################################################################
class Pxt(object):
    '''
 Class:	Pxt				# Template text

 Description of Pxt Class:
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
    __slots__ = ["line","priority","y1","y2","member","_line","_priority","_y1","_y2"]
######################################
    #                                                                           #
    # Initialize the line attributes.                                           #
    #                                                                           #
    #############################################################################
    def __init__(self, member):
#    def __init__(self, template, member=None):
	#                                                         #
        ###########################################################
	# Initialize the line class and its members               #
        #							  #
	# The getPxtmember function retrieves the values of the    #
        # line members in the C structure and passes back the     #
	# appropriate Python Object.                              #
        ###########################################################
	#                                                         #
        self.member=member
        self.priority=1
        if member == "xtic1":
          self.y1 = 0.259999990463
          self.y2 = 0.24699999392
        elif member == "xtic2":
          self.y1 = 0.860000014305
          self.y2 = 0.871999979019
        elif member == "xmintic1":
          self.priority=0
          self.y1 = 0.259999990463
          self.y2 = 0.256999999285
        elif member == "xmintic2":
          self.priority=0
          self.y1 = 0.860000014305
          self.y2 = 0.860000014305
        self.line= "default"

    #############################################################################
    #                                                                           #
    # Set template text  attributes.                                            #
    #                                                                           #
    #############################################################################
    priority = VCS_validation_functions.priority
    y1 = VCS_validation_functions.y1
    y2 = VCS_validation_functions.y2
    line = VCS_validation_functions.line

    #############################################################################
    #                                                                           #
    # List out template text members (attributes).                              #
    #                                                                           #
    #############################################################################
    def list(self):
        print "member = ", self.member
        print "     priority =", self.priority
        print "     y1 =", self.y1
        print "     y2 =", self.y2
        print "     line =", self.line


#################################################################################
#        END OF FILE								#
#################################################################################
