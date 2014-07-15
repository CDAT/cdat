# Adapted for numpy/ma/cdms2 by convertcdms.py
"""
# Template Format (Pf) module
"""
#################################################################################
#                                                                               #
# Module:       Template Format (Pf) module                                     #
#                                                                               #
# Copyright:    2000, Regents of the University of California                   #
#               This software may not be distributed to others without          #
#               permission of the author.                                       #
#                                                                               #
# Author:       PCMDI Software Team                                             #
#               Lawrence Livermore NationalLaboratory:                          #
#               support@pcmdi.llnl.gov                                          #
#                                                                               #
# Description:  Python command wrapper for VCS's template format object.        #
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
# Template text (Pf) Class.                                                 #
#                                                                           #
#############################################################################
class Pf(object):
    """
 Class:	Pf				# Template text

 Description of Pf Class:
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
"""
    __slots__ = ["format","priority","x","y","texttable","textorientation","member","_format","_priority","_x","_y","_texttable","_textorientation"]
    #############################################################################
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
	# The getPfmember function retrieves the values of the    #
        # line members in the C structure and passes back the     #
	# appropriate Python Object.                              #
        ###########################################################
	#                                                         #

        self.priority=1
        self.format = "default"
        self.texttable="default"
        self.textorientation="default"
        if member=="xvalue":
          self.x = 0.800000011921
          self.y = 0.941999971867
        elif member=="yvalue":
          self.x = 0.800000011921
          self.y = 0.922999978065
        elif member=="zvalue":
          self.x = 0.800000011921
          self.y = 0.902999997139
        elif member=="tvalue":
          self.x = 0.800000011921
          self.y = 0.883000016212
        elif member=="mean":
          self.x = 0.0500000007451
          self.y = 0.899999976158
        elif member=="min":
          self.x = 0.449999988079
          self.y = 0.899999976158
        elif member=="max":
          self.x = 0.25
          self.y = 0.899999976158
        self.member = member

    #############################################################################
    #                                                                           #
    # Set template text  attributes.                                            #
    #                                                                           #
    #############################################################################
    priority = VCS_validation_functions.priority
    x = VCS_validation_functions.x
    y = VCS_validation_functions.y
    texttable = VCS_validation_functions.texttable
    textorientation = VCS_validation_functions.textorientation
    def _getformat(self):
      return self._format
    def _setformat(self,value):
      self._format = VCS_validation_functions.checkString(self,"format",value)
    format = property(_getformat,_setformat)

    #############################################################################
    #                                                                           #
    # List out template text members (attributes).                              #
    #                                                                           #
    #############################################################################
    def list(self):
        print "member = ", self.member
        print "     priority =", self.priority
        print "     x =", self.x
        print "     y =", self.y
        print "     format =", self.format
        print "     texttable =", self.texttable
        print "     textorientation =", self.textorientation


#################################################################################
#        END OF FILE        							#
#################################################################################
