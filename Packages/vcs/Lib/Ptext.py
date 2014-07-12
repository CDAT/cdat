# Adapted for numpy/ma/cdms2 by convertcdms.py
"""
# Template Text (Pt) module
"""
#################################################################################
#                                                                               #
# Module:       Template Text (Pt) module                                       #
#                                                                               #
# Copyright:    2000, Regents of the University of California                   #
#               This software may not be distributed to others without          #
#               permission of the author.                                       #
#                                                                               #
# Author:       PCMDI Software Team                                             #
#               Lawrence Livermore NationalLaboratory:                          #
#               support@pcmdi.llnl.gov                                          #
#                                                                               #
# Description:  Python command wrapper for VCS's template text object.          #
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
# Template text (Pt) Class.                                                 #
#                                                                           #
#############################################################################
class Pt(object):
    '''
 Class:	Pt				# Template text

 Description of Pt Class:
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
    __slots__ = ["_priority","priority","_x","x","_y","y","_texttable","texttable","_textorientation","textorientation","member"]
    def __init__(self,member):
	#                                                         #
        ###########################################################
	# Initialize the line class and its members               #
        #							  #
	# The getPtmember function retrieves the values of the    #
        # line members in the C structure and passes back the     #
	# appropriate Python Object.                              #
        ###########################################################
	#                                                         #
        self.priority=1
        self.texttable="default"
        self.textorientation="default"
        if member=="file":
          self.x = 0.0500000007451
          self.y = 0.0130000002682
        elif member == "function":
          self.x = 0.0500000007451
          self.y = 0.0130000002682
        elif member == "logicalmask":
          self.x = 0.0500000007451
          self.y = 0.0329999998212
        elif member == "transformation":
          self.x = 0.0500000007451
          self.y = 0.0529999993742
        elif member == "source":
          self.x = 0.0500000007451
          self.y = 0.941999971867
        elif member == "dataname":
          self.x = 0.0500000007451
          self.y = 0.922999978065
        elif member == "title":
          self.x = 0.15000000596
          self.y = 0.922999978065
        elif member == "units":
          self.x = 0.670000016689
          self.y = 0.922999978065
        elif member == "crdate":
          self.x = 0.75
          self.y = 0.922999978065
        elif member == "crtime":
          self.x = 0.850000023842
          self.y = 0.922999978065
        elif member == "comment1":
          self.x = 0.10000000149
          self.y = 0.954999983311
        elif member == "comment2":
          self.x = 0.10000000149
          self.y = 0.975000023842
        elif member == "comment3":
          self.x = 0.10000000149
          self.y = 0.995000004768
        elif member == "comment4":
          self.x = 0.10000000149
          self.y = 0.999000012875
        elif member == "xname":
          self.x = .5
          self.y = 0.277000010014
        elif member == "yname":
          self.x = 0.0168999992311
          self.y = 0.420033991337
        elif member == "zname":
          self.x = 0.
          self.y = 0.995000004768
        elif member == "tname":
          self.x = 0.
          self.y = 0.995000004768
        elif member == "xunits":
          self.x = 0.600000023842
          self.y = 0.277000010014
        elif member == "yunits":
          self.x = 0.019999999553
          self.y = 0.658999979496
        elif member == "zunits":
          self.x = 0.
          self.y = 0.995000004768
        elif member == "tunits":
          self.x = 0.
          self.y = 0.995000004768
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
        print "     texttable =", self.texttable
        print "     textorientation =", self.textorientation


#################################################################################
#        END OF FILE								#
#################################################################################
