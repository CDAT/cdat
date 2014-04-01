# Adapted for numpy/ma/cdms2 by convertcdms.py
"""
# Template Data Space (Pds) module
"""
#################################################################################
#                                                                               #
# Module:       Template Data Space (Pds) module                                #
#                                                                               #
# Copyright:    2000, Regents of the University of California                   #
#               This software may not be distributed to others without          #
#               permission of the author.                                       #
#                                                                               #
# Author:       PCMDI Software Team                                             #
#               Lawrence Livermore NationalLaboratory:                          #
#               support@pcmdi.llnl.gov                                          #
#                                                                               #
# Description:  Python command wrapper for VCS's template data space object.    #
#                                                                               #
# Version:      4.0                                                             #
#                                                                               #
#################################################################################
#
#
#
from types import *
class Pds(object):
    """
 Class:	Pds				# Template text

 Description of Pds Class:
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
    #############################################################################
    #                                                                           #
    # Initialize the line attributes.                                           #
    #                                                                           #
    #############################################################################
    def __init__(self, template, template_parent, member=None):
#    def __init__(self, template, member=None):
	#                                                         #
        ###########################################################
	# Initialize the line class and its members               #
        #							  #
	# The getPdsmember function retrieves the values of the    #
        # line members in the C structure and passes back the     #
	# appropriate Python Object.                              #
        ###########################################################
	#                                                         #
        self.priority= 1 
        self.x1=0.0500000007451
        self.y1=0.259999990463
        self.x2=0.949999988079
        self.y2=0.860000014305
        self.ratio = -999
        self.member = member


    #############################################################################
    #                                                                           #
    # Set template text  attributes.                                            #
    #                                                                           #
    #############################################################################
    def __setattr__(self, name, value):
        if (name == 'priority'):
           if (isinstance(value, IntType)):
              self.__dict__[name]=value
           else:
              raise ValueError, 'The priority value must be an integer.'
        elif (name == 'x1'):
           if (type(value) in (IntType, FloatType)):
              self.__dict__[name]=value
           else:
              raise ValueError, 'The x1 value must be an integer or float.'
        elif (name == 'y1'):
           if (type(value) in (IntType, FloatType)):
              self.__dict__[name]=value
           else:
              raise ValueError, 'The y1 value must be an integer or float.'
        elif (name == 'x2'):
           if (type(value) in (IntType, FloatType)):
              self.__dict__[name]=value
           else:
              raise ValueError, 'The x2 value must be an integer or float.'
        elif (name == 'y2'):
           if (type(value) in (IntType, FloatType)):
              self.__dict__[name]=value
           else:
              raise ValueError, 'The y2 value must be an integer or float.'
        elif (name == '_ratio'):
           if (type(value) in (IntType, FloatType)):
              self.__dict__[name]=value
           else:
              raise ValueError, 'The ratio value must be an integer or float.'
        else:
             raise Exception,"Error invalid attribute %s for data member of template" % name
        elif name == "member":
          if isinstance(value,str):
            self.__dict__["member"]=value
          else:
            raise ValueError,"'member' must be a string"
        else:
          raise ValueError,"BAD ATTRIBUTE: %s" % name


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


#################################################################################
#        END OF FILE        							#
#################################################################################
