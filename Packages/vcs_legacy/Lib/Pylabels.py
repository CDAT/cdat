# Adapted for numpy/ma/cdms2 by convertcdms.py
"""
# Template Y - Labels (Pyl) module
"""
#################################################################################
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
#################################################################################
#
#
#
#################################################################################
#                                                                               #
# Import: VCS C extension module.                                               #
#                                                                               #
#################################################################################
import _vcs_legacy, queries
from types import *
#################################################################################
#                                                                               #
# Function:	setPylmember                                                    #
#                                                                               #
# Description of Function:                                                      #
# 	Private function to update the VCS canvas plot. If the canvas mode is   #
#       set to 0, then this function does nothing.              		#
#                                                                               #
#                                                                               #
# Example of Use:                                                               #
#      setPylmember(self,name,value)						#
#              where: self is the class (e.g., Pyl)                             #
#                     name is the name of the member that is being changed      #
#                     value is the new value of the member (or attribute)       #
#                                                                               #
#################################################################################
def setPylmember(self,member,attribute,value):
     _vcs_legacy.setPylmember(self.parent, member, attribute, value, self.template_parent.mode)
#     _vcs_legacy.setPylmember(self, member, value, self.parent.mode)

#################################################################################
#                                                                               #
# Function:     getPylmember                                                    #
#                                                                               #
# Description of Function:                                                      #
#       Private function that retrieves the line members from the C             #
#       structure and passes it back to Python.                                 #
#                                                                               #
#                                                                               #
# Example of Use:                                                               #
#      return_value =								#
#      getPylmember(self,name)                                                  #
#              where: self is the class (e.g., Pyl)                             #
#                     name is the name of the member that is being found        #
#                                                                               #
#################################################################################
def getPylmember(self,member,attribute):
     return _vcs_legacy.getPylmember(self,member,attribute)

#############################################################################
#                                                                           #
# Template text (Pyl) Class.                                                #
#                                                                           #
#############################################################################
class Pyl:
    """
 Class:	Pyl				# Template text

 Description of Pyl Class:
    The Template text object allows the manipulation of line type, width, and color index. 

    This class is used to define an line table entry used in VCS, or it
    can be used to change some or all of the line attributes in an
    existing line table entry.

 Other Useful Functions:
 	     a=vcs_legacy.init()		# Constructor
	     a.show('line')		# Show predefined line objects
             a.update()               	# Updates the VCS Canvas at user's request
             a.mode=1, or 0           	# If 1, then automatic update, else if
                                          0, then use update function to
                                          update the VCS Canvas.

 Example of Use:
    a=vcs_legacy.init()
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
	# The getPylmember function retrieves the values of the    #
        # line members in the C structure and passes back the     #
	# appropriate Python Object.                              #
        ###########################################################
	#                                                         #
        self.__dict__['member']=member
        self.__dict__['priority']=getPylmember(template,member,'priority')
        self.__dict__['x']=getPylmember(template,member,'x')
        self.__dict__['texttable']=getPylmember(template,member,'texttable')
        self.__dict__['textorientation']=getPylmember(template,member,'textorientation')
        #                                                         #
        ###########################################################
        # Keep track of the parent and grandparent.               #
        ###########################################################
        #                                                         #
        self.__dict__['parent']=template
        self.__dict__['template_parent']=template_parent


    #############################################################################
    #                                                                           #
    # Set template text  attributes.                                            #
    #                                                                           #
    #############################################################################
    def __setattr__(self, name, value):
        if (self.parent.name == '__removed_from_VCS__'):
           raise ValueError, 'This instance has been removed from VCS.'
        if (name == 'priority'):
           if (isinstance(value, IntType)):
              self.__dict__[name]=value
           else:
              raise ValueError, 'The priority value must be an integer.'
        if (name == 'x'):
           if (type(value) in (IntType, FloatType)):
              self.__dict__[name]=value
           else:
              raise ValueError, 'The x value must be an integer or float.'
        elif (name == 'texttable'):
           if (queries.istexttable(value)==1):
              self.__dict__[name]=value.name
              value = value.name
           elif (queries.istextcombined(value)==1):
              self.__dict__[name]=value.Tt_name
              value = value.name
           elif (type(value) == StringType):
              self.__dict__[name]=value
           else:
              raise ValueError, 'The texttable value must be a texttable.'
        elif (name == 'textorientation'):
           if (queries.istextorientation(value)==1):
              self.__dict__[name]=value.name
              value = value.name
           elif (queries.istextcombined(value)==1):
              self.__dict__[name]=value.To_name
              value = value.name
           elif (type(value) == StringType):
              self.__dict__[name]=value
           else:
              raise ValueError, 'The texttable value must be a textorientation.'
        setPylmember(self,self.member,name,value) # update the plot


    #############################################################################
    #                                                                           #
    # List out template text members (attributes).                              #
    #                                                                           #
    #############################################################################
    def list(self):
        if (self.parent.name == '__removed_from_VCS__'):
           raise ValueError, 'This instance has been removed from VCS.'
        print "member = ", self.member
        print "     priority =", self.priority
        print "     x =", self.x
        print "     texttable =", self.texttable
        print "     textorientation =", self.textorientation


#################################################################################
#        END OF FILE								#
#################################################################################
