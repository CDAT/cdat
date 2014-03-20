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
#################################################################################
#                                                                               #
# Import: VCS C extension module.                                               #
#                                                                               #
#################################################################################
import _vcs_legacy
from types import *
#################################################################################
#                                                                               #
# Function:	setPdsmember                                                    #
#                                                                               #
# Description of Function:                                                      #
# 	Private function to update the VCS canvas plot. If the canvas mode is   #
#       set to 0, then this function does nothing.              		#
#                                                                               #
#                                                                               #
# Example of Use:                                                               #
#      setPdsmember(self,name,value)						#
#              where: self is the class (e.g., Pds)                             #
#                     name is the name of the member that is being changed      #
#                     value is the new value of the member (or attribute)       #
#                                                                               #
#################################################################################
def setPdsmember(self,member,attribute,value):
     _vcs_legacy.setPdsmember(self.parent, member, attribute, value, self.template_parent.mode)
#     _vcs_legacy.setPdsmember(self, member, value, self.parent.mode)

#################################################################################
#                                                                               #
# Function:     getPdsmember                                                    #
#                                                                               #
# Description of Function:                                                      #
#       Private function that retrieves the line members from the C             #
#       structure and passes it back to Python.                                 #
#                                                                               #
#                                                                               #
# Example of Use:                                                               #
#      return_value =								#
#      getPdsmember(self,name)                                                  #
#              where: self is the class (e.g., Pds)                             #
#                     name is the name of the member that is being found        #
#                                                                               #
#################################################################################
def getPdsmember(self,member,attribute):
     return _vcs_legacy.getPdsmember(self,member,attribute)

#############################################################################
#                                                                           #
# Template text (Pds) Class.                                                 #
#                                                                           #
#############################################################################
class Pds:
    """
 Class:	Pds				# Template text

 Description of Pds Class:
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
	# The getPdsmember function retrieves the values of the    #
        # line members in the C structure and passes back the     #
	# appropriate Python Object.                              #
        ###########################################################
	#                                                         #
        self.__dict__['member']=member
        self.__dict__['priority']=getPdsmember(template,member,'priority')
        self.__dict__['x1']=getPdsmember(template,member,'x1')
        self.__dict__['y1']=getPdsmember(template,member,'y1')
        self.__dict__['x2']=getPdsmember(template,member,'x2')
        self.__dict__['y2']=getPdsmember(template,member,'y2')
        self.__dict__['_ratio']=getPdsmember(template,member,'_ratio')
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
        setPdsmember(self,self.member,name,value) # update the plot


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
        print "     x1 =", self.x1
        print "     y1 =", self.y1
        print "     x2 =", self.x2
        print "     y2 =", self.y2


#################################################################################
#        END OF FILE        							#
#################################################################################
