"""
# Colormap (Cp) module
"""
#################################################################################
#                                                                               #
# Module:       colormap (Cp) module                                            #
#                                                                               #
# Copyright:    2000, Regents of the University of California                   #
#               This software may not be distributed to others without          #
#               permission of the author.                                       #
#                                                                               #
# Author:       PCMDI Software Team                                             #
#               Lawrence Livermore NationalLaboratory:                          #
#               support@pcmdi.llnl.gov                                          #
#                                                                               #
# Description:  Python command wrapper for VCS's colormap secondary object.     #
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
# Function:     setCpmember                                                     #
#                                                                               #
# Description of Function:                                                      #
#       Private function to update the VCS canvas plot. If the canvas mode is   #
#       set to 0, then this function does nothing.                              #
#                                                                               #
#                                                                               #
# Example of Use:                                                               #
#      setCpmember(self,name,key,value)                                         #
#              where: self is the class (e.g., Cp)                              #
#                     name is the name of the member that is being changed      #
#                     key is the index value into the colormap table            #
#                     value is the new value of the member (or attribute)       #
#                                                                               #
#################################################################################
def setCpmember(self,member,key,value):
     _vcs_legacy.setCpmember(self.parent.canvas, self, member, key, value, self.parent.mode)

#################################################################################
#                                                                               #
# Function:     getCpmember                                                     #
#                                                                               #
# Description of Function:                                                      #
#       Private function that retrieves the colormap members from the C         #
#       structure and passes it back to Python.                                 #
#                                                                               #
#                                                                               #
# Example of Use:                                                               #
#      return_value =                                                           #
#      getCpmember(self,name)                                                   #
#              where: self is the class (e.g., Cp)                              #
#                     name is the name of the member that is being found        #
#                                                                               #
#################################################################################
def getCpmember(self,member):
     return _vcs_legacy.getCpmember(self,member)

#################################################################################
#                                                                               #
# Function:     renameCp                                                        #
#                                                                               #
# Description of Function:                                                      #
#       Private function that renames the name of an existing colormap          #
#       graphics method.                                                        #
#                                                                               #
#                                                                               #
# Example of Use:                                                               #
#      renameCp(old_name, new_name)                                             #
#              where: old_name is the current name of colormap graphics method  #
#                     new_name is the new name for the colormap graphics method #
#                                                                               #
#################################################################################
def renameCp(self, old_name, new_name):
     return _vcs_legacy.renameCp(old_name, new_name)

#################################################################################
#                                                                               #
# Function:     copyCp                                                          #
#                                                                               #
# Description of Function:                                                      #
#       Function that copies an existing colormap to an new colormap.           #
#                                                                               #
#                                                                               #
# Example of Use:                                                               #
#      copyCp(old_name, new_name)                                               #
#              where: old_name is the current name of colormap graphics method  #
#                     new_name is the new name for the colormap graphics method #
#                                                                               #
#################################################################################
def copyCp(old_name, new_name):
     return _vcs_legacy.copyCp(old_name, new_name)

#################################################################################
#                                                                               #
# Function:     removeCp                                                        #
#                                                                               #
# Description of Function:                                                      #
#       Function that removes an existing colormap.                             #
#                                                                               #
#                                                                               #
# Example of Use:                                                               #
#      removeCp(name)                                                           #
#              where: name is the current name of colormap graphics method      #
#                                                                               #
#################################################################################
def removeCp(name):
     return _vcs_legacy.removeCp(name)

#
#############################################################################
#                                                                           #
# RGB_Table Class is a class wrapper around the built-in dictionay type.    #
# 	It uses the UserDict base class which overrides the methods: init,  #
# 	setitem, and getitem.						    #
#                                                                           #
# The colormap table is structured as follows: {0:[R,G,B],1:[R,G,B],...,    #
#	255:[R,B,G]}.							    #
#                                                                           #
#############################################################################
from UserDict import UserDict
class RGB_Table(UserDict):
    def __init__(self, parent, name, dict=None):
       self.data = {}
       self.parent = parent
       self.name = name
       for i in range(0,256):
          self.data[i]=getCpmember(parent, i)  # Load colormap from VCS colormap table
       if dict is not None: self.update(dict)
    def __setitem__(self,key,value):
       if (self.parent.name == '__removed_from_VCS__'):
           raise ValueError, 'This instance has been removed from VCS.'
       if (self.name == 'default'):
           raise ValueError, 'You cannot modify the default colormap.'
       if (key not in range(0,240)):
           raise ValueError, 'Cell index must be in the range 0 to 239.'
       if (type(value) in (ListType, TupleType)):
           value = list(value)
           if len(value) != 3:
              raise ValueError, 'Must be a tuple or list of size 3.'
           for i in range(len(value)):
              if value[i] not in range(0,101):
                 raise ValueError, 'The R,G,B values must be in the range 0 to 100.'
           setCpmember(self.parent,'index',key, value) # update the plot
       else:
           raise ValueError, 'Must be either a list object, tuple object, or integer value.'
       self.data[key] = value
    def __getitem__(self,key):
       if (self.parent.name == '__removed_from_VCS__'):
           raise ValueError, 'This instance has been removed from VCS.'
       if (key not in range(0,256)):
           raise ValueError, 'Cell index must be in the range 0 to 255.'
       return self.data[key]
#
#
#############################################################################
#                                                                           #
# Colormap (Cp) Class.                                                      #
#                                                                           #
#############################################################################
class Cp:
    """
 Class: Cp                              # Colormap

 Description of Cp Class:
    The Colormap object allows the manipulation of the colormap index R,G,B values.

    This class is used to define a colormap table entry used in VCS, or it
    can be used to change some or all of the colormap R,G,B attributes in an
    existing colormap table entry.

 Other Useful Functions:
             a=vcs_legacy.init()               # Constructor
             a.show('colormap')         # Show predefined colormap objects
             a.update()                 # Updates the VCS Canvas at user's request
             a.mode=1, or 0             # If 1, then automatic update, else if
                                          0, then use update function to
                                          update the VCS Canvas.

 Example of Use:
    a=vcs_legacy.init()
    To Create a new instance of colormap use:
     cp=a.createcolormap('new','quick') # Copies content of 'red' to 'new'
     cp=a.createcolormap('new')         # Copies content of 'default' to 'new'

    To Modify an existing colormap use:
     cp=a.getcolormap('quick')

    cp.list()                           # Will list all the colormap indices and 
					#      R,G,B attribute values
    cp.color=16,100,0,0                 # Index, R, G, B      
    cp.color=16,0,100,0                 # Index range from 0 to 255, but can only
					#      modify from 0 to 239
    cp.color=17,0,0,100                 # R, G, B values range from 0 to 100, where
 					#      0 is low intensity and 100 is highest intensity
"""
    #############################################################################
    #                                                                           #
    # Initialize the colormap attributes.                                       #
    #                                                                           #
    #############################################################################
    def __init__(self, parent, Cp_name=None, Cp_name_src='default', createCp=0):
        #                                                             #
        ###############################################################
        # Initialize the colormap class and its members               #
        #                                                             #
        # The getCpmember function retrieves the values of the        #
        # colormap members in the C structure and passes back the     #
        # appropriate Python Object.                                  #
        ###############################################################
        #                                                             #
        if (createCp == 0):
           if (Cp_name == None):
              raise ValueError, 'Must provide a colormap name.'
           else:
              _vcs_legacy.copyCp(Cp_name_src, Cp_name)
              self.__dict__['name'] = Cp_name
        else:
              self.__dict__['name']=Cp_name_src
        self.__dict__['s_name']='Cp'
        self.__dict__['parent']=parent
        self.__dict__['index']=RGB_Table(self,self.__dict__['name']) # Create colormap dictionary

    #############################################################################
    #                                                                           #
    # Set colormap attributes.                                                  #
    #                                                                           #
    #############################################################################
    def __setattr__(self, name, value):
        if (self.name == '__removed_from_VCS__'):
           raise ValueError, 'This instance has been removed from VCS.%s' % name
        if (self.name == 'default'):
           raise ValueError, 'You cannot modify the default colormap.'
        if (name == 'name'):
           if (type(value) == StringType):
              renameCp(self,self.name, value)
              self.__dict__['name']=value
           else:
              raise ValueError, 'The name attribute must be a string.'
        elif (name == 'index'):
           raise ValueError, 'Invalid statement.'

	# Note: See RGB_Table Class for "index" setting of the colormap entries

    #############################################################################
    #                                                                           #
    # List out colormap members (attributes).                                   #
    #                                                                           #
    #############################################################################
    def list(self):
        if (self.name == '__removed_from_VCS__'):
           raise ValueError, 'This instance has been removed from VCS.'
        print "","----------Colormap (Cp) member (attribute) listings ----------"
        print 'Canvas Mode =',self.parent.mode
        print "secondary method =", self.s_name
        print "name =", self.name
        print "index =", self.index

    #############################################################################
    #                                                                           #
    # Script out secondary colormap method in VCS to a file.                    #
    #                                                                           #
    #############################################################################
    def script(self, script_filename=None, mode=None):
        '''
 Function:     script                           # Calls _vcs_legacy.scriptCp

 Description of Function:
       Saves out a colormap graphics method in VCS or Python script form to a
       designated file.

 Example of Use:
    script(scriptfile_name, mode)
              where: scriptfile_name is the output name of the script file.
                     mode is either "w" for replace or "a" for append.

              Note: If the the filename has a ".py" at the end, it will produce a
                    Python script. If the filename has a ".scr" at the end, it will
                    produce a VCS script. If neither extensions are give, then by
                    default a Python script will be produced.

    a=vcs_legacy.init()
    cp=a.createcolormap('temp')
    cp.script('filename.py')         # Append to a Python file "filename.py"
    cp.script('filename.scr')        # Append to a VCS file "filename.scr"
    cp.script('filename','w')        # Create or overwrite to a Python file "filename.py"
'''
        if (script_filename == None):
          raise ValueError, 'Error - Must provide an output script file name.'

        if (mode == None):
           mode = 'a'
        elif (mode not in ('w', 'a')):
          raise ValueError, 'Error - Mode can only be "w" for replace or "a" for append.'

        # By default, save file in python script mode
        scr_type = script_filename[len(script_filename)-4:len(script_filename)]
        if (scr_type == '.scr'):
           print _vcs_legacy.scriptCp(self.name,script_filename,mode)
        else:
           mode = mode + '+'
           py_type = script_filename[len(script_filename)-3:len(script_filename)]
           if (py_type != '.py'):
              script_filename = script_filename + '.py'

           # Write to file
           fp = open(script_filename,mode)
           if (fp.tell() == 0): # Must be a new file, so include below
              fp.write("#####################################\n")
              fp.write("#                                 #\n")
              fp.write("# Import and Initialize VCS     #\n")
              fp.write("#                             #\n")
              fp.write("#############################\n")
              fp.write("import vcs_legacy\n")
              fp.write("v=vcs_legacy.init()\n\n")

           unique_name = '__Cp__' + self.name
           fp.write("#----------Colormap (Cp) member (attribute) listings ----------\n")
           fp.write("tl_list=v.listelements('colormap')\n")
           fp.write("if ('%s' in tl_list):\n" % self.name)
           fp.write("   %s = v.getcolormap('%s')\n" % (unique_name, self.name))
           fp.write("else:\n")
           fp.write("   %s = v.createcolormap('%s')\n" % (unique_name, self.name))
           fp.write("%s.index = '%s'\n" % (unique_name, self.index))


#################################################################################
#        END OF FILE                                                            #
#################################################################################
