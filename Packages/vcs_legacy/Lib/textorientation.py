"""
# Text Orientation (To) module
"""
#################################################################################
#                                                                               #
# Module:       textorientation (To) module                                     #
#                                                                               #
# Copyright:    2000, Regents of the University of California                   #
#               This software may not be distributed to others without          #
#               permission of the author.                                       #
#                                                                               #
# Author:       PCMDI Software Team                                             #
#               Lawrence Livermore NationalLaboratory:                          #
#               support@pcmdi.llnl.gov                                          #
#                                                                               #
# Description:  Python command wrapper for VCS's text orientation secondary 	#
#		object.    							#
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
import Canvas
from types import *
import VCS_validation_functions

#################################################################################
#                                                                               #
# Function:	setTomember                                                     #
#                                                                               #
# Description of Function:                                                      #
# 	Private function to update the VCS canvas plot. If the canvas mode is   #
#       set to 0, then this function does nothing.              		#
#                                                                               #
#                                                                               #
# Example of Use:                                                               #
#      setTomember(self,name,value)						#
#              where: self is the class (e.g., To)                              #
#                     name is the name of the member that is being changed      #
#                     value is the new value of the member (or attribute)       #
#                                                                               #
#################################################################################
def setTomember(self,member,value):
     # If the VCS Canvas is displayed, then bring the canvas to the front before 
     # redisplaying the updated contents.
     if (self.parent.mode == 1) and (self.parent.iscanvasdisplayed()):
        Canvas.finish_queued_X_server_requests( self.parent )
        self.parent.canvas.BLOCK_X_SERVER()
        self.parent.canvasraised()

     _vcs_legacy.setTomember(self, member, value, self.parent.mode)

     # If the VCS Canvas is displayed, then update the backing store
     if (self.parent.mode == 1) and (self.parent.iscanvasdisplayed()):
        self.parent.flush()
        self.parent.backing_store()
        self.parent.canvas.UNBLOCK_X_SERVER()
setmember = setTomember

#################################################################################
#                                                                               #
# Function:     getTomember                                                     #
#                                                                               #
# Description of Function:                                                      #
#       Private function that retrieves the text orientation members from the C # 
#       structure and passes it back to Python.                                 #
#                                                                               #
#                                                                               #
# Example of Use:                                                               #
#      return_value =								#
#      getTomember(self,name)                                                   #
#              where: self is the class (e.g., To)                              #
#                     name is the name of the member that is being found        #
#                                                                               #
#################################################################################
def getTomember(self,member):
     return _vcs_legacy.getTomember(self,member)
getmember = getTomember

#################################################################################
#                                                                               #
# Function:     renameTo                                                        #
#                                                                               #
# Description of Function:                                                      #
#       Private function that renames the name of an existing text orientation  #
#       graphics method.                                                        #
#                                                                               #
#                                                                               #
# Example of Use:                                                               #
#     renameTo(old_name, new_name)                                              #
#      where: old_name is the current name of text orientation graphics method  #
#             new_name is the new name for the text orientation graphics method #
#                                                                               #
#################################################################################
def renameTo(self, old_name, new_name):
     return _vcs_legacy.renameTo(old_name, new_name)

#############################################################################
#                                                                           #
# Text Orientation (To) Class.                                              #
#                                                                           #
#############################################################################
class To(object):
    """
 Class:	To				# Text Orientation

 Description of To Class:
    The (To) Text Orientation lists text attribute set names that define the font, spacing,
    expansion, and color index.

    This class is used to define an text orientation table entry used in VCS, or it
    can be used to change some or all of the text orientation attributes in an
    existing text orientation table entry.

 Other Useful Functions:
 	     a=vcs_legacy.init()		# Constructor
	     a.show('textorientation')	# Show predefined text orientation objects
             a.update()               	# Updates the VCS Canvas at user's request
             a.mode=1, or 0           	# If 1, then automatic update, else if
                                          0, then use update function to
                                          update the VCS Canvas.

 Example of Use:
    a=vcs_legacy.init()
    To Create a new instance of text orientation use:
     to=a.createtextorientation('new','7left')	# Copies content of '7left' to 'new'
     to=a.createtextorientation('new') 	# Copies content of 'default' to 'new'

    To Modify an existing textorientation use:
     to=a.gettextorientation('7left')

    to.list()  				# Will list all the textorientation attribute values

    Specify the text height:
     to.height=20			# The height value must be an integer

    Specify the text angle:
     to.angle=0				# The angle value must be in the range 0 to 360

    Specify the text path:
     to.path='right'			# Same as to.path=0
     to.path='left'			# Same as to.path=1
     to.path='up'			# Same as to.path=2
     to.path='down'			# Same as to.path=3

    Specify the text horizontal alignment:
     to.halign='right'			# Same as to.halign=0
     to.halign='center'			# Same as to.halign=1
     to.halign='right'			# Same as to.halign=2

    Specify the text vertical alignment:
     to.valign='top'			# Same as tovalign=0
     to.valign='cap'			# Same as tovalign=1
     to.valign='half'			# Same as tovalign=2
     to.valign='base'			# Same as tovalign=3
     to.valign='bottom'			# Same as tovalign=4
     """
    __slots__ = [
         'setmember',
         'parent',
         'name',
         's_name',
         'height',
         'angle',
         'path',
         'halign',
         'valign',
         '_name',
         ]
    def _getname(self):
         return self._name
    def _setname(self,value):
         value=VCS_validation_functions.checkname(self,'name',value)
         if value is not None:
              self._name=value
              setTomember(self,'name',value)
    name=property(_getname,_setname)

    def _getheight(self):
         return getmember(self,'height')
    def _setheight(self,value):
         value = VCS_validation_functions.checkNumber(self,'height',value)
         setTomember(self,'height',value)
    height=property(_getheight,_setheight)

    def _getangle(self):
         return getmember(self,'angle')
    def _setangle(self,value):
         value = VCS_validation_functions.checkInt(self,'angle',value,minvalue=-360,maxvalue=360)
         setTomember(self,'angle',value)
    angle=property(_getangle,_setangle)

    def _getpath(self):
         return getmember(self,'path')
    def _setpath(self,value):
         vals = ["right","left","up","down"]
         value = VCS_validation_functions.checkInStringsListInt(self,'path',value,vals)
         setTomember(self,'path',vals[value])
    path = property(_getpath,_setpath)
    
    def _gethalign(self):
         return getmember(self,'halign')
    def _sethalign(self,value):
         vals = ["left","center","right"]
         value = VCS_validation_functions.checkInStringsListInt(self,'halign',value,vals)
         setTomember(self,'halign',vals[value])
    halign = property(_gethalign,_sethalign)
    
    def _getvalign(self):
         return getmember(self,'valign')
    def _setvalign(self,value):
         vals = ["top","cap","half","base","bottom"]
         value = VCS_validation_functions.checkInStringsListInt(self,'valign',value,vals)
         setTomember(self,'valign',vals[value])
    valign = property(_getvalign,_setvalign)
    
    #############################################################################
    #                                                                           #
    # Initialize the text orientation attributes.                               #
    #                                                                           #
    #############################################################################
    def __init__(self, parent, To_name=None, To_name_src='default', createTo=0):
	#                                                           #
        #############################################################
	# Initialize the text orientation class and its members     #
        #							    #
	# The getTomember function retrieves the values of the      #
        # text orientation members in the C structure and passes    #
        # back the appropriate Python Object.                       #
        #############################################################
	#                                                           #
        if (createTo == 0):
           if (To_name == None):
              raise ValueError, 'Must provide a text orientation name.'
           else:
              _vcs_legacy.copyTo(To_name_src, To_name)
              self._name = To_name
        else:
              self._name = To_name_src
        self.s_name = 'To'
##         self.__dict__['height']=getTomember(self, 'height')
##         self.__dict__['angle']=getTomember(self, 'angle')
##         self.__dict__['path']=getTomember(self, 'path')
##         self.__dict__['halign']=getTomember(self, 'halign')
##         self.__dict__['valign']=getTomember(self, 'valign')
        #                                                         #
        ###########################################################
        # Find and set the text orientation structure in VCS C    #
        # pointer list. If the text orientation name does not     #
        # exist, then use default text orientation.               #
        ###########################################################
        #                                                         #
        self.parent = parent

##     #############################################################################
##     #                                                                           #
##     # Set text orientation attributes.                                          #
##     #                                                                           #
##     #############################################################################
##     def __setattr__(self, name, value):
##         if (self.name == '__removed_from_VCS__'):
##            raise ValueError, 'This instance has been removed from VCS.'
##         if (self.name == 'default'):
##            raise ValueError, 'You cannot modify the default text orientation.'
##         if (name == 'name'):
##            if (type(value) == StringType):
##               renameTo(self,self.name, value)
##               self.__dict__['name']=value
##            else:
##               raise ValueError, 'The name attribute must be a string.'
##         elif (name == 'height'):
##            if (value == None):
##               self.__dict__['height']=None
##               setTomember(self,'height',self.height) # update the plot
##            elif (type(value) in [IntType, FloatType]):
## #              if value not in range(1,1001): # must be an integer
## #                 raise ValueError, 'The height value must be in the range 1 to 1000.'
## #              else:
##                  value = float ( value )
##                  self.__dict__['height']=value
##                  setTomember(self,'height',self.height) # update the plot
##            else:
##               raise ValueError, 'The height attribute value must be an integer or float.'
##         elif (name == 'angle'):
##            if (value == None):
##               self.__dict__['angle']=None
##               setTomember(self,'angle',self.angle) # update the plot
##            elif (type(value) in (IntType,FloatType)):
##               value = int(value)
##               if value not in range(-360,361): # must be an integer
##                  raise ValueError, 'The angle value must be in the range -360 to 360.'
##               else:
##                  self.__dict__['angle']=value
##                  setTomember(self,'angle',self.angle) # update the plot
##            else:
##               raise ValueError, 'The angle attribute value must be an integer.'
##         elif (name == 'path'):
##            if (value in ('right', 'left', 'up', 'down', 0, 1, 2, 3)):
##               if value in ('right', 0):
##                  value='right'
##               elif value in ('left', 1):
##                  value='left'
##               elif value in ('up', 2):
##                  value='up'
##               elif value in ('down', 3):
##                  value='down'
##               self.__dict__['path']=value
##               setTomember(self,'path',self.path) # update the plot
##            else:
##               raise ValueError, 'The path attribute must be either ("right","left","up","down") or (0,1,2,3).'
##         elif (name == 'halign'):
##            if (value in ( 'left', 'center', 'right', 0, 1, 2)):
##               if value in ('left', 0):
##                  value='left'
##               elif value in ('center', 1):
##                  value='center'
##               elif value in ('right', 2):
##                  value='right'
##               self.__dict__['halign']=value
##               setTomember(self,'halign',self.halign) # update the plot
##            else:
##               raise ValueError, 'The halign attribute must be either ("left","center","right") or (0,1,2).'
##         elif (name == 'valign'):
##            if (value in ('top', 'cap', 'half', 'base', 'bottom', 0, 1, 2, 3, 4)):
##               if value in ('top', 0):
##                  value='top'
##               elif value in ('cap', 1):
##                  value='cap'
##               elif value in ('half', 2):
##                  value='half'
##               elif value in ('base', 3):
##                  value='base'
##               elif value in ('bottom', 4):
##                  value='bottom'
##               self.__dict__['valign']=value
##               setTomember(self,'valign',self.valign) # update the plot
##            else:
##               raise ValueError, 'The valign attribute must be either ("top","cap","half","base","bottom") or (0,1,2,3,4).'

    #############################################################################
    #                                                                           #
    # List out text orientation members (attributes).                           #
    #                                                                           #
    #############################################################################
    def list(self):
        if (self.name == '__removed_from_VCS__'):
           raise ValueError, 'This instance has been removed from VCS.'
        print "","----------Text Orientation (To) member (attribute) listings ----------"
        print 'Canvas Mode =',self.parent.mode
        print "secondary method =", self.s_name
        print "name =", self.name
        print "height =", self.height
        print "angle =", self.angle
        print "path =", self.path
        print "halign =", self.halign
        print "valign =", self.valign

    #############################################################################
    #                                                                           #
    # Script out secondary text orientation method in VCS to a file.            #
    #                                                                           #
    #############################################################################
    def script(self, script_filename=None, mode=None):
        '''
 Function:     script                           # Calls _vcs_legacy.scripTo

 Description of Function:
       Saves out a text orientation graphics method in Python or VCS script form
       to a designated file.

 Example of Use:
    script(scriptfile_name, mode)
              where: scriptfile_name is the output name of the script file.
	      	     mode is either "w" for replace or "a" for append.

              Note: If the the filename has a ".py" at the end, it will produce a
                    Python script. If the filename has a ".scr" at the end, it will
                    produce a VCS script. If neither extensions are give, then by
                    default a Python script will be produced.

    a=vcs_legacy.init()
    to=a.createtextorientation('temp')
    to.script('filename.py')         # Append to a Python file "filename.py"
    to.script('filename.scr')        # Append to a VCS file "filename.scr"
    to.script('filename','w')        # Create or overwrite to a Python file "filename.py"
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
           print _vcs_legacy.scriptTo(self.name,script_filename,mode)
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

           unique_name = '__To__' + self.name
           fp.write("#----------Text Orientation (To) member (attribute) listings ----------\n")
           fp.write("to_list=v.listelements('textorientation')\n")
           fp.write("if ('%s' in to_list):\n" % self.name)
           fp.write("   %s = v.gettextorientation('%s')\n" % (unique_name, self.name))
           fp.write("else:\n")
           fp.write("   %s = v.createtextorientation('%s')\n" % (unique_name, self.name))
           fp.write("%s.height = %g\n" % (unique_name, self.height))
           fp.write("%s.angle = %g\n" % (unique_name, self.angle))
           fp.write("%s.path = '%s'\n" % (unique_name, self.path))
           fp.write("%s.halign = '%s'\n" % (unique_name, self.halign))
           fp.write("%s.valign = '%s'\n\n" % (unique_name, self.valign))
           fp.close()


#################################################################################
#        END OF FILE								#
#################################################################################
