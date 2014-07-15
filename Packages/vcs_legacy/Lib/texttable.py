# Adapted for numpy/ma/cdms2 by convertcdms.py
"""
# Text Table (Tt) module
"""
###############################################################################
#                                                                             #
# Module:       texttable (Tt) module                                         #
#                                                                             #
# Copyright:    2000, Regents of the University of California                 #
#               This software may not be distributed to others without        #
#               permission of the author.                                     #
#                                                                             #
# Author:       PCMDI Software Team                                           #
#               Lawrence Livermore NationalLaboratory:                        #
#               support@pcmdi.llnl.gov                                        #
#                                                                             #
# Description:  Python command wrapper for VCS's text table secondary object. #
#                                                                             #
# Version:      4.0                                                           #
#                                                                             #
###############################################################################
#
#
#
###############################################################################
#                                                                             #
# Import: VCS C extension module.                                             #
#                                                                             #
###############################################################################
import _vcs_legacy
import Canvas
from types import *
import VCS_validation_functions

###############################################################################
#                                                                             #
# Function:	setTtmember                                                   #
#                                                                             #
# Description of Function:                                                    #
# 	Private function to update the VCS canvas plot. If the canvas mode is #
#       set to 0, then this function does nothing.              	      #
#                                                                             #
#                                                                             #
# Example of Use:                                                             #
#      setTtmember(self,name,value)					      #
#              where: self is the class (e.g., Tt)                            #
#                     name is the name of the member that is being changed    #
#                     value is the new value of the member (or attribute)     #
#                                                                             #
###############################################################################
def setTtmember(self,member,value):
     # If the VCS Canvas is displayed, then bring the canvas to the front before 
     # redisplaying the updated contents.
     if (self.parent.mode == 1) and (self.parent.iscanvasdisplayed()):
        Canvas.finish_queued_X_server_requests( self.parent )
        self.parent.canvas.BLOCK_X_SERVER()
        self.parent.canvasraised()

     _vcs_legacy.setTtmember(self, member, value, self.parent.mode)

     # If the VCS Canvas is displayed, then update the backing store
     if (self.parent.mode == 1) and (self.parent.iscanvasdisplayed()):
        self.parent.flush()
        self.parent.backing_store()
        self.parent.canvas.UNBLOCK_X_SERVER()
setmember = setTtmember

###############################################################################
#                                                                             #
# Function:     getTtmember                                                   #
#                                                                             #
# Description of Function:                                                    #
#       Private function that retrieves the text table members from the C     # 
#       structure and passes it back to Python.                               #
#                                                                             #
#                                                                             #
# Example of Use:                                                             #
#      return_value =							      #
#      getTtmember(self,name)                                                 #
#              where: self is the class (e.g., Tt)                            #
#                     name is the name of the member that is being found      #
#                                                                             #
###############################################################################
def getTtmember(self,member):
     return _vcs_legacy.getTtmember(self,member)
getmember = getTtmember

###############################################################################
#                                                                             #
# Function:     renameTt                                                      #
#                                                                             #
# Description of Function:                                                    #
#       Private function that renames the name of an existing text table      #
#       graphics method.                                                      #
#                                                                             #
#                                                                             #
# Example of Use:                                                             #
#      renameTt(old_name, new_name)                                           #
#            where: old_name is the current name of text table graphics method#
#                   new_name is the new name for the text table graphics method#
#                                                                             #
###############################################################################
def renameTt(self, old_name, new_name):
     return _vcs_legacy.renameTt(old_name, new_name)

#############################################################################
#                                                                           #
# Text Table (Tt) Class.                                                    #
#                                                                           #
#############################################################################
class Tt(object):
    """
 Class:	Tt				# Text Table

 Description of Tt Class:
    The (Tt) Text Table lists text attribute set names that define the font, spacing,
    expansion, and color index.

    This class is used to define an text table table entry used in VCS, or it
    can be used to change some or all of the text table attributes in an
    existing text table table entry.

 Other Useful Functions:
 	     a=vcs_legacy.init()		# Constructor
	     a.show('texttable')	# Show predefined text table objects
             a.update()               	# Updates the VCS Canvas at user's request
             a.mode=1, or 0           	# If 1, then automatic update, else if
                                          0, then use update function to
                                          update the VCS Canvas.

 Example of Use:
    a=vcs_legacy.init()
    To Create a new instance of text table use:
     tt=a.createtexttable('new','std')	# Copies content of 'std' to 'new'
     tt=a.createtexttable('new') 	# Copies content of 'default' to 'new'

    To Modify an existing texttable use:
     tt=a.gettexttable('std')

    tt.list()  				# Will list all the texttable attribute values

    Specify the text font type:
     tt.font=1				# The font value must be in the range 1 to 9

    Specify the text spacing:
     tt.spacing=2			# The spacing value must be in the range -50 to 50

    Specify the text expansion:
     tt.expansion=100			# The expansion value must be in the range 50 to 150

    Specify the text color:
     tt.color=241			# The text color attribute value must be in the range 1 to 257

     tt.priority=1                      # Set the graphics priority on the canvas
     tt.viewport=[0, 1.0, 0,1.0]        # FloatType [0,1]x[0,1]
     tt.worldcoordinate=[0,1.0,0,1.0]   # FloatType [#,#]x[#,#]

     tt.x=[[0,.1,.2], [.3,.4,.5]]        # List of FloatTypes
     tt.y=[[.5,.4,.3], [.2,.1,0]]        # List of FloatTypes

"""
    __slots__ = [
         'setmember',
         'parent',
         'name',
         's_name',
         'color',
         'fillincolor',
         'priority',
         'font',
         'string',
         'spacing',
         'expansion',
         'viewport',
         'worldcoordinate',
         'x',
         'y',
         'projection',
         '_name',
         ]
    def _getname(self):
         return self._name
    def _setname(self,value):
         value=VCS_validation_functions.checkname(self,'name',value)
         if value is not None:
              self._name=value
              setTtmember(self,'name',value)
    name=property(_getname,_setname)
    
    def _getcolor(self):
         return getmember(self,'color')
    def _setcolor(self,value):
         if not value is None:
              value = VCS_validation_functions.checkColor(self,'color',value)
         setTtmember(self,'color',value)
    color=property(_getcolor,_setcolor)

    def _getfillincolor(self):
         return getmember(self,'fillincolor')
    def _setfillincolor(self,value):
         if not value is None:
              value = VCS_validation_functions.checkColor(self,'fillincolor',value)
         setTtmember(self,'fillincolor',value)
    fillincolor=property(_getfillincolor,_setfillincolor)

    def _getspacing(self):
         return getmember(self,'spacing')
    def _setspacing(self,value):
         value = VCS_validation_functions.checkInt(self,'spacing',value,minvalue=-50,maxvalue=50)
         setTtmember(self,'spacing',value)
    spacing=property(_getspacing,_setspacing)

    def _getexpansion(self):
         return getmember(self,'expansion')
    def _setexpansion(self,value):
         value = VCS_validation_functions.checkInt(self,'expansion',value,minvalue=50,maxvalue=150)
         setTtmember(self,'expansion',value)
    expansion=property(_getexpansion,_setexpansion)

    def _getfont(self):
         return getmember(self,'font')
    def _setfont(self,value):
         value = VCS_validation_functions.checkFont(self,'font',value)
         setTtmember(self,'font',value)
    font=property(_getfont,_setfont)

    def _getstring(self):
         return getmember(self,'string')
    def _setstring(self,value):
         if isinstance(value,str):
              value = [value,]
         elif isinstance(value,(list,tuple)):
              vals = []
              for v in value:
                   vals.append(VCS_validation_functions.checkString(self,'string',v))
              value = vals
         else:
              raise ValueError, 'Must be a string or a list of strings.'
         setTtmember(self,'string',value)
    string = property(_getstring,_setstring)
    
    def _getpriority(self):
         return getmember(self,'priority')
    def _setpriority(self,value):
         value = VCS_validation_functions.checkInt(self,'priority',value,minvalue=0)
         setTtmember(self,'priority',value)
    priority = property(_getpriority,_setpriority)

    def _getprojection(self):
         return getmember(self,'projection')
    def _setprojection(self,value):
         value=VCS_validation_functions.checkProjection(self,'projection',value)
         setTtmember(self,'projection',value)
    projection=property(_getprojection,_setprojection)
    
    def _getwc(self):
         return getmember(self,'worldcoordinate')
    def _setwc(self,value):
         value = VCS_validation_functions.checkListOfNumbers(self,'worldcoordinate',value,maxelements=4)
         setTtmember(self,'worldcoordinate',value)
    worldcoordinate=property(_getwc,_setwc)
    
    def _getvp(self):
         return getmember(self,'viewport')
    def _setvp(self,value):
         value = VCS_validation_functions.checkListOfNumbers(self,'viewport',value,maxelements=4,minvalue=0.,maxvalue=1.)
         setTtmember(self,'viewport',value)
    viewport=property(_getvp,_setvp)

    def _getx(self):
         return getmember(self,'x')
    def _setx(self,value):
         if isinstance(value,(int,float)):
              value=[value,]
         if not isinstance(value,(list,tuple)):
              raise ValueError, '%s must be a tuple or list of values.'
         try:
              # first we'll see if it is simply a list of values
              value = VCS_validation_functions.checkListOfNumbers(self,'x',value)
         except:
              # ok it was not, so it maybe a list of list of numbers ?
              val = []
              for v in value:
                   tmp = VCS_validation_functions.checkListOfNumbers(self,'x',v)
                   val.append(tmp)
              value=val
         setTtmember(self,'x',value)
    x = property(_getx,_setx)
    
    def _gety(self):
         return getmember(self,'y')
    def _sety(self,value):
         if isinstance(value,(int,float)):
              value=[value,]
         if not isinstance(value,(list,tuple)):
              raise ValueError, '%s must be a tuple or list of values.'
         try:
              # first we'll see if it is simply a list of values
              value = VCS_validation_functions.checkListOfNumbers(self,'x',value)
         except:
              # ok it was not, so it maybe a list of list of numbers ?
              val = []
              for v in value:
                   tmp = VCS_validation_functions.checkListOfNumbers(self,'x',v)
                   val.append(tmp)
              value=val
         setTtmember(self,'y',value)
    y = property(_gety,_sety)
    
    #############################################################################
    #                                                                           #
    # Initialize the text table attributes.                                     #
    #                                                                           #
    #############################################################################
    def __init__(self, parent, Tt_name=None, Tt_name_src='default', createTt=0):
	#                                                           #
        #############################################################
	# Initialize the text table class and its members           #
        #							    #
	# The getTtmember function retrieves the values of the      #
        # text table members in the C structure and passes back the #
	# appropriate Python Object.                                #
        #############################################################
	#                                                           #
        if (createTt == 0):
           if (Tt_name == None):
              raise ValueError, 'Must provide a text table name.'
           else:
              _vcs_legacy.copyTt(Tt_name_src, Tt_name)
              self._name = Tt_name
        else:
              self._name = Tt_name_src
        self.s_name = 'Tt'
##         self.__dict__['string']=getTtmember(self, 'string')
##         self.__dict__['font']=getTtmember(self, 'font')
##         self.__dict__['spacing']=getTtmember(self, 'spacing')
##         self.__dict__['expansion']=getTtmember(self, 'expansion')
##         self.__dict__['color']=getTtmember(self, 'color')
##         self.__dict__['priority']=getTtmember(self, 'priority')
##         self.__dict__['viewport']=getTtmember(self, 'viewport')
##         self.__dict__['worldcoordinate']=getTtmember(self, 'worldcoordinate')
##         self.__dict__['x']=getTtmember(self, 'x')
##         self.__dict__['y']=getTtmember(self, 'y')
##         self.__dict__['projection']=getTtmember(self, 'projection')
        #                                                         #
        ###########################################################
        # Find and set the text table structure in VCS C pointer  #
        # list. If the text table name does not exist, then use   #
        # default text table.                                     #
        ###########################################################
        #                                                         #
        self.parent = parent

##     #############################################################################
##     #                                                                           #
##     # Set text table attributes.                                                #
##     #                                                                           #
##     #############################################################################
##     def __setattr__(self, name, value):
##         if (self.name == '__removed_from_VCS__'):
##            raise ValueError, 'This instance has been removed from VCS.'
##         if (self.name == 'default'):
##            raise ValueError, 'You cannot modify the default text table.'
##         if (name == 'name'):
##            if (type(value) == StringType):
##               renameTt(self,self.name, value)
##               self.__dict__['name']=value
##            else:
##               raise ValueError, 'The name attribute must be a string.'
##         elif (name == 'string'): # Set the string
##            if (type(value) == StringType):
##               l = []
##               l.append( value )
##               self.__dict__[name]=l
##               setTtmember(self,name,l) # update the plot
##            elif ( (type(value) in (ListType, TupleType)) and (value not in [ [], () ]) ):
##               value=list(value)
##               for x in value:
##                  if type(x) != StringType:
##                    raise ValueError, 'List must contain strings only.'
##                    break
##               self.__dict__[name]=value
##               setTtmember(self,name,value) # update the plot
##            elif value is None:
##               self.__dict__[name]=value
##               setTtmember(self,name,value) # update the plot
##            else:
##               raise ValueError, 'Must be a string or a list of strings.'
##            return
##         elif (name == 'font'):
##              value = VCS_validation_functions.checkFont('',name,value)
##              self.__dict__['font']=value
##              setTtmember(self,'font',self.font) # update the plot               
##         elif (name == 'spacing'):
##            if (value == None):
##               self.__dict__['spacing']=None
##               setTtmember(self,'spacing',self.spacing) # update the plot
##            elif (isinstance(value, IntType)):
##               if value not in range(-50,51): # must be an integer
##                  raise ValueError, 'The spacing value must be in the range -50 to 50.'
##               else:
##                  self.__dict__['spacing']=value
##                  setTtmember(self,'spacing',self.spacing) # update the plot
##            else:
##               raise ValueError, 'The spacing attribute values must be an integer.'
##         elif (name == 'expansion'):
##            if (value == None):
##               self.__dict__['expansion']=None
##               setTtmember(self,'expansion',self.expansion) # update the plot
##            elif (isinstance(value, IntType)):
##               if value not in range(50,151): # must be an integer
##                  raise ValueError, 'The expansion value must be in the range 50 to 150.'
##               else:
##                  self.__dict__['expansion']=value
##                  setTtmember(self,'expansion',self.expansion) # update the plot
##            else:
##               raise ValueError, 'The expansion attribute value must be an integer.'
##         elif (name == 'color'):
##            if (value == None):
##               self.__dict__['color']=None
##               setTtmember(self,'color',self.color) # update the plot
##            elif (isinstance(value, IntType)):
##               if value not in range(0,256): # must be an integer
##                  raise ValueError, 'The text table color value must be in the range 0 to 255.'
##               else:
##                  self.__dict__['color']=value
##                  setTtmember(self,'color',self.color) # update the plot
##            else:
##               raise ValueError, 'The color attribute value must be an integer in the range 0 to 255.'
##         elif (name == 'priority'):
##            if (value == None):
##               self.__dict__['priority']=None
##               setTtmember(self,'priority',self.priority) # update the plot
##            elif (isinstance(value, IntType)):
##               self.__dict__['priority']=value
##               setTtmember(self,'priority',self.priority) # update the plot
##            else:
##               raise ValueError, 'The priority attribute value must be an integer.'
##         elif (name == 'viewport'):
##            if (value == None):
##              self.__dict__[name]= [0.0, 1.0, 0.0, 1.0]
##              setTtmember(self,name,[0.0, 1.0, 0.0, 1.0]) # update the plot
##            else:
##              if (type(value) in (ListType, TupleType)):
##               value = list(value)  # make sure that values list is a list
##               if len(value) != 4:
##                  self.__dict__[name]= [0.0, 1.0, 0.0, 1.0]
##                  raise ValueError, 'Viewport must contain 4 integer or float values.'
##               else:
##                  self.__dict__[name]=value
##                  setTtmember(self,name,value) # update the plot
##              else:
##               raise ValueError, 'The viewport attribute must be a tuple or list of values.'
##         elif (name == 'worldcoordinate'):
##            if (value == None):
##              self.__dict__[name]= [0.0, 1.0, 0.0, 1.0]
##              setTtmember(self,name,[0.0, 1.0, 0.0, 1.0]) # update the plot
##            else:
##              if (type(value) in (ListType, TupleType)):
##               value = list(value)  # make sure that values list is a list
##               if len(value) != 4:
##                  self.__dict__[name]= [0.0, 1.0, 0.0, 1.0]
##                  raise ValueError, 'World coordinates must contain 4 integer or float values.'
##               else:
##                  self.__dict__[name]=value
##                  setTtmember(self,name,value) # update the plot
##              else:
##               raise ValueError, 'The world coordinates attribute must be a tuple or list of values.'
##         elif (name == 'x'):
##            if (value == None):
##              self.__dict__[name] = None
##              setTtmember(self,name,value) # update the plot
##            else:
##              if isinstance(value, numpy.ArrayType): value = value.tolist()
##              if (type(value) in (ListType, TupleType)):
##                 value = list(value)  # make sure that values list is a list
##                 for i in range(len(value)):
##                     if isinstance(value[i], numpy.ArrayType): value[i]=value[i].tolist()
##                     elif type(value[i]) is TupleType: value[i] = list( value[i] )
##                 self.__dict__[name]=value
##                 setTtmember(self,name,value) # update the plot
##              else:
##               raise ValueError, 'The x attribute must be a tuple or list of values.'
##         elif (name == 'y'):
##            if (value == None):
##              self.__dict__[name] = None
##              setTtmember(self,name,value) # update the plot
##            else:
##              if isinstance(value, numpy.ArrayType): value = value.tolist()
##              if (type(value) in (ListType, TupleType)):
##                 value = list(value)  # make sure that values list is a list
##                 for i in range(len(value)):
##                     if isinstance(value[i], numpy.ArrayType): value[i]=value[i].tolist()
##                     elif type(value[i]) is TupleType: value[i] = list( value[i] )
##                 self.__dict__[name]=value
##                 setTtmember(self,name,value) # update the plot
##              else:
##               raise ValueError, 'The y attribute must be a tuple or list of values.'
##         elif (name == 'y'):
##            if (value == None):
##              self.__dict__[name] = 'default'
##              setTtmember(self,name,'default') # update the plot
##            else:
##                 value=VCS_validation_functions.checkProjection(self,'projection',value)
##                 self.__dict__[name]=value
##                 setTtmember(self,name,value) # update the plot

    #############################################################################
    #                                                                           #
    # List out text table members (attributes).                                 #
    #                                                                           #
    #############################################################################
    def list(self):
        if (self.name == '__removed_from_VCS__'):
           raise ValueError, 'This instance has been removed from VCS.'
        print "","----------Text Table (Tt) member (attribute) listings ----------"
        print 'Canvas Mode =',self.parent.mode
        print "secondary method =", self.s_name
        print "name =", self.name
#        print "string =", self.string
        print "font =", self.font
        print "spacing =", self.spacing
        print "expansion =", self.expansion
        print "color =", self.color
        print "fillincolor =", self.fillincolor
#        print "priority =", self.priority
#        print "viewport =", self.viewport
#        print "worldcoordinate =", self.worldcoordinate
#        print "x =", self.x
#        print "y =", self.y

    #############################################################################
    #                                                                           #
    # Script out secondary text table method in VCS to a file.                  #
    #                                                                           #
    #############################################################################
    def script(self, script_filename=None, mode=None):
        '''
 Function:     script                           # Calls _vcs_legacy.scriptTt

 Description of Function:
       Saves out a text table graphics method in VCS Python or script form to
       a designated file.

 Example of Use:
    script(scriptfile_name, mode)
              where: scriptfile_name is the output name of the script file.
                     mode is either "w" for replace or "a" for append.

              Note: If the the filename has a ".py" at the end, it will produce a
                    Python script. If the filename has a ".scr" at the end, it will
                    produce a VCS script. If neither extensions are give, then by
                    default a Python script will be produced.

    a=vcs_legacy.init()
    tt=a.createtexttable('temp')
    tt.script('filename.py')         # Append to a Python file "filename.py"
    tt.script('filename.scr')        # Append to a VCS file "filename.scr"
    tt.script('filename','w')        # Create or overwrite to a Python file "filename.py"
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
           print _vcs_legacy.scriptTt(self.name,script_filename,mode)
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

           unique_name = '__Tt__' + self.name
           fp.write("#----------Text Table (Tt) member (attribute) listings ----------\n")
           fp.write("tt_list=v.listelements('texttable')\n")
           fp.write("if ('%s' in tt_list):\n" % self.name)
           fp.write("   %s = v.gettexttable('%s')\n" % (unique_name, self.name))
           fp.write("else:\n")
           fp.write("   %s = v.createtexttable('%s')\n" % (unique_name, self.name))
           fp.write("%s.font = %g\n" % (unique_name, self.font))
           fp.write("%s.spacing = %g\n" % (unique_name, self.spacing))
           fp.write("%s.expansion = %g\n" % (unique_name, self.expansion))
           fp.write("%s.color = %g\n\n" % (unique_name, self.color))
           fp.write("%s.fillincolor = %g\n\n" % (unique_name, self.fillincolor))
           fp.write("%s.priority = %d\n" % (unique_name, self.priority))
           fp.write("%s.viewport = %s\n" % (unique_name, self.viewport))
           fp.write("%s.worldcoordinate = %s\n" % (unique_name, self.worldcoordinate))
           fp.write("%s.x = %s\n" % (unique_name, self.x))
           fp.write("%s.y = %s\n\n" % (unique_name, self.y))
           fp.write("%s.projection = %s\n\n" % (unique_name, self.projection))


#################################################################################
#        END OF FILE								#
#################################################################################
