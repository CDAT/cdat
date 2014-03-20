# Adapted for numpy/ma/cdms2 by convertcdms.py
"""
# Fillarea (Tf) module
"""
#################################################################################
#                                                                               #
# Module:       fillarea (Tf) module                                            #
#                                                                               #
# Copyright:    2000, Regents of the University of California                   #
#               This software may not be distributed to others without          #
#               permission of the author.                                       #
#                                                                               #
# Author:       PCMDI Software Team                                             #
#               Lawrence Livermore NationalLaboratory:                          #
#               support@pcmdi.llnl.gov                                          #
#                                                                               #
# Description:  Python command wrapper for VCS's fill area secondary object.    #
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
import VCS_validation_functions

#################################################################################
#                                                                               #
# Function:	setTfmember                                                     #
#                                                                               #
# Description of Function:                                                      #
# 	Private function to update the VCS canvas plot. If the canvas mode is   #
#       set to 0, then this function does nothing.              		#
#                                                                               #
#                                                                               #
# Example of Use:                                                               #
#      setTfmember(self,name,value)						#
#              where: self is the class (e.g., Tf)                              #
#                     name is the name of the member that is being changed      #
#                     value is the new value of the member (or attribute)       #
#                                                                               #
#################################################################################
def setTfmember(self,member,value):
     # If the VCS Canvas is displayed, then bring the canvas to the front before 
     # redisplaying the updated contents.
     if (self.parent.mode == 1) and (self.parent.iscanvasdisplayed()):
        Canvas.finish_queued_X_server_requests( self.parent )
        self.parent.canvas.BLOCK_X_SERVER()
        self.parent.canvasraised()

     _vcs_legacy.setTfmember(self, member, value, self.parent.mode)

     # If the VCS Canvas is displayed, then update the backing store
     if (self.parent.mode == 1) and (self.parent.iscanvasdisplayed()):
        self.parent.flush()
        self.parent.backing_store()
        self.parent.canvas.UNBLOCK_X_SERVER()

setmember = setTfmember # for validation functions...
#################################################################################
#                                                                               #
# Function:     getTfmember                                                     #
#                                                                               #
# Description of Function:                                                      #
#       Private function that retrieves the fillarea members from the C         #
#       structure and passes it back to Python.                                 #
#                                                                               #
#                                                                               #
# Example of Use:                                                               #
#      return_value =								#
#      getTfmember(self,name)                                                   #
#              where: self is the class (e.g., Tf)                              #
#                     name is the name of the member that is being found        #
#                                                                               #
#################################################################################
def getTfmember(self,member):
     return _vcs_legacy.getTfmember(self,member)
getmember=getTfmember

#################################################################################
#                                                                               #
# Function:     renameTf                                                        #
#                                                                               #
# Description of Function:                                                      #
#       Private function that renames the name of an existing fillarea          #
#       graphics method.                                                        #
#                                                                               #
#                                                                               #
# Example of Use:                                                               #
#      renameTf(old_name, new_name)                                             #
#              where: old_name is the current name of fillarea graphics method  #
#                     new_name is the new name for the fillarea graphics method #
#                                                                               #
#################################################################################
def renameTf(self, old_name, new_name):
     return _vcs_legacy.renameTf(old_name, new_name)

#############################################################################
#                                                                           #
# Fillarea (Tm) Class.                                                      #
#                                                                           #
#############################################################################
class Tf(object):
    """
 Class:	Tf				# Fillarea

 Description of Tf Class:
    The Fillarea class object allows the user to edit fillarea attributes, including
    fillarea interior style, style index, and color index.

    This class is used to define an fillarea table entry used in VCS, or it
    can be used to change some or all of the fillarea attributes in an
    existing fillarea table entry.

 Other Useful Functions:
 	     a=vcs_legacy.init()		# Constructor
	     a.show('fillarea')		# Show predefined fillarea objects
             a.update()               	# Updates the VCS Canvas at user's request
             a.mode=1, or 0           	# If 1, then automatic update, else if
                                          0, then use update function to
                                          update the VCS Canvas.

 Example of Use:
    a=vcs_legacy.init()
    To Create a new instance of fillarea use:
     fa=a.createfillarea('new','def37')	# Copies content of 'def37' to 'new'
     fa=a.createfillarea('new') 	# Copies content of 'default' to 'new'

    To Modify an existing fillarea use:
     fa=a.getfillarea('red')

    fa.list()  				# Will list all the fillarea attribute values

    There are three possibilities for setting the isofill style (Ex):
    fa.style = 'solid'
    fa.style = 'hatch'
    fa.style = 'pattern'
    fa.index=1			        # Range from 1 to 20
    fa.color=100			# Range from 1 to 256

    Specify the fillarea index:
     fa.index=1
     fa.index=2
     fa.index=3
     fa.index=4
     fa.index=5
     fa.index=6
     fa.index=7
     fa.index=8
     fa.index=9
     fa.index=10
     fa.index=11
     fa.index=12
     fa.index=13
     fa.index=14
     fa.index=15
     fa.index=16
     fa.index=17
     fa.index=18
     fa.index=19
     fa.index=20

     fa.priority=1                      # Set the graphics priority on the canvas
     fa.viewport=[0, 1.0, 0,1.0]        # FloatType [0,1]x[0,1]
     fa.worldcoordinate=[0,1.0,0,1.0]   # FloatType [#,#]x[#,#]

     fa.x=[[0,.1,.2], [.3,.4,.5]]        # List of FloatTypes
     fa.y=[[.5,.4,.3], [.2,.1,0]]        # List of FloatTypes
"""
    __slots__ = [
         'setmember',
         'parent',
         'name',
         's_name',
         'color',
         'priority',
         'style',
         'index',
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
              setmember(self,'name',value)
    name=property(_getname,_setname)

    def _getfillareacolors(self):
         return getmember(self,'color')
    def _setfillareacolors(self,value):
         if isinstance(value,int):
              value=[value,]
         if not value is None:
              value = VCS_validation_functions.checkColorList(self,'color',value)
         setmember(self,'color',value)
    color=property(_getfillareacolors,_setfillareacolors)

    def _getfillareaindices(self):
         return getmember(self,'index')
    def _setfillareaindices(self,value):
         if not isinstance(value,(list,tuple)) and value is not None:
              value=[value,]
         if value is not None:
              value = VCS_validation_functions.checkIndicesList(self,'index',value)
         setmember(self,'index',value)
    index=property(_getfillareaindices,_setfillareaindices)

    def _getfillareastyle(self):
         return getmember(self,'style')
    
    def _setfillareastyle(self,value):
         if isinstance(value,(str,int)):
              value=[value,]
         vals=[]
         for v in value:
              v=VCS_validation_functions.checkFillAreaStyle(self,'style',v)
              vals.append(v)
         value=vals
         setmember(self,'style',value)
    style=property(_getfillareastyle,_setfillareastyle)

    def _getpriority(self):
         return getmember(self,'priority')
    def _setpriority(self,value):
         value = VCS_validation_functions.checkInt(self,'priority',value,minvalue=0)
         setmember(self,'priority',value)
    priority = property(_getpriority,_setpriority)

    def _getprojection(self):
         return getmember(self,'projection')
    def _setprojection(self,value):
         value=VCS_validation_functions.checkProjection(self,'projection',value)
         setmember(self,'projection',value)
    projection=property(_getprojection,_setprojection)
    
    def _getwc(self):
         return getmember(self,'worldcoordinate')
    def _setwc(self,value):
         value = VCS_validation_functions.checkListOfNumbers(self,'worldcoordinate',value,maxelements=4)
         setmember(self,'worldcoordinate',value)
    worldcoordinate=property(_getwc,_setwc)
    
    def _getvp(self):
         return getmember(self,'viewport')
    def _setvp(self,value):
         value = VCS_validation_functions.checkListOfNumbers(self,'viewport',value,maxelements=4,minvalue=0.,maxvalue=1.)
         setmember(self,'viewport',value)
    viewport=property(_getvp,_setvp)

    def _getx(self):
         return getmember(self,'x')
    def _setx(self,value):
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
         # ok it worked
         setmember(self,'x',value)
    x = property(_getx,_setx)
    
    def _gety(self):
         return getmember(self,'y')
    def _sety(self,value):
         if not isinstance(value,(list,tuple)):
              raise ValueError, '%s must be a tuple or list of values.'
         try:
              # first we'll see if it is simply a list of values
              value = VCS_validation_functions.checkListOfNumbers(self,'y',value)
         except:
              # ok it was not, so it maybe a list of list of numbers ?
              val = []
              for v in value:
                   tmp = VCS_validation_functions.checkListOfNumbers(self,'y',v)
                   val.append(tmp)
              value=val
         # ok it worked
         setmember(self,'y',value)
    y = property(_gety,_sety)
    
    #############################################################################
    #                                                                           #
    # Initialize the fillarea attributes.                                       #
    #                                                                           #
    #############################################################################
    def __init__(self, parent, Tf_name=None, Tf_name_src='default', createTf=0):
	#                                                         #
        ###########################################################
	# Initialize the fillarea class and its members           #
        #							  #
	# The getTfmember function retrieves the values of the    #
        # fillarea members in the C structure and passes back the #
	# appropriate Python Object.                              #
        ###########################################################
	#                                                         #
        if (createTf == 0):
           if (Tf_name == None):
              raise ValueError, 'Must provide a fillarea name.'
           else:
              _vcs_legacy.copyTf(Tf_name_src, Tf_name)
              self._name = Tf_name
        else:
              self._name = Tf_name_src
        self.s_name='Tf'
##         self._style=getTfmember(self, 'style')
##         self._index=getTfmember(self, 'index')
##         self._color=getTfmember(self, 'color')
##         self._priority=getTfmember(self, 'priority')
##         self._viewport=getTfmember(self, 'viewport')
##         self._worldcoordinate=getTfmember(self, 'worldcoordinate')
##         self._x=getTfmember(self, 'x')
##         self._y=getTfmember(self, 'y')
##         self._projection=getTfmember(self, 'projection')
        #                                                         #
        ###########################################################
        # Find and set the fillarea structure in VCS C pointer    #
        # list. If the fillarea name does not exist, then use     #
        # default fillarea.                                       #
        ###########################################################
        #                                                         #
        self.parent = parent

    #############################################################################
    #                                                                           #
    # Set fillarea attributes.                                                  #
    #                                                                           #
    #############################################################################
##     def __setattr__(self, name, value):
##         if (self.name == '__removed_from_VCS__'):
##            raise ValueError, 'This instance has been removed from VCS.'
##         if (self.name == 'default'):
##            raise ValueError, 'You cannot modify the default fillarea.'
##         if (name == 'name'):
##            if isinstance(value,str):
##               renameTf(self,self.name, value)
##               self.__dict__['name']=value
##            else:
##               raise ValueError, 'The name attribute must be a string.'
##         elif (name == 'style'):
##            if (value in ('solid', 'hatch', 'pattern')):
##               l=[]
##               l.append(value)
##               self.__dict__['style']=l
##               setTfmember(self,'style',self.style) # update the plot
##            elif VCS_validation_functions.isListorTuple(value):
##               if isinstance(value,tuple): value = list(value)  # must have a list
##               for x in value:
##                  if x not in ('solid', 'hatch', 'pattern'):
##                     raise ValueError, 'Style attribute must be either solid, hatch, or pattern.'
##               self.__dict__[name]=value
##               setTfmember(self,name,value) # update the plot
##            else:
##               raise ValueError, 'The style attribute must be either solid, hatch, or pattern.'
##         elif (name == 'index'):
##            if (value == None):
##               self.__dict__['index']=None
##               setTfmember(self,'index',self.index) # update the plot
##            elif isinstance(value,int):
##               if value not in range(1,21): # must be an integer
##                  raise ValueError, 'The index value must be in the range 1 to 20.'
##               else:
##                  l=[]
##                  l.append(value)
##                  self.__dict__['index']=l
##                  setTfmember(self,'index',self.index) # update the plot
##            elif VCS_validation_functions.isListorTuple(value):
##               if isinstance(value,tuple): value = list(value)  # must have a list
##               for x in value:
##                  if ((not isinstance(x,int)) or (x not in range(0,21))):
##                     raise ValueError, 'Index values must be in the range 0 to 20.'
##               self.__dict__[name]=value
##               setTfmember(self,name,value) # update the plot
##            else:
##               raise ValueError, 'The index attribute must be an integer, list, or tuple with values ranging from 0 to 20.'
##         elif (name == 'color'):
##            if (value == None):
##               self.__dict__['color']=None
##               setTfmember(self,'color',self.color) # update the plot
##            elif isinstance(value,int):
##               if value not in range(0,256): # must be an integer
##                  raise ValueError, 'The fillarea color value must be in the range 0 to 255.'
##               else:
##                  l=[]
##                  l.append(value)
##                  self.__dict__['color']=l
##                  setTfmember(self,'color',self.color) # update the plot
##            elif VCS_validation_functions.isListorTuple(value):
##               if isinstance(value,tuple): value = list(value)  # must have a list
##               for x in value:
##                  if ((not isinstance(x,int)) or (x not in range(0,256))):
##                     raise ValueError, 'Color values must be in the range 0 to 255.'
##               self.__dict__[name]=value
##               setTfmember(self,name,value) # update the plot
##            else:
##               raise ValueError, 'The color attribute must be an integer, list, or tuple with values ranging from 0 to 255.'
##         elif (name == 'priority'):
##            if (value == None):
##               self.__dict__['priority']=None
##               setTfmember(self,'priority',self.priority) # update the plot
##            elif isinstance(value,int):
##               self.__dict__['priority']=value
##               setTfmember(self,'priority',self.priority) # update the plot
##            else:
##               raise ValueError, 'The priority attribute value must be an integer.'
##         elif (name == 'viewport'):
##            if (value == None):
##              self.__dict__[name]= [0.0, 1.0, 0.0, 1.0]
##              setTfmember(self,name,[0.0, 1.0, 0.0, 1.0]) # update the plot
##            else:
##              if VCS_validation_functions.isListorTuple(value):
##               if isinstance(value,tuple): value = list(value)  # must have a list
##               if len(value) != 4:
##                  self.__dict__[name]= [0.0, 1.0, 0.0, 1.0]
##                  raise ValueError, 'Viewport must contain 4 integer or float values.'
##               else:
##                  self.__dict__[name]=value
##                  setTfmember(self,name,value) # update the plot
##              else:
##               raise ValueError, 'The viewport attribute must be a tuple or list of values.'
##         elif (name == 'worldcoordinate'):
##            if (value == None):
##              self.__dict__[name]= [0.0, 1.0, 0.0, 1.0]
##              setTfmember(self,name,[0.0, 1.0, 0.0, 1.0]) # update the plot
##            else:
##              if VCS_validation_functions.isListorTuple(value):
##               if isinstance(value,tuple): value = list(value)  # must have a list
##               if len(value) != 4:
##                  self.__dict__[name]= [0.0, 1.0, 0.0, 1.0]
##                  raise ValueError, 'World coordinates must contain 4 integer or float values.'
##               else:
##                  self.__dict__[name]=value
##                  setTfmember(self,name,value) # update the plot
##              else:
##               raise ValueError, 'The world coordinates attribute must be a tuple or list of values.'
##         elif (name == 'x'):
##            if (value == None):
##              self.__dict__[name] = None
##              setTfmember(self,name,value) # update the plot
##            else:
##              if isinstance(value,numpy.ndarray): value = value.tolist()
##              if VCS_validation_functions.isListorTuple(value):
##                 if isinstance(value,tuple): value = list(value)  # must have a list
##                 for i in range(len(value)):
##                     if isinstance(value[i],numpy.ndarray): value[i]=value[i].tolist()
##                     elif isinstance(value[i],tuple): value[i] = list( value[i] )
##                 self.__dict__[name]=value
##                 setTfmember(self,name,value) # update the plot
##              else:
##               raise ValueError, 'The x attribute must be a tuple or list of values.'
##         elif (name == 'y'):
##            if (value == None):
##              self.__dict__[name] = None
##              setTfmember(self,name,value) # update the plot
##            else:
##              if isinstance(value,numpy.ndarray): value = value.tolist()
##              if VCS_validation_functions.isListorTuple(value):
##                 if isinstance(value,tuple): value = list(value)  # must have a list
##                 for i in range(len(value)):
##                     if isinstance(value[i],numpy.ndarray): value[i]=value[i].tolist()
##                     elif isinstance(value[i],tuple): value[i] = list( value[i] )
##                 self.__dict__[name]=value
##                 setTfmember(self,name,value) # update the plot
##              else:
##               raise ValueError, 'The y attribute must be a tuple or list of values.'
##         elif (name == 'projection'):
##              value=VCS_validation_functions.checkProjection(self,'projection',value)
##              self.__dict__[name]=value
##              setTfmember(self,name,value) # update the plot

    #############################################################################
    #                                                                           #
    # Fillarea out line members (attributes).                                   #
    #                                                                           #
    #############################################################################
    def list(self):
        if (self.name == '__removed_from_VCS__'):
           raise ValueError, 'This instance has been removed from VCS.'
        print "","----------Fillarea (Tf) member (attribute) listings ----------"
        print 'Canvas Mode =',self.parent.mode
        print "secondary method =", self.s_name
        print "name =", self.name
        print "style =", self.style
        print "index =", self.index
        print "color =", self.color
        print "priority =", self.priority
        print "viewport =", self.viewport
        print "worldcoordinate =", self.worldcoordinate
        print "x =", self.x
        print "y =", self.y
        print "projection =", self.projection

    #############################################################################
    #                                                                           #
    # Script out secondary fillarea method in VCS to a file.                    #
    #                                                                           #
    #############################################################################
    def script(self, script_filename=None, mode=None):
        """
 Function:     script                           # Calls _vcs_legacy.scriptTf

 Description of Function:
       Saves out a fillarea graphics method in Python or VCS script form to a
       designated file.

 Example of Use:
    script(scriptfile_name)
              where: scriptfile_name is the output name of the script file.
                     mode is either "w" for replace or "a" for append.

              Note: If the the filename has a ".py" at the end, it will produce a
                    Python script. If the filename has a ".scr" at the end, it will
                    produce a VCS script. If neither extensions are give, then by
                    default a Python script will be produced.

    a=vcs_legacy.init()
    fa=a.createfillarea('temp')
    fa.script('filename.py')         # Append to a Python file "filename.py"
    fa.script('filename.scr')        # Append to a VCS file "filename.scr"
    fa.script('filename','w')        # Create or overwrite to a Python file "filename.py"
    """
        if (script_filename == None):
          raise ValueError, 'Error - Must provide an output script file name.'

        if (mode == None):
           mode = 'a'
        elif (mode not in ('w', 'a')):
          raise ValueError, 'Error - Mode can only be "w" for replace or "a" for append.'

        # By default, save file in python script mode
        scr_type = script_filename[len(script_filename)-4:len(script_filename)]
        if (scr_type == '.scr'):
           print _vcs_legacy.scriptTf(self.name,script_filename,mode)
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

           unique_name = '__Tf__' + self.name
           fp.write("#----------Fillarea (Tf) member (attribute) listings ----------\n")
           fp.write("tf_list=v.listelements('fillarea')\n")
           fp.write("if ('%s' in tf_list):\n" % self.name)
           fp.write("   %s = v.getfillarea('%s')\n" % (unique_name, self.name))
           fp.write("else:\n")
           fp.write("   %s = v.createfillarea('%s')\n" % (unique_name, self.name))
           fp.write("%s.style = %s\n" % (unique_name, self.style))
           fp.write("%s.index = %s\n" % (unique_name, self.index))
           fp.write("%s.color = %s\n\n" % (unique_name, self.color))
           fp.write("%s.priority = %d\n" % (unique_name, self.priority))
           fp.write("%s.viewport = %s\n" % (unique_name, self.viewport))
           fp.write("%s.worldcoordinate = %s\n" % (unique_name, self.worldcoordinate))
           fp.write("%s.x = %s\n" % (unique_name, self.x))
           fp.write("%s.y = %s\n\n" % (unique_name, self.y))
           fp.write("%s.projection = %s\n\n" % (unique_name, self.projection))


#################################################################################
#        END OF FILE								#
#################################################################################
