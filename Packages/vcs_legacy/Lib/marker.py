# Adapted for numpy/ma/cdms2 by convertcdms.py
"""
# Marker (Tm) module
"""
###############################################################################
#                                                                             #
# Module:       marker (Tm) module                                            #
#                                                                             #
# Copyright:    2000, Regents of the University of California                 #
#               This software may not be distributed to others without        #
#               permission of the author.                                     #
#                                                                             #
# Author:       PCMDI Software Team                                           #
#               Lawrence Livermore NationalLaboratory:                        #
#               support@pcmdi.llnl.gov                                        #
#                                                                             #
# Description:  Python command wrapper for VCS's marker secondary object.     #
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
import VCS_validation_functions
from types import *

###############################################################################
#                                                                             #
# Function:	setTmmember                                                   #
#                                                                             #
# Description of Function:                                                    #
# 	Private function to update the VCS canvas plot. If the canvas mode is #
#       set to 0, then this function does nothing.                            #
#                                                                             #
#                                                                             #
# Example of Use:                                                             #
#      setTmmember(self,name,value)					      #
#              where: self is the class (e.g., Tm)                            #
#                     name is the name of the member that is being changed    #
#                     value is the new value of the member (or attribute)     #
#                                                                             #
###############################################################################
def setTmmember(self,member,value):
     # If the VCS Canvas is displayed, then bring the canvas to the front before 
     # redisplaying the updated contents.
     if (self.parent.mode == 1) and (self.parent.iscanvasdisplayed()):
        Canvas.finish_queued_X_server_requests( self.parent )
        self.parent.canvas.BLOCK_X_SERVER()
        self.parent.canvasraised()

     _vcs_legacy.setTmmember(self, member, value, self.parent.mode)

     # If the VCS Canvas is displayed, then update the backing store
     if (self.parent.mode == 1) and (self.parent.iscanvasdisplayed()):
        self.parent.flush()
        self.parent.backing_store()
        self.parent.canvas.UNBLOCK_X_SERVER()
setmember = setTmmember

###############################################################################
#                                                                             #
# Function:     getTmmember                                                   #
#                                                                             #
# Description of Function:                                                    #
#       Private function that retrieves the marker members from the C         #
#       structure and passes it back to Python.                               #
#                                                                             #
#                                                                             #
# Example of Use:                                                             #
#      return_value =							      #
#      getTmmember(self,name)                                                 #
#              where: self is the class (e.g., Tm)                            #
#                     name is the name of the member that is being found      #
#                                                                             #
###############################################################################
def getTmmember(self,member):
     return _vcs_legacy.getTmmember(self,member)
getmember = getTmmember

###############################################################################
#                                                                             #
# Function:     renameTm                                                      #
#                                                                             #
# Description of Function:                                                    #
#       Private function that renames the name of an existing marker          #
#       graphics method.                                                      #
#                                                                             #
#                                                                             #
# Example of Use:                                                             #
#      renameTm(old_name, new_name)                                           #
#              where: old_name is the current name of marker graphics method  #
#                     new_name is the new name for the marker graphics method #
#                                                                             #
###############################################################################
def renameTm(self, old_name, new_name):
     return _vcs_legacy.renameTm(old_name, new_name)

#############################################################################
#                                                                           #
# Marker (Tm) Class.                                                        #
#                                                                           #
#############################################################################
class Tm(object):
    """
 Class:	Tm				# Marker

 Description of Tm Class:
    The Marker object allows the manipulation of marker type, size, and color index. 

    This class is used to define an marker table entry used in VCS, or it
    can be used to change some or all of the marker attributes in an
    existing marker table entry.

 Other Useful Functions:
 	     a=vcs_legacy.init()		# Constructor
	     a.show('marker')		# Show predefined marker objects
             a.update()               	# Updates the VCS Canvas at user's request
             a.mode=1, or 0           	# If 1, then automatic update, else if
                                          0, then use update function to
                                          update the VCS Canvas.

 Example of Use:
    a=vcs_legacy.init()
    To Create a new instance of marker use:
     mk=a.createmarker('new','red') 	# Copies content of 'red' to 'new'
     mk=a.createmarker('new') 		# Copies content of 'default' to 'new'

    To Modify an existing marker use:
     mk=a.getmarker('red')

    mk.list()  				# Will list all the marker attribute values
    mk.color=100			# Range from 1 to 256
    mk.size=100				# Range from 1 to 300

    Specify the marker type:
     mk.type='dot'          		# Same as mk.type=1
     mk.type='plus'          		# Same as mk.type=2
     mk.type='star'          		# Same as mk.type=3
     mk.type='circle'          		# Same as mk.type=4
     mk.type='cross'          		# Same as mk.type=5
     mk.type='diamond'          	# Same as mk.type=6
     mk.type='triangle_up'          	# Same as mk.type=7
     mk.type='triangle_down'          	# Same as mk.type=8
     mk.type='triangle_left'          	# Same as mk.type=9
     mk.type='triangle_right'          	# Same as mk.type=10
     mk.type='square'          		# Same as mk.type=11
     mk.type='diamond_fill'          	# Same as mk.type=12
     mk.type='triangle_up_fill'         # Same as mk.type=13
     mk.type='triangle_down_fill'       # Same as mk.type=14
     mk.type='triangle_left_fill'       # Same as mk.type=15
     mk.type='triangle_right_fill'      # Same as mk.type=16
     mk.type='square_fill'          	# Same as mk.type=17

     mk.priority=1			# Set the graphics priority on the canvas
     mk.viewport=[0, 1.0, 0,1.0]        # FloatType [0,1]x[0,1]
     mk.worldcoordinate=[0,1.0,0,1.0]   # FloatType [#,#]x[#,#]

     mk.x=[[0,.1,.2], [.3,.4,.5]]       # List of FloatTypes
     mk.y=[[.5,.4,.3], [.2,.1,0]]       # List of FloatTypes
"""
    __slots__ = [
         'setmember',
         'parent',
         'name',
         's_name',
         'color',
         'priority',
         'type',
         'size',
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

    def _gettype(self):
         return getmember(self,'type')
    def _settype(self,value):
         if not isinstance(value,(list,tuple)) and value is not None:
              value=[value,]
         if value is not None:
              value = VCS_validation_functions.checkMarkersList(self,'type',value)
         setmember(self,'type',value)
    type = property(_gettype,_settype)

    def _getsize(self):
         return getmember(self,'size')
    def _setsize(self,value):
         if isinstance(value,int):
              value=[value,]
         if value is not None:
              value = VCS_validation_functions.checkListOfNumbers(self,'size',value,minvalue=1,maxvalue=300)
         setmember(self,'size',value)
    size=property(_getsize,_setsize)
    
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
    # Initialize the marker attributes.                                         #
    #                                                                           #
    #############################################################################
    def __init__(self, parent, Tm_name=None, Tm_name_src='default', createTm=0):
	#                                                         #
        ###########################################################
	# Initialize the marker class and its members             #
        #							  #
	# The getTmmember function retrieves the values of the    #
        # marker members in the C structure and passes back the   #
	# appropriate Python Object.                              #
        ###########################################################
	#                                                         #
        if (createTm == 0):
           if (Tm_name == None):
              raise ValueError, 'Must provide a marker name.'
           else:
              _vcs_legacy.copyTm(Tm_name_src, Tm_name)
              self._name = Tm_name
        else:
              self._name = Tm_name_src
        self.s_name = 'Tm'
##         self.__dict__['type']=getTmmember(self, 'type')
##         self.__dict__['size']=getTmmember(self, 'size')
##         self.__dict__['color']=getTmmember(self, 'color')
##         self.__dict__['priority']=getTmmember(self, 'priority')
##         self.__dict__['viewport']=getTmmember(self, 'viewport')
##         self.__dict__['worldcoordinate']=getTmmember(self, 'worldcoordinate')
##         self.__dict__['x']=getTmmember(self, 'x')
##         self.__dict__['y']=getTmmember(self, 'y')
##         self.__dict__['projection']=getTmmember(self, 'projection')
        #                                                         #
        ###########################################################
        # Find and set the marker structure in VCS C pointer      #
        # list. If the marker name does not exist, then use       #
        # default marker.                                         #
        ###########################################################
        #                                                         #
        self.parent = parent

##     #############################################################################
##     #                                                                           #
##     # Set marker attributes.                                                    #
##     #                                                                           #
##     #############################################################################
##     def __setattr__(self, name, value):
##         if (self.name == '__removed_from_VCS__'):
##            raise ValueError, 'This instance has been removed from VCS.'
##         if (self.name == 'default'):
##            raise ValueError, 'You cannot modify the default marker.'
##         if (name == 'name'):
##            if (type(value) == StringType):
##               renameTm(self,self.name, value)
##               self.__dict__['name']=value
##            else:
##               raise ValueError, 'The name attribute must be a string.'
##         elif (name == 'type'):
##            if (value == None):
##               self.__dict__['type']=None
##               setTmmember(self,'type',self.type) # update the plot
##            elif (value in (None, 'dot', 'plus', 'star', 'circle', 'cross', 'diamond', 'triangle_up', 'triangle_down', 'triangle_left', 'triangle_right', 'square', 'diamond_fill', 'triangle_up_fill', 'triangle_down_fill', 'triangle_left_fill', 'triangle_right_fill', 'square_fill', 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14 ,15, 16, 17)):
##               if value in (None, 0):
##                  value=None
##               elif value in ('dot', 1):
##                  value='dot'
##               elif value in ('plus', 2):
##                  value='plus'
##               elif value in ('star', 3):
##                  value='star'
##               elif value in ('circlet', 4):
##                  value='circle'
##               elif value in ('cross', 5):
##                  value='cross'
##               elif value in ('diamond', 6):
##                  value='diamond'
##               elif value in ('triangle_up', 7):
##                  value='triangle_up'
##               elif value in ('triangle_down', 8):
##                  value='triangle_down'
##               elif value in ('triangle_left', 9):
##                  value='triangle_left'
##               elif value in ('triangle_right', 10):
##                  value='triangle_right'
##               elif value in ('square', 11):
##                  value='square'
##               elif value in ('diamond_fill', 12):
##                  value='diamond_fill'
##               elif value in ('triangle_up_fill', 13):
##                  value='triangle_up_fill'
##               elif value in ('triangle_down_fill', 14):
##                  value='triangle_down_fill'
##               elif value in ('triangle_left_fill', 15):
##                  value='triangle_left_fill'
##               elif value in ('triangle_right_fill', 16):
##                  value='triangle_right_fill'
##               elif value in ('square_fill', 17):
##                  value='square_fill'
##               l=[] 
##               l.append(value)
##               self.__dict__['type']=l
##               setTmmember(self,'type',self.type) # update the plot
##            elif (type(value) in (ListType, TupleType)):
##               if (type(value) == TupleType): value = list(value)  # must have a list
##               nvalue = []
##               for x in value:
##                  if ((x not in (None, 'dot', 'plus', 'star', 'circle', 'cross', 'diamond', 'triangle_up', 'triangle_down', 'triangle_left', 'triangle_right', 'square', 'diamond_fill', 'triangle_up_fill', 'triangle_down_fill', 'triangle_left_fill', 'triangle_right_fill', 'square_fill', 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14 ,15, 16, 17))):
##                     raise ValueError, 'The line value can either be (None, "dot", "plus", "star", "circle", "cross", "diamond", "triangle_up", "triangle_down", "triangle_left", "triangle_right", "square", "diamond_fill", "triangle_up_fill", "triangle_down_fill", "triangle_left_fill", "triangle_right_fill", "square_fill") or (0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17)'
##                  else:
##                     if x in (None, 0):
##                        nvalue.append(None)
##                     elif x in ('dot', 1):
##                        nvalue.append('dot')
##                     elif x in ('plus', 2):
##                        nvalue.append('plus')
##                     elif x in ('star', 3):
##                        nvalue.append('star')
##                     elif x in ('circlet', 4):
##                        nvalue.append('circle')
##                     elif x in ('cross', 5):
##                        nvalue.append('cross')
##                     elif x in ('diamond', 6):
##                        nvalue.append('diamond')
##                     elif x in ('triangle_up', 7):
##                        nvalue.append('triangle_up')
##                     elif x in ('triangle_down', 8):
##                        nvalue.append('triangle_down')
##                     elif x in ('triangle_left', 9):
##                        nvalue.append('triangle_left')
##                     elif x in ('triangle_right', 10):
##                        nvalue.append('triangle_right')
##                     elif x in ('square', 11):
##                        nvalue.append('square')
##                     elif x in ('diamond_fill', 12):
##                        nvalue.append('diamond_fill')
##                     elif x in ('triangle_up_fill', 13):
##                        nvalue.append('triangle_up_fill')
##                     elif x in ('triangle_down_fill', 14):
##                        nvalue.append('triangle_down_fill')
##                     elif x in ('triangle_left_fill', 15):
##                        nvalue.append('triangle_left_fill')
##                     elif x in ('triangle_right_fill', 16):
##                        nvalue.append('triangle_right_fill')
##                     elif x in ('square_fill', 17):
##                        nvalue.append('square_fill')
##               self.__dict__['type']=nvalue
##               setTmmember(self,'type',nvalue) # update the plot
##            else:
##               raise ValueError, 'The line value can either be (None, "dot", "plus", "star", "circle", "cross", "diamond", "triangle_up", "triangle_down", "triangle_left", "triangle_right", "square", "diamond_fill", "triangle_up_fill", "triangle_down_fill", "triangle_left_fill", "triangle_right_fill", "square_fill") or (0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17)'
##         elif (name == 'size'):
##            if (value == None):
##               self.__dict__['size']=None
##               setTmmember(self,'size',self.size) # update the plot
##            elif (type(value) in (IntType,FloatType)):
##               if (value < 1) or (value > 300): # must be an integer or float
##                  raise ValueError, 'The size value must be in the range 1 to 300.'
##               else:
##                  l = []
##                  l.append( value )
##                  self.__dict__[name]=l
##                  setTmmember(self,'size',self.size) # update the plot
##            elif (type(value) in (ListType, TupleType)):
##               if (type(value) == TupleType): value = list(value)  # must have a list
##               for x in value:
##                  if ((type(x) not in (IntType, FloatType)) or (x < 1) or (x > 300)): # must be an integer or float
##                     raise ValueError, 'The size value must be in the range 1 to 300.'
##               self.__dict__[name]=value
##               setTmmember(self,name,value) # update the plot
##            else:
##               raise ValueError, 'The indices attribute values must be integer and stored in either a list or a tuple.'
##         elif (name == 'color'):
##            if (value == None):
##               self.__dict__['color']=None
##               setTmmember(self,'color',self.color) # update the plot
##            elif (isinstance(value, IntType)):
##               if value not in range(0,256): # must be an integer
##                  raise ValueError, 'The marker color value must be in the range 0 to 255.'
##               else:
##                  l=[] 
##                  l.append(value)
##                  self.__dict__['color']=l
##                  setTmmember(self,'color',self.color) # update the plot
##            elif (type(value) in (ListType, TupleType)):
##               if (type(value) == TupleType): value = list(value)  # must have a list
##               for x in value:
##                  if ((type(x) != IntType) or (x not in range(0,256))):#must be an integer
##                     raise ValueError, 'The marker color value must be in the range 0 to 255. '
##               self.__dict__[name]=value
##               setTmmember(self,name,value) # update the plot
##            else:
##               raise ValueError, 'The color attribute value must be an integer in the range 0 to 255.'
##         elif (name == 'priority'):
##            if (value == None):
##               self.__dict__['priority']=None
##               setTmmember(self,'priority',self.priority) # update the plot
##            elif (isinstance(value, IntType)):
##               self.__dict__['priority']=value
##               setTmmember(self,'priority',self.priority) # update the plot
##            else:
##               raise ValueError, 'The priority attribute value must be an integer.'
##         elif (name == 'viewport'):
##            if (value == None):
##              self.__dict__[name]= [0.0, 1.0, 0.0, 1.0]
##              setTmmember(self,name,[0.0, 1.0, 0.0, 1.0]) # update the plot
##            else:
##              if (type(value) in (ListType, TupleType)):
##               value = list(value)  # make sure that values list is a list
##               if len(value) != 4:
##                  self.__dict__[name]= [0.0, 1.0, 0.0, 1.0]
##                  raise ValueError, 'Viewport must contain 4 integer or float values.'
##               else:
##                  self.__dict__[name]=value
##                  setTmmember(self,name,value) # update the plot
##              else:
##               raise ValueError, 'The viewport attribute must be a tuple or list of values.'
##         elif (name == 'worldcoordinate'):
##            if (value == None):
##              self.__dict__[name]= [0.0, 1.0, 0.0, 1.0]
##              setTmmember(self,name,[0.0, 1.0, 0.0, 1.0]) # update the plot
##            else:
##              if (type(value) in (ListType, TupleType)):
##               value = list(value)  # make sure that values list is a list
##               if len(value) != 4:
##                  self.__dict__[name]= [0.0, 1.0, 0.0, 1.0]
##                  raise ValueError, 'World coordinates must contain 4 integer or float values.'
##               else:
##                  self.__dict__[name]=value
##                  setTmmember(self,name,value) # update the plot
##              else:
##               raise ValueError, 'The world coordinates attribute must be a tuple or list of values.'
##         elif (name == 'x'):
##            if (value == None):
##              self.__dict__[name] = None
##              setTmmember(self,name,value) # update the plot
##            else:
##              if isinstance(value, numpy.ndarray): value = value.tolist()
##              if (type(value) in (ListType, TupleType)):
##                 value = list(value)  # make sure that values list is a list
##                 for i in range(len(value)):
##                     if isinstance(value[i], numpy.ndarray): value[i]=value[i].tolist()
##                     elif type(value[i]) is TupleType: value[i] = list( value[i] )
##                 self.__dict__[name]=value
##                 setTmmember(self,name,value) # update the plot
##              else:
##               raise ValueError, 'The x attribute must be a tuple or list of values.'
##         elif (name == 'y'):
##            if (value == None):
##              self.__dict__[name] = None
##              setTmmember(self,name,value) # update the plot
##            else:
##              if isinstance(value, numpy.ndarray): value = value.tolist()
##              if (type(value) in (ListType, TupleType)):
##                 value = list(value)  # make sure that values list is a list
##                 for i in range(len(value)):
##                     if isinstance(value[i], numpy.ndarray): value[i]=value[i].tolist()
##                     elif type(value[i]) is TupleType: value[i] = list( value[i] )
##                 self.__dict__[name]=value
##                 setTmmember(self,name,value) # update the plot
##              else:
##               raise ValueError, 'The y attribute must be a tuple or list of values.'
##         elif (name == 'projection'):
##              value=VCS_validation_functions.checkProjection(self,'projection',value)
##              self.__dict__[name]=value
##              setTmmember(self,name,value) # update the plot

    #############################################################################
    #                                                                           #
    # List out marker members (attributes).                                     #
    #                                                                           #
    #############################################################################
    def list(self):
        if (self.name == '__removed_from_VCS__'):
           raise ValueError, 'This instance has been removed from VCS.'
        print "","----------Marker (Tm) member (attribute) listings ----------"
        print 'Canvas Mode =',self.parent.mode
        print "secondary method =", self.s_name
        print "name =", self.name
        print "type =", self.type
        print "size =", self.size
        print "color =", self.color
        print "priority =", self.priority
        print "viewport =", self.viewport
        print "worldcoordinate =", self.worldcoordinate
        print "x =", self.x
        print "y =", self.y
        print "projection =", self.projection

    #############################################################################
    #                                                                           #
    # Script out secondary marker method in VCS to a file.                      #
    #                                                                           #
    #############################################################################
    def script(self, script_filename=None, mode=None):
        """
 Function:     script                           # Calls _vcs_legacy.scriptTm

 Description of Function:
       Saves out a marker graphics method in Python or VCS script form to a
       designated file.

 Example of Use:
    script(scriptfile_name, mode)
              where: script_name is the output name of the script file.
                     mode is either "w" for replace or "a" for append.

              Note: If the the filename has a ".py" at the end, it will produce a
                    Python script. If the filename has a ".scr" at the end, it will
                    produce a VCS script. If neither extensions are give, then by
                    default a Python script will be produced.

    a=vcs_legacy.init()
    mrk=a.createboxfill('temp')
    mrk.script('filename.py')         # Append to a Python file "filename.py"
    mrk.script('filename.scr')        # Append to a VCS file "filename.scr"
    mrk.script('filename','w')        # Create or overwrite to a Python file "filename.py"
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
           print _vcs_legacy.scriptTm(self.name,script_filename,mode)
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

           unique_name = '__Tm__' + self.name
           fp.write("#----------Marker (Tm) member (attribute) listings ----------\n")
           fp.write("tm_list=v.listelements('marker')\n")
           fp.write("if ('%s' in tm_list):\n" % self.name)
           fp.write("   %s = v.getmarker('%s')\n" % (unique_name, self.name))
           fp.write("else:\n")
           fp.write("   %s = v.createmarker('%s')\n" % (unique_name, self.name))
           fp.write("%s.type = %s\n" % (unique_name, self.type))
           fp.write("%s.size = %s\n" % (unique_name, self.size))
           fp.write("%s.color = %s\n\n" % (unique_name, self.color))
           fp.write("%s.priority = %d\n" % (unique_name, self.priority))
           fp.write("%s.viewport = %s\n" % (unique_name, self.viewport))
           fp.write("%s.worldcoordinate = %s\n" % (unique_name, self.worldcoordinate))
           fp.write("%s.x = %s\n" % (unique_name, self.x))
           fp.write("%s.y = %s\n" % (unique_name, self.y))
           fp.write("%s.projection = %s\n" % (unique_name, self.projection))


#################################################################################
#        END OF FILE								#
#################################################################################
