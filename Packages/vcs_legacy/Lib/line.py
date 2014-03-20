# Adapted for numpy/ma/cdms2 by convertcdms.py
"""
# Line (Tl) module
"""
###############################################################################
#                                                                             #
# Module:       line (Tl) module                                              #
#                                                                             #
# Copyright:    2000, Regents of the University of California                 #
#               This software may not be distributed to others without        #
#               permission of the author.                                     #
#                                                                             #
# Author:       PCMDI Software Team                                           #
#               Lawrence Livermore NationalLaboratory:                        #
#               support@pcmdi.llnl.gov                                        #
#                                                                             #
# Description:  Python command wrapper for VCS's line secondary object.       #
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

###############################################################################
#                                                                             #
# Function:	setTlmember                                                   #
#                                                                             #
# Description of Function:                                                    #
# 	Private function to update the VCS canvas plot. If the canvas mode is #
#       set to 0, then this function does nothing.                            #
#                                                                             #
#                                                                             #
# Example of Use:                                                             #
#      setTlmember(self,name,value)                                           #
#              where: self is the class (e.g., Tl)                            #
#                     name is the name of the member that is being changed    #
#                     value is the new value of the member (or attribute)     #
#                                                                             #
###############################################################################
def setTlmember(self,member,value):
     # If the VCS Canvas is displayed, then bring the canvas to the front before 
     # redisplaying the updated contents.
     if (self.parent.mode == 1) and (self.parent.iscanvasdisplayed()):
        Canvas.finish_queued_X_server_requests( self.parent )
        self.parent.canvas.BLOCK_X_SERVER()
        self.parent.canvasraised()

     _vcs_legacy.setTlmember(self, member, value, self.parent.mode)

     # If the VCS Canvas is displayed, then update the backing store
     if (self.parent.mode == 1) and (self.parent.iscanvasdisplayed()):
        self.parent.flush()
        self.parent.backing_store()
        self.parent.canvas.UNBLOCK_X_SERVER()
setmember = setTlmember
###############################################################################
#                                                                             #
# Function:     getTlmember                                                   #
#                                                                             #
# Description of Function:                                                    #
#       Private function that retrieves the line members from the C           #
#       structure and passes it back to Python.                               #
#                                                                             #
#                                                                             #
# Example of Use:                                                             #
#      return_value =                                                         #
#      getTlmember(self,name)                                                 #
#              where: self is the class (e.g., Tl)                            #
#                     name is the name of the member that is being found      #
#                                                                             #
###############################################################################
def getTlmember(self,member):
     return _vcs_legacy.getTlmember(self,member)
getmember=getTlmember

###############################################################################
#                                                                             #
# Function:     renameTl                                                      #
#                                                                             #
# Description of Function:                                                    #
#       Private function that renames the name of an existing line            #
#       graphics method.                                                      #
#                                                                             #
#                                                                             #
# Example of Use:                                                             #
#      renameTl(old_name, new_name)                                           #
#              where: old_name is the current name of line graphics method    #
#                     new_name is the new name for the line graphics method   #
#                                                                             #
###############################################################################
def renameTl(self, old_name, new_name):
     return _vcs_legacy.renameTl(old_name, new_name)

###############################################################################
#                                                                             #
# Line (Tl) Class.                                                            #
#                                                                             #
###############################################################################
class Tl(object):
    """
 Class:	Tl				# Line

 Description of Tl Class:
    The Line object allows the manipulation of line type, width, color index,
    view port, world coordinates, and (x,y) points. 

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

    ln.priority=1			# Set the graphics priority on the canvas
    ln.viewport=[0, 1.0, 0,1.0]		# FloatType [0,1]x[0,1]
    ln.worldcoordinate=[0,1.0,0,1.0]	# FloatType [#,#]x[#,#]

    ln.x=[[0,.1,.2], [.3,.4,.5]]	# List of FloatTypes
    ln.y=[[.5,.4,.3], [.2,.1,0]]	# List of FloatTypes
"""
    __slots__ = [
         'setmember',
         'parent',
         'name',
         's_name',
         'color',
         'priority',
         'type',
         'width',
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
         if isinstance(value,(str,int)):
              value=[value,]
         if value is not None:
              value = VCS_validation_functions.checkLinesList(self,'index',value)
         setmember(self,'type',value)
    type=property(_gettype,_settype)

    def _getwidth(self):
         return getmember(self,'width')
    def _setwidth(self,value):
         if isinstance(value,(int,float)):
              value=[value,]
         if value is not None:
              value = VCS_validation_functions.checkListOfNumbers(self,'width',value,minvalue=1,maxvalue=300)
         setmember(self,'width',value)
    width=property(_getwidth,_setwidth)

    
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
    # Initialize the line attributes.                                           #
    #                                                                           #
    #############################################################################
    def __init__(self, parent, Tl_name=None, Tl_name_src='default', createTl=0):
	#                                                         #
        ###########################################################
	# Initialize the line class and its members               #
        #							  #
	# The getTlmember function retrieves the values of the    #
        # line members in the C structure and passes back the     #
	# appropriate Python Object.                              #
        ###########################################################
	#                                                         #
        if (createTl == 0):
           if (Tl_name == None):
              raise ValueError, 'Must provide a line name.'
           else:
              _vcs_legacy.copyTl(Tl_name_src, Tl_name)
              self._name = Tl_name
        else:
              self._name=Tl_name_src
        self.s_name='Tl'
##         self.__dict__['type']=getTlmember(self, 'type')
##         self.__dict__['projection']=getTlmember(self, 'projection')
##         self.__dict__['width']=getTlmember(self, 'width')
##         self.__dict__['color']=getTlmember(self, 'color')
##         self.__dict__['priority']=getTlmember(self, 'priority')
##         self.__dict__['viewport']=getTlmember(self, 'viewport')
##         self.__dict__['worldcoordinate']=getTlmember(self, 'worldcoordinate')
##         self.__dict__['x']=getTlmember(self, 'x')
##         self.__dict__['y']=getTlmember(self, 'y')
        #                                                         #
        ###########################################################
        # Find and set the line structure in VCS C pointer        #
        # list. If the line name does not exist, then use         #
        # default line.                                           #
        ###########################################################
        #                                                         #
        self.parent = parent

##     #############################################################################
##     #                                                                           #
##     # Set line attributes.                                                      #
##     #                                                                           #
##     #############################################################################
##     def __setattr__(self, name, value):
##         if (self.name == '__removed_from_VCS__'):
##            raise ValueError, 'This instance has been removed from VCS.'
##         if (self.name == 'default'):
##            raise ValueError, 'You cannot modify the default line.'
##         if (name == 'name'):
##            if (type(value) == StringType):
##               renameTl(self,self.name, value)
##               self.__dict__['name']=value
##            else:
##               raise ValueError, 'The name attribute must be a string.'
##         elif (name == 'type'):
##            if (value == None):
##               self.__dict__['type']=None
##               setTlmember(self,'type',self.type) # update the plot
##            elif (value in ('solid', 'dash', 'dot', 'dash-dot', 'long-dash', 0, 1, 2, 3, 4)):
##               if value in ('solid', 0):
##                  value='solid'
##               elif value in ('dash', 1):
##                  value='dash'
##               elif value in ('dot', 2):
##                  value='dot'
##               elif value in ('dash-dot', 3):
##                  value='dash-dot'
##               elif value in ('long-dash', 4):
##                  value='long-dash'
##               l=[] 
##               l.append(value)
##               self.__dict__['type']=l
##               setTlmember(self,name,l) # update the plot
##            elif (type(value) in (ListType, TupleType)):
##               if (type(value) == TupleType): value = list(value)  # must have a list
##               nvalue = []
##               for x in value:
##                  if (x not in ('solid', 'dash', 'dot', 'dash-dot', 'long-dash', 0, 1, 2, 3, 4)):
##                     raise ValueError, 'The line value can either be ("solid", "dash", "dot", "dash-dot", "long-dash") or (0, 1, 2, 3, 4)'
##                  else:
##                     if x in ('solid', 0):
##                        nvalue.append('solid')
##                     elif x in ('dash', 1):
##                        nvalue.append('dash')
##                     elif x in ('dot', 2):
##                        nvalue.append('dot')
##                     elif x in ('dash-dot', 3):
##                        nvalue.append('dash-dot')
##                     elif x in ('long-dash', 4):
##                        nvalue.append('long-dash')
##               self.__dict__[name]=nvalue
##               setTlmember(self,name,nvalue) # update the plot
##            else:
##               raise ValueError, 'The line value can either be ("solid", "dash", "dot", "dash-dot", "long-dash") or (0, 1, 2, 3, 4)'
##         elif (name == 'width'):
##            if (value == None):
##               self.__dict__['width']=None
##               setTlmember(self,'width',self.width) # update the plot
##            elif (type(value) in (IntType,FloatType)):
##               if (value < 1) or (value > 300): # must be an integer or float
##                  raise ValueError, 'The width value must be in the range 1 to 300.'
##               else:
##                  l=[] 
##                  l.append(value)
##                  self.__dict__[name]=l
##                  setTlmember(self,name,self.width) # update the plot
##            elif (type(value) in (ListType, TupleType)):
##               if (type(value) == TupleType): value = list(value)  # must have a list
##               for x in value:
##                  if ((type(x) not in (IntType, FloatType)) or (x < 1) or (x > 300)):
##                    raise ValueError, 'The width value must be in the range 1 to 300.'
##               self.__dict__[name]=value
##               setTlmember(self,name,value) # update the plot
##            else:
##               raise ValueError, 'The indices attribute values must be integer and stored in either a list or a tuple.'
##         elif (name == 'color'):
##            if (value == None):
##               self.__dict__['color']=None
##               setTlmember(self,'color',self.color) # update the plot
##            elif (isinstance(value, IntType)):
##               if value not in range(0,256): # must be an integer
##                  raise ValueError, 'The line color value must be in the range 0 to 255.'
##               else:
##                  l=[] 
##                  l.append(value)
##                  self.__dict__['color']=l
##                  setTlmember(self,'color',self.color) # update the plot
##            elif (type(value) in (ListType, TupleType)):
##               if (type(value) == TupleType): value = list(value)  # must have a list
##               for x in value:
##                  if ((type(x) != IntType) or (x not in range(0,256))):
##                     raise ValueError, 'The line color values must be in the range 0 to 255.'
##               self.__dict__[name]=value
##               setTlmember(self,name,value) # update the plot
##            else:
##               raise ValueError, 'The color attribute value must be an integer in the range 0 to 255.'
##         elif (name == 'priority'):
##            if (value == None):
##               self.__dict__['priority']=None
##               setTlmember(self,'priority',self.priority) # update the plot
##            elif (isinstance(value, IntType)):
##               self.__dict__['priority']=value
##               setTlmember(self,'priority',self.priority) # update the plot
##            else:
##               raise ValueError, 'The priority attribute value must be an integer.'
##         elif (name == 'viewport'):
##            if (value == None):
##              self.__dict__[name]= [0.0, 1.0, 0.0, 1.0]
##              setTlmember(self,name,[0.0, 1.0, 0.0, 1.0]) # update the plot
##            else:
##              if (type(value) in (ListType, TupleType)):
##               value = list(value)  # make sure that values list is a list
##               if len(value) != 4:
##                  self.__dict__[name]= [0.0, 1.0, 0.0, 1.0]
##                  raise ValueError, 'Viewport must contain 4 integer or float values.'
##               else:
##                  self.__dict__[name]=value
##                  setTlmember(self,name,value) # update the plot
##              else:
##               raise ValueError, 'The viewport attribute must be a tuple or list of values.'
##         elif (name == 'worldcoordinate'):
##            if (value == None):
##              self.__dict__[name]= [0.0, 1.0, 0.0, 1.0]
##              setTlmember(self,name,[0.0, 1.0, 0.0, 1.0]) # update the plot
##            else:
##              if (type(value) in (ListType, TupleType)):
##               value = list(value)  # make sure that values list is a list
##               if len(value) != 4:
##                  self.__dict__[name]= [0.0, 1.0, 0.0, 1.0]
##                  raise ValueError, 'World coordinates must contain 4 integer or float values.'
##               else:
##                  self.__dict__[name]=value
##                  setTlmember(self,name,value) # update the plot
##              else:
##               raise ValueError, 'The world coordinates attribute must be a tuple or list of values.'
##         elif (name == 'x'):
##            if (value == None) or (value==[]):
##              self.__dict__[name] = None
##              setTlmember(self,name,None) # update the plot
##            else:
##              if isinstance(value, numpy.ndarray): value = value.tolist()
##              if (type(value) in (ListType, TupleType)):
##                 value = list(value)  # make sure that values list is a list
##                 for i in range(len(value)):
##                     if isinstance(value[i], numpy.ndarray): value[i]=value[i].tolist()
##                     elif type(value[i]) is TupleType: value[i] = list( value[i] )
##                 self.__dict__[name]=value
##                 setTlmember(self,name,value) # update the plot
##              else:
##               raise ValueError, 'The x attribute must be a tuple or list of values.'
##         elif (name == 'y'):
##            if (value == None) or (value==None):
##              self.__dict__[name] = None
##              setTlmember(self,name,None) # update the plot
##            else:
##              if isinstance(value, numpy.ndarray): value = value.tolist()
##              if (type(value) in (ListType, TupleType)):
##                 value = list(value)  # make sure that values list is a list
##                 for i in range(len(value)):
##                     if isinstance(value[i], numpy.ndarray): value[i]=value[i].tolist()
##                     elif type(value[i]) is TupleType: value[i] = list( value[i] )
##                 self.__dict__[name]=value
##                 setTlmember(self,name,value) # update the plot
##              else:
##               raise ValueError, 'The y attribute must be a tuple or list of values.'
##         elif (name == 'projection'):
##              value=VCS_validation_functions.checkProjection(self,'projection',value)
##              self.__dict__[name]=value
##              setTlmember(self,name,value) # update the plot

    #############################################################################
    #                                                                           #
    # List out line members (attributes).                                       #
    #                                                                           #
    #############################################################################
    def list(self):
        if (self.name == '__removed_from_VCS__'):
           raise ValueError, 'This instance has been removed from VCS.'
        print "","----------Line (Tl) member (attribute) listings ----------"
        print 'Canvas Mode =',self.parent.mode
        print "secondary method =", self.s_name
        print "name =", self.name
        print "type =", self.type
        print "width =", self.width
        print "color =", self.color
        print "priority =", self.priority
        print "viewport =", self.viewport
        print "worldcoordinate =", self.worldcoordinate
        print "x =", self.x
        print "y =", self.y
        print "projection =",self.projection

    #############################################################################
    #                                                                           #
    # Script out secondary line method in VCS to a file.                        #
    #                                                                           #
    #############################################################################
    def script(self, script_filename=None, mode=None):
        """
 Function:     script                           # Calls _vcs_legacy.scriptTl

 Description of Function:
       Saves out a line graphics method in VCS or Python script form to a
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
    ln=a.createboxfill('temp')
    ln.script('filename.py')         # Append to a Python file "filename.py"
    ln.script('filename.scr')        # Append to a VCS file "filename.scr"
    ln.script('filename','w')        # Create or overwrite to a Python file "filename.py"
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
           print _vcs_legacy.scriptTl(self.name,script_filename,mode)
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

           unique_name = '__Tl__' + self.name
           fp.write("#----------Line (Tl) member (attribute) listings ----------\n")
           fp.write("tl_list=v.listelements('line')\n")
           fp.write("if ('%s' in tl_list):\n" % self.name)
           fp.write("   %s = v.getline('%s')\n" % (unique_name, self.name))
           fp.write("else:\n")
           fp.write("   %s = v.createline('%s')\n" % (unique_name, self.name))
           fp.write("%s.type = %s\n" % (unique_name, self.type))
           fp.write("%s.width = %s\n" % (unique_name, self.width))
           fp.write("%s.color = %s\n" % (unique_name, self.color))
           fp.write("%s.priority = %d\n" % (unique_name, self.priority))
           fp.write("%s.viewport = %s\n" % (unique_name, self.viewport))
           fp.write("%s.worldcoordinate = %s\n" % (unique_name, self.worldcoordinate))
           fp.write("%s.x = %s\n" % (unique_name, self.x))
           fp.write("%s.y = %s\n\n" % (unique_name, self.y))
           fp.write("%s.projection = %s\n\n" % (unique_name, self.projection))


#################################################################################
#        END OF FILE								#
#################################################################################
