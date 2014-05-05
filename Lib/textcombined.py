# Adapted for numpy/ma/cdms2 by convertcdms.py
"""
# Text Combined (Tc) module
"""
###############################################################################
#                                                                             #
# Module:       textcombined (Tc) module                                      #
#                                                                             #
# Copyright:    2000, Regents of the University of California                 #
#               This software may not be distributed to others without        #
#               permission of the author.                                     #
#                                                                             #
# Author:       PCMDI Software Team                                           #
#               Lawrence Livermore NationalLaboratory:                        #
#               support@pcmdi.llnl.gov                                        #
#                                                                             #
# Description:  Python command wrapper for VCS's text combined secondary      #
#               object. Can be thought of as the combination of text table    #
#               and text orientation secondary objects.			      #
#                                                                             #
# Version:      4.0                                                           #
#                                                                             #
###############################################################################
#
#
#
###############################################################################
#                                                                             #
# Import: Text table (Tt), and  Text orientation  (To)                        #
#                                                                             #
###############################################################################
import texttable
import textorientation
from types import *
import VCS_validation_functions

#############################################################################
#                                                                           #
# Text Combined (Tc) Class.                                                 #
#                                                                           #
#############################################################################
class Tc(object):
    """
 Class: Tc                              # Text Combined

 Description of Tc Class:
    The (Tc) Text Combined class will combine a text table class and a text orientation
    class together. From combining the two classess, the user will be able to set 
    attributes for both classes (i.e., define the font, spacing, expansion, color
    index, height, angle, path, vertical alignment, and horizontal alignment).

    This class is used to define and list a combined text table and text orientation
    entry used in VCS.

 Other Useful Functions:
	     a=vcs.init()               # Constructor
             a.show('texttable')        # Show predefined text table objects
             a.show('textorientation')  # Show predefined text orientation objects
             a.update()                 # Updates the VCS Canvas at user's request
             a.mode=1, or 0             # If 1, then automatic update, else if
                                          0, then use update function to
                                          update the VCS Canvas.
 Example of Use:
    a=vcs.init()
    To Create a new instance of text table use:
     tc=a.createtextcombined('new_tt','std','new_to','7left')  # Copies content of 
                                        # 'std' to 'new_tt' and '7left' to 'new_to'

    To Modify an existing texttable use:
     tc=a.gettextcombined('std','7left')

    tc.list()                           # Will list all the textcombined attribute values
					# (i.e., texttable and textorientation attributes
    Specify the text font type:
     tc.font=1                          # The font value must be in the range 1 to 9

    Specify the text spacing:
     tc.spacing=2                       # The spacing value must be in the range -50 to 50

    Specify the text expansion:
     tc.expansion=100                   # The expansion value ranges from 50 to 150

    Specify the text color:
     tc.color=241                       # The text color value ranges from 1 to 257
 
    Specify the graphics text priority on the VCS Canvas:
     tt.priority = 1

    Specify the viewport and world coordinate:
     tt.viewport=[0, 1.0, 0,1.0]        # FloatType [0,1]x[0,1]
     tt.worldcoordinate=[0,1.0,0,1.0]   # FloatType [#,#]x[#,#]

    Specify the location of the text:
     tt.x=[[0,.1,.2], [.3,.4,.5]]        # List of FloatTypes
     tt.y=[[.5,.4,.3], [.2,.1,0]]        # List of FloatTypes

    Specify the text height:
     tc.height=20                       # The height value must be an integer

    Specify the text angle:
     tc.angle=0                         # The angle value ranges from 0 to 360

    Specify the text path:
     tc.path='right'                    # Same as tc.path=0
     tc.path='left'                     # Same as tc.path=1
     tc.path='up'                       # Same as tc.path=2
     tc.path='down'                     # Same as tc.path=3

    Specify the text horizontal alignment:
     tc.halign='right'                  # Same as tc.halign=0
     tc.halign='center'                 # Same as tc.halign=1
     tc.halign='right'                  # Same as tc.halign=2

    Specify the text vertical alignment:
     tc.valign='tcp'                    # Same as tcvalign=0
     tc.valign='cap'                    # Same as tcvalign=1
     tc.valign='half'                   # Same as tcvalign=2
     tc.valign='base'                   # Same as tcvalign=3
     tc.valign='bottom'                 # Same as tcvalign=4
"""

    __slots__=[
        's_name',
        'name',
        'Tt_name',
        'To_name',
        'To',
        'Tt',
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
        'height',
        'angle',
        'path',
        'halign',
        'valign',
        'parent',
        ]
    
    def _getTtname(self):
        return self.Tt.name
    def _setTtname(self,value):
        self.Tt.name = value
    Tt_name = property(_getTtname,_setTtname)
    def _getToname(self):
        return self.To.name
    def _setToname(self,value):
        self.To.name = value
    To_name = property(_getToname,_setToname)

    def _getcolor(self):
        return self.Tt.color
    def _setcolor(self,value):
        self.Tt.color=value
    color = property(_getcolor,_setcolor)

    def _getfcolor(self):
        return self.Tt.fillincolor
    def _setfcolor(self,value):
        self.Tt.fillincolor=value
    fillincolor = property(_getfcolor,_setfcolor)
    
    def _getpriority(self):
        return self.Tt.priority
    def _setpriority(self,value):
        self.Tt.priority=value
    priority = property(_getpriority,_setpriority)
    
    def _getfont(self):
        return self.Tt.font
    def _setfont(self,value):
        self.Tt.font=value
    font = property(_getfont,_setfont)
    
    def _getstring(self):
        return self.Tt.string
    def _setstring(self,value):
        self.Tt.string=value
    string = property(_getstring,_setstring)
    
    def _getspacing(self):
        return self.Tt.spacing
    def _setspacing(self,value):
        self.Tt.spacing=value
    spacing = property(_getspacing,_setspacing)
    
    def _getexpansion(self):
        return self.Tt.expansion
    def _setexpansion(self,value):
        self.Tt.expansion=value
    expansion = property(_getexpansion,_setexpansion)
    
    def _getx(self):
        return self.Tt.x
    def _setx(self,value):
        self.Tt.x=value
    x = property(_getx,_setx)
    
    def _gety(self):
        return self.Tt.y
    def _sety(self,value):
        self.Tt.y=value
    y = property(_gety,_sety)
    
    def _getviewport(self):
        return self.Tt.viewport
    def _setviewport(self,value):
        self.Tt.viewport=value
    viewport = property(_getviewport,_setviewport)
    
    def _getworldcoordinate(self):
        return self.Tt.worldcoordinate
    def _setworldcoordinate(self,value):
        self.Tt.worldcoordinate=value
    worldcoordinate = property(_getworldcoordinate,_setworldcoordinate)
    
    def _getprojection(self):
        return self.Tt.projection
    def _setprojection(self,value):
        self.Tt.projection=value
    projection = property(_getprojection,_setprojection)
    
    def _getheight(self):
        return self.To.height
    def _setheight(self,value):
        self.To.height=value
    height = property(_getheight,_setheight)
    
    def _getangle(self):
        return self.To.angle
    def _setangle(self,value):
        self.To.angle=value
    angle = property(_getangle,_setangle)
    
    def _getpath(self):
        return self.To.path
    def _setpath(self,value):
        self.To.path=value
    path = property(_getpath,_setpath)
    
    def _gethalign(self):
        return self.To.halign
    def _sethalign(self,value):
        self.To.halign=value
    halign = property(_gethalign,_sethalign)
    
    def _getvalign(self):
        return self.To.valign
    def _setvalign(self,value):
        self.To.valign=value
    valign = property(_getvalign,_setvalign)
    
    #############################################################################
    #                                                                           #
    # Initialize the text combine attributes.                                   #
    #                                                                           #
    #############################################################################
    def __init__(self, Tt_name=None, Tt_name_src='default', To_name=None, To_name_src='default'):
        import vcs
        if (Tt_name == None):
           raise ValueError, 'Must provide a text table name.'
        if (To_name == None):
            To_name = Tt_name # Uses the same name than Tt

        if Tt_name in vcs.elements["texttable"]:
          raise Exception,"Error texttable object: '%s' already exists" % Tt_name
        if To_name in vcs.elements["textorientation"]:
          raise Exception,"Error textorientation object: '%s' already exists" % To_name
        #                                                                 #
        ###################################################################
        # Inherits texttable and textorientation secondary sub-classes.   #
        ###################################################################
        #                                                                 #
        self.Tt = texttable.Tt(Tt_name, Tt_name_src)
        self.To = textorientation.To(To_name, To_name_src)
        self.name = "%s:::%s" % (Tt_name,To_name)
        self.s_name = 'Tc'
        vcs.elements["textcombined"][self.name] = self
        #                                                         #
        ###########################################################
        # Save the parent class.                                  #
        ###########################################################
        #                                                         #

##     #############################################################################
##     #                                                                           #
##     # Set text table and text orientation attributes.                           #
##     #                                                                           #
##     #############################################################################
##     def __setattr__(self, name, value):
##         if ((self.Tt_name == '__removed_from_VCS__') or
##             (self.To_name == '__removed_from_VCS__')):
##            raise ValueError, 'This instance has been removed from VCS.'

## 	# Set the name to the appropriate class name (i.e., Tt or To)
##         if   ((name in ('Tt_name','font','spacing','expansion','color', 'string',
##                         'priority', 'viewport','worldcoordinate','x','y','projection')) and
##               (self.__dict__['Tt_name'] == 'default')):
##            raise ValueError, 'The default attributes must not be changed.'
##         elif ((name in ('To_name','height','angle','path','halign','valign')) and
##               (self.__dict__['To_name'] == 'default')):
##            raise ValueError, 'The default attributes must not be changed.'
##         elif (name in ('Tt_name','font','spacing','expansion','color', 'string',
##                         'priority', 'viewport','worldcoordinate','x','y','projection')):
##            self.__dict__['name'] = self.__dict__['Tt_name']
##         elif (name in ('To_name','height','angle','path','halign','valign')):
##            self.__dict__['name'] = self.__dict__['To_name']
##         else:
##            raise ValueError, 'This attribute is not valid.'

##         # Change Text Table attributes
##         if (name == 'Tt_name'):
##            if (type(value) == StringType):
##               renameTt(self,self.name, value)
##               self.__dict__['Tt_name']=value
##            else:
##               raise ValueError, 'The name attribute must be a string.'
##         elif (name == 'font'):
##             value = VCS_validation_functions.checkFont('',name,value)
##             self.__dict__['font']=value
##             setTtmember(self,'font',self.font) # update the plot               
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
##               raise ValueError, 'The color attribute value must be an integer in the range 0 to 256.'
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
##              if (type(value) in (ListType, TupleType)):
##                 value = list(value)  # make sure that values list is a list
##                 self.__dict__[name]=value
##                 setTtmember(self,name,value) # update the plot
##              else:
##               raise ValueError, 'The x attribute must be a tuple or list of values.'
##         elif (name == 'y'):
##            if (value == None):
##              self.__dict__[name] = None
##              setTtmember(self,name,value) # update the plot
##            else:
##              if (type(value) in (ListType, TupleType)):
##                 value = list(value)  # make sure that values list is a list
##                 self.__dict__[name]=value
##                 setTtmember(self,name,value) # update the plot
##              else:
##               raise ValueError, 'The y attribute must be a tuple or list of values.'
##         elif (name == 'projection'):
##             if value is None:
##                 self.__dict__['projection'] = 'default'
##             else:
##                 value=VCS_validation_functions.checkProjection(self,'projection',value)
##                 self.__dict__[name]= value
##                 setTtmember(self,name,value) # update the plot
##         # Change Text orientation attributes
##         elif (name == 'To_name'):
##            if (type(value) == StringType):
##               renameTo(self,self.name, value)
##               self.__dict__['To_name']=value
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
##                  value = float( value )
##                  self.__dict__['height']=value
##                  setTomember(self,'height',self.height) # update the plot
##            else:
##               raise ValueError, 'The height attribute value must be an integer or float.'
##         elif (name == 'angle'):
##            if (value == None):
##               self.__dict__['angle']=None
##               setTomember(self,'angle',self.angle) # update the plot
##            elif (isinstance(value, IntType)):
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
    # List out text combined members (attributes).                              #
    #                                                                           #
    #############################################################################
    def list(self):
        if ((self.Tt_name == '__removed_from_VCS__') or
            (self.To_name == '__removed_from_VCS__')):
           raise ValueError, 'This instance has been removed from VCS.'
        print "","----------Text combined (Tc) member (attribute) listings ----------"
        print "secondary method =", self.s_name
        print "","----------Text Table (Tt) member (attribute) listings ----------"
        print "Tt_name =",self.Tt_name
        print "font =", self.font
        print "spacing =", self.spacing
        print "expansion =", self.expansion
        print "color =", self.color
        print "fillincolor =", self.fillincolor
        print "priority =", self.priority
        print "string =", self.string
        print "viewport =", self.viewport
        print "worldcoordinate =", self.worldcoordinate
        print "x =", self.x
        print "y =", self.y
        print "projection =", self.projection
        print "","----------Text Orientation (To) member (attribute) listings ----------"
        print "To_name =",self.To_name
        print "height =", self.height
        print "angle =", self.angle
        print "path =", self.path
        print "halign =", self.halign
        print "valign =", self.valign

    #############################################################################
    #                                                                           #
    # Script out secondary text table and orientation methods in VCS to a file. #
    #                                                                           #
    #############################################################################
    def script(self, script_filename=None,mode=None):
        '''
 Function:     script                           # Calls _vcs.scripTo

 Description of Function:
       Saves out a text table and text orientation graphics method in Python or
       VCS script form to a designated file.

 Example of Use:
    script(scriptfile_name,mode)
              where: scriptfile_name is the output name of the script file.
                     mode is either "w" for replace or "a" for append.

	      Note: If the the filename has a ".py" at the end, it will produce a
		    Python script. If the filename has a ".scr" at the end, it will
		    produce a VCS script. If neither extensions are give, then by
		    default a Python script will be produced.

    a=vcs.init()
    tc=a.createtextcombined('new_tt','std','new_to','7left')
    tc.script('filename.py')	     # Append to a Python file "filename.py"
    tc.script('filename.scr')	     # Append to a VCS file "filename.scr"
    tc.script('filename','w')	     # Create or overwrite to a Python file "filename.py"
'''
        import _vcs
 
        if (script_filename == None):
          raise ValueError, 'Error - Must provide an output script file name.'

        if (mode == None):
           mode = 'a'
        elif (mode not in ('w', 'a')):
          raise ValueError, 'Error - Mode can only be "w" for replace or "a" for append.'

        # By default, save file in python script mode
        scr_type = script_filename[len(script_filename)-4:len(script_filename)]
        if (scr_type == '.scr'):
           _vcs.scriptTt(self.Tt_name,script_filename,mode)
           print _vcs.scriptTo(self.To_name,script_filename,"a")
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
              fp.write("import vcs\n")
              fp.write("v=vcs.init()\n\n")

           unique_name = '__Tt__' + self.Tt_name
           fp.write("#----------Text Table (Tt) member (attribute) listings ----------\n")
           fp.write("tt_list=v.listelements('texttable')\n")
           fp.write("if ('%s' in tt_list):\n" % self.Tt_name)
           fp.write("   %s = v.gettexttable('%s')\n" % (unique_name, self.Tt_name))
           fp.write("else:\n")
           fp.write("   %s = v.createtexttable('%s')\n" % (unique_name, self.Tt_name))
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

           unique_name = '__To__' + self.To_name
           fp.write("#----------Text Orientation (To) member (attribute) listings ----------\n")
           fp.write("to_list=v.listelements('textorientation')\n")
           fp.write("if ('%s' in to_list):\n" % self.To_name)
           fp.write("   %s = v.gettextorientation('%s')\n" % (unique_name, self.To_name))
           fp.write("else:\n")
           fp.write("   %s = v.createtextorientation('%s')\n" % (unique_name, self.To_name))
           fp.write("%s.height = %g\n" % (unique_name, self.height))
           fp.write("%s.angle = %g\n" % (unique_name, self.angle))
           fp.write("%s.path = '%s'\n" % (unique_name, self.path))
           fp.write("%s.halign = '%s'\n" % (unique_name, self.halign))
           fp.write("%s.valign = '%s'\n\n" % (unique_name, self.valign))
           fp.close()

#################################################################################
#        END OF FILE                                                            #
#################################################################################
