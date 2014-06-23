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
import Canvas
import VCS_validation_functions
import vcs
import genutil

def process_src(nm,code):
  """Takes VCS script code (string) as input and generates boxfill gm from it"""
  try:
    tt = Tt(nm)
  except Exception,err:
    tt = vcs.elements["texttable"][nm]
  ## process attributes with = as assignement
  atts={}
  for a in ["string","vp","wc","x","y"]:
    i = code.find(a+"(")
    v=genutil.get_parenthesis_content(code[i:])
    #print nm,a,v
    if v!="":
      vals = []
      for V in v.split(","):
        try:
          vals.append(int(V))
        except:
          vals.append(float(V))
      atts[a]=vals
    tt.viewport = atts.get("vp",tt.viewport)
    tt.worldcoordinate = atts.get("wc",tt.worldcoordinate)
    tt.string = atts.get("string",tt.string)
    tt.x = atts.get("x",tt.x)
    tt.y = atts.get("y",tt.y)
    i = code.find("projection=")
    if i>-1:
      j=code[i:].find(",")+i
      tt.projection = code[i+11:j]
    #rest of attributes
    sp = code.split(",")
    tt.font = int(sp[0])
    tt.expansion = int(float(sp[2])*100.)
    tt.spacing = int(float(sp[3])*10.)
    tt.color = int(sp[4])
    tt.priority = int(sp[5])
    if len(sp)!=14:
      tt.fillincolor = int(sp[6])

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
 	     a=vcs.init()		# Constructor
	     a.show('texttable')	# Show predefined text table objects
             a.update()               	# Updates the VCS Canvas at user's request
             a.mode=1, or 0           	# If 1, then automatic update, else if
                                          0, then use update function to
                                          update the VCS Canvas.

 Example of Use:
    a=vcs.init()
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
         's_name',
         'name',
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
         'colormap',
         '_name',
         '_color',
         '_fillincolor',
         '_priority',
         '_font',
         '_string',
         '_spacing',
         '_expansion',
         '_viewport',
         '_worldcoordinate',
         '_x',
         '_y',
         '_projection',
         '_colormap',
         ]
    colormap = VCS_validation_functions.colormap
    def _getname(self):
         return self._name
    def _setname(self,value):
         value=VCS_validation_functions.checkname(self,'name',value)
         if value is not None:
              self._name=value
    name=property(_getname,_setname)
    
    def _getcolor(self):
         return self._color
    def _setcolor(self,value):
         if not value is None:
              value = VCS_validation_functions.checkColor(self,'color',value)
         self._color = value
    color=property(_getcolor,_setcolor)

    def _getfillincolor(self):
         return self._fillincolor
    def _setfillincolor(self,value):
         if not value is None:
              value = VCS_validation_functions.checkColor(self,'fillincolor',value)
         self._fillincolor=value
    fillincolor=property(_getfillincolor,_setfillincolor)

    def _getspacing(self):
         return self._spacing
    def _setspacing(self,value):
         self._spacing = VCS_validation_functions.checkInt(self,'spacing',value,minvalue=-50,maxvalue=50)
    spacing=property(_getspacing,_setspacing)

    def _getexpansion(self):
         return self._expansion
    def _setexpansion(self,value):
         self._expansion = VCS_validation_functions.checkInt(self,'expansion',value,minvalue=50,maxvalue=150)
    expansion=property(_getexpansion,_setexpansion)

    def _getfont(self):
         return self._font
    def _setfont(self,value):
         self._font = VCS_validation_functions.checkFont(self,'font',value)
    font=property(_getfont,_setfont)

    def _getstring(self):
         return self._string
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
         self._string = value
    string = property(_getstring,_setstring)
    
    def _getpriority(self):
         return self._priority
    def _setpriority(self,value):
         self._priority = VCS_validation_functions.checkInt(self,'priority',value,minvalue=0)
    priority = property(_getpriority,_setpriority)

    def _getprojection(self):
         return self._projection
    def _setprojection(self,value):
         self._projection=VCS_validation_functions.checkProjection(self,'projection',value)
    projection=property(_getprojection,_setprojection)
    
    def _getwc(self):
         return self._worldcoordinate
    def _setwc(self,value):
         self._worldcoordinate = VCS_validation_functions.checkListOfNumbers(self,'worldcoordinate',value,maxelements=4)
    worldcoordinate=property(_getwc,_setwc)
    
    def _getvp(self):
         return self._viewport
    def _setvp(self,value):
         self._viewport = VCS_validation_functions.checkListOfNumbers(self,'viewport',value,maxelements=4,minvalue=0.,maxvalue=1.)
    viewport=property(_getvp,_setvp)

    def _getx(self):
         return self._x
    def _setx(self,value):
         if value is None:
           self._x = None
           return
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
         self._x = value
    x = property(_getx,_setx)
    
    def _gety(self):
         return self._y
    def _sety(self,value):
         if value is None:
           self._y = None
           return
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
         self._y=value
    y = property(_gety,_sety)
    
    #############################################################################
    #                                                                           #
    # Initialize the text table attributes.                                     #
    #                                                                           #
    #############################################################################
    def __init__(self, Tt_name=None, Tt_name_src='default'):
	#                                                           #
        #############################################################
	# Initialize the text table class and its members           #
        #							    #
	# The getTtmember function retrieves the values of the      #
        # text table members in the C structure and passes back the #
	# appropriate Python Object.                                #
        #############################################################
	#                                                           #
        if (Tt_name == None):
          raise ValueError, 'Must provide a text table name.'
        if Tt_name in vcs.elements["texttable"].keys():
          raise ValueError, "texttable '%s' already exists" % Tt_name
        self._name = Tt_name
        self.s_name = 'Tt'
        if Tt_name=="default":
          self._string= ""
          self._font= 1
          self._spacing= 2
          self._expansion= 100
          self._color=1
          self._fillincolor=0
          self._priority=1
          self._viewport=[0.0, 1.0, 0.0, 1.0]
          self._worldcoordinate=[0.0, 1.0, 0.0, 1.0]
          self._x=None
          self._y=None
          self._projection="default"
          self._colormap = None
        else:
          if isinstance(Tt_name_src,Tt):
            Tt_name_src = Tt_name_src.name
          if not Tt_name_src in vcs.elements["texttable"].keys():
            raise ValueError, "Source texttable: '%s' does not exists" % Tt_name_src
          src = vcs.elements["texttable"][Tt_name_src]
          self.string= src.string
          self.font=src.font
          self.spacing=src.spacing
          self.expansion=src.expansion
          self.color=src.color
          self.fillincolor=src.fillincolor
          self.priority=src.priority
          self.viewport=src.viewport
          self.worldcoordinate=src.worldcoordinate
          self.x=src.x
          self.y=src.y
          self.projection=src.projection
          self.colormap = src.colormap
        vcs.elements["texttable"][Tt_name]=self

    #############################################################################
    #                                                                           #
    # List out text table members (attributes).                                 #
    #                                                                           #
    #############################################################################
    def list(self):
        if (self.name == '__removed_from_VCS__'):
           raise ValueError, 'This instance has been removed from VCS.'
        print "","----------Text Table (Tt) member (attribute) listings ----------"
        print "secondary method =", self.s_name
        print "name =", self.name
        print "string =", self.string
        print "font =", self.font
        print "spacing =", self.spacing
        print "expansion =", self.expansion
        print "color =", self.color
        print "fillincolor =", self.fillincolor
        print "priority =", self.priority
        print "viewport =", self.viewport
        print "worldcoordinate =", self.worldcoordinate
        print "x =", self.x
        print "y =", self.y
        print 'colormap =',self.colormap

    #############################################################################
    #                                                                           #
    # Script out secondary text table method in VCS to a file.                  #
    #                                                                           #
    #############################################################################
    def script(self, script_filename=None, mode=None):
        '''
 Function:     script                           # Calls _vcs.scriptTt

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

    a=vcs.init()
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

        # By default, save file in json
        scr_type = script_filename.split(".")
        if len(scr_type)==1 or len(scr_type[-1])>5:
          scr_type= "json"
          if script_filename!="initial.attributes":
            script_filename+=".json"
        else:
          scr_type = scr_type[-1]
        if scr_type == '.scr':
           raise DeprecationWarning("scr script are no longer generated")
        elif scr_type == "py":
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
           fp.write("%s.colormap = '%s'\n\n" % (unique_name, repr(self.colormap)))
        else:
          #Json type
          mode+="+"
          f = open(script_filename,mode)
          vcs.utils.dumpToJson(self,f)
          f.close()


#################################################################################
#        END OF FILE								#
#################################################################################
