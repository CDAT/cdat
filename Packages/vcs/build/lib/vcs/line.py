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
import Canvas
import VCS_validation_functions
import vcs
import genutil

def process_src(nm,code):
  try:
    f = Tl(nm)
  except Exception,err:
    #print "No good:",err
    f=vcs.elements["line"][nm]
  atts={}
  for a in ["ltyp","lwsf","lci","vp","wc","x","y"]:
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
    f.color = atts.get("lci",f.color)
    f.width = atts.get("lwsf",f.width)
    f.type = atts.get("ltyp",f.type)
    f.viewport = atts.get("vp",f.viewport)
    f.worldcoordinate = atts.get("wc",f.worldcoordinate)
    f.x = atts.get('x',f.x)
    f.y = atts.get('y',f.y)
    i = code.find("projection=")
    if i>-1:
      j=code[i:].find(",")+i
      f.projection = code[i+11:j]

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
 	     a=vcs.init()		# Constructor
	     a.show('line')		# Show predefined line objects
             a.update()               	# Updates the VCS Canvas at user's request
             a.mode=1, or 0           	# If 1, then automatic update, else if
                                          0, then use update function to
                                          update the VCS Canvas.

 Example of Use:
    a=vcs.init()
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
         's_name',
         'name',
         'color',
         'priority',
         'type',
         'width',
         'viewport',
         'worldcoordinate',
         'x',
         'y',
         'projection',
         'colormap',
         '_color',
         '_priority',
         '_type',
         '_width',
         '_viewport',
         '_worldcoordinate',
         '_x',
         '_y',
         '_projection',
         '_name',
         '_colormap',
         ]
    colormap = VCS_validation_functions.colormap
    def _getname(self):
         return self._name
    def _setname(self,value):
         value=VCS_validation_functions.checkname(self,'name',value)
         self._name=value
    name=property(_getname,_setname)
    
    def _getfillareacolors(self):
         return self._color
    def _setfillareacolors(self,value):
         if isinstance(value,int):
              value=[value,]
         self._color = VCS_validation_functions.checkColorList(self,'color',value)
    color=property(_getfillareacolors,_setfillareacolors)

    def _gettype(self):
         return self._type
    def _settype(self,value):
         if isinstance(value,(str,int)):
              value=[value,]
         self._type = VCS_validation_functions.checkLinesList(self,'index',value)
    type=property(_gettype,_settype)

    def _getwidth(self):
         return self._width
    def _setwidth(self,value):
         if isinstance(value,(int,float)):
              value=[value,]
         self._width = VCS_validation_functions.checkListOfNumbers(self,'width',value,minvalue=1,maxvalue=300)
    width=property(_getwidth,_setwidth)

    def _getpriority(self):
         return self._priority
    def _setpriority(self,value):
         self._priority = VCS_validation_functions.checkInt(self,'priority',value,minvalue=0)
    priority = property(_getpriority,_setpriority)

    def _getprojection(self):
         return self._projection
    def _setprojection(self,value):
         self._projection = VCS_validation_functions.checkProjection(self,'projection',value)
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
           self._x=None
           return
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
         self._x = value
    x = property(_getx,_setx)
    
    def _gety(self):
         return self._y
    def _sety(self,value):
         if value is None:
           self._y=None
           return
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
         self._y = value
    y = property(_gety,_sety)

    #############################################################################
    #                                                                           #
    # Initialize the line attributes.                                           #
    #                                                                           #
    #############################################################################
    def __init__(self, Tl_name, Tl_name_src='default'):
	#                                                         #
        ###########################################################
	# Initialize the line class and its members               #
        #							  #
	# The getTlmember function retrieves the values of the    #
        # line members in the C structure and passes back the     #
	# appropriate Python Object.                              #
        ###########################################################
	#                                                         #
        if Tl_name in vcs.elements["line"].keys():
          raise ValueError,"lineobject '%' already exists" % Tl_name
        self._name = Tl_name
        if isinstance(Tl_name_src ,Tl):
              Tl_name_src = Tl_name_src.name
        self.s_name='Tl'
        if Tl_name == "default":
          self._type = ['solid',]
          self._projection = "default"
          self._width = [1.0,]
          self._color = [1,]
          self._priority = 1
          self._viewport = [0.,1.,0.,1.]
          self._worldcoordinate = [0.,1.,0.,1.]
          self._x = None
          self._y = None
          self._colormap=None
        else:
          if not Tl_name_src in vcs.elements["line"].keys():
            raise ValueError, "The line source '%s' does not exists" % Tl_name_src
          src = vcs.elements["line"][Tl_name_src]
          self.type =src.type
          self.projection = src.projection
          self.width = src.width
          self.color = src.color
          self.priority = src.priority
          self.viewport = src.viewport
          self.worldcoordinate = src.worldcoordinate
          self.x = src.x
          self.y = src.y
          self.colormap = src.colormap
        vcs.elements["line"][Tl_name] = self

    #############################################################################
    #                                                                           #
    # List out line members (attributes).                                       #
    #                                                                           #
    #############################################################################
    def list(self):
        if (self.name == '__removed_from_VCS__'):
           raise ValueError, 'This instance has been removed from VCS.'
        print "","----------Line (Tl) member (attribute) listings ----------"
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
        print "colormap =",self.colormap

    #############################################################################
    #                                                                           #
    # Script out secondary line method in VCS to a file.                        #
    #                                                                           #
    #############################################################################
    def script(self, script_filename=None, mode=None):
        """
 Function:     script                           # Calls _vcs.scriptTl

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

    a=vcs.init()
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
