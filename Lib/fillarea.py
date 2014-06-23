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
import VCS_validation_functions
import vcs
import genutil

def getmember(self,name):
  return getattr(self,"_%s" % name)

def process_src(nm,code):
  try:
    f = Tf(nm)
  except Exception,err:
    #print "No good:",err
    f=vcs.elements["fillarea"][nm]
  atts={}
  for a in ["faci","fasi","fais","vp","wc","x","y"]:
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
    f.style=atts.get("fais",f.style)
    I = atts.get("fasi",f.index)
    for i,v in enumerate(I):
      if v==0:
        I[i]=1
    f.index = I
    f.color = atts.get("faci",f.color)
    f.viewport = atts.get("vp",f.viewport)
    f.worldcoordinate = atts.get("wc",f.worldcoordinate)
    f.x = atts.get('x',f.x)
    f.y = atts.get('y',f.y)
    i = code.find("projection=")
    if i>-1:
      j=code[i:].find(",")+i
      f.projection = code[i+11:j]
    for b in vcs.elements["boxfill"].values()+vcs.elements["isofill"].values()+vcs.elements["meshfill"].values():
      if b.fillareaindices is not None:
       for i,v in enumerate(b.fillareaindices):
        if isinstance(v,str) and v==nm:
          b._fillareaindices[i]=self.index
          b._fillareacolor[i]=self.color
          b._fillareastyle = self.style

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
 	     a=vcs.init()		# Constructor
	     a.show('fillarea')		# Show predefined fillarea objects
             a.update()               	# Updates the VCS Canvas at user's request
             a.mode=1, or 0           	# If 1, then automatic update, else if
                                          0, then use update function to
                                          update the VCS Canvas.

 Example of Use:
    a=vcs.init()
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
         'colormap',
         '_name',
         '_color',
         '_priority',
         '_style',
         '_index',
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

    def _getfillareacolors(self):
         return getmember(self,'color')
    def _setfillareacolors(self,value):
         if isinstance(value,Tf):
           value = value.color
         if isinstance(value,(str,int)):
              value=[value,]
         if not value is None:
              value = VCS_validation_functions.checkColorList(self,'color',value)
         else:
           value = [241]
         self._color=value
    color=property(_getfillareacolors,_setfillareacolors)

    def _getfillareaindices(self):
         return getmember(self,'index')
    def _setfillareaindices(self,value):
         if not isinstance(value,(list,tuple)) and value is not None:
              value=[value,]
         if value is not None:
              value = VCS_validation_functions.checkIndicesList(self,'index',value)
         if value in [ (), []]:
           raise ValueError, "You cannot set fillarea index to an empty list"
         self._index = value
    index=property(_getfillareaindices,_setfillareaindices)

    def _getfillareastyle(self):
         return getmember(self,'style')
    
    def _setfillareastyle(self,value):
         if isinstance(value,(str,int,Tf)):
              value=[value,]
         vals=[]
         for v in value:
              v=VCS_validation_functions.checkFillAreaStyle(self,'style',v)
              vals.append(v)
         if vals == []:
            raise ValueError, "fillareastyle cannot be empty list"
         value=vals
         self._style= value
    style=property(_getfillareastyle,_setfillareastyle)

    def _getpriority(self):
         return getmember(self,'priority')
    def _setpriority(self,value):
         value = VCS_validation_functions.checkInt(self,'priority',value,minvalue=0)
         self._priority = value
    priority = property(_getpriority,_setpriority)

    def _getprojection(self):
         return getmember(self,'projection')
    def _setprojection(self,value):
         value=VCS_validation_functions.checkProjection(self,'projection',value)
         self._projection = value
    projection=property(_getprojection,_setprojection)
    
    def _getwc(self):
         return getmember(self,'worldcoordinate')
    def _setwc(self,value):
         value = VCS_validation_functions.checkListOfNumbers(self,'worldcoordinate',value,maxelements=4)
         self._worldcoordinate = value
    worldcoordinate=property(_getwc,_setwc)
    
    def _getvp(self):
         return getmember(self,'viewport')
    def _setvp(self,value):
         value = VCS_validation_functions.checkListOfNumbers(self,'viewport',value,maxelements=4,minvalue=0.,maxvalue=1.)
         self._viewport = value
    viewport=property(_getvp,_setvp)

    def _getx(self):
         return getmember(self,'x')
    def _setx(self,value):
         if value is None:
           self._x = value
           return
         if not isinstance(value,(list,tuple)):
              raise ValueError, 'x must be a tuple or list of values.'
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
         self._x=value
    x = property(_getx,_setx)
    
    def _gety(self):
         return getmember(self,'y')
    def _sety(self,value):
         if value is None:
           self._y = None
           return
         if not isinstance(value,(list,tuple)):
              raise ValueError, 'y must be a tuple or list of values.'
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
         self._y=value
    y = property(_gety,_sety)
    
    #############################################################################
    #                                                                           #
    # Initialize the fillarea attributes.                                       #
    #                                                                           #
    #############################################################################
    def __init__(self, Tf_name=None, Tf_name_src='default'):
	#                                                         #
        ###########################################################
	# Initialize the fillarea class and its members           #
        #							  #
	# The getTfmember function retrieves the values of the    #
        # fillarea members in the C structure and passes back the #
	# appropriate Python Object.                              #
        ###########################################################
	#                                                         #
        if isinstance(Tf_name_src,Tf):
          Tf_name_src=Proj_name_src.name
        if Tf_name_src!="default" and not Tf_name_src in vcs.elements["fillarea"].keys():
          raise ValueError, "Fillarea '%s' does not exists" % Proj_name_src
        if (Tf_name == None):
          raise ValueError, 'Must provide a fillarea name.'
        else:
          if Tf_name in vcs.elements["fillarea"].keys():
            raise ValueError, "The fillarea '%s' already exists, use getfillarea instead" % Tf_name
        self._name = Tf_name
        self.s_name='Tf'

        if Tf_name=="default":
          self._style=['solid',]
          self._index=[1,]
          self._color = [1,]
          self._priority=1
          self._viewport = [0.,1.,0.,1.]
          self._worldcoordinate = [0.,1.,0.,1.]
          self._x=None
          self._y=None
          self._projection="default"
          self._colormap = None
        else:
          src = vcs.elements["fillarea"][Tf_name_src]
          self.style=src.style
          self.index=src.index
          self.color = src.color
          self.priority= src.priority
          self.viewport =src.viewport
          self.worldcoordinate = src.worldcoordinate
          self.x=src.x
          self.y=src.y
          self.projection=src.projection
          self.colormap = src.colormap

        vcs.elements["fillarea"][Tf_name] = self


    #############################################################################
    #                                                                           #
    # Fillarea out line members (attributes).                                   #
    #                                                                           #
    #############################################################################
    def list(self):
        if (self.name == '__removed_from_VCS__'):
           raise ValueError, 'This instance has been removed from VCS.'
        print "","----------Fillarea (Tf) member (attribute) listings ----------"
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
        print "colormap =",self.colormap

    #############################################################################
    #                                                                           #
    # Script out secondary fillarea method in VCS to a file.                    #
    #                                                                           #
    #############################################################################
    def script(self, script_filename=None, mode=None):
        """
 Function:     script                           # Calls _vcs.scriptTf

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

    a=vcs.init()
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
           fp.write("%s.colormap = %s\n\n" % (unique_name, repr(self.colormap)))
        else:
          #Json type
          mode+="+"
          f = open(script_filename,mode)
          vcs.utils.dumpToJson(self,f)
          f.close()


#################################################################################
#        END OF FILE								#
#################################################################################
