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
import Canvas
import VCS_validation_functions
import vcs

def process_src(nm,code):
  """Takes VCS script code (string) as input and generates boxfill gm from it"""
  try:
    to = To(nm)
  except:
    to = vcs.elements["textorientation"][nm]
  ## process attributes with = as assignement
  sp = code.split(",")
  to.height = int(float(sp[0])*1000)
  to.angle = int(sp[1])
  to.path=["r","l","u","d"].index(sp[2])
  to.halign=["l","c","r"].index(sp[3])
  to.valign=["t","c","h","b","s"].index(sp[4])


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
 	     a=vcs.init()		# Constructor
	     a.show('textorientation')	# Show predefined text orientation objects
             a.update()               	# Updates the VCS Canvas at user's request
             a.mode=1, or 0           	# If 1, then automatic update, else if
                                          0, then use update function to
                                          update the VCS Canvas.

 Example of Use:
    a=vcs.init()
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
         's_name',
         'name',
         'height',
         'angle',
         'path',
         'halign',
         'valign',
         '_name',
         '_height',
         '_angle',
         '_path',
         '_halign',
         '_valign',
         ]
    def _getname(self):
         return self._name
    def _setname(self,value):
         value=VCS_validation_functions.checkname(self,'name',value)
         self._name=value
    name=property(_getname,_setname)

    def _getheight(self):
         return self._height
    def _setheight(self,value):
         self._height = VCS_validation_functions.checkNumber(self,'height',value)
    height=property(_getheight,_setheight)

    def _getangle(self):
         return self._angle
    def _setangle(self,value):
         self._angle = VCS_validation_functions.checkInt(self,'angle',value,minvalue=-360,maxvalue=360)
    angle=property(_getangle,_setangle)

    def _getpath(self):
         return self._path
    def _setpath(self,value):
         vals = ["right","left","up","down"]
         self._path = VCS_validation_functions.checkInStringsListInt(self,'path',value,vals)
    path = property(_getpath,_setpath)
    
    def _gethalign(self):
         return self._halign
    def _sethalign(self,value):
         vals = ["left","center","right"]
         self._halign = VCS_validation_functions.checkInStringsListInt(self,'halign',value,vals)
    halign = property(_gethalign,_sethalign)
    
    def _getvalign(self):
         return self._valign
    def _setvalign(self,value):
         vals = ["top","cap","half","base","bottom"]
         self._valign = VCS_validation_functions.checkInStringsListInt(self,'valign',value,vals)
    valign = property(_getvalign,_setvalign)
    
    #############################################################################
    #                                                                           #
    # Initialize the text orientation attributes.                               #
    #                                                                           #
    #############################################################################
    def __init__(self, To_name, To_name_src='default'):
	#                                                           #
        #############################################################
	# Initialize the text orientation class and its members     #
        #							    #
	# The getTomember function retrieves the values of the      #
        # text orientation members in the C structure and passes    #
        # back the appropriate Python Object.                       #
        #############################################################
	#                                                           #
        if To_name in vcs.elements["textorientation"].keys():
          raise ValueError,"textorientation object '%' already exists" % To_name
        self._name = To_name
        if isinstance(To_name_src ,To):
              To_name_src = To_name_src.name
        self.s_name = 'To'
        if To_name == "default":
          self._height = 14
          self._angle = 0
          self._path="right"
          self._halign = "left"
          self._valign = "half"
        else:
          if not To_name_src in vcs.elements["textorientation"].keys():
            raise valueError,"source textorientation '%s' does not exists" % To_name_src
          src = vcs.elements["textorientation"][To_name_src]
          self.height = src.height
          self.angle = src.angle
          self.path= src.path
          self.halign = src.halign
          self.valign = src.valign
        vcs.elements["textorientation"][To_name]=self

    #############################################################################
    #                                                                           #
    # List out text orientation members (attributes).                           #
    #                                                                           #
    #############################################################################
    def list(self):
        if (self.name == '__removed_from_VCS__'):
           raise ValueError, 'This instance has been removed from VCS.'
        print "","----------Text Orientation (To) member (attribute) listings ----------"
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
 Function:     script                           # Calls _vcs.scripTo

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

    a=vcs.init()
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
           print _vcs.scriptTo(self.name,script_filename,mode)
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
