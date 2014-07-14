## Automatically adapted for numpy.oldnumeric Jul 06, 2007 by numeric2numpy.py

"""
# Display Plot (Dp) module
"""
###############################################################################
#                                                                             #
# Module:       Display Plot (Dp) module                                      #
#                                                                             #
# Copyright:    2000, Regents of the University of California                 #
#               This software may not be distributed to others without        #
#               permission of the author.                                     #
#                                                                             #
# Authors:      PCMDI Software Team                                           #
#               Lawrence Livermore NationalLaboratory:                        #
#               support@pcmdi.llnl.gov                                        #
#                                                                             #
# Description:  Python command wrapper for VCS's display plot object.         #
#                                                                             #
# Version:      4.0                                                           #
#                                                                             #
###############################################################################
#
#
#
import VCS_validation_functions
import vcs

class Dp(object):
    """
 Class:	Dp				# Display Plot

 Description of Dp Class:
    The Display plot object allows the manipulation of the plot name, off,
    priority, template, graphics type, graphics name, and data array(s).

    This class is used to define a display plot table entry used in VCS, or it
    can be used to change some or all of the display plot attributes in an
    existing display plot table entry.

 Other Useful Functions:
 	     a=vcs.init()		# Constructor
	     a.show('plot')		# Show display plot objects
             a.update()               	# Updates the VCS Canvas at user's request
             a.mode=1, or 0           	# If 1, then automatic update, else if
                                          0, then use update function to
                                          update the VCS Canvas.

 Example of Use:
    a=vcs.init()
    To Create a new instance of line use:
     p1=a.plot(s) 			# Create a plot object

    To Modify an existing plot in use:
     p1=a.getplot('dpy_plot_1')

    p1.list()  				# Will list all the display plot attributes
    p1.off=1				# "On" or "Off" status, 1=on, 0=off
    p1.priority=1			# Piority to place plot in front of other objects
    p1.template='quick'			# Name of template object
    p1.g_type='boxfill'			# Graphics method type
    p1.g_name='quick'			# Graphics method name
    p1.array=['a1']			# List of all the array names
    """
    __slots__ = [ "name",
                  "_name",
                  "s_name",
                  "off",
                  "priority",
                  "template",
                  "_template_origin",
                  "g_type",
                  "g_name",
                  "array",
                  "continents",
                  "extradisplays",
                  "parent",
                  "_off",
                  "_priority",
                  "_template",
                  "__template_origin",
                  "_g_type",
                  "_g_name",
                  "_array",
                  "_continents",
                  "ratio",
    ]


    def _repr_png_(self):
         import tempfile
         tmp = tempfile.mktemp()+".png"
         self.parent.png(tmp)
         f=open(tmp)
         st = f.read()
         f.close()
         return st
#TODO: html,json,jpeg,png,svg,latex
    def _getname(self):
         return self._name
    def _setname(self,value):
         value=VCS_validation_functions.checkname(self,'name',value)
         if value is not None:
              self._name=value
              setmember(self,'name',value)
    name=property(_getname,_setname)

    def _getcontinents(self):
         return self._continents
    def _setcontinents(self,value):
         self._continents = VCS_validation_functions.checkInt(self,'continents',value,minvalue=-1)
    continents = property(_getcontinents,_setcontinents)

    def _getpriority(self):
         return self._priority
    def _setpriority(self,value):
         self._priority = VCS_validation_functions.checkInt(self,'priority',value,minvalue=0)
    priority = property(_getpriority,_setpriority)

    def _getoff(self):
         return self._off
    def _setoff(self,value):
         self._off = VCS_validation_functions.checkInt(self,'off',value,minvalue=0,maxvalue=1)
         for d in self.extradisplays:
              d.off=self._off
    off = property(_getoff,_setoff)

    def _getg_name(self):
         return self._g_name
    def _setg_name(self,value):
         self._g_name = VCS_validation_functions.checkString(self,'g_name',value)
    g_name = property(_getg_name,_setg_name)

    def _getarray(self):
         return self._array
    def _setarray(self,value):
         if not isinstance(value,list):
              raise ValueError, 'The array must be contained in a list object.'             
         self._array=value
    array = property(_getarray,_setarray)

    def _gettemplate(self):
         return self._template
    def _settemplate(self,value):
         self._template = VCS_validation_functions.checkString(self,'template',value)
    template = property(_gettemplate,_settemplate)

    def _gettemplate_origin(self):
         return self.__template_origin
    def _settemplate_origin(self,value):
         self.__template_origin = VCS_validation_functions.checkString(self,'_template_origin',value)
    _template_origin = property(_gettemplate_origin,_settemplate_origin)

    def _getg_type(self):
         return self._g_type
    def _setg_type(self,value):
         value = VCS_validation_functions.checkString(self,'g_type',value)
         value=value.lower()
         if not value in vcs.elements and value!="text":
           raise ValueError,"invalid g_type '%s' must be one of: %s " % (value,vcs.elements.keys())
         self._g_type=value
    g_type = property(_getg_type,_setg_type)

    #############################################################################
    #                                                                           #
    # Initialize the display plot attributes.                                   #
    #                                                                           #
    #############################################################################
    def __init__(self, Dp_name, Dp_name_src='default'):
	#                                                                           #
        #############################################################################
	# Initialize the display plot's class and its members                       #
        #							                    #
	# The getDpmember function retrieves the values of the                      #
        # display plot members in the C structure and passes back the               #
	# appropriate Python Object.                                                #
        #############################################################################
	#                                                                           #
        self.extradisplays=[]
        self._name = Dp_name
        self.s_name='Dp'
        if self._name == "default":
          self._off = 0
          self._priority=0
          self._template="default"
          self.__template_origin="default"
          self._g_type = "boxfill"
          self._g_name = "default"
          self._array=[]
          self._continents = -1
          self.ratio = None
        else:
          src=vcs.elements["display"][Dp_name_src]
          self.off = src.off
          self.array = src.array
          self.template=src.template
          self._template_origin = src._template_origin
          self.g_type=src.g_type
          self.g_name=src.g_name
          self.continents=src.continents
          self.priority=src.priority
          self.ratio = src.ratio

        vcs.elements["display"][self._name]=self
    #############################################################################
    #                                                                           #
    # List out display plot members (attributes).                               #
    #                                                                           #
    #############################################################################
    def list(self):
        if (self.name == '__removed_from_VCS__'):
           raise ValueError, 'This instance has been removed from VCS.'
        print "","----------Display Plot (Dp) member (attribute) listings ----------"
        print "Display plot method =", self.s_name
        print "name =", self.name
        print "off =", self.off
        print "priority =", self.priority
        print "template =", self.template
        print "template_origin =", self._template_origin
        print "g_type =", self.g_type
        print "g_name =", self.g_name
        print "array =", self.array
        print "continents =", self.continents
        print "extradisplays =", self.extradisplays
        print "ratio =", self.ratio

    #############################################################################
    #                                                                           #
    # Script out display plot object in VCS to a file.                          #
    #                                                                           #
    #############################################################################
    def script(self, script_filename=None, mode=None):
        '''
 Function:     script                           # Calls _vcs.scriptDp

 Description of Function:
       Saves out a display plot object in VCS or Python script form to a
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
           print _vcs.scriptDp(self.name,script_filename,mode)
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

           unique_name = '__Dp__' + self.name
           fp.write("#----------Display Plot (Dp) member (attribute) listings ----------\n")
           fp.write("dp_list=v.listelements('Plot')\n")
           fp.write("if ('%s' in dp_list):\n" % self.name)
           fp.write("   %s = v.getplot('%s')\n" % (unique_name, self.name))
           fp.write("else:\n")
           fp.write("   %s = v.createplot('%s')\n" % (unique_name, self.name))
           fp.write("%s.off = '%s'\n" % (unique_name, self.off))
           fp.write("%s.priority = %g\n" % (unique_name, self.priority))
           fp.write("%s.template = %g\n\n" % (unique_name, self.template))
           fp.write("%s._template_origin = %g\n\n" % (unique_name, self._template_origin))
           fp.write("%s.g_type = %g\n\n" % (unique_name, self.g_type))
           fp.write("%s.g_name = %g\n\n" % (unique_name, self.g_name))
           fp.write("%s.array = %g\n\n" % (unique_name, self.array))
           fp.write("%s.continents = %g\n\n" % (unique_name, self.continents))


#################################################################################
#        END OF FILE								#
#################################################################################
