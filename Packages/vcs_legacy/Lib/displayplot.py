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
###############################################################################
#                                                                             #
# Import: VCS C extension module.                                             #
#                                                                             #
###############################################################################
import _vcs_legacy
import VCS_validation_functions

###############################################################################
#                                                                             #
# Function:	setDpmember                                                   #
#                                                                             #
# Description of Function:                                                    #
# 	Private function to update the VCS canvas plot. If the canvas mode is #
#       set to 0, then this function does nothing.                            #
#                                                                             #
#                                                                             #
# Example of Use:                                                             #
#      setDpmember(self,name,value)				              #
#              where: self is the class (e.g., Dp)                            #
#                     name is the name of the member that is being changed    #
#                     value is the new value of the member (or attribute)     #
#                                                                             #
###############################################################################
def setDpmember(self,member,value):
     out = _vcs_legacy.setDpmember(self, member, value, self.parent.mode)
     self.parent.backing_store()
     return out
setmember = setDpmember
###############################################################################
#                                                                             #
# Function:     getDpmember                                                   #
#                                                                             #
# Description of Function:                                                    #
#       Private function that retrieves the display plot members from the C   #
#       structure and passes it back to Python.                               #
#                                                                             #
#                                                                             #
# Example of Use:                                                             #
#      return_value =						              #
#      getDpmember(self,name)                                                 #
#              where: self is the class (e.g., Dp)                            #
#                     name is the name of the member that is being found      #
#                                                                             #
###############################################################################
def getDpmember(self,member):
     return _vcs_legacy.getDpmember(self,member)
getmember = getDpmember
###############################################################################
#                                                                             #
# Function:     renameDp                                                      #
#                                                                             #
# Description of Function:                                                    #
#       Private function that renames the name of an existing display plot    #
#       object.                                                               #
#                                                                             #
#                                                                             #
# Example of Use:                                                             #
#      renameDp(old_name, new_name)                                           #
#              where: old_name is the current name of display plot object     #
#                     new_name is the new name for the display plot object    #
#                                                                             #
###############################################################################
def renameDp(self, old_name, new_name):
     return _vcs_legacy.renameDp(old_name, new_name)
#############################################################################
#                                                                           #
# Display Plot (Dp) Class.                                                  #
#                                                                           #
#############################################################################
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
 	     a=vcs_legacy.init()		# Constructor
	     a.show('plot')		# Show display plot objects
             a.update()               	# Updates the VCS Canvas at user's request
             a.mode=1, or 0           	# If 1, then automatic update, else if
                                          0, then use update function to
                                          update the VCS Canvas.

 Example of Use:
    a=vcs_legacy.init()
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
    rename = renameDp
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
         return getmember(self,'continents')
    def _setcontinents(self,value):
         value = VCS_validation_functions.checkInt(self,'continents',value,minvalue=-1)
         setmember(self,'continents',value)
    continents = property(_getcontinents,_setcontinents)

    def _getpriority(self):
         return getmember(self,'priority')
    def _setpriority(self,value):
         value = VCS_validation_functions.checkInt(self,'priority',value,minvalue=0)
         setmember(self,'priority',value)
    priority = property(_getpriority,_setpriority)

    def _getoff(self):
         return getmember(self,'off')
    def _setoff(self,value):
         value = VCS_validation_functions.checkInt(self,'off',value,minvalue=0,maxvalue=1)
         setmember(self,'off',value)
         for d in self.extradisplays:
              d.off=value
    off = property(_getoff,_setoff)

    def _getg_name(self):
         return getmember(self,'g_name')
    def _setg_name(self,value):
         value = VCS_validation_functions.checkString(self,'g_name',value)
         setmember(self,'g_name',value)
    g_name = property(_getg_name,_setg_name)

    def _getarray(self):
         return getmember(self,'array')
    def _setarray(self,value):
         if not isinstance(value,list):
              raise ValueError, 'The array must be contained in a list object.'             
         setmember(self,'array',value)
    array = property(_getarray,_setarray)

    def _gettemplate(self):
         return getmember(self,'template')
    def _settemplate(self,value):
         value = VCS_validation_functions.checkString(self,'template',value)
         setmember(self,'template',value)
    template = property(_gettemplate,_settemplate)

    def _gettemplate_origin(self):
         return getmember(self,'_template_origin')
    def _settemplate_origin(self,value):
         value = VCS_validation_functions.checkString(self,'_template_origin',value)
         setmember(self,'_template_origin',value)
    _template_origin = property(_gettemplate_origin,_settemplate_origin)

    def _getg_type(self):
         return getmember(self,'g_type')
    def _setg_type(self,value):
         value = VCS_validation_functions.checkString(self,'g_type',value)
         value=value.lower()
         if not value in ('boxfill', 'isofill', 'isoline', 'outfill', 'outline', 'continents', 'scatter', 'vector', 'xvsy', 'xyvsy', 'yxvsx','taylordiagram','meshfill'):
              raise ValueError,"g_type must be one of: ('boxfill', 'isofill', 'isoline', 'outfill', 'outline', 'continents', 'scatter', 'vector', 'xvsy', 'xyvsy', 'yxvsx','taylordiagram','meshfill')"
         setmember(self,'g_type',value)
    g_type = property(_getg_type,_setg_type)

    #############################################################################
    #                                                                           #
    # Initialize the display plot attributes.                                   #
    #                                                                           #
    #############################################################################
    def __init__(self, parent, Dp_name=None, Dp_name_src='default', createDp=0):
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
        if (createDp == 0):
           if (Dp_name == None):
              raise ValueError, 'Must provide a display plot name.'
           else:
              _vcs_legacy.copyD(Dp_name_src, Dp_name)
              self.name = Dp_name
        else:
              self._name=Dp_name_src
        self.s_name='Dp'
##         self.off=getDpmember(self, 'off')
##         self.__dict__['priority']=getDpmember(self, 'priority')
##         self.__dict__['template']=getDpmember(self, 'template')
##         self._template_origin=self.template
##         self.__dict__['g_type']=getDpmember(self, 'g_type')
##         self.__dict__['g_name']=getDpmember(self, 'g_name')
##         self.__dict__['array']=getDpmember(self, 'array')
        #                                                                   #
        #####################################################################
        # Find and set the display plot's structure in VCS C pointer        #
        # list. If the display plot name does not exist, then use           #
        # None and the display plot object.                                 #
        #####################################################################
        #                                                                   #
        self.parent=parent
 # DEAN 8/13/07 --- comment the below line out so that the original template name is show
 #               This is now done in Canvas.py: getplot
        self._template_origin=self.template

    #############################################################################
    #                                                                           #
    # Set display plot attributes.                                              #
    #                                                                           #
    #############################################################################
##     def __setattr__(self, name, value):
##         if (self.name == '__removed_from_VCS__'):
##            raise ValueError, 'This instance has been removed from VCS.'
##         if (self.name == 'default'):
##            raise ValueError, 'You cannot modify the default display.'
##         if (name == 'name'):
##            if (type(value) == StringType):
##               renameDp(self,self.name, value)
##               self.__dict__['name']=value
##            else:
##               raise ValueError, 'The name attribute must be a string.'
##         elif (name == 'off'):
##            if (value in (0,1)):
##               self.__dict__[name]=int(value)
##               setDpmember(self,name,self.off) # update the off status
##            else:
##               raise ValueError, 'The off status can either be 0 for plot or 1 no plot.'
##         elif (name == 'priority'):
##            if (isinstance(value, IntType)):
##               self.__dict__[name]=int(value)
##               setDpmember(self,name,self.priority) # update the priority
##            else:
##               raise ValueError, 'The priority value must be an integer.'
##         elif (name == 'template'):
##            if (type(value) == StringType):
##               self.__dict__[name]=value
##               setDpmember(self,name,self.template) # update the template
##            else:
##               raise ValueError, 'The template must be a string.'
##         elif (name == '_template_origin'):
##            if (type(value) == StringType):
##               self.__dict__[name]=value
##            else:
##               raise ValueError, 'The template must be a string.'
##         elif (name == 'g_type'):
##            if (string.lower(value) in ('boxfill', 'isofill', 'isoline', 'outfill', 'outline', 'continents', 'scatter', 'vector', 'xvsy', 'xyvsy', 'yxvsx','taylordiagram','meshfill')):
##               self.__dict__[name]=string.lower(value)
##               setDpmember(self,name,self.g_type) # update the template
##            else:
##               raise ValueError, 'Cannot find the graphics method.'
##         elif (name == 'g_name'):
##            if (type(value) == StringType):
##               self.__dict__[name]=value
##               setDpmember(self,name,self.g_name) # update the graphics method name
##            else: 
##               raise ValueError, 'The graphics method name must be a string.'
##         elif (name == 'array'):
##            if (type(value) == ListType):
##               self.__dict__[name]=value
##               self.__dict__[name]=setDpmember(self,name,self.array) # update the array
##            else: 
##               raise ValueError, 'The array must be contained in a list object.'
##         else:
##            raise ValueError, 'The member was not found.'


    #############################################################################
    #                                                                           #
    # List out display plot members (attributes).                               #
    #                                                                           #
    #############################################################################
    def list(self):
        if (self.name == '__removed_from_VCS__'):
           raise ValueError, 'This instance has been removed from VCS.'
        print "","----------Display Plot (Dp) member (attribute) listings ----------"
        print 'Canvas Mode =',self.parent.mode
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

    #############################################################################
    #                                                                           #
    # Script out display plot object in VCS to a file.                          #
    #                                                                           #
    #############################################################################
    def script(self, script_filename=None, mode=None):
        '''
 Function:     script                           # Calls _vcs_legacy.scriptDp

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

    a=vcs_legacy.init()
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
           print _vcs_legacy.scriptDp(self.name,script_filename,mode)
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
