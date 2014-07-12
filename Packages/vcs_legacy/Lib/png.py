# Adapted for numpy/ma/cdms2 by convertcdms.py
"""
# Fillarea (Tf) module
"""
################################################################################
#                                                                              #
# Module:       fillarea (Tf) module                                           #
#                                                                              #
# Copyright:    2000, Regents of the University of California                  #
#               This software may not be distributed to others without         #
#               permission of the author.                                      #
#                                                                              #
# Author:       PCMDI Software Team                                            #
#               Lawrence Livermore NationalLaboratory:                         #
#               support@pcmdi.llnl.gov                                         #
#                                                                              #
# Description:  Python command wrapper for VCS's fill area secondary object.   #
#                                                                              #
# Version:      4.0                                                            #
#                                                                              #
################################################################################
#
#
#
################################################################################
#                                                                              #
# Import: VCS C extension module.                                              #
#                                                                              #
################################################################################
import _vcs_legacy
import Canvas
import VCS_validation_functions

################################################################################
#                                                                              #
# Function:	setTimember                                                    #
#                                                                              #
# Description of Function:                                                     #
# 	Private function to update the VCS canvas plot. If the canvas mode is  #
#       set to 0, then this function does nothing.              	       #
#                                                                              #
#                                                                              #
# Example of Use:                                                              #
#      setTimember(self,name,value)					       #
#              where: self is the class (e.g., Ti)                             #
#                     name is the name of the member that is being changed     #
#                     value is the new value of the member (or attribute)      #
#                                                                              #
################################################################################
def setTimember(self,member,value):
     # If the VCS Canvas is displayed, then bring the canvas to the front before 
     # redisplaying the updated contents.
     if (self.parent.mode == 1) and (self.parent.iscanvasdisplayed()):
        Canvas.finish_queued_X_server_requests( self.parent )
        self.parent.canvas.BLOCK_X_SERVER()
        self.parent.canvasraised()

     _vcs_legacy.setifmember(self, member, value, self.parent.mode)

     # If the VCS Canvas is displayed, then update the backing store
     if (self.parent.mode == 1) and (self.parent.iscanvasdisplayed()):
        self.parent.flush()
        self.parent.backing_store()
        self.parent.canvas.UNBLOCK_X_SERVER()

setmember = setTimember # for validation functions...
#################################################################################
#                                                                               #
# Function:     getTimember                                                     #
#                                                                               #
# Description of Function:                                                      #
#       Private function that retrieves the fillarea members from the C         #
#       structure and passes it back to Python.                                 #
#                                                                               #
#                                                                               #
# Example of Use:                                                               #
#      return_value =								#
#      getTimember(self,name)                                                   #
#              where: self is the class (e.g., Ti)                              #
#                     name is the name of the member that is being found        #
#                                                                               #
#################################################################################
def getTimember(self,member):
     return _vcs_legacy.getTimember(self,member)
getmember=getTimember

#################################################################################
#                                                                               #
# Function:     renameTi                                                        #
#                                                                               #
# Description of Function:                                                      #
#       Private function that renames the name of an existing fillarea          #
#       graphics method.                                                        #
#                                                                               #
#                                                                               #
# Example of Use:                                                               #
#      renameTi(old_name, new_name)                                             #
#              where: old_name is the current name of fillarea graphics method  #
#                     new_name is the new name for the fillarea graphics method #
#                                                                               #
#################################################################################
def renameTi(self, old_name, new_name):
     return _vcs_legacy.renameTi(old_name, new_name)

#############################################################################
#                                                                           #
# Fillarea (Tm) Class.                                                      #
#                                                                           #
#############################################################################
class Ti(object):
    """
 Class:	Ti				# Fillarea

 Description of Ti Class:
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
    def __init__(self, parent, Ti_name=None, Ti_name_src='default', createTi=0):
	#                                                         #
        ###########################################################
	# Initialize the fillarea class and its members           #
        #							  #
	# The getTimember function retrieves the values of the    #
        # fillarea members in the C structure and passes back the #
	# appropriate Python Object.                              #
        ###########################################################
	#                                                         #
        if (createTi == 0):
           if (Ti_name == None):
              raise ValueError, 'Must provide a fillarea name.'
           else:
              _vcs_legacy.copyTi(Ti_name_src, Ti_name)
              self._name = Ti_name
        else:
              self._name = Ti_name_src
        self.s_name='Ti'
        self.parent = parent


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
