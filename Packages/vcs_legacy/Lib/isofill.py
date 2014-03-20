"""
# Isofill (Gfi) module
"""
###############################################################################
#                                                                             #
# Module:       isofill (Gfi) module                                          #
#                                                                             #
# Copyright:    2000, Regents of the University of California                 #
#               This software may not be distributed to others without        #
#               permission of the author.                                     #
#                                                                             #
# Author:       PCMDI Software Team                                           #
#               Lawrence Livermore NationalLaboratory:                        #
#               support@pcmdi.llnl.gov                                        #
#                                                                             #
# Description:  Python command wrapper for VCS's isofill graphics method.     #
#                                                                             #
# Version:      5.0                                                           #
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
import _vcs_legacy, vcs_legacy, VCS_validation_functions, cdtime
import Canvas
from types import *
import AutoAPI
import xmldocs

###############################################################################
#                                                                             #
# Function:	setGfimember                                                  #
#                                                                             #
# Description of Function:                                                    #
# 	Private function to update the VCS canvas plot. If the canvas mode is #
#       set to 0, then this function does nothing.                            #
#                                                                             #
#                                                                             #
# Example of Use:                                                             #
#      setGfimember(self,name,value)					      #
#              where: self is the class (e.g., Gfi)                           #
#                     name is the name of the member that is being changed    #
#                     value is the new value of the member (or attribute)     #
#                                                                             #
###############################################################################
def setGfimember(self,member,value):
     # If the VCS Canvas is displayed, then bring the canvas to the front before 
     # redisplaying the updated contents.
     if (self.parent.mode == 1) and (self.parent.iscanvasdisplayed()): 
        Canvas.finish_queued_X_server_requests( self.parent )
        self.parent.canvas.BLOCK_X_SERVER()
        self.parent.canvasraised()

     _vcs_legacy.setGfimember(self, member, value, self.parent.mode)

     # If the VCS Canvas is displayed, then update the backing store
     if (self.parent.mode == 1) and (self.parent.iscanvasdisplayed()):
        self.parent.flush()
        self.parent.backing_store()
        self.parent.canvas.UNBLOCK_X_SERVER()

setmember=setGfimember

###############################################################################
#                                                                             #
# Function:     getGfimember                                                  #
#                                                                             #
# Description of Function:                                                    #
#       Private function that retrieves the isofill members from the C        #
#       structure and passes it back to Python.                               #
#                                                                             #
#                                                                             #
# Example of Use:                                                             #
#      return_value =                                                         #
#      getGfimember(self,name)                                                #
#              where: self is the class (e.g., Gfi)                           #
#                     name is the name of the member that is being found      #
#                                                                             #
###############################################################################
def getGfimember(self,member):
     return _vcs_legacy.getGfimember(self,member)
getmember=getGfimember

###############################################################################
#                                                                             #
# Function:     renameGfi                                                     #
#                                                                             #
# Description of Function:                                                    #
#       Private function that renames the name of an existing isofill         #
#       graphics method.                                                      #
#                                                                             #
#                                                                             #
# Example of Use:                                                             #
#      renameGfi(old_name, new_name)                                          #
#              where: old_name is the current name of isofill graphics method #
#                     new_name is the new name for the isofill graphics method#
#                                                                             #
###############################################################################
def renameGfi(self, old_name, new_name):
     return _vcs_legacy.renameGfi(old_name, new_name)

###############################################################################
#                                                                             #
# Function:     add_level_ext_1                                               #
#                                                                             #
# Description of Function:                                                    #
#       Private function that adds the extension triangle to the left of the  #
#       legend on the plot                                                    #
#                                                                             #
#                                                                             #
# Example of Use:                                                             #
#      add_level_ext_1(self, ext_value)                                       #
#              where: self is the class (e.g., Gfi)                           #
#                     ext_value is either 'n' to remove the triangle on the   #
#		     	legend or 'y' to show the triangle on the triangle    #
#                                                                             #
###############################################################################
def add_level_ext_1(self, ext_value):
    if ((ext_value == 'n') and (self.ext_1 == 'y')): # remove extension
       if (type(self.levels[0]) == ListType): # remove from tuple of lists
          self.levels[0].remove(self.levels[0][0])
          return self.levels
       if (type(self.levels) == TupleType):       # remove from list
          ret_tup = []
          for i in range(len(self.levels)):
             ret_tup.insert(i+1,self.levels[i])
          ret_tup.remove(self.levels[0])
          return ret_tup

    if (type(self.levels) == TupleType):
       if (type(self.levels[0]) == ListType): # add to tuple of lists
          self.levels[0].insert(0,-1e20)
          return self.levels
       else:                                  # must be a mutable tuple
          ret_tup = [-1e20]		      # therefore, covert to a list
          for i in range(len(self.levels)):   # then add extension to list
             ret_tup.insert(i+1,self.levels[i])
          return ret_tup
    if (type(self.levels) == ListType):       # add extension to list
          if (((self.levels[(len(self.levels)-1)] - self.levels[0])) >= 0):
             self.levels.insert(0,-1e20)
          else:
             self.levels.insert(0,-1e20)
          return self.levels

###############################################################################
#                                                                             #
# Function:     add_level_ext_2                                               #
#                                                                             #
# Description of Function:                                                    #
#       Private function that adds the extension triangle to the right of the #
#       legend on the plot                                                    #
#                                                                             #
#                                                                             #
# Example of Use:                                                             #
#      add_level_ext_2(self, ext_value)                                       #
#              where: self is the class (e.g., Gfi)                           #
#                     ext_value is either 'n' to remove the triangle on the   #
#                       legend or 'y' to show the triangle on the triangle    #
#                                                                             #
###############################################################################
def add_level_ext_2(self, ext_value):
    if ((ext_value == 'n') and (self.ext_2 == 'y')): # remove extension
       if (type(self.levels[0]) == ListType): # remove from tuple of lists
          last=len(self.levels) - 1
          last2=len(self.levels[last]) - 1
          self.levels[last].remove(self.levels[last][last2])
          return self.levels
       if (type(self.levels) == TupleType):       # remove from list
          ret_tup = []		      	
          for i in range(len(self.levels)-1):
             ret_tup.insert(i+1,self.levels[i])
          return ret_tup

    if (type(self.levels) == TupleType):
       last=len(self.levels) - 1
       if (type(self.levels[last]) == ListType): # add to tuple of lists
          self.levels[last].append(1e20)
          return self.levels
       else:                                  # must be a mutable tuple
          ret_tup = []			      # therefore, covert to a list
          for i in range(len(self.levels)):   # then add extension to list
             ret_tup.insert(i,self.levels[i])
          ret_tup.insert(i+1,1e20)
          return ret_tup
    if (type(self.levels) == ListType):       # add extension to list
          if (((self.levels[(len(self.levels)-1)] - self.levels[0])) > 0):
             self.levels.insert(len(self.levels),1e20)
          else:
             self.levels.insert(len(self.levels),-1e20)
          return self.levels

class Gfi(object,AutoAPI.AutoAPI):
    """
    Options:::
%s
%s
:::

Class: Gfi				# Isofill

 Description of Gfb Class:
    The Isofill graphics method fills the area between selected isolevels
    (levels of constant value) of a two-dimensional array with a
    user-specified color. The example below shows how to display an isofill
    plot on the VCS Canvas and how to create and remove isofill isolevels.
 
    This class is used to define an isofill table entry used in VCS, or it
    can be used to change some or all of the isofill attributes in an
    existing isofill table entry.

 Other Useful Functions:
 	     a=vcs_legacy.init()		# Constructor
	     a.show('isofill')		# Show predefined isofill graphics methods
	     a.show('fillarea')		# Show predefined fillarea objects
	     a.show('template')		# Show predefined fillarea objects
	     a.setcolormap("AMIP")	# Change the VCS color map
             a.createtemplate('test')	# Create a template
             a.createfillarea('fill')	# Create a fillarea
	     a.gettemplate('AMIP')	# Get an existing template
	     a.getfillarea('def37')	# Get an existing fillarea
	     a.isofill(s,i,t)		# Plot array 's' with isofill 'i' and
					       template 't'
             a.update()               	# Updates the VCS Canvas at user's request
             a.mode=1, or 0           	# If 1, then automatic update, else if
                                          0, then use update function to
                                          update the VCS Canvas.

 Example of Use:
    a=vcs_legacy.init()
    To Create a new instance of isofill use:
     iso=a.createisofill('new','quick') # Copies content of 'quick' to 'new'
     iso=a.createisofill('new') 	# Copies content of 'default' to 'new'

    To Modify an existing isofill use:
     iso=a.getisofill('AMIP_psl')

    iso.list()  			# Will list all the isofill attribute values
    iso.projection='linear'
    lon30={-180:'180W',-150:'150W',0:'Eq'}
    iso.xticlabels1=lon30
    iso.xticlabels2=lon30
    iso.xticlabels(lon30, lon30)  	# Will set them both
    iso.xmtics1=''
    iso.xmtics2=''
    iso.xmtics(lon30, lon30)  		# Will set them both
    iso.yticlabels1=lat10
    iso.yticlabels2=lat10
    iso.yticlabels(lat10, lat10)  	# Will set them both
    iso.ymtics1=''
    iso.ymtics2=''
    iso.ymtics(lat10, lat10)  		# Will set them both
    iso.datawc_y1=-90.0
    iso.datawc_y2=90.0
    iso.datawc_x1=-180.0
    iso.datawc_x2=180.0
    iso.datawc(-90, 90, -180, 180)  	# Will set them all
    xaxisconvert='linear'
    yaxisconvert='linear'
    iso.xyscale('linear', 'area_wt')  	# Will set them both
    missing=241				# Color index value range 0 to 255

    iso.legend=None

    There are two possibilities for setting the isofill levels:
     A) Levels are all contiguous (Examples):
		iso.levels=([0,20,25,30,35,40],)
		iso.levels=([0,20,25,30,35,40,45,50])
		iso.levels=[0,20,25,30,35,40]
		iso.levels=(0.0,20.0,25.0,30.0,35.0,40.0,50.0)
     B) Levels are not contiguous (Examples):
		iso.levels=([0,20],[30,40],[50,60])
		iso.levels=([0,20,25,30,35,40],[30,40],[50,60])

    There are three possibilities for setting the fillarea color indices (Ex):
      		iso.fillareacolors=([22,33,44,55,66,77])
		iso.fillareacolors=(16,19,33,44)
      		iso.fillareacolors=None

    There are three possibilities for setting the fillarea style (Ex):
		iso.fillareastyle = 'solid'
		iso.fillareastyle = 'hatch'
		iso.fillareastyle = 'pattern'

    There are two ways to set the fillarea hatch or pattern indices (Ex):
      		iso.fillareaindices=([1,3,5,6,9,20])
      		iso.fillareaindices=(7,1,4,9,6,15)
	        See using fillarea objects below!

    Using the fillarea secondary object (Ex):
		f=createfillarea('fill1')
    		To Create a new instance of fillarea use:
     		   fill=a.createisofill('new','quick') # Copies 'quick' to 'new'
                   fill=a.createisofill('new') 	# Copies 'default' to 'new'

             	To Modify an existing isofill use:
                   fill=a.getisofill('def37')

      		iso.fillareaindices=(7,fill,4,9,fill,15) # Set index using fillarea
		fill.list()				 # list fillarea attributes
 		fill.style='hatch'			 # change style
		fill.color=241				 # change color
		fill.index=3				 # change style index

    ext_1='n'
    ext_2='y'
    iso.exts('n', 'y' )  		# Will set them both
"""
    rename=renameGfi # Alias for VCS_Validation_Functions
    __slots__=[
         '__doc__',
         'setmember',
         'parent',
         'name',
         'g_name',
         'xaxisconvert',
         'yaxisconvert',
         'levels',
         'fillareacolors',
         'fillareastyle',
         'fillareaindices',
         'ext_1',
         'ext_2',
         'missing',
         'projection',
         'xticlabels1',
         'xticlabels2',
         'yticlabels1',
         'yticlabels2',
         'xmtics1',
         'xmtics2',
         'ymtics1',
         'ymtics2',
         'datawc_x1',
         'datawc_x2',
         'datawc_y1',
         'datawc_y2',
         'legend',
         'datawc_timeunits',
         'datawc_calendar',
         '_name',
         '_xaxisconvert',
         '_yaxisconvert',
         '_levels',
         '_fillareacolors',
         '_fillareastyle',
         '_fillareaindices',
         '_ext_1',
         '_ext_2',
         '_missing',
         '_projection',
         '_xticlabels1',
         '_xticlabels2',
         '_yticlabels1',
         '_yticlabels2',
         '_xmtics1',
         '_xmtics2',
         '_ymtics1',
         '_ymtics2',
         '_datawc_x1',
         '_datawc_x2',
         '_datawc_y1',
         '_datawc_y2',
         '_legend',
         '_datawc_timeunits',
         '_datawc_calendar',
         'info',
         ]
 
    def _getname(self):
         return self._name
    def _setname(self,value):
         value=VCS_validation_functions.checkname(self,'name',value)
         if value is not None:
              self._name=value
              setmember(self,'name',value)
    name=property(_getname,_setname)

    def _getcalendar(self):
         return self._datawc_calendar
    def _setcalendar(self,value):
         value=VCS_validation_functions.checkCalendar(self,'datawc_calendar',value)
         setmember(self,'datawc_calendar',value)
         self._datawc_calendar=value
    datawc_calendar=property(_getcalendar,_setcalendar)

    def _gettimeunits(self):
         return self._datawc_timeunits
    def _settimeunits(self,value):
         value=VCS_validation_functions.checkTimeUnits(self,'datawc_timeunits',value)
         setmember(self,'datawc_timeunits',value)
         self._datawc_timeunits=value
    datawc_timeunits=property(_gettimeunits,_settimeunits)

    def _getxaxisconvert(self):
         return self._xaxisconvert
    def _setxaxisconvert(self,value):
         value=VCS_validation_functions.checkAxisConvert(self,'xaxisconvert',value)
         self._xaxisconvert=value
         setmember(self,'xaxisconvert',value)
    xaxisconvert=property(_getxaxisconvert,_setxaxisconvert)

    def _getyaxisconvert(self):
         return self._yaxisconvert
    def _setyaxisconvert(self,value):
         value=VCS_validation_functions.checkAxisConvert(self,'yaxisconvert',value)
         self._yaxisconvert=value
         setmember(self,'yaxisconvert',value)
    yaxisconvert=property(_getyaxisconvert,_setyaxisconvert)

    def _getlevels(self):
         return self._levels
    def _setlevels(self,value):
         value=list(VCS_validation_functions.checkListTuple(self,'levels',value))

         if (value[0]<-9.9E19):
             self._ext_1='y'
         else:
             self._ext_1='n'

         if (value[-1]>9.9E19):
            self._ext_2='y'
         else:
            self._ext_2='n'
         self._levels=tuple(value)
         setmember(self,'levels',self._levels)
    levels=property(_getlevels,_setlevels)

    def _getfillareacolors(self):
         return self._fillareacolors
    def _setfillareacolors(self,value):
         if not value is None:
              value = VCS_validation_functions.checkColorList(self,'fillareacolors',value)
         self._fillareacolors=value
         setmember(self,'levels',self.levels)
    fillareacolors=property(_getfillareacolors,_setfillareacolors)

    def _getfillareaindices(self):
         return self._fillareaindices
    def _setfillareaindices(self,value):
         if value is not None:
              value = VCS_validation_functions.checkIndicesList(self,'fillareaindices',value)
         self._fillareaindices=value
         setmember(self,'levels',self.levels)
    fillareaindices=property(_getfillareaindices,_setfillareaindices)

    def _getfillareastyle(self):
         return self._fillareastyle
    def _setfillareastyle(self,value):
         value=VCS_validation_functions.checkFillAreaStyle(self,'fillareastyle',value)
         self._fillareastyle=value
         setmember(self,'levels',self.levels)
    fillareastyle=property(_getfillareastyle,_setfillareastyle)

    def _getext_1(self):
         return self._ext_1
    def _setext_1(self,value):
         do = VCS_validation_functions.checkExt(self,'ext_1',value)
         if do:
              if value=='y' and (-9.9E19<self.levels[0]<9.9E19):
                 returned_levels = add_level_ext_1(self, value)
                 self._ext_1=value
                 self._setlevels(returned_levels)
              elif value=='n':
                 returned_levels = add_level_ext_1(self, value)
                 self._ext_1=value
                 self._setlevels(returned_levels)
              else:
                 self._ext_1=value
    ext_1=property(_getext_1,_setext_1)

    def _getext_2(self):
         return self._ext_2
    def _setext_2(self,value):
         do = VCS_validation_functions.checkExt(self,'ext_2',value)
         if do:
              if value=='y' and (-9.9E19<self.levels[-1]<9.9E19):
                 returned_levels = add_level_ext_2(self, value)
                 self._ext_2=value
                 self._setlevels(returned_levels)
              elif value=='n':
                 returned_levels = add_level_ext_2(self, value)
                 self._ext_2=value
                 self._setlevels(returned_levels)
              else:
                 self._ext_2=value
    ext_2=property(_getext_2,_setext_2)

    def _getmissing(self):
         return self._missing
    def _setmissing(self,value):
         value=VCS_validation_functions.checkColor(self,'missing',value)
         self._missing=value
         setmember(self,'missing',value)
    missing=property(_getmissing,_setmissing)

    def _getlegend(self):
         return self._legend
    def _setlegend(self,value):
         value=VCS_validation_functions.checkLegend(self,'legend',value)
         self._legend=value
         setmember(self,'legend',value)
    legend=property(_getlegend,_setlegend)

    def _getprojection(self):
         return self._projection
    def _setprojection(self,value):
         value=VCS_validation_functions.checkProjection(self,'projection',value)
         self._projection=value
         setmember(self,'projection',value)
    projection=property(_getprojection,_setprojection)

    def _getxticlabels1(self):
         return self._xticlabels1
    def _setxticlabels1(self,value):
         value=VCS_validation_functions.checkStringDictionary(self,'xticlabels1',value)
         self._xticlabels1=value
         setmember(self,'xticlabels1',value)
    xticlabels1=property(_getxticlabels1,_setxticlabels1)

    def _getxticlabels2(self):
         return self._xticlabels2
    def _setxticlabels2(self,value):
         value=VCS_validation_functions.checkStringDictionary(self,'xticlabels2',value)
         self._xticlabels2=value
         setmember(self,'xticlabels2',value)
    xticlabels2=property(_getxticlabels2,_setxticlabels2)

    def _getyticlabels1(self):
         return self._yticlabels1
    def _setyticlabels1(self,value):
         value=VCS_validation_functions.checkStringDictionary(self,'yticlabels1',value)
         self._yticlabels1=value
         setmember(self,'yticlabels1',value)
    yticlabels1=property(_getyticlabels1,_setyticlabels1)

    def _getyticlabels2(self):
         return self._yticlabels2
    def _setyticlabels2(self,value):
         value=VCS_validation_functions.checkStringDictionary(self,'yticlabels2',value)
         self._yticlabels2=value
         setmember(self,'yticlabels2',value)
    yticlabels2=property(_getyticlabels2,_setyticlabels2)

    def _getxmtics1(self):
         return self._xmtics1
    def _setxmtics1(self,value):
         value=VCS_validation_functions.checkStringDictionary(self,'xmtics1',value)
         self._xmtics1=value
         setmember(self,'xmtics1',value)
    xmtics1=property(_getxmtics1,_setxmtics1)

    def _getxmtics2(self):
         return self._xmtics2
    def _setxmtics2(self,value):
         value=VCS_validation_functions.checkStringDictionary(self,'xmtics2',value)
         self._xmtics2=value
         setmember(self,'xmtics2',value)
    xmtics2=property(_getxmtics2,_setxmtics2)

    def _getymtics1(self):
         return self._ymtics1
    def _setymtics1(self,value):
         value=VCS_validation_functions.checkStringDictionary(self,'ymtics1',value)
         self._ymtics1=value
         setmember(self,'ymtics1',value)
    ymtics1=property(_getymtics1,_setymtics1)

    def _getymtics2(self):
         return self._ymtics2
    def _setymtics2(self,value):
         value=VCS_validation_functions.checkStringDictionary(self,'ymtics2',value)
         self._ymtics2=value
         setmember(self,'ymtics2',value)
    ymtics2=property(_getymtics2,_setymtics2)

    def _getdatawc_x1(self):
         if getmember(self,'_tdatawc_x1') :
              return cdtime.reltime(self._datawc_x1,self.datawc_timeunits).tocomp(self.datawc_calendar)
         else:
              return self._datawc_x1
    def _setdatawc_x1(self,value):
         value=VCS_validation_functions.checkDatawc(self,'datawc_x1',value)
         self._datawc_x1=value[0]
         setmember(self,'datawc_x1',value[0])
         setmember(self,'_tdatawc_x1',value[1])
    datawc_x1=property(_getdatawc_x1,_setdatawc_x1)

    def _getdatawc_x2(self):
         if getmember(self,'_tdatawc_x2') :
              return cdtime.reltime(self._datawc_x2,self.datawc_timeunits).tocomp(self.datawc_calendar)
         else:
              return self._datawc_x2
    def _setdatawc_x2(self,value):
         value=VCS_validation_functions.checkDatawc(self,'datawc_x2',value)
         self._datawc_x2=value[0]
         setmember(self,'datawc_x2',value[0])
         setmember(self,'_tdatawc_x2',value[1])
    datawc_x2=property(_getdatawc_x2,_setdatawc_x2)
    
    def _getdatawc_y1(self):
         if getmember(self,'_tdatawc_y1') :
              return cdtime.reltime(self._datawc_y1,self.datawc_timeunits).tocomp(self.datawc_calendar)
         else:
              return self._datawc_y1
    def _setdatawc_y1(self,value):
         value=VCS_validation_functions.checkDatawc(self,'datawc_y1',value)
         self._datawc_y1=value[0]
         setmember(self,'datawc_y1',value[0])
         setmember(self,'_tdatawc_y1',value[1])
    datawc_y1=property(_getdatawc_y1,_setdatawc_y1)

    def _getdatawc_y2(self):
         if getmember(self,'_tdatawc_y2') :
              return cdtime.reltime(self._datawc_y2,self.datawc_timeunits).tocomp(self.datawc_calendar)
         else:
              return self._datawc_y2
    def _setdatawc_y2(self,value):
         value=VCS_validation_functions.checkDatawc(self,'datawc_y2',value)
         self._datawc_y2=value[0]
         setmember(self,'datawc_y2',value[0])
         setmember(self,'_tdatawc_y2',value[1])
    datawc_y2=property(_getdatawc_y2,_setdatawc_y2)


    def __init__(self, parent, Gfi_name=None, Gfi_name_src='default', createGfi=0):
	#                                                         #
        ###########################################################
	# Initialize the isofill class and its members            #
        #							  #
	# The getGfimember function retrieves the values of the   #
        # isofill members in the C structure and passes back the  #
	# appropriate Python Object.                              #
        ###########################################################
	#                                                         #
        self.setmember=setmember
        self.parent=parent
        if (createGfi == 0):
           if (Gfi_name == None):
              raise ValueError, 'Must provide a isofill name.'
           else:
              _vcs_legacy.copyGfi(Gfi_name_src, Gfi_name)
              self._name = Gfi_name
        else:
              self._name = Gfi_name_src
              
        self._projection=getmember(self, 'projection')
        self._xticlabels1=getmember(self, 'xticlabels1')
        self._xticlabels2=getmember(self, 'xticlabels2')
        self._xmtics1=getmember(self, 'xmtics1')
        self._xmtics2=getmember(self, 'xmtics2')
        self._yticlabels1=getmember(self, 'yticlabels1')
        self._yticlabels2=getmember(self, 'yticlabels2')
        self._ymtics1=getmember(self, 'ymtics1')
        self._ymtics2=getmember(self, 'ymtics2')
        self._datawc_y1=getmember(self, 'datawc_y1')
        self._datawc_y2=getmember(self, 'datawc_y2')
        self._datawc_x1=getmember(self, 'datawc_x1')
        self._datawc_x2=getmember(self, 'datawc_x2')
        self.g_name='Gfi'
        self._xaxisconvert=getmember(self, 'xaxisconvert')
        self._yaxisconvert=getmember(self, 'yaxisconvert')
        self._missing=getmember(self, 'missing')
        if self._missing>255:
             self._missing=241
        self._ext_1='n'
        self._ext_2='n'
        self._fillareastyle='solid'
        self._fillareaindices=None
        self._fillareacolors=getmember(self, 'fillareacolors')
        self._levels=getmember(self, 'levels')
        self._legend=getmember(self, 'legend')
        self._datawc_timeunits=getmember(self, 'datawc_timeunits')
        self._datawc_calendar=getmember(self, 'datawc_calendar')
        self.info=AutoAPI.Info(self)
        self.info.expose=['ALL']
        #self.info.hide+=["fillareastyle","fillareaindices"]
        self.__doc__ = self.__doc__ % (xmldocs.graphics_method_core,xmldocs.isofill_doc)
        #                                                         #
        ###########################################################
        # Find and set the isofill structure in VCS C pointer     #
        # list. If the isofill name does not exist, then use      #
        # default isofill.                                        #
        ###########################################################
        #                                                         #


    def colors(self, color1=16, color2=239):
        mode=self.parent.mode
        self.parent.mode=0
        self.fillareacolors=range(color1,color2)
        self.parent.mode=mode
    colors.__doc__ = xmldocs.colorsdoc
    
    def exts(self, ext1='n', ext2='y'):
        mode=self.parent.mode
        self.parent.mode=0
        if (ext2 == self.ext_2):
           self.parent.mode=mode
        self.ext_1 =  ext1
        self.parent.mode=mode
        self.ext_2 = ext2
    exts.__doc__ = xmldocs.extsdoc
# 
# Doesn't make sense to inherit. This would mean more coding in C.
# I put this code back.                                 
#
    def xticlabels(self, xtl1='', xtl2=''):
        mode=self.parent.mode
        self.parent.mode=0
        self.xticlabels1 = xtl1
        self.parent.mode=mode
        self.xticlabels2 = xtl2
    xticlabels.__doc__ = xmldocs.xticlabelsdoc

    def xmtics(self,xmt1='', xmt2=''):
        mode=self.parent.mode
        self.parent.mode=0
        self.xmtics1 = xmt1
        self.parent.mode=mode
        self.xmtics2 = xmt2
    xmtics.__doc__ = xmldocs.xmticsdoc

    def yticlabels(self, ytl1='', ytl2=''):
        mode=self.parent.mode
        self.parent.mode=0
        self.yticlabels1 = ytl1
        self.parent.mode=mode
        self.yticlabels2 = ytl2
    yticlabels.__doc__ = xmldocs.yticlabelsdoc

    def ymtics(self, ymt1='', ymt2=''):
        mode=self.parent.mode
        self.parent.mode=0
        self.ymtics1 = ymt1
        self.parent.mode=mode
        self.ymtics2 = ymt2
    ymtics.__doc__ = xmldocs.ymticsdoc

    def datawc(self, dsp1=1e20, dsp2=1e20, dsp3=1e20, dsp4=1e20):
        mode=self.parent.mode
        self.parent.mode=0
        self.datawc_y1 = dsp1
        self.datawc_y2 = dsp2
        self.datawc_x1 = dsp3
        self.parent.mode=mode
        self.datawc_x2 = dsp4
    datawc.__doc__ = xmldocs.datawcdoc

    def xyscale(self, xat='', yat=''):
        mode=self.parent.mode
        self.parent.mode=0
        self.xaxisconvert = xat
        self.parent.mode=mode
        self.yaxisconvert = yat
    xyscale.__doc__= xmldocs.xyscaledoc

    def list(self):
        if (self.name == '__removed_from_VCS__'):
           raise ValueError, 'This instance has been removed from VCS.'
        print "","----------Isofill (Gfi) member (attribute) listings ----------"
        print 'Canvas Mode =',self.parent.mode
        print "graphics method =", self.g_name
        print "name =", self.name
        print "projection =", self.projection
        print "xticlabels1 =", self.xticlabels1
        print "xticlabels2 =", self.xticlabels2
        print "xmtics1 =", self.xmtics1
        print "xmtics2 =", self.xmtics2
        print "yticlabels1 =", self.yticlabels1
        print "yticlabels2 =", self.yticlabels2
        print "ymtics1 = ", self.ymtics1
        print "ymtics2 = ", self.ymtics2
        print "datawc_x1 =", self.datawc_x1
        print "datawc_y1 = ", self.datawc_y1
        print "datawc_x2 = ", self.datawc_x2
        print "datawc_y2 = ", self.datawc_y2
        print "datawc_timeunits = ", self.datawc_timeunits
        print "datawc_calendar = ", self.datawc_calendar
        print "xaxisconvert = ", self.xaxisconvert
        print "yaxisconvert = ", self.yaxisconvert
        print "missing = ", self.missing
        print "ext_1 = ", self.ext_1
        print "ext_2 = ", self.ext_2
        print "fillareastyle = ", self.fillareastyle
        print "fillareaindices = ", self.fillareaindices
        print "fillareacolors = ", self.fillareacolors
        print "levels = ", self.levels
        print "legend = ", self.legend
    list.__doc__ = xmldocs.listdoc

    ###########################################################################
    #                                                                         #
    # Script out primary isofill graphics method in VCS to a file.            #
    #                                                                         #
    ###########################################################################
    def script(self, script_filename, mode='a'):
        """
%s
Function:     script                           # Calls _vcs_legacy.scriptGfi

 Description of Function:
       Saves out a isofill graphics method in Python or VCS script form to
       a designated file.

 Example of Use:
    script(scriptfile_name, mode)
              where: scriptfile_name is the output name of the script file.
                     mode is either "w" for replace or "a" for append.

              Note: If the the filename has a ".py" at the end, it will produce a
                    Python script. If the filename has a ".scr" at the end, it will
                    produce a VCS script. If neither extensions are give, then by
                    default a Python script will be produced.

    a=vcs_legacy.init()
    iso=a.createisofill('temp')
    iso.script('filename.py')         # Append to a Python file "filename.py"
    iso.script('filename.scr')        # Append to a VCS file "filename.scr"
    iso.script('filename','w')
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
           print _vcs_legacy.scriptGfi(self.name,script_filename,mode)
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

           unique_name = '__Gfi__' + self.name
           fp.write("#----------Isofill (Gfi) member (attribute) listings ----------\n")
           fp.write("gfi_list=v.listelements('isofill')\n")
           fp.write("if ('%s' in gfi_list):\n" % self.name)
           fp.write("   %s = v.getisofill('%s')\n" % (unique_name, self.name))
           fp.write("else:\n")
           fp.write("   %s = v.createisofill('%s')\n" % (unique_name, self.name))
           # Common core graphics method attributes
           fp.write("%s.projection = '%s'\n" % (unique_name, self.projection))
           fp.write("%s.xticlabels1 = '%s'\n" % (unique_name, self.xticlabels1))
           fp.write("%s.xticlabels2 = '%s'\n" % (unique_name, self.xticlabels2))
           fp.write("%s.xmtics1 = '%s'\n" % (unique_name, self.xmtics1))
           fp.write("%s.xmtics2 = '%s'\n" % (unique_name, self.xmtics2))
           fp.write("%s.yticlabels1 = '%s'\n" % (unique_name, self.yticlabels1))
           fp.write("%s.yticlabels2 = '%s'\n" % (unique_name, self.yticlabels2))
           fp.write("%s.ymtics1 = '%s'\n" % (unique_name, self.ymtics1))
           fp.write("%s.ymtics2 = '%s'\n" % (unique_name, self.ymtics2))
           if isinstance(self.datawc_x1,(int,long,float)):
                fp.write("%s.datawc_x1 = %g\n" % (unique_name, self.datawc_x1))
           else:
                fp.write("%s.datawc_x1 = '%s'\n" % (unique_name, self.datawc_x1))
           if isinstance(self.datawc_y1,(int,long,float)):
                fp.write("%s.datawc_y1 = %g\n" % (unique_name, self.datawc_y1))
           else:
                fp.write("%s.datawc_y1 = '%s'\n" % (unique_name, self.datawc_y1))
           if isinstance(self.datawc_x2,(int,long,float)):
                fp.write("%s.datawc_x2 = %g\n" % (unique_name, self.datawc_x2))
           else:
                fp.write("%s.datawc_x2 = '%s'\n" % (unique_name, self.datawc_x2))
           if isinstance(self.datawc_y2,(int,long,float)):
                fp.write("%s.datawc_y2 = %g\n" % (unique_name, self.datawc_y2))
           else:
                fp.write("%s.datawc_y2 = '%s'\n" % (unique_name, self.datawc_y2))
           fp.write("%s.datawc_calendar = %g\n" % (unique_name, self.datawc_calendar))
           fp.write("%s.datawc_timeunits = '%s'\n\n" % (unique_name, self.datawc_timeunits))
           fp.write("%s.xaxisconvert = '%s'\n" % (unique_name, self.xaxisconvert))
           fp.write("%s.yaxisconvert = '%s'\n" % (unique_name, self.yaxisconvert))
           # Unique attribute for isofill
           fp.write("%s.missing = %g\n" % (unique_name, self.missing))
           fp.write("%s.ext_1 = '%s'\n" % (unique_name, self.ext_1))
           fp.write("%s.ext_2 = '%s'\n" % (unique_name, self.ext_2))
           fp.write("%s.fillareastyle = '%s'\n" % (unique_name, self.fillareastyle))
           fp.write("%s.fillareaindices = %s\n" % (unique_name, self.fillareaindices))
           fp.write("%s.fillareacolors = %s\n" % (unique_name, self.fillareacolors))
           fp.write("%s.levels = %s\n" % (unique_name, self.levels))
           fp.write("%s.legend = %s\n" % (unique_name, self.legend))
    script.__doc__ = script.__doc__ % xmldocs.scriptdoc


###############################################################################
#        END OF FILE						              #
###############################################################################
