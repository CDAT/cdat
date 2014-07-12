"""
# Meshfill (Gfm) module
"""
###############################################################################
#									      #
# Module:	meshfill (Gfm) module					      #
#									      #
# Copyright:    2000, Regents of the University of California		      #
#               This software may not be distributed to others without	      #
#               permission of the author.				      #
#									      #
# Author:       PCMDI Software Team                                           #
#               Lawrence Livermore NationalLaboratory:                        #
#               support@pcmdi.llnl.gov                                        #
#									      #
# Description:	Python command wrapper for VCS's meshfill graphics method.    #
#									      #
# Version:      4.0							      #
#									      #
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
import AutoAPI
import xmldocs
import cdtime
from types import *
#### from gm_core import * No need to import

###############################################################################
#                                                                             #
# Function:	setGfmmember                                                  #
#                                                                             #
# Description of Function:                                                    #
# 	Private function to update the VCS canvas plot. If the canvas mode is #
#       set to 0, then this function does nothing.              	      #
#                                                                             #
#                                                                             #
# Example of Use:                                                             #
#      setGfmmember(self,name,value)					      #
#              where: self is the class (e.g., Gfm)                           #
#                     name is the name of the member that is being changed    #
#                     value is the new value of the member (or attribute)     #
#                                                                             #
###############################################################################
def setGfmmember(self,member,value):
     # If the VCS Canvas is displayed, then bring the canvas to the front before 
     # redisplaying the updated contents.
     if (self.parent.mode == 1) and (self.parent.iscanvasdisplayed()):
        Canvas.finish_queued_X_server_requests( self.parent )
        self.parent.canvas.BLOCK_X_SERVER()
        self.parent.canvasraised()

     _vcs_legacy.setGfmmember(self, member, value, self.parent.mode)

     # If the VCS Canvas is displayed, then update the backing store
     if (self.parent.mode == 1) and (self.parent.iscanvasdisplayed()):
        self.parent.flush()
        self.parent.backing_store()
        self.parent.canvas.UNBLOCK_X_SERVER()

#################################################################################
#                                                                               #
# Function:     getGfmmember                                                    #
#                                                                               #
# Description of Function:                                                      #
#       Private function that retrieves the meshfill members from the C         #
#       structure and passes it back to Python.                                 #
#                                                                               #
#                                                                               #
# Example of Use:                                                               #
#      return_value =								#
#      getGfmmember(self,name)                                                  #
#              where: self is the class (e.g., Gfm)                             #
#                     name is the name of the member that is being found        #
#                                                                               #
#################################################################################

setmember=setGfmmember
     
def getGfmmember(self,member):
     return _vcs_legacy.getGfmmember(self,member)

getmember=getGfmmember

#################################################################################
#                                                                               #
# Function:     renameGfm                                                       #
#                                                                               #
# Description of Function:                                                      #
#       Private function that renames the name of an existing meshfill           #
#       graphics method.                                                        #
#                                                                               #
#                                                                               #
# Example of Use:                                                               #
#      renameGfm(old_name, new_name)                                            #
#              where: old_name is the current name of meshfill graphics method   #
#                     new_name is the new name for the meshfill graphics method  #
#                                                                               #
################################################################################
def renameGfm(self, old_name, new_name):
     return _vcs_legacy.renameGfm(old_name, new_name)


#################################################################################
#                                                                               #
# Function:     add_level_ext_1                                                 #
#                                                                               #
# Description of Function:                                                      #
#       Private function that adds the extension triangle to the left of the    #
#       legend on the plot                                                      #
#                                                                               #
#                                                                               #
# Example of Use:                                                               #
#      add_level_ext_1(self, ext_value)                                         #
#              where: self is the class (e.g., Gfi)                             #
#                     ext_value is either 'n' to remove the triangle on the     #
#		     	legend or 'y' to show the triangle on the triangle      #
#                                                                               #
#################################################################################
def add_level_ext_1(self, ext_value):
    if ((ext_value == 'n') and (self.ext_1 == 'y')): # remove extension
       if (type(self.levels[0]) == ListType): # remove from tuple of lists
          self.levels[0].remove(self.levels[0][0])
          return None
       if (type(self.levels) == TupleType):       # remove from list
          ret_tup = []
          for i in range(len(self.levels)):
             ret_tup.insert(i+1,self.levels[i])
          ret_tup.remove(self.levels[0])
          self.levels = ret_tup
          return None

    if (type(self.levels) == TupleType):
       if (type(self.levels[0]) == ListType): # add to tuple of lists
          self.levels[0].insert(0,1e20)
          return None
       else:                                  # must be a mutable tuple
          ret_tup = [1e20]		      # therefore, covert to a list
          for i in range(len(self.levels)):   # then add extension to list
             ret_tup.insert(i+1,self.levels[i])
          self.levels=ret_tup
          return None
    if (type(self.levels) == ListType):       # add extension to list
          if (((self.levels[(len(self.levels)-1)] - self.levels[0])) >= 0):
             self.levels.insert(0,-1e20)
          else:
             self.levels.insert(0,1e20)
          return None

#################################################################################
#                                                                               #
# Function:     add_level_ext_2                                                 #
#                                                                               #
# Description of Function:                                                      #
#       Private function that adds the extension triangle to the right of the   #
#       legend on the plot                                                      #
#                                                                               #
#                                                                               #
# Example of Use:                                                               #
#      add_level_ext_2(self, ext_value)                                         #
#              where: self is the class (e.g., Gfi)                             #
#                     ext_value is either 'n' to remove the triangle on the     # 
#                       legend or 'y' to show the triangle on the triangle      #
#                                                                               #
#################################################################################
def add_level_ext_2(self, ext_value):
    if ((ext_value == 'n') and (self.ext_2 == 'y')): # remove extension
       if (type(self.levels[0]) == ListType): # remove from tuple of lists
          last=len(self.levels) - 1
          last2=len(self.levels[last]) - 1
          self.levels[last].remove(self.levels[last][last2])
          return None
       if (type(self.levels) == TupleType):       # remove from list
          ret_tup = []		      	
          for i in range(len(self.levels)-1):
             ret_tup.insert(i+1,self.levels[i])
          self.levels=ret_tup
          return None

    if (type(self.levels) == TupleType):
       last=len(self.levels) - 1
       if (type(self.levels[last]) == ListType): # add to tuple of lists
          self.levels[last].append(1e20)
          return None
       else:                                  # must be a mutable tuple
          ret_tup = []			      # therefore, covert to a list
          for i in range(len(self.levels)):   # then add extension to list
             ret_tup.insert(i,self.levels[i])
          ret_tup.insert(i+1,1e20)
          self.levels=ret_tup
          return None
    if (type(self.levels) == ListType):       # add extension to list
          if (((self.levels[(len(self.levels)-1)] - self.levels[0])) > 0):
             self.levels.insert(len(self.levels),1e20)
          else:
             self.levels.insert(len(self.levels),-1e20)
          return None


#############################################################################
#                                                                           #
# Meshfill (Gfm) graphics method Class.                                      #
#                                                                           #
#############################################################################
#class Gfm(graphics_method_core):
class Gfm(object,AutoAPI.AutoAPI):
    """
    Options:::
%s
%s
mesh :: (str/int) (0) Draws the mesh
wrap :: ([float,float]) ([0.,0.]) Modulo to wrap around on either axis (automatically sets to 360 for longiutde axes)
:::

Class:	Gfm                       	# Meshfill 
                                               
 Description of Gfm Class:                          
    The meshfill graphics method (Gfm) displays a two-dimensional data array
    by surrounding each data value by a colored grid mesh.
                                                          
    This class is used to define a meshfill table entry used in VCS, or it
    can be used to change some or all of the attributes in an existing
    meshfill table entry
    .                                 
 Other Useful Functions:                    
               a=vcs_legacy.init()             # Constructor                       
               a.show('meshfill')        # Show predefined meshfill graphics methods
               a.setcolormap("AMIP")    # Change the VCS color map
               a.meshfill(s,b,'default') # Plot data 's' with meshfill 'b' and 
                                               'default' template
               a.update()               # Updates the VCS Canvas at user's request
               a.mode=1, or 0           # If 1, then automatic update, else if
                                          0, then use the update function to
                                          update the VCS Canvas.   
                                                              
 Example of Use:                                               
    a=vcs_legacy.init()                                              
    To Create a new instance of meshfill use:                   
     mesh=a.createmeshfill('new','quick') # Copies content of 'quick' to 'new' 
     mesh=a.createmeshfill('new') 	# Copies content of 'default' to 'new'  

    To Modify an existing meshfill use:                    
     mesh=a.getmeshfill('AMIP_psl')                          

    mesh.list()  			# Will list all the meshfill attribute values
    mesh.projection='linear'                             
    lon30={-180:'180W',-150:'150W',0:'Eq'}               
    mesh.xticlabels1=lon30                                 
    mesh.xticlabels2=lon30                                   
    mesh.xticlabels(lon30, lon30)  	# Will set them both	
    mesh.xmtics1=''                              
    mesh.xmtics2=''                               
    mesh.xmtics(lon30, lon30)  		# Will set them both
    mesh.yticlabels1=lat10                           
    mesh.yticlabels2=lat10                            
    mesh.yticlabels(lat10, lat10)  	# Will set them both
    mesh.ymtics1=''                                        
    mesh.ymtics2=''                                  
    mesh.ymtics(lat10, lat10)  		# Will set them both	
    mesh.datawc_y1=-90.0                             
    mesh.datawc_y2=90.0                               
    mesh.datawc_x1=-180.0                              
    mesh.datawc_x2=180.0                                
    mesh.datawc(-90, 90, -180, 180)  	# Will set them all
    There are two possibilities for setting the meshfill levels:
     A) Levels are all contiguous (Examples):
		mesh.levels=([0,20,25,30,35,40],)
		mesh.levels=([0,20,25,30,35,40,45,50])
		mesh.levels=[0,20,25,30,35,40]
		mesh.levels=(0.0,20.0,25.0,30.0,35.0,40.0,50.0)
     B) Levels are not contiguous (Examples):
		mesh.levels=([0,20],[30,40],[50,60])
		mesh.levels=([0,20,25,30,35,40],[30,40],[50,60])

    There are three possibilities for setting the fillarea color indices (Ex):
      		mesh.fillareacolors=([22,33,44,55,66,77])
		mesh.fillareacolors=(16,19,33,44)
      		mesh.fillareacolors=None

    There are three possibilities for setting the fillarea style (Ex):
		mesh.fillareastyle = 'solid'
		mesh.fillareastyle = 'hatch'
		mesh.fillareastyle = 'pattern'

    There are two ways to set the fillarea hatch or pattern indices (Ex):
      		mesh.fillareaindices=([1,3,5,6,9,20])
      		mesh.fillareaindices=(7,1,4,9,6,15)
	        See using fillarea objects below!

    Using the fillarea secondary object (Ex):
		f=createfillarea('fill1')
    		To Create a new instance of fillarea use:
     		   fill=a.createfillarea('new','quick') # Copies 'quick' to 'new'
                   fill=a.createfillarea('new') 	# Copies 'default' to 'new'

             	To Modify an existing fillarea use:
                   fill=a.getmfillarea('def37')

      		mesh.fillareaindices=(7,fill,4,9,fill,15) # Set index using fillarea
		fill.list()				 # list fillarea attributes
 		fill.style='hatch'			 # change style
		fill.color=241				 # change color
		fill.index=3				 # change style index

    ext_1='n'
    ext_2='y'
    mesh.exts('n', 'y' )  		# Will set them both
    missing=241                         # Color index value range 0 to 255
"""
    rename=renameGfm # Alias for VCS_Validation_Functions
    #############################################################################
    #                                                                           #
    # Initialize the meshfill attributes.                                        #
    #                                                                           #
    #############################################################################
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
         'wrap',
         'mesh',
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
         '_wrap',
         '_mesh',
         '_legend',
         '_datawc_timeunits',
         '_datawc_calendar',
         ]
 
    def _getname(self):
         return self._name
    def _setname(self,value):
         value=VCS_validation_functions.checkname(self,'name',value)
         if value is not None:
              self._name=value
              setmember(self,'name',value)
    name=property(_getname,_setname)

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
         value=VCS_validation_functions.checkListTuple(self,'levels',value)
         self._levels=tuple(value)
         setmember(self,'levels',self._levels)
    levels=property(_getlevels,_setlevels)

    def _getfillareacolors(self):
         return self._fillareacolors
    def _setfillareacolors(self,value):
         if not value is None:
              value = list(VCS_validation_functions.checkListTuple(self,'fillareacolors',value))
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
              add_level_ext_1(self, value)
              self._ext_1=value
    
    ext_1=property(_getext_1,_setext_1)

    def _getext_2(self):
         return self._ext_2
    def _setext_2(self,value):
         do = VCS_validation_functions.checkExt(self,'ext_2',value)
         if do:
              add_level_ext_2(self, value)
              self._ext_2=value
    ext_2=property(_getext_2,_setext_2)

    def _getmissing(self):
         return self._missing
    def _setmissing(self,value):
         value=VCS_validation_functions.checkColor(self,'missing',value)
         self._missing=value
         setmember(self,'missing',value)
    missing=property(_getmissing,_setmissing)

    def _getmesh(self):
         return self._mesh
    def _setmesh(self,value):
         value=VCS_validation_functions.checkOnOff(self,'mesh',value)
         self._mesh=value
         setmember(self,'mesh',value)
    mesh=property(_getmesh,_setmesh)

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

    def _getwrap(self):
         return self._wrap
    def _setwrap(self,value):
         value=VCS_validation_functions.checkWrap(self,'wrap',value)
         self._wrap=value
         setmember(self,'wrap',value)         
    wrap=property(_getwrap,_setwrap)
    
    def __init__(self, parent, Gfm_name=None, Gfm_name_src='default', createGfm=0):
	#                                                         #
        ###########################################################
	# Initialize the meshfill class and its members            #
        #							  #
	# The getGfmmember function retrieves the values of the   #
        # meshfill members in the C structure and passes back the  #
	# appropriate Python Object.                              #
        ###########################################################
	#                                                         #
        self.setmember=setmember
        self.parent=parent
        if (createGfm == 0):
           if (Gfm_name == None):
              raise ValueError, 'Must provide a meshfill name.'
           else:
              _vcs_legacy.copyGfm(Gfm_name_src, Gfm_name)
              self._name = Gfm_name
        else:
              self._name=Gfm_name_src
	#                                                         #
        ###########################################################
        # Inherits core graphics method attributes.		  #
        ###########################################################
	#                                                         #
	# graphics_method_core.__init__(self, parent)
        # Doesn't make sense to inherit. This would mean writing more code
        self._projection=getGfmmember(self, 'projection')
        self._xticlabels1=getGfmmember(self, 'xticlabels1')
        self._xticlabels2=getGfmmember(self, 'xticlabels2')
        self._xmtics1=getGfmmember(self, 'xmtics1')
        self._xmtics2=getGfmmember(self, 'xmtics2')
        self._yticlabels1=getGfmmember(self, 'yticlabels1')
        self._yticlabels2=getGfmmember(self, 'yticlabels2')
        self._ymtics1=getGfmmember(self, 'ymtics1')
        self._ymtics2=getGfmmember(self, 'ymtics2')
        self._datawc_y1=getGfmmember(self, 'datawc_y1')
        self._datawc_y2=getGfmmember(self, 'datawc_y2')
        self._datawc_x1=getGfmmember(self, 'datawc_x1')
        self._datawc_x2=getGfmmember(self, 'datawc_x2')
	# End Core Graphics Method attributes
        self.g_name='Gfm'
        self._xaxisconvert=getGfmmember(self, 'xaxisconvert')
        self._yaxisconvert=getGfmmember(self, 'yaxisconvert')
        self._fillareastyle='solid'
        self._fillareaindices=None
        self._fillareacolors=None
##         self._levels=tuple(getGfmmember(self, 'levels'))
        self._levels=getGfmmember(self, 'levels')
        self._ext_1='n'
        self._ext_2='n'
        self._missing=getGfmmember(self, 'missing')
        self._wrap=getGfmmember(self, 'wrap')
        self._mesh=getGfmmember(self, 'mesh')
        self._legend=getGfmmember(self, 'legend')
        self._datawc_timeunits=getGfmmember(self, 'datawc_timeunits')
        self._datawc_calendar=getGfmmember(self, 'datawc_calendar')
        self.info=AutoAPI.Info(self)
        self.info.expose=['ALL']
        #self.info.hide+=["fillareastyle","fillareaindices"]
        self.__doc__ = self.__doc__ % (xmldocs.graphics_method_core,xmldocs.isofill_doc)
        #                                                         #
        ###########################################################
        # Find and set the meshfill structure in VCS C pointer     #
        # list. If the meshfill name does not exist, then use      #
        # default meshfill.                                        #
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
        self.ext_1=ext1
        self.ext_2=ext2
        self.parent.mode=mode
    exts.__doc__ = xmldocs.extsdoc

# 
# Doesn't make sense to inherit. This would mean more coding in C.
# I put this code back.                                 
#
    def xticlabels(self, xtl1='', xtl2=''):
        mode=self.parent.mode
        self.parent.mode=0
        self.xticlabels1=xtl1
        self.xticlabels2=xtl2
        self.parent.mode=mode
    xticlabels.__doc__ = xmldocs.xticlabelsdoc

    def xmtics(self,xmt1='', xmt2=''):
        mode=self.parent.mode
        self.parent.mode=0
        self.xmtics1=xmt1
        self.xmtics2=xmt2
        self.parent.mode=mode
    xmtics.__doc__ = xmldocs.xmticsdoc

    def yticlabels(self, ytl1='', ytl2=''):
        mode=self.parent.mode
        self.parent.mode=0
        self.yticlabels1=ytl1
        self.yticlabels2=ytl2
        self.parent.mode=mode
    yticlabels.__doc__ = xmldocs.yticlabelsdoc

    def ymtics(self, ymt1='', ymt2=''):
        mode=self.parent.mode
        self.parent.mode=0
        self.ymtics1=ymt1
        self.ymtics2=ymt2
        self.parent.mode=mode
    ymtics.__doc__ = xmldocs.ymticsdoc

    def datawc(self, dsp1=1e20, dsp2=1e20, dsp3=1e20, dsp4=1e20):
        mode=self.parent.mode
        self.parent.mode=0
        self.datawc_y1=dsp1
        self.datawc_y2=dsp2
        self.datawc_x1=dsp3
        self.datawc_x2=dsp4
        self.parent.mode=mode
    datawc.__doc__ = xmldocs.datawcdoc

    def xyscale(self, xat='', yat=''):
        mode=self.parent.mode
        self.parent.mode=0
        self.xaxisconvert=xat
        self.yaxisconvert=yat
        self.parent.mode=mode
    xyscale.__doc__= xmldocs.xyscaledoc

    #############################################################################
    #                                                                           #
    # List out meshfill graphics method members (attributes).                    #
    #                                                                           #
    #############################################################################
    def list(self):
        if (self.name == '__removed_from_VCS__'):
           raise ValueError, 'This instance has been removed from VCS.'
        print ' ---------- Meshfill (Gmf) member (attribute) listings ---------'
        print 'graphics method =',self.g_name
        print 'name =',self.name
        print 'projection =',self.projection
        print 'xticlabels1 =',self.xticlabels1
        print 'xticlabels2 =',self.xticlabels2
        print 'xmtics1 =',self.xmtics1
        print 'xmtics2 =',self.xmtics2
        print 'yticlabels1 =',self.yticlabels1
        print 'yticlabels2 =',self.yticlabels2
        print 'ymtics1 =',self.ymtics1
        print 'ymtics2 =',self.ymtics2
        print 'datawc_x1 =',self.datawc_x1
        print 'datawc_y1 =',self.datawc_y1
        print 'datawc_x2 =',self.datawc_x2
        print 'datawc_y2 =',self.datawc_y2
        print "datawc_timeunits = ", self.datawc_timeunits
        print "datawc_calendar = ", self.datawc_calendar
        print 'xaxisconvert =',self.xaxisconvert
        print 'yaxisconvert =',self.yaxisconvert
        print 'levels =',self.levels
        print 'fillareacolors =',self.fillareacolors
        print 'fillareastyle =',self.fillareastyle
        print 'fillareaindices =',self.fillareaindices
        print 'legend =',self.legend
        print 'ext_1 =',self.ext_1
        print 'ext_2 =',self.ext_2
        print 'missing =',self.missing
        print 'mesh =',self.mesh
        print 'wrap =',self.wrap
        return
    list.__doc__ = xmldocs.listdoc


    #############################################################################
    #                                                                           #
    # Script out primary meshfill graphics method in VCS to a file.              #
    #                                                                           #
    #############################################################################
    def script(self, script_filename, mode='a'):
         """
         %s
         Function:     script				# Calls _vcs_legacy.scriptGfm

         Description of Function:                                                      
              Saves out a meshfill graphics method in Python or VCS script form to a
              designated file.

         Example of Use:                                                               
              script(scriptfile_name, mode)                                           
                   where: scriptfile_name is the output name of the script file.
                        mode is either 'w' for replace or 'a' for append.

                   Note: If the the filename has a '.py' at the end, it will produce a
                        Python script. If the filename has a '.scr' at the end, it will
                        produce a VCS script. If neither extensions are give, then by
                        default a Python script will be produced.

              a=vcs_legacy.init()
              mesh=a.createmeshfill('temp')
              mesh.script('filename.py')         # Append to a Python file 'filename.py'
              mesh.script('filename.scr')        # Append to a VCS file 'filename.scr'
              mesh.script('filename','w')
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
              print _vcs_legacy.scriptGfm(self.name,script_filename,mode)
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

              unique_name = '__Gfm__' + self.name
              fp.write("#----------Meshfill (Gfm) member (attribute) listings ----------\n")
              fp.write("gfm_list=v.listelements('meshfill')\n")
              fp.write("if ('%s' in gfm_list):\n" % self.name)
              fp.write("   %s = v.getmeshfill('%s')\n" % (unique_name, self.name))
              fp.write("else:\n")
              fp.write("   %s = v.createmeshfill('%s')\n" % (unique_name, self.name))
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
              # Unique attribute for meshfill
              fp.write("%s.ext_1 = '%s'\n" % (unique_name, self.ext_1))
              fp.write("%s.ext_2 = '%s'\n" % (unique_name, self.ext_2))
              fp.write("%s.levels = '%s'\n" % (unique_name, self.levels))
              fp.write("%s.fillareacolors = '%s'\n" % (unique_name, self.fillareacolors))
              fp.write("%s.fillareastyle = '%s'\n" % (unique_name, self.fillareastyle))
              fp.write("%s.fillareindices = '%s'\n" % (unique_name, self.fillareaindices))
              fp.write("%s.legend = '%s'\n" % (unique_name, self.legend))
              fp.write("%s.mesh = '%s'\n" % (unique_name, self.mesh))
              fp.write("%s.wrap = '%s'\n" % (unique_name, self.wrap))
              fp.write("%s.missing = %g\n\n" % (unique_name, self.missing))
              pass
         return
    script.__doc__ = script.__doc__ % xmldocs.scriptdoc
    
      
           
#################################################################################
#        END OF FILE								#
#################################################################################
