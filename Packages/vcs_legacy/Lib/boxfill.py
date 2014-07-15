"""
# Boxfill (Gfb) module
"""
###############################################################################
#									      #
# Module:	boxfill (Gfb) module					      #
#									      #
# Copyright:    2000, Regents of the University of California		      #
#               This software may not be distributed to others without	      #
#               permission of the author.				      #
#									      #
# Author:       PCMDI Software Team                                           #
#               Lawrence Livermore NationalLaboratory:                        #
#               support@pcmdi.llnl.gov                                        #
#									      #
# Description:	Python command wrapper for VCS's boxfill graphics method.     #
#									      #
# Version:      5.0							      #
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
import vcs_legacy
import Canvas
import isofill
import cdtime
from types import *
import VCS_validation_functions
import AutoAPI
import xmldocs
#### from gm_core import * No need to import

###############################################################################
#                                                                             #
# Function:	setGfbmember                                                  #
#                                                                             #
# Description of Function:                                                    #
# 	Private function to update the VCS canvas plot. If the canvas mode is #
#       set to 0, then this function does nothing.              	      #
#                                                                             #
#                                                                             #
# Example of Use:                                                             #
#      setGfbmember(self,name,value)					      #
#              where: self is the class (e.g., Gfb)                           #
#                     name is the name of the member that is being changed    #
#                     value is the new value of the member (or attribute)     #
#                                                                             #
###############################################################################
def setGfbmember(self,member,value):
     # If the VCS Canvas is displayed, then bring the canvas to the front before 
     # redisplaying the updated contents.
     if (self.parent.mode == 1) and (self.parent.iscanvasdisplayed()): 
        Canvas.finish_queued_X_server_requests( self.parent )
        self.parent.canvas.BLOCK_X_SERVER()
        self.parent.canvasraised()

     _vcs_legacy.setGfbmember(self, member, value, self.parent.mode)

     # If the VCS Canvas is displayed, then update the backing store
     if (self.parent.mode == 1) and (self.parent.iscanvasdisplayed()): 
        self.parent.flush()
        self.parent.backing_store()
        self.parent.canvas.UNBLOCK_X_SERVER()

setmember=setGfbmember

###############################################################################
#                                                                             #
# Function:     getGfbmember                                                  #
#                                                                             #
# Description of Function:                                                    #
#       Private function that retrieves the boxfill members from the C        #
#       structure and passes it back to Python.                               #
#                                                                             #
#                                                                             #
# Example of Use:                                                             #
#      return_value =							      #
#      getGfbmember(self,name)                                                #
#              where: self is the class (e.g., Gfb)                           #
#                     name is the name of the member that is being found      #
#                                                                             #
###############################################################################
def getGfbmember(self,member):
     return _vcs_legacy.getGfbmember(self,member)

###############################################################################
#                                                                             #
# Function:     renameGfb                                                     #
#                                                                             #
# Description of Function:                                                    #
#       Private function that renames the name of an existing boxfill         #
#       graphics method.                                                      #
#                                                                             #
#                                                                             #
# Example of Use:                                                             #
#      renameGfb(old_name, new_name)                                          #
#              where: old_name is the current name of boxfill graphics method #
#                     new_name is the new name for the boxfill graphics method#
#                                                                             #
###############################################################################
def renameGfb(self, old_name, new_name):
     return _vcs_legacy.renameGfb(old_name, new_name)

#############################################################################
#                                                                           #
# Boxfill (Gfb) graphics method Class.                                      #
#                                                                           #
#############################################################################
#class Gfb(graphics_method_core):
class Gfb(object,AutoAPI.AutoAPI):
    """
    Options:::
    %s
    boxfill_type :: (str) ('linear') type of boxfill legend linear/log10 (i.e using color_1/2 and level_1/2 as limits) or custom (similar to isofill method, using levels and colors to set intervals)
    level_1 :: (float) (1.E20) used in conjunction with boxfill_type linear/log10, sets the value of the legend's first level
    level_2 :: (float) (1.E20) used in conjunction with boxfill_type linear/log10, sets the value of the legend's end level
    color_1 :: (float) (1.E20) used in conjunction with boxfill_type linear/log10, sets the legend's color range first value
    color_2 :: (float) (1.E20) used in conjunction with boxfill_type linear/log10, sets the legend's color range lasst value
    levels :: ([float,...]/[[float,float],...]) (([1.E20,1.E20],)) used in conjunction for boxfill_type custom, sets the levels range to use, can be either a list of contiguous levels, or list of tuples indicating, first and last value of the range
    fillareacolors :: (list) (None) used in conjunction for boxfill_type custom, colors to use for each level
    legend :: ({float:str}) (None) used in conjunction with boxfill_type linear/log10, replaces the legend values in the dictionary keys with their associated string
    ext_1 :: (str) ('n') draws an extension arrow on right side (values less than first range value)
    ext_2 :: (str) ('n') draws an extension arrow on left side (values greater than last range value)
    missing :: (int) (241) color to use for missing value or values not in defined ranges
   :::
 Class:	Gfb                       	# Boxfill 
                                               
 Description of Gfb Class:                          
    The boxfill graphics method (Gfb) displays a two-dimensional data array
    by surrounding each data value by a colored grid box.
                                                          
    This class is used to define a boxfill table entry used in VCS, or it
    can be used to change some or all of the attributes in an existing
    boxfill table entry.                                 

 Other Useful Functions:                    
               a=vcs_legacy.init()             # Constructor                       
               a.show('boxfill')        # Show predefined boxfill graphics methods
               a.setcolormap("AMIP")    # Change the VCS color map
               a.boxfill(s,b,'default') # Plot data 's' with boxfill 'b' and 
                                               'default' template
               a.update()               # Updates the VCS Canvas at user's request
               a.mode=1, or 0           # If 1, then automatic update, else if
                                          0, then use the update function to
                                          update the VCS Canvas.   
                                                              
 Example of Use:                                               
    a=vcs_legacy.init()                                              
    To Create a new instance of boxfill use:                   
     box=a.createboxfill('new','quick') # Copies content of 'quick' to 'new' 
     box=a.createboxfill('new') 	# Copies content of 'default' to 'new'  

    To Modify an existing boxfill use:                    
     box=a.getboxfill('AMIP_psl')                          

    box.list()  			# Will list all the boxfill attribute values
    box.projection='linear'                             
    lon30={-180:'180W',-150:'150W',0:'Eq'}               
    box.xticlabels1=lon30                                 
    box.xticlabels2=lon30                                   
    box.xticlabels(lon30, lon30)  	# Will set them both	
    box.xmtics1=''                              
    box.xmtics2=''                               
    box.xmtics(lon30, lon30)  		# Will set them both
    box.yticlabels1=lat10                           
    box.yticlabels2=lat10                            
    box.yticlabels(lat10, lat10)  	# Will set them both
    box.ymtics1=''                                        
    box.ymtics2=''                                  
    box.ymtics(lat10, lat10)  		# Will set them both	
    box.datawc_y1=-90.0                             
    box.datawc_y2=90.0                               
    box.datawc_x1=-180.0                              
    box.datawc_x2=180.0                                
    box.datawc(-90, 90, -180, 180)  	# Will set them all 
    box.xaxisconvert='linear'                                   
    box.yaxisconvert='linear'                                    
    box.xyscale('linear', 'area_wt')  	# Will set them both	
    box.level_1=1e20                                    
    box.level_2=1e20                                     
    box.color_1=16                                           
    box.color_2=239                                           
    box.colors(16, 239 )  		# Will set them both	
    box.boxfill_type='linear'		# 'linear' - compute or specify legend
                                        # 'log10' - plot using log10,
                                        # 'custom' - use custom values to display legend evenly
    box.legend=None                     # Hold the legend values 
    box.ext_1='n'                       # Show left overflow arrow
    box.ext_2='y'                       # Show right overflow arrow 
    box.exts('n', 'y' )  		# Will set them both	
    box.missing=241                     # Color index value range 0 to 255

    There are two possibilities for setting the boxfill levels:
     A) Levels are all contiguous (Examples):
                box.levels=([0,20,25,30,35,40],)
                box.levels=([0,20,25,30,35,40,45,50])
                box.levels=[0,20,25,30,35,40]
                box.levels=(0.0,20.0,25.0,30.0,35.0,40.0,50.0)
     B) Levels are not contiguous (Examples):
                box.levels=([0,20],[30,40],[50,60])
                box.levels=([0,20,25,30,35,40],[30,40],[50,60])

    There are three possibilities for setting the fillarea color indices (Ex):
                box.fillareacolors=([22,33,44,55,66,77])
                box.fillareacolors=(16,19,33,44)
                box.fillareacolors=None

    There are three possibilities for setting the fillarea style (Ex):
                box.fillareastyle = 'solid'
                box.fillareastyle = 'hatch'
                box.fillareastyle = 'pattern'

    There are two ways to set the fillarea hatch or pattern indices (Ex):
                box.fillareaindices=([1,3,5,6,9,20])
                box.fillareaindices=(7,1,4,9,6,15)
                See using fillarea objects below!

    Using the fillarea secondary object (Ex):
                f=createfillarea('fill1')
                To Create a new instance of fillarea use:
                   fill=a.createfillarea('new','quick') # Copies 'quick' to 'new'
                   fill=a.createfillarea('new')  # Copies 'default' to 'new'

                To Modify an existing boxfill use:
                   fill=a.getboxfill('def37')

                box.fillareaindices=(7,fill,4,9,fill,15) # Set index using fillarea
                fill.list()                              # list fillarea attributes
                fill.style='hatch'                       # change style
                fill.color=241                           # change color
                fill.index=3                             # change style index
"""
    rename=renameGfb # Alias for VCS_Validation_Functions
    __slots__=[
         '__doc__',
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
         'boxfill_type',
         'color_1',
         'color_2',
         'level_1',
         'level_2',
         'legend',
         'boxfill_type',
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
         '_boxfill_type',
         '_color_1',
         '_color_2',
         '_level_1',
         '_level_2',
         '_datawc_timeunits',
         '_datawc_calendar',
         'info',
         ]

### Removed from doc string
##     box.levels(10, 90)  		# Will set them both		

    ###########################################################################
    #                                                                         #
    # Initialize the boxfill attributes.                                      #
    #                                                                         #
    ###########################################################################
    def __init__(self, parent, Gfb_name=None, Gfb_name_src='default', createGfb=0):
	#                                                         #
        ###########################################################
	# Initialize the boxfill class and its members            #
        #							  #
	# The getGfbmember function retrieves the values of the   #
        # boxfill members in the C structure and passes back the  #
	# appropriate Python Object.                              #
        ###########################################################
	#                                                         #
        if (createGfb == 0):
           if (Gfb_name == None):
              raise ValueError, 'Must provide a boxfill name.'
           else:
              _vcs_legacy.copyGfb(Gfb_name_src, Gfb_name)
              self._name = Gfb_name
        else:
              self._name=Gfb_name_src
	#                                                         #
        ###########################################################
        # Inherits core graphics method attributes.		  #
        ###########################################################
	#                                                         #
        self._projection=getGfbmember(self, 'projection')
        self._xticlabels1=getGfbmember(self, 'xticlabels1')
        self._xticlabels2=getGfbmember(self, 'xticlabels2')
        self._xmtics1=getGfbmember(self, 'xmtics1')
        self._xmtics2=getGfbmember(self, 'xmtics2')
        self._yticlabels1=getGfbmember(self, 'yticlabels1')
        self._yticlabels2=getGfbmember(self, 'yticlabels2')
        self._ymtics1=getGfbmember(self, 'ymtics1')
        self._ymtics2=getGfbmember(self, 'ymtics2')
        self._datawc_y1=getGfbmember(self, 'datawc_y1')
        self._datawc_y2=getGfbmember(self, 'datawc_y2')
        self._datawc_x1=getGfbmember(self, 'datawc_x1')
        self._datawc_x2=getGfbmember(self, 'datawc_x2')
	# End Core Graphics Method attributes
        self.g_name='Gfb'
        self._xaxisconvert=getGfbmember(self, 'xaxisconvert')
        self._yaxisconvert=getGfbmember(self, 'yaxisconvert')
        self._ext_1='n'
        self._ext_2='n'
        self._missing=getGfbmember(self, 'missing')
        self._fillareastyle='solid'
        self._fillareaindices=None
        self._fillareacolors=None
        self._levels=getGfbmember(self, 'levels')
        self._level_1=getGfbmember(self, 'level_1')
        self._level_2=getGfbmember(self, 'level_2')
        self._color_1=getGfbmember(self, 'color_1')
        self._color_2=getGfbmember(self, 'color_2')
        self._boxfill_type=getGfbmember(self, 'boxfill_type')
        self._datawc_timeunits=None
        self._datawc_calendar=cdtime.DefaultCalendar
        self._legend=getGfbmember(self, 'legend')
        self._datawc_timeunits=getGfbmember(self, 'datawc_timeunits')
        self._datawc_calendar=getGfbmember(self, 'datawc_calendar')
        #                                                         #
        ###########################################################
        # Find and set the boxfill structure in VCS C pointer     #
        # list. If the boxfill name does not exist, then use      #
        # default boxfill.                                        #
        ###########################################################
        #                                                         #
        self.parent=parent
        self.info=AutoAPI.Info(self)
        self.info.expose=['ALL']
        self.info.hide+=["fillareastyle","fillareaindices"]
        self.__doc__ = self.__doc__ % xmldocs.graphics_method_core


    ###########################################################################
    #                                                                         #
    # Set boxfill attributes.                                                 #
    #                                                                         #
    ###########################################################################

    def _getcalendar(self):
         return self._datawc_calendar
    def _setcalendar(self,value):
         value=VCS_validation_functions.checkCalendar(self,'datawc_calendar',value)
         setGfbmember(self,'datawc_calendar',value)
         self._datawc_calendar=value
    datawc_calendar=property(_getcalendar,_setcalendar)

    def _gettimeunits(self):
         return self._datawc_timeunits
    def _settimeunits(self,value):
         value=VCS_validation_functions.checkTimeUnits(self,'datawc_timeunits',value)
         setGfbmember(self,'datawc_timeunits',value)
         self._datawc_timeunits=value
    datawc_timeunits=property(_gettimeunits,_settimeunits)
    
    def _getboxfilltype(self):
         return self._boxfill_type
    def _setboxfilltype(self,value):
         value=VCS_validation_functions.checkBoxfillType(self,'boxfill_type',value)
         setGfbmember(self,'boxfill_type',value)
         self._boxfill_type=value
    boxfill_type=property(_getboxfilltype,_setboxfilltype)
         
    def _getlevel_1(self):
         return self._level_1
    def _setlevel_1(self,value):
         value=VCS_validation_functions.checkIntFloat(self,'level_1',value)
         setGfbmember(self,'level_1',value)
         self._level_1=value
    level_1=property(_getlevel_1,_setlevel_1)
    
    def _getlevel_2(self):
         return self._level_2
    def _setlevel_2(self,value):
         value=VCS_validation_functions.checkIntFloat(self,'level_2',value)
         setGfbmember(self,'level_2',value)
         self._level_2=value
    level_2=property(_getlevel_2,_setlevel_2)
    
    def _getcolor_1(self):
         return self._color_1
    def _setcolor_1(self,value):
         value=VCS_validation_functions.checkColor(self,'color_1',value)
         setGfbmember(self,'color_1',value)
         self._color_1=value
    color_1=property(_getcolor_1,_setcolor_1)
    
    def _getcolor_2(self):
         return self._color_2
    def _setcolor_2(self,value):
         value=VCS_validation_functions.checkColor(self,'color_2',value)
         setGfbmember(self,'color_2',value)
         self._color_2=value
    color_2=property(_getcolor_2,_setcolor_2)
    
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
              if self.levels!=(1.0000000200408773e+20, 1.0000000200408773e+20) or self.boxfill_type=='custom':
                   if (self.levels[0] in [-1e20, 1e20]):
                      levs=isofill.add_level_ext_1(self, 'n')
                   else:
                      levs=isofill.add_level_ext_1(self, 'y')
                   self.levels=levs
              self._ext_1=value
         if value=='n':
              setmember(self,'ext_1',110)
         else:
              setmember(self,'ext_1',121)
              
    
    ext_1=property(_getext_1,_setext_1)

    def _getext_2(self):
         return self._ext_2
    def _setext_2(self,value):
         do = VCS_validation_functions.checkExt(self,'ext_2',value)
         if do:
              if self.levels!=(1.0000000200408773e+20, 1.0000000200408773e+20) or self.boxfill_type=='custom':
                   if (self.levels[-1] in [-1e20, 1e20]):
                      levs=isofill.add_level_ext_2(self, 'n')
                   else:
                      levs=isofill.add_level_ext_2(self, 'y')
                   self.levels=levs
              self._ext_2=value
         if value=='n':
              setmember(self,'ext_2',110)
         else:
              setmember(self,'ext_2',121)
              
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
    
    def _getname(self):
         return self._name
    def _setname(self,value):
         value=VCS_validation_functions.checkname(self,'name',value)
         if value is not None:
              setmember(self,'name',value)
              self._name=value
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
    yticlabels1=property(_getyticlabels1,_setyticlabels1,None,"haha")

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
         if getGfbmember(self,'_tdatawc_x1') :
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
         if getGfbmember(self,'_tdatawc_x2') :
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
         if getGfbmember(self,'_tdatawc_y1') :
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
         if getGfbmember(self,'_tdatawc_y2') :
              return cdtime.reltime(self._datawc_y2,self.datawc_timeunits).tocomp(self.datawc_calendar)
         else:
              return self._datawc_y2
    def _setdatawc_y2(self,value):
         value=VCS_validation_functions.checkDatawc(self,'datawc_y2',value)
         self._datawc_y2=value[0]
         setmember(self,'datawc_y2',value[0])
         setmember(self,'_tdatawc_y2',value[1])
    datawc_y2=property(_getdatawc_y2,_setdatawc_y2)
    
    def colors(self, color1=16, color2=239):
        mode=self.parent.mode
        self.parent.mode=0
        self.color_1= color1
        self.color_2=color2
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

    def xyscale(self, xat='linear', yat='linear'):
        mode=self.parent.mode
        self.parent.mode=0
        self.xaxisconvert = xat
        self.parent.mode=mode
        self.yaxisconvert = yat
    xyscale.__doc__= xmldocs.xyscaledoc
    
    ###########################################################################
    #                                                                         #
    # List out boxfill graphics method members (attributes).                  #
    #                                                                         #
    ###########################################################################
    def list(self):
        """ Lists object values """
        if (self.name == '__removed_from_VCS__'):
           raise ValueError, 'This instance has been removed from VCS.'
        print "","----------Boxfill (Gfb) member (attribute) listings ----------"
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
        print "boxfill_type = ", self.boxfill_type
        print "level_1 = ", self.level_1
        print "level_2 = ", self.level_2
        print "levels = ", self.levels
        print "color_1 = ", self.color_1
        print "color_2 = ", self.color_2
        print "fillareacolors = ", self.fillareacolors
        print "legend = ", self.legend
        print "ext_1 = ", self.ext_1
        print "ext_2 = ", self.ext_2
        print "missing = ", self.missing
#        print "fillareastyle = ", self.fillareastyle
#        print "fillareaindices = ", self.fillareaindices
    list.__doc__ = xmldocs.listdoc
    ###########################################################################
    #                                                                         #
    # Script out primary boxfill graphics method in VCS to a file.            #
    #                                                                         #
    ###########################################################################
    def script(self, script_filename, mode='a'):
        """
%s
 Function:     script				# Calls _vcs_legacy.scriptGfb

 Description of Function:                                                      
       Saves out a boxfill graphics method in Python or VCS script form to a
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
    box=a.createboxfill('temp')
    box.script('filename.py')         # Append to a Python file "filename.py"
    box.script('filename.scr')        # Append to a VCS file "filename.scr"
    box.script('filename','w')
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
           print _vcs_legacy.scriptGfb(self.name,script_filename,mode)
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

           unique_name = '__Gfb__' + self.name
           fp.write("#----------Boxfill (Gfb) member (attribute) listings ----------\n")
           fp.write("gfb_list=v.listelements('boxfill')\n")
           fp.write("if ('%s' in gfb_list):\n" % self.name)
           fp.write("   %s = v.getboxfill('%s')\n" % (unique_name, self.name))
           fp.write("else:\n")
           fp.write("   %s = v.createboxfill('%s')\n" % (unique_name, self.name))
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
           fp.write("%s.xaxisconvert = '%s'\n" % (unique_name, self.xaxisconvert))
           fp.write("%s.yaxisconvert = '%s'\n" % (unique_name, self.yaxisconvert))
	   # Unique attribute for boxfill
           fp.write("%s.boxfill_type = '%s'\n" % (unique_name, self.boxfill_type))
           fp.write("%s.level_1 = %g\n" % (unique_name, self.level_1))
           fp.write("%s.level_2 = %g\n" % (unique_name, self.level_2))
           fp.write("%s.levels = %s\n" % (unique_name, self.levels))
           fp.write("%s.color_1 = %g\n" % (unique_name, self.color_1))
           fp.write("%s.color_2 = %g\n" % (unique_name, self.color_2))
           fp.write("%s.fillareacolors = %s\n" % (unique_name, self.fillareacolors))
           fp.write("%s.legend = %s\n" % (unique_name, self.legend))
           fp.write("%s.ext_1 = '%s'\n" % (unique_name, self.ext_1))
           fp.write("%s.ext_2 = '%s'\n" % (unique_name, self.ext_2))
           fp.write("%s.missing = %g\n" % (unique_name, self.missing))
           fp.write("%s.datawc_calendar = %g\n" % (unique_name, self.datawc_calendar))
           fp.write("%s.datawc_timeunits = '%s'\n\n" % (unique_name, self.datawc_timeunits))
#           fp.write("%s.fillareastyle = '%s'\n" % (unique_name, self.fillareastyle))
#           fp.write("%s.fillareaindices = %s\n" % (unique_name, self.fillareaindices))
    script.__doc__ = script.__doc__ % xmldocs.scriptdoc
###############################################################################
#        END OF FILE							      #
###############################################################################
