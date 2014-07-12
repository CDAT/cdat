"""
# Vector (Gv) module
"""
###############################################################################
#                                                                             #
# Module:       vector (Gv) module                                            #
#                                                                             #
# Copyright:    2000, Regents of the University of California                 #
#               This software may not be distributed to others without        #
#               permission of the author.                                     #
#                                                                             #
# Authors:      PCMDI Software Team                                           #
#               Lawrence Livermore NationalLaboratory:                        #
#               support@pcmdi.llnl.gov                                        #
#                                                                             #
# Description:  Python command wrapper for VCS's vector graphics method.      #
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
import _vcs_legacy, queries, vcs_legacy, VCS_validation_functions, cdtime
import Canvas
from types import *
###############################################################################
#                                                                             #
# Function:	setGvmember                                                   #
#                                                                             #
# Description of Function:                                                    #
# 	Private function to update the VCS canvas plot. If the canvas mode is #
#       set to 0, then this function does nothing.              	      #
#                                                                             #
#                                                                             #
# Example of Use:                                                             #
#      setGvmember(self,name,value)					      #
#              where: self is the class (e.g., Gv)                            #
#                     name is the name of the member that is being changed    #
#                     value is the new value of the member (or attribute)     #
#                                                                             #
###############################################################################
def setGvmember(self,member,value):
     # If the VCS Canvas is displayed, then bring the canvas to the front before 
     # redisplaying the updated contents.
     if (self.parent.mode == 1) and (self.parent.iscanvasdisplayed()):
        Canvas.finish_queued_X_server_requests( self.parent )
        self.parent.canvas.BLOCK_X_SERVER()
        self.parent.canvasraised()

     _vcs_legacy.setGvmember(self, member, value, self.parent.mode)

     # If the VCS Canvas is displayed, then update the backing store
     if (self.parent.mode == 1) and (self.parent.iscanvasdisplayed()):
        self.parent.flush()
        self.parent.backing_store()
        self.parent.canvas.UNBLOCK_X_SERVER()
setmember=setGvmember
###############################################################################
#                                                                             #
# Function:     getGvmember                                                   #
#                                                                             #
# Description of Function:                                                    #
#       Private function that retrieves the vector members from the C         #
#       structure and passes it back to Python.                               #
#                                                                             #
#                                                                             #
# Example of Use:                                                             #
#      return_value =							      #
#      getGvmember(self,name)                                                 #
#              where: self is the class (e.g., Gv)                            #
#                     name is the name of the member that is being found      #
#                                                                             #
###############################################################################
def getGvmember(self,member):
     return _vcs_legacy.getGvmember(self,member)
getmember=getGvmember

###############################################################################
#                                                                             #
# Function:     renameGv                                                      #
#                                                                             #
# Description of Function:                                                    #
#       Private function that renames the name of an existing vector          #
#       graphics method.                                                      #
#                                                                             #
#                                                                             #
# Example of Use:                                                             #
#      renameGv(old_name, new_name)                                           #
#              where: old_name is the current name of vector graphics method  #
#                     new_name is the new name for the vector graphics method #
#                                                                             #
###############################################################################
def renameGv(self, old_name, new_name):
     return _vcs_legacy.renameGv(old_name, new_name)

class Gv(object):
    """
 Class:	Gv				# Vector

 Description of Gv Class:
    The vector graphics method displays a vector plot of a 2D vector field. Vectors
    are located at the coordinate locations and point in the direction of the data
    vector field. Vector magnitudes are the product of data vector field lengths and
    a scaling factor. The example below shows how to modify the vector's line, scale,
    alignment, type, and reference.

    This class is used to define an vector table entry used in VCS, or it  can be
    used to change some or all of the vector attributes in an existing vector table
    entry.

 Other Useful Functions:
	 a=vcs_legacy.init()			# Constructor
	 a.show('vector')		# Show predefined vector graphics methods
	 a.show('line')			# Show predefined VCS line objects
	 a.setcolormap("AMIP")		# Change the VCS color Map
         a.vector(s1, s2, v,'default')	# Plot data 's1', and 's2' with vector 'v'
			                 and 'default' template
	 a.update()		 	# Updates the VCS Canvas at user's request
	 a.mode=1, or 0 	 	# If 1, then automatic update, else if
				          0, then use update function to
				          update the VCS Canvas.

 Example of Use:
    a=vcs_legacy.init()
    To Create a new instance of vector use:
     vc=a.createvector('new','quick')	# Copies content of 'quick' to 'new'
     vc=a.createvector('new') 		# Copies content of 'default' to 'new'

    To Modify an existing vector use:
     vc=a.getvector('AMIP_psl')

    vc.list()  				# Will list all the vector attribute values
    vc.projection='linear'   		# Can only be 'linear'
    lon30={-180:'180W',-150:'150W',0:'Eq'}
    vc.xticlabels1=lon30
    vc.xticlabels2=lon30
    vc.xticlabels(lon30, lon30)  	# Will set them both
    vc.xmtics1=''
    vc.xmtics2=''
    vc.xmtics(lon30, lon30)  		# Will set them both
    vc.yticlabels1=lat10
    vc.yticlabels2=lat10
    vc.yticlabels(lat10, lat10)  	# Will set them both
    vc.ymtics1=''
    vc.ymtics2=''
    vc.ymtics(lat10, lat10)  		# Will set them both
    vc.datawc_y1=-90.0
    vc.datawc_y2=90.0
    vc.datawc_x1=-180.0
    vc.datawc_x2=180.0
    vc.datawc(-90, 90, -180, 180)  	# Will set them all
    xaxisconvert='linear'
    yaxisconvert='linear'
    vc.xyscale('linear', 'area_wt')  	# Will set them both

    Specify the line style:
     vc.line=0 				# Same as vc.line='solid'
     vc.line=1 				# Same as vc.line='dash'
     vc.line=2 				# Same as vc.line='dot'
     vc.line=3 				# Same as vc.line='dash-dot'
     vc.line=4 				# Same as vc.line='long-dot'

    Specify the line color of the vectors:
     vc.linecolor=16   			# Color range: 16 to 230, default line color is black
     vc.linewidth=1   			# Width range: 1 to 100, default size is 1
   
    Specify the vector scale factor:
     vc.scale=2.0   			# Can be an integer or float

    Specify the vector alignment:
     vc.alignment=0			# Same as vc.alignment='head'
     vc.alignment=1			# Same as vc.alignment='center'
     vc.alignment=2			# Same as vc.alignment='tail'

    Specify the vector type:
      vc.type=0   			# Same as vc.type='arrow head'
      vc.type=1   			# Same as vc.type='wind barbs'
      vc.type=2   			# Same as vc.type='solid arrow head'
   
    Specify the vector reference:
      vc.reference=4    		# Can be an integer or float
"""
    rename=renameGv # Alias for VCS_Validation_Functions
    __slots__=[
         'setmember',
         'parent',
         'name',
         'g_name',
         'xaxisconvert',
         'yaxisconvert',
         'linecolor',
         'line',
         'linewidth',
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
         'datawc_timeunits',
         'datawc_calendar',
         'scale',
         'alignment',
         'type',
         'reference',
         '_name',
         '_xaxisconvert',
         '_yaxisconvert',
         '_linecolor',
         '_line',
         '_linewidth',
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
         '_datawc_timeunits',
         '_datawc_calendar',
         '_scale',
         '_alignment',
         '_type',
         '_reference',
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
         return self._level
    def _setlevels(self,value):
         value=VCS_validation_functions.checkIsolineLevels(self,'levels',value)
         self._level=value
         setmember(self,'level',value)
    level=property(_getlevels,_setlevels)
    levels=property(_getlevels,_setlevels)

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
    
    def _getreference(self):
         return self._reference
    def _setreference(self,value):
         value = VCS_validation_functions.checkNumber(self,'reference',value)
         self._reference=value
         setmember(self,'reference',value)
    reference=property(_getreference,_setreference)
    
    def _getscale(self):
         return self._scale
    def _setscale(self,value):
         value = VCS_validation_functions.checkNumber(self,'scale',value)
         self._scale=value
         setmember(self,'scale',float(value))
    scale=property(_getscale,_setscale)
    
    def _getlinewidth(self):
         return self._linewidth
    def _setlinewidth(self,value):
         if not value is None:
              value = VCS_validation_functions.checkNumber(self,'linewidth',value,0,300)
         self._linewidth=value
         setmember(self,'linewidth',value)
    linewidth=property(_getlinewidth,_setlinewidth)
    
    def _getlinecolor(self):
         return self._linecolor
    def _setlinecolor(self,value):
         if not value is None:
              value = VCS_validation_functions.checkColor(self,'linecolor',value)
         self._linecolor=value
         setmember(self,'linecolor',value)
    linecolor=property(_getlinecolor,_setlinecolor)
    
    def _getline(self):
         return self._line
    
    def _setline(self,value):
         if not value is None:
              value = VCS_validation_functions.checkLineType(self,'line',value)
         self._line=value
         setmember(self,'line',self.line)
    line=property(_getline,_setline)
    
    def _gettype(self):
         return self._type
    def _settype(self,value):
         value = VCS_validation_functions.checkVectorType(self,'type',value)
         self._type=value
         setmember(self,'type',self.type)
    type=property(_gettype,_settype)
    
    def _getalignment(self):
         return self._alignment
    def _setalignment(self,value):
         value = VCS_validation_functions.checkVectorAlignment(self,'alignment',value)
         self._alignment=value
         setmember(self,'alignment',self.alignment)
    alignment=property(_getalignment,_setalignment)
    

    def __init__(self, parent, Gv_name=None, Gv_name_src='default', createGv=0):
	#                                                         #
        ###########################################################
	# Initialize the vector class and its members             #
        #							  #
	# The getGvmember function retrieves the values of the    #
        # vector members in the C structure and passes back the   #
	# appropriate Python Object.                              #
        ###########################################################
	#                                                         #
        if (createGv == 0):
           if (Gv_name == None):
              raise ValueError, 'Must provide a vector name.'
           else:
              _vcs_legacy.copyGv(Gv_name_src, Gv_name)
              self._name = Gv_name
        else:
              self._name =Gv_name_src
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
        self.g_name='Gv'
        self._xaxisconvert=getmember(self, 'xaxisconvert')
        self._yaxisconvert=getmember(self, 'yaxisconvert')
        self._line=None
        self._linecolor=None
        self._linewidth=None
        self._scale=getmember(self, 'scale')
        self._alignment=getmember(self, 'alignment')
        self._type=getmember(self, 'type')
        self._reference=getmember(self, 'reference')
        self._datawc_timeunits=getmember(self, 'datawc_timeunits')
        self._datawc_calendar=getmember(self, 'datawc_calendar')
        #                                                         #
        ###########################################################
        # Find and set the vector structure in VCS C pointer      #
        # list. If the vector name does not exist, then use       #
        # default vector.                                         #
        ###########################################################
        #                                                         #
        self.parent=parent

##     def __setattr__(self, name, value):
##         if (self.name == '__removed_from_VCS__'):
##            raise ValueError, 'This instance has been removed from VCS.'
##         if (self.name == 'default'):
##            raise ValueError, 'You cannot modify the default vector.'
##         if (name == 'name'):
##            if (type(value) == StringType):
##               renameGv(self,self.name, value)
##               self.__dict__['name']=value
##            else:
##               raise ValueError, 'The name attribute must be a string.'
##         elif (name == 'line'):
##            if (value == None):
##               self.__dict__['line']=None
##               setGvmember(self,'line',self.line) # update the plot
##            elif ((value in ('solid', 'dash', 'dot', 'dash-dot', 'long-dash', 0, 1, 2, 3, 4)) or (queries.isline(value)==1)):
##               if value in ('solid', 0):
##                  value='solid'
##               elif value in ('dash', 1):
##                  value='dash'
##               elif value in ('dot', 2):
##                  value='dot'
##               elif value in ('dash-dot', 3):
##                  value='dash-dot'
##               elif value in ('long-dash', 4):
##                  value='long-dash'
##               elif (queries.isline(value)==1):
##                  value=value.name
##               self.__dict__['line']=value
##               setGvmember(self,'line',self.line) # update the plot
##            else:
##               raise ValueError, 'The line value can either be ("solid", "dash", "dot", "dash-dot", "long-dash"), or (0, 1, 2, 3, 4), or a line object.'
##         elif (name == 'linecolor'):
##            if (value == None):
##               self.__dict__['linecolor']=None
##               setGvmember(self,'linecolor',self.linecolor) # update the plot
##            elif (type(value) in (IntType, IntType)):
##               if value not in range(0,256): # must be an integer
##                  raise ValueError, 'The line color values must be in the range 0 to 255.'
##               else:
##                  self.__dict__['linecolor']=value
##                  setGvmember(self,'linecolor',self.linecolor) # update the plot
##            else:
##               raise ValueError, 'The line color attribute value must be an integer in the range 0 to 255.'
##         elif (name == 'linewidth'):
##            if (value == None):
##               self.__dict__['linewidth']=None
##               setGvmember(self,'linewidth',self.linewidth) # update the plot
##            elif (type(value) in (IntType, IntType)):
##               if value not in range(1,101): # must be an integer
##                  raise ValueError, 'The line width values must be in the range 1 to 100.'
##               else:
##                  self.__dict__['linewidth']=value
##                  setGvmember(self,'linewidth',self.linewidth) # update the plot
##            else:
##               raise ValueError, 'The line width attribute value must be an integer in the range 1 to 100.'
##         elif (name == 'scale'):
##            if (type(value) in (IntType, FloatType)):
##               self.__dict__['scale']=value
##               setGvmember(self,'scale',self.scale) # update the plot
##            else:
##               raise ValueError, 'The vector scale attribute value must be a float.'
##         elif (name == 'alignment'):
##            if (value in ('head', 'center', 'tail', 0, 1, 2)):
##               if value in ('head', 0):
##                  value='head'
##               elif value in ('center', 1):
##                  value='center'
##               elif value in ('tail', 2):
##                  value='tail'
##               self.__dict__['alignment']=value
##               setGvmember(self,'alignment',self.alignment) # update the plot
##            else:
##               raise ValueError, 'The vector alignment attribute value must be ("head", "center", "tail") or (0, 1, 2).'
##         elif (name == 'type'):
##            if (value in ('arrows', 'barbs', 'solidarrows', 0, 1, 2)):
##               if value in ('arrows', 0):
##                  value='arrows'
##               elif value in ('barbs', 1):
##                  value='barbs'
##               elif value in ('solidarrows', 2):
##                  value='solidarrows'
##               self.__dict__['type']=value
##               setGvmember(self,'type',self.type) # update the plot
##            else:
##               raise ValueError, 'The vector type attribute value must be ("arrows", "barbs", "solidarrows") or (0, 1, 2).'
##         elif (name == 'reference'):
##            if (type(value) in (FloatType, IntType)):
##               self.__dict__['reference']=value
##               setGvmember(self,'reference',self.reference) # update the plot
##            else:
##               raise ValueError, 'The vector reference attribute value must be a float.'
##         elif (name == 'projection'):
##            if isinstance(value,vcs_legacy.projection.Proj): value=value.name
##            if (_vcs_legacy.checkProj(value)):
##               setGvmember(self,name,value)
##               self.__dict__['projection']=value
##            else:
##               raise ValueError, 'The projection '+value+' does not exist'
## ##            if (value == 'linear'):
## ##               setGvmember(self,name,value)
## ##               self.__dict__['projection']=value
## ##            else:
## ##               raise ValueError, 'The projection attribute must be linear.'
##         elif (name == 'xticlabels1'):
##            if (type(value) in (StringType, DictType)):
##               setGvmember(self,name,value)
##               self.__dict__['xticlabels1']=value
##            else:
##               raise ValueError, 'The xticlabels1 attribute must be either a string or a dictionary.'
##         elif (name == 'xticlabels2'):
##            if (type(value) in (StringType, DictType)):
##               setGvmember(self,name,value)
##               self.__dict__['xticlabels2']=value
##            else:
##               raise ValueError, 'The xticlabels2 attribute must be either a string or a dictionary.'
##         elif (name == 'xmtics1'):
##            if (type(value) in (StringType, DictType)):
##               setGvmember(self,name,value)
##               self.__dict__['xmtics1']=value
##            else:
##               raise ValueError, 'The xmtics1 attribute must be either a string or a dictionary.'
##         elif (name == 'xmtics2'):
##            if (type(value) in (StringType, DictType)):
##               setGvmember(self,name,value)
##               self.__dict__['xmtics2']=value
##            else:
##               raise ValueError, 'The xmtics2 attribute must be either a string or a dictionary.'
##         elif (name == 'yticlabels1'):
##            if (type(value) in (StringType, DictType)):
##               setGvmember(self,name,value)
##               self.__dict__['yticlabels1']=value
##            else:
##               raise ValueError, 'The yticlabels1 attribute must be either a string or a dictionary.'
##         elif (name == 'yticlabels2'):
##            if (type(value) in (StringType, DictType)):
##               setGvmember(self,name,value)
##               self.__dict__['yticlabels2']=value
##            else:
##               raise ValueError, 'The yticlabels2 attribute must be either a string or a dictionary.'
##         elif (name == 'ymtics1'):
##            if (type(value) in (StringType, DictType)):
##               setGvmember(self,name,value)
##               self.__dict__['ymtics1']=value
##            else:
##               raise ValueError, 'The ymtics1 attribute must be either a string or a dictionary.'
##         elif (name == 'ymtics2'):
##            if (type(value) in (StringType, DictType)):
##               setGvmember(self,name,value)
##               self.__dict__['ymtics2']=value
##            else:
##               raise ValueError, 'The ymtics2 attribute must be either a string or a dictionary.'
##         elif (name == 'datawc_x1'):
##            if (type(value) in (IntType, FloatType)):
##               value = float(value)
##               setGvmember(self,name,value)
##               self.__dict__['datawc_x1']=value
##            else:
##               raise ValueError, 'The datawc_x1 attribute must be either an integer or a float value.'
##         elif (name == 'datawc_x2'):
##            if (type(value) in (IntType, FloatType)):
##               value = float(value)
##               setGvmember(self,name,value)
##               self.__dict__['datawc_x2']=value
##            else:
##               raise ValueError, 'The datawc_x2 attribute must be either an interger or a  float value.'
##         elif (name == 'datawc_y1'):
##            if (type(value) in (IntType, FloatType)):
##               value = float(value)
##               setGvmember(self,name,value)
##               self.__dict__['datawc_y1']=value
##            else:
##               raise ValueError, 'The datawc_y1 attribute must be either an interger or a  float value.'
##         elif (name == 'datawc_y2'):
##            if (type(value) in (IntType, FloatType)):
##               value = float(value)
##               setGvmember(self,name,value)
##               self.__dict__['datawc_y2']=value
##            else:
##               raise ValueError, 'The datawc_y2 attribute must be either an interger or a  float value.'
##         elif (name == 'xaxisconvert'):
##            if (value in ('linear', 'log10', 'ln','exp','area_wt')):
##               setGvmember(self,name,value)
##               self.__dict__['xaxisconvert']=value
##            else:
##               raise ValueError, 'The xaxisconvert attribute must be either: linear, log10, ln, exp, or area_wt'
##         elif (name == 'yaxisconvert'):
##            if (value in ('linear', 'log10', 'ln', 'exp', 'area_wt')):
##               setGvmember(self,name,value)
##               self.__dict__['yaxisconvert']=value
##            else:
##               raise ValueError, 'The yaxisconvert attribute must be either: linear, log10, ln, exp, or area_wt'
##         else:
##            raise ValueError, 'The member was not found.'

# 
# Doesn't make sense to inherit. This would mean more coding in C.
# I put this code back.                                 
#
    def xticlabels(self, xtl1='', xtl2=''):
        mode=self.parent.mode
        self.parent.mode=0
        self.xticlabels1= xtl1
        self.parent.mode=mode
        self.xticlabels2= xtl2

    def xmtics(self,xmt1='', xmt2=''):
        mode=self.parent.mode
        self.parent.mode=0
        self.xmtics1= xmt1
        self.parent.mode=mode
        self.xmtics2= xmt2

    def yticlabels(self, ytl1='', ytl2=''):
        mode=self.parent.mode
        self.parent.mode=0
        self.yticlabels1= ytl1
        self.parent.mode=mode
        self.yticlabels2= ytl2

    def ymtics(self, ymt1='', ymt2=''):
        mode=self.parent.mode
        self.parent.mode=0
        self.ymtics1= ymt1
        self.parent.mode=mode
        self.ymtics2= ymt2

    def datawc(self, dsp1=1e20, dsp2=1e20, dsp3=1e20, dsp4=1e20):
        mode=self.parent.mode
        self.parent.mode=0
        self.datawc_y1= dsp1
        self.datawc_y2= dsp2
        self.datawc_x1= dsp3
        self.parent.mode=mode
        self.datawc_x2= dsp4

    def xyscale(self, xat='', yat=''):
        mode=self.parent.mode
        self.parent.mode=0
        self.xaxisconvert= xat
        self.parent.mode=mode
        self.yaxisconvert= yat

    def list(self):
        if (self.name == '__removed_from_VCS__'):
           raise ValueError, 'This instance has been removed from VCS.'
        print "","----------Vector (Gv) member (attribute) listings ----------"
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
        print "line = ", self.line
        print "linecolor = ", self.linecolor
        print "linewidth = ", self.linewidth
        print "scale = ", self.scale
        print "alignment = ", self.alignment
        print "type = ", self.type
        print "reference = ", self.reference

    #############################################################################
    #                                                                           #
    # Script vector (Gv) object to a file.                                      #
    #                                                                           #
    #############################################################################
    def script(self, script_filename=None, mode=None):
        '''
 Function:     script                           # Calls _vcs_legacy.scriptGv

 Description of Function:
       Saves out a vector graphics method in Python or VCS script form to
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
    vec=a.createboxfill('temp')
    vec.script('filename.py')         # Append to a Python file "filename.py"
    vec.script('filename.scr')        # Append to a VCS file "filename.scr"
    vec.script('filename','w')
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
           print _vcs_legacy.scriptGv(self.name,script_filename,mode)
        else:
           mode = mode + '+'
           py_type = script_filename[len(script_filename)-3:len(script_filename)]
           if (py_type != '.py'):
              script_filename = script_filename + '.py'

           # Write to file
           fp = open(script_filename,mode)
           if (fp.tell() == 0): # Must be a new file, so include below
              fp.write("#####################################\n")
              fp.write("#                                   #\n")
              fp.write("# Import and Initialize VCS         #\n")
              fp.write("#                                   #\n")
              fp.write("#####################################\n")
              fp.write("import vcs_legacy\n")
              fp.write("v=vcs_legacy.init()\n\n")

           unique_name = '__Gv__' + self.name
           fp.write("#----------Vector (Gv) member (attribute) listings ----------\n")
           fp.write("gv_list=v.listelements('vector')\n")
           fp.write("if ('%s' in gv_list):\n" % self.name)
           fp.write("   %s = v.getvector('%s')\n" % (unique_name, self.name))
           fp.write("else:\n")
           fp.write("   %s = v.createvector('%s')\n" % (unique_name, self.name))
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
           # Unique attribute for vector
           fp.write("%s.line = %s\n" % (unique_name, self.line))
           fp.write("%s.linecolor = %s\n" % (unique_name, self.linecolor))
           fp.write("%s.linewidth = %s\n" % (unique_name, self.linewidth))
           fp.write("%s.scale = %s\n" % (unique_name, self.scale))
           fp.write("%s.alignment = '%s'\n" % (unique_name, self.alignment))
           fp.write("%s.type = '%s'\n" % (unique_name, self.type))
           fp.write("%s.reference = %s\n\n" % (unique_name, self.reference))


###############################################################################
#        END OF FILE							      #
###############################################################################
