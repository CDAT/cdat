# Adapted for numpy/ma/cdms2 by convertcdms.py
"""
# Continents (Gcon) module
"""
#################################################################################
#                                                                               #
# Module:       continents (Gcon) module                                        #
#                                                                               #
# Copyright:    2000, Regents of the University of California                   #
#               This software may not be distributed to others without          #
#               permission of the author.                                       #
#                                                                               #
# Authors:      PCMDI Software Team                                             #
#               Lawrence Livermore NationalLaboratory:                          #
#               support@pcmdi.llnl.gov                                          #
#                                                                               #
# Description:  Python command wrapper for VCS's continents graphics method.    #
#                                                                               #
# Version:      5.0                                                             #
#                                                                               #
#################################################################################
#
#
#
import queries,VCS_validation_functions
import Canvas
from types import *
import AutoAPI
import xmldocs

#################################################################################
#                                                                               #
# Function:	setGconmember                                                   #
#                                                                               #
# Description of Function:                                                      #
# 	Private function to update the VCS canvas plot. If the canvas mode is   #
#       set to 0, then this function does nothing.              		#
#                                                                               #
#                                                                               #
# Example of Use:                                                               #
#      setGconmember(self,name,value)						#
#              where: self is the class (e.g., Gcon)                            #
#                     name is the name of the member that is being changed      #
#                     value is the new value of the member (or attribute)       #
#                                                                               #
#################################################################################
def setGconmember(self,member,value):
     # If the VCS Canvas is displayed, then bring the canvas to the front before 
     # redisplaying the updated contents.
     if (self.parent.mode == 1) and (self.parent.iscanvasdisplayed()):
        Canvas.finish_queued_X_server_requests( self.parent )
        self.parent.canvas.BLOCK_X_SERVER()
        self.parent.canvasraised()

     _vcs.setGconmember(self, member, value, self.parent.mode)

     # If the VCS Canvas is displayed, then update the backing store
     if (self.parent.mode == 1) and (self.parent.iscanvasdisplayed()):
        self.parent.flush()
        self.parent.backing_store()
        self.parent.canvas.UNBLOCK_X_SERVER()

setmember = setGconmember
#################################################################################
#                                                                               #
# Function:     getGconmember                                                   #
#                                                                               #
# Description of Function:                                                      #
#       Private function that retrieves the continents members from the C       #
#       structure and passes it back to Python.                                 #
#                                                                               #
#                                                                               #
# Example of Use:                                                               #
#      return_value =								#
#      getGconmember(self,name)                                                 #
#              where: self is the class (e.g., Gcon)                            #
#                     name is the name of the member that is being found        #
#                                                                               #
#################################################################################
def getGconmember(self,member):
     return _vcs.getGconmember(self,member)

#################################################################################
#                                                                               #
# Function:     renameGcon                                                      #
#                                                                               #
# Description of Function:                                                      #
#       Private function that renames the name of an existing continents        #
#       graphics method.                                                        #
#                                                                               #
#                                                                               #
# Example of Use:                                                               #
#      renameGcon(old_name, new_name)                                           #
#              where: old_name is the current name of continents graphics method#
#                    new_name is the new name for the continents graphics method#
#                                                                               #
#################################################################################
getmember = getGconmember

def renameGcon(self, old_name, new_name):
     return _vcs.renameGcon(old_name, new_name)
rename = renameGcon

class Gcon(object,AutoAPI.AutoAPI):
    """
    Options:::
%s
%s
 Class:	Gcon 				# Continents
                  
 Description of Gcon Class:
    The Continents graphics method draws a predefined, generic set of continental
    outlines in a longitude by latitude space. (To draw continental outlines, no
    external data set is required.) 

    This class is used to define an continents table entry used in VCS, or it
    can be used to change some or all of the continents attributes in an
    existing continents table entry.
	
 Other Useful Functions:
		a=vcs.init()		  # Constructor
		a.show('continents')	  # Show predefined boxfill graphics methods
		a.show('line')		  # Show predefined line class objects
		a.setcolormap("AMIP")     # Change the VCS color map
		a.continents(c,'default') # Plot continents, where 'c' is the defined
                                            continents object
		a.update()		  # Updates the VCS Canvas at user's request
		a.mode=1, or 0 		  # If 1, then automatic update, else if
					    0, then use update function to
					    update the VCS Canvas.
               
 Example of Use:
    a=vcs.init()
    To Create a new instance of continents use:
     con=a.createcontinents('new','quick')#copies content of 'quick' to 'new'
     con=a.createcontinents('new') # copies content of 'default' to 'new'

    To Modify an existing continents use:
     con=a.getcontinents('AMIP_psl')

    con.list()  			# Will list all the continents attribute values
    con.projection='linear'
    lon30={-180:'180W',-150:'150W',0:'Eq'}
    con.xticlabels1=lon30
    con.xticlabels2=lon30
    con.xticlabels(lon30, lon30)  	# Will set them both
    con.xmtics1=''
    con.xmtics2=''
    con.xmtics(lon30, lon30)  		# Will set them both
    con.yticlabels1=lat10
    con.yticlabels2=lat10
    con.yticlabels(lat10, lat10)  	# Will set them both
    con.ymtics1=''
    con.ymtics2=''
    con.ymtics(lat10, lat10)  		# Will set them both
    con.datawc_y1=-90.0
    con.datawc_y2=90.0
    con.datawc_x1=-180.0
    con.datawc_x2=180.0 
    con.datawc(-90, 90, -180, 180)  	# Will set them all

    Specify the continents line style (or type):
     con.line=0  		# Same as con.line='solid'
     con.line=1   		# Same as con.line='dash'
     con.line=2   		# Same as con.line='dot'
     con.line=3   		# Same as con.line='dash-dot'
     con.line=4   		# Same as con.line='long-dash'

    There are three possibilities for setting the line color indices (Ex):
     con.linecolor=22 		# Same as con.linecolor=(22)
     con.linecolor=([22])	# Will set the continents to a specific color index			
     con.linecolor=None		# Turns off the line color index, defaults to Black

   con.linewidth=1     		# width range: 1 to 100, default color is 1
"""
    __slots__=[
         '__doc__',
         'parent',
         'name',
         'g_name',
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
         'linecolor',
         'line',
         'linewidth',
         '_name',
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
         '_linecolor',
         '_line',
         '_linewidth',
         'info',
         'type',
         '_type',
         ]
    
    def _getname(self):
         return self._name
    def _setname(self,value):
         value=VCS_validation_functions.checkname(self,'name',value)
         if value is not None:
              self._name=value
              setmember(self,'name',value)
    name=property(_getname,_setname)
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
         return self._datawc_x1
    def _setdatawc_x1(self,value):
         value=VCS_validation_functions.checkDatawc(self,'datawc_x1',value)
         self._datawc_x1=value[0]
         setmember(self,'datawc_x1',value[0])
    datawc_x1=property(_getdatawc_x1,_setdatawc_x1)

    def _getdatawc_x2(self):
         return self._datawc_x2
    def _setdatawc_x2(self,value):
         value=VCS_validation_functions.checkDatawc(self,'datawc_x2',value)
         self._datawc_x2=value[0]
         setmember(self,'datawc_x2',value[0])
    datawc_x2=property(_getdatawc_x2,_setdatawc_x2)
    
    def _getdatawc_y1(self):
         return self._datawc_y1
    def _setdatawc_y1(self,value):
         value=VCS_validation_functions.checkDatawc(self,'datawc_y1',value)
         self._datawc_y1=value[0]
         setmember(self,'datawc_y1',value[0])
    datawc_y1=property(_getdatawc_y1,_setdatawc_y1)

    def _getdatawc_y2(self):
         return self._datawc_y2
    def _setdatawc_y2(self,value):
         value=VCS_validation_functions.checkDatawc(self,'datawc_y2',value)
         self._datawc_y2=value[0]
         setmember(self,'datawc_y2',value[0])
    datawc_y2=property(_getdatawc_y2,_setdatawc_y2)
    
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
         setmember(self,'line',value)
    line=property(_getline,_setline)
    
    def _gettype(self):
         return self._type
    def _settype(self,value):
         if not value is None:
              value = VCS_validation_functions.checkContType(self,'type',value)
         self._type=value
         setmember(self,'type',value)
    type=property(_gettype,_settype)
    
    def __init__(self, parent, Gcon_name=None, Gcon_name_src='default', createGcon=0):
	#                                                           #
        #############################################################
	# Initialize the continents class and its members           #
        #							    #
	# The getGconmember function retrieves the values of the    #
        # continents members in the C structure and passes back the #
	# appropriate Python Object.                                #
        #############################################################
	#                                                           #
        self.parent=parent
        if (createGcon == 0):
           if (Gcon_name == None):
              raise ValueError, 'Must provide a continents name.'
           else:
              _vcs.copyGcon(Gcon_name_src, Gcon_name)
              self._name = Gcon_name
        else:
              self._name = Gcon_name_src
              
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
        
        self.g_name='Gcon'
        self._line='solid'
        self._linecolor=None
        self._linewidth=None
        
        self.type=getmember(self, 'type')
        
        self.info=AutoAPI.Info(self)
        self.info.expose=['ALL']
        self.__doc__ = self.__doc__ % (xmldocs.graphics_method_core_notime,xmldocs.linedoc)
##         #                                                         #
##         ###########################################################
##         # Find and set the continents structure in VCS C pointer  #
##         # list. If the continents name does not exist, then use   #
##         # default continents.                                     #
##         ###########################################################
##         #                                                         #

##     def __setattr__(self, name, value):
##         if (self.name == 'default'):
##            raise ValueError, 'You cannot modify the default continents.'
##         if (self.name == '__removed_from_VCS__'):
##            raise ValueError, 'This instance has been removed from VCS.'
##         if (name == 'name'):
##            if (type(value) == StringType):
##               renameGcon(self,self.name, value)
##               self.__dict__['name']=value
##            else:
##               raise ValueError, 'The name attribute must be a string.'
##         elif (name == 'linecolor'):
##            if (value == None):
##               self.__dict__['linecolor']=None
##               setGconmember(self,'linecolor',self.linecolor) # update the plot
##            elif (isinstance(value, IntType)):
##               if value not in range(0,256): # must be an integer
##                  raise ValueError, 'The line color values must be in the range 0 to 256.'
##               else:
##                  self.__dict__['linecolor']=value
##                  setGconmember(self,'linecolor',self.linecolor) # update the plot
##            else:
##               raise ValueError, 'The line color attribute value must be an integer in the range 0 to 256.'
##         elif (name == 'linewidth'):
##            if (value == None):
##               self.__dict__['linewidth']=None
##               setGconmember(self,'linewidth',self.linewidth) # update the plot
##            elif (type(value) in (IntType, IntType)):
##               if value not in range(1,101): # must be an integer
##                  raise ValueError, 'The line width values must be in the range 1 to 100.'
##               else:
##                  self.__dict__['linewidth']=value
##                  setGconmember(self,'linewidth',self.linewidth) # update the plot
##            else:
##               raise ValueError, 'The line width attribute value must be an integer in the range 1 to 100.'
##         elif (name == 'line'):
##            if (value == None):
##               self.__dict__['line']='solid'
##               setGconmember(self,'line',self.line) # update the plot
##            elif ((value in ('solid','dash','dot','dash-dot','long-dash', 0,1,2,3,4)) or
##                  (queries.isline(value)==1)):
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
##               setGconmember(self,'line',self.line) # update the plot
##            else:
##               raise ValueError, 'The line value can either be ("solid", "dash", "dot", "dash-dot", "long-dash"), (0, 1, 2, 3, 4), or a line object.'
##         elif (name == 'type'):
##            if (isinstance(value, IntType)):
##               setGconmember(self,name,value)
##               self.__dict__['type']=value
##            else:
##               raise ValueError, 'The continents type attribute must be an integer.'
##         elif (name == 'projection'):
##            if (value in ('linear', 'mollweide', 'robinson','polar')):
##               setGconmember(self,name,value)
##               self.__dict__['projection']=value
##            else:
##               raise ValueError, 'The projection attribute must be either: linear, mollweide, robinson, or polar'
##         elif (name == 'xticlabels1'):
##            if (type(value) in (StringType, DictType)):
##               setGconmember(self,name,value)
##               self.__dict__['xticlabels1']=value
##            else:
##               raise ValueError, 'The xticlabels1 attribute must be either a string or a dictionary.'
##         elif (name == 'xticlabels2'):
##            if (type(value) in (StringType, DictType)):
##               setGconmember(self,name,value)
##               self.__dict__['xticlabels2']=value
##            else:
##               raise ValueError, 'The xticlabels2 attribute must be either a string or a dictionary.'
##         elif (name == 'xmtics1'):
##            if (type(value) in (StringType, DictType)):
##               setGconmember(self,name,value)
##               self.__dict__['xmtics1']=value
##            else:
##               raise ValueError, 'The xmtics1 attribute must be either a string or a dictionary.'
##         elif (name == 'xmtics2'):
##            if (type(value) in (StringType, DictType)):
##               setGconmember(self,name,value)
##               self.__dict__['xmtics2']=value
##            else:
##               raise ValueError, 'The xmtics2 attribute must be either a string or a dictionary.'
##         elif (name == 'yticlabels1'):
##            if (type(value) in (StringType, DictType)):
##               setGconmember(self,name,value)
##               self.__dict__['yticlabels1']=value
##            else:
##               raise ValueError, 'The yticlabels1 attribute must be either a string or a dictionary.'
##         elif (name == 'yticlabels2'):
##            if (type(value) in (StringType, DictType)):
##               setGconmember(self,name,value)
##               self.__dict__['yticlabels2']=value
##            else:
##               raise ValueError, 'The yticlabels2 attribute must be either a string or a dictionary.'
##         elif (name == 'ymtics1'):
##            if (type(value) in (StringType, DictType)):
##               setGconmember(self,name,value)
##               self.__dict__['ymtics1']=value
##            else:
##               raise ValueError, 'The ymtics1 attribute must be either a string or a dictionary.'
##         elif (name == 'ymtics2'):
##            if (type(value) in (StringType, DictType)):
##               setGconmember(self,name,value)
##               self.__dict__['ymtics2']=value
##            else:
##               raise ValueError, 'The ymtics2 attribute must be either a string or a dictionary.'
##         elif (name == 'datawc_x1'):
##            if (type(value) in (IntType, FloatType)):
##               value = float(value)
##               setGconmember(self,name,value)
##               self.__dict__['datawc_x1']=value
##            else:
##               raise ValueError, 'The datawc_x1 attribute must be either an integer or a float value.'
##         elif (name == 'datawc_x2'):
##            if (type(value) in (IntType, FloatType)):
##               value = float(value)
##               setGconmember(self,name,value)
##               self.__dict__['datawc_x2']=value
##            else:
##               raise ValueError, 'The datawc_x2 attribute must be either an interger or a  float value.'
##         elif (name == 'datawc_y1'):
##            if (type(value) in (IntType, FloatType)):
##               value = float(value)
##               setGconmember(self,name,value)
##               self.__dict__['datawc_y1']=value
##            else:
##               raise ValueError, 'The datawc_y1 attribute must be either an interger or a  float value.'
##         elif (name == 'datawc_y2'):
##            if (type(value) in (IntType, FloatType)):
##               value = float(value)
##               setGconmember(self,name,value)
##               self.__dict__['datawc_y2']=value
##            else:
##               raise ValueError, 'The datawc_y2 attribute must be either an interger or a  float value.'
##         else:
##            raise ValueError, 'The member was not found.'

# 
# Doesn't make sense to inherit. This would mean more coding in C.
# I put this code back.                                 
#
    def xticlabels(self, xtl1='', xtl2=''):
        mode=self.parent.mode
        self.parent.mode=0
        self.__setattr__('xticlabels1', xtl1)
        self.parent.mode=mode
        self.__setattr__('xticlabels2', xtl2)
    xticlabels.__doc__ = xmldocs.xticlabelsdoc

    def xmtics(self,xmt1='', xmt2=''):
        mode=self.parent.mode
        self.parent.mode=0
        self.__setattr__('xmtics1', xmt1)
        self.parent.mode=mode
        self.__setattr__('xmtics2', xmt2)
    xmtics.__doc__ = xmldocs.xmticsdoc

    def yticlabels(self, ytl1='', ytl2=''):
        mode=self.parent.mode
        self.parent.mode=0
        self.__setattr__('yticlabels1', ytl1)
        self.parent.mode=mode
        self.__setattr__('yticlabels2', ytl2)
    yticlabels.__doc__ = xmldocs.yticlabelsdoc

    def ymtics(self, ymt1='', ymt2=''):
        mode=self.parent.mode
        self.parent.mode=0
        self.__setattr__('ymtics1', ymt1)
        self.parent.mode=mode
        self.__setattr__('ymtics2', ymt2)
    ymtics.__doc__ = xmldocs.ymticsdoc

    def datawc(self, dsp1=1e20, dsp2=1e20, dsp3=1e20, dsp4=1e20):
        mode=self.parent.mode
        self.parent.mode=0
        self.__setattr__('datawc_y1', dsp1)
        self.__setattr__('datawc_y2', dsp2)
        self.__setattr__('datawc_x1', dsp3)
        self.parent.mode=mode
        self.__setattr__('datawc_x2', dsp4)
    datawc.__doc__ = xmldocs.datawcdoc

    def list(self):
        if (self.name == '__removed_from_VCS__'):
           raise ValueError, 'This instance has been removed from VCS.'
        print "","----------Continents (Gcon) member (attribute) listings ----------"
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
        print "line = ", self.line
        print "linecolor = ", self.linecolor
        print "linewidth = ", self.linewidth
        print "type = ", self.type
    list.__doc__ = xmldocs.listdoc

    #############################################################################
    #                                                                           #
    # Script out primary boxfill graphics method in VCS to a file.              #
    #                                                                           #
    #############################################################################
    def script(self, script_filename, mode='a'):
        """
        %s
 Function:     script                           # Calls _vcs.scriptGcon

 Description of Function:
       Saves out a continents graphics method in Python or VCS script form to a
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
    con=a.createcontinents('temp')
    con.script('filename.py')         # Append to a Python file "filename.py"
    con.script('filename.scr')        # Append to a VCS file "filename.scr"
    con.script('filename','w')
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
           print _vcs.scriptGcon(self.name,script_filename,mode)
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

           unique_name = '__Gcon__' + self.name
           fp.write("#----------Continents (Gcon) member (attribute) listings ----------\n")
           fp.write("gcon_list=v.listelements('continents')\n")
           fp.write("if ('%s' in gcon_list):\n" % self.name)
           fp.write("   %s = v.getcontinents('%s')\n" % (unique_name, self.name))
           fp.write("else:\n")
           fp.write("   %s = v.createcontinents('%s')\n" % (unique_name, self.name))
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
           fp.write("%s.datawc_x1 = %g\n" % (unique_name, self.datawc_x1))
           fp.write("%s.datawc_y1 = %g\n" % (unique_name, self.datawc_y1))
           fp.write("%s.datawc_x2 = %g\n" % (unique_name, self.datawc_x2))
           fp.write("%s.datawc_y2 = %g\n" % (unique_name, self.datawc_y2))
           # Unique attribute for continents
           fp.write("%s.line = '%s'\n" % (unique_name, self.line))
           fp.write("%s.linecolor = %s\n" % (unique_name, self.linecolor))
           fp.write("%s.linewidth = %s\n" % (unique_name, self.linewidth))
           fp.write("%s.type = %g\n\n" % (unique_name, self.type))
    script.__doc__ = script.__doc__ % xmldocs.scriptdoc


#################################################################################
#        END OF FILE								#
#################################################################################
