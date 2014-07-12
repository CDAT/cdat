#
# Isoline (Gi) module
#
#################################################################################
#                                                                               #
# Module:       isoline (Gi) module                                             #
#                                                                               #
# Copyright:    2000, Regents of the University of California                   #
#               This software may not be distributed to others without          #
#               permission of the author.                                       #
#                                                                               #
# Author:       PCMDI Software Team                                             #
#               Lawrence Livermore NationalLaboratory:                          #
#               support@pcmdi.llnl.gov                                          #
#                                                                               #
# Description:  Python command wrapper for VCS's isoline graphics method.       #
#                                                                               #
# Version:      5.0                                                             #
#                                                                               #
#################################################################################
#
#
#
#################################################################################
#                                                                               #
# Import: VCS C extension module.                                               #
#                                                                               #
#################################################################################
import queries, vcs, VCS_validation_functions, cdtime
import Canvas
import AutoAPI
import xmldocs
import genutil
def load(nm,json_dict = {}):
  return
def get_att_from_sub(code,att):
  i = code.find(att)
  if i>-1:
    j = code[i:].find(',')
    if j!=-1:
      j+=i
    sp= code[i:j].split("=")
    try:
      return int(sp[1])
    except:
      try:
        return float(sp[1])
      except:
        return sp[1]

def process_src(nm,code):
  """Takes VCS script code (string) as input and generates isoline gm from it"""
  try:
    g = Gi(nm)
  except Exception,err:
    g = vcs.elements["isoline"][nm]
  ## process attributes with = as assignement
  for att in ["projection",
      "xticlabels#1","xticlabels#2",
      "xmtics#1","xmtics#2",
      "yticlabels#1","yticlabels#2",
      "ymtics#1","ymtics#2",
      "xaxisconvert","yaxisconvert",
      "datawc_tunits",
      "boxfill_type",
      "legend",
      "ext_1","ext_2",
      "missing",
      "datawc_calendar","make_labels"]:
    i = code.find(att)
    if i==-1:
      continue
    j = code[i:].find(",")+i
    if j-i==-1: # last one no comma
      j=None
    scode = code[i:j]
    sp = scode.split("=")
    nm = sp[0].strip()
    nm=nm.replace("#","")
    if nm=="make_labels":
      nm="label"
    elif nm=="datawc_tunits":
      nm = "datawc_timeunits"
    try:
      #int will be converted
      setattr(g,nm,int(sp[1]))
    except Exception,err:
      try:
        #int and floats will be converted
        setattr(g,nm,eval(sp[1]))
      except Exception,err:
        # strings
        try:
          setattr(g,nm,sp[1])
        except:
          pass # oh well we stick to default value
    #Datawc
    idwc = code.find(" datawc(")
    if idwc>-1:
      jdwc = code[idwc:].find(")")+idwc
      cd = code[idwc+8:jdwc]
      vals = cd.split(",")
      g.datawc_x1 = float(vals[0])
      g.datawc_y1 = float(vals[1])
      g.datawc_x2 = float(vals[2])
      g.datawc_y2 = float(vals[3])
    #idatawc
    idwc = code.find("idatawc(")
    if idwc>-1:
      jdwc = code[idwc:].find(")")+idwc
      cd = code[idwc+8:jdwc]
      vals = cd.split(",")
      if int(vals[0])==1:
        g.datawc_x1 = cdtime.reltime(gm.datawc_x1,g.datawc_timeunits).tocomp(g.datawc_calendar)
      if int(vals[1])==1:
        g.datawc_y1 = cdtime.reltime(gm.datawc_x2,g.datawc_timeunits).tocomp(g.datawc_calendar)
      if int(vals[2])==1:
        g.datawc_x2 = cdtime.reltime(gm.datawc_y1,g.datawc_timeunits).tocomp(g.datawc_calendar)
      if int(vals[3])==1:
        g.datawc_y2 = cdtime.reltime(gm.datawc_y2,g.datawc_timeunits).tocomp(g.datawc_calendar)
    irg=code.find("lines")
    if irg>-1:
      scode = code[irg:]
      label = 'n'
      tl=[]
      tt=[]
      to=[]
      clock = []
      scales = []
      angles =[]
      spacing = []
      levs=[]
      while scode.find("id=")>-1:
        sub = genutil.get_parenthesis_content(code[irg:])
        if get_att_from_sub(sub,"label") not in ["*",None]:
          g.label='y'
        levs.append(get_att_from_sub(sub,"level"))
        tl.append(get_att_from_sub(sub,"Tl"))
        to.append(get_att_from_sub(sub,"To"))
        tt.append(get_att_from_sub(sub,"Tt"))
        clock.append(get_att_from_sub(sub,"clockwise"))
        scales.append(get_att_from_sub(sub,"length"))
        angles.append(get_att_from_sub(sub,"angle"))
        spacing.append(get_att_from_sub(sub,"spacing"))
        scode = scode[scode.find(sub)+len(sub):]
      g.level = levs
      try:
        g.line = tl
      except:
        g._line = tl
      try:
        g.text = to
      except:
        g._text= to
      try:
        g.textcolor = tt
      except:
        g._textcolor = tt

      gd=vcs.elements["isoline"]["default"]
      try:
        g.scale = scales
      except:
        g.scale = gd.scale
      try:
        g.angle = angles
      except:
        g.angle=gd.angle
      try:
        g.clockwise = clock
      except:
        g.clockwise = gd.clockwise
class Gi(object,AutoAPI.AutoAPI):
    """
    Options:::
%s
    label :: (str) ('n') turn on/off labels on isolines
%s
%s
    level :: ([float,...]) ([[0.0, 1.0000000200408773e+20]]) isocountours to display
    clockwise :: ([int,...]) ([0]) draw directional arrows +-(0,1,2) indicate none/clockwise/clokwise on y axis >0/clockwise on x axis positive negative value invert behaviour
    scale :: ([float,...]) ([1.0]) scales the directional arrows length
    angle :: ([float,...]) ([35.]) directional arrows head angle
    spacing :: ([float,...]) ([1.0]) scales spacing between directional arrows
    :::
 Class:	Gi				# Isoline

 Description of Gi Class:
    The Isoline graphics method draws lines of constant value at specified
    levels in order to graphically represent a two-dimensional array. It
    also labels the values of these isolines on the VCS Canvas. The example
    below shows how to plot isolines of different types at specified levels 
    and how to create isoline labels having user-specified text and line type
    and color.

    This class is used to define an isoline table entry used in VCS, or it can
    be used to change some or all of the isoline attributes in an existing isoline
    table entry.

 Other Useful Functions:
	    a=vcs.init()		# Constructor
	    a.show('isoline')		# Show predefined isoline graphics methods
	    a.show('line')		# Show predefined VCS line objects
	    a.setcolormap("AMIP")	# Change the VCS color map
	    a.isoline(s,a,'default') 	# Plot data 's' with isoline 'i' and 
						'default' template
	    a.update()		 	# Updates the VCS Canvas at user's request
	    a.mode=1, or 0 	 	# If 1, then automatic update, else if
				   	  0, then use update function to
				   	  update the VCS Canvas.

 Example of Use:
    a=vcs.init()
    To Create a new instance of isoline use:
     iso=a.createisoline('new','quick') # Copies content of 'quick' to 'new'
     iso=a.createisoline('new') 	# Copies content of 'default' to 'new'

    To Modify an existing isoline use:
	iso=a.getisoline('AMIP_psl')

    iso.list()  			# Will list all the isoline attribute values
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

    There are many possibilities ways to set the isoline values:
	A) As a list of tuples (Examples):
		iso.level=[(23,32,45,50,76),]
		iso.level=[(22,33,44,55,66)]
		iso.level=[(20,0.0),(30,0),(50,0)]
		iso.level=[(23,32,45,50,76), (35, 45, 55)]
	B) As a tuple of lists (Examples):
		iso.level=([23,32,45,50,76],)
		iso.level=([22,33,44,55,66])
		iso.level=([23,32,45,50,76],)
		iso.level=([0,20,25,30,35,40],[30,40],[50,60])
	C) As a list of lists (Examples):
		iso.level=[[20,0.0],[30,0],[50,0]]
	D) As a tuple of tuples (Examples):
		iso.level=((20,0.0),(30,0),(50,0),(60,0),(70,0))

	Note: a combination of a pair (i.e., (30,0) or [30,0]) represents
		the isoline value plus it increment value. Thus, to let VCS
		generate "default" isolines enter the following:	
		iso.level=[[0,1e20]]  	# Same as iso.level=((0,1e20),)

    Displaying isoline labels:
	iso.label='y'  			# Same as iso.label=1, will display isoline labels
	iso.label='n'  			# Same as iso.label=0, will turn isoline labels off

    Specify the isoline line style (or type):
	iso.line=([0,1,2,3,4])   	# Same as
	iso.line=(['solid, 'dash', 'dot', 'dash-dot', 'long-dash']), will
		specify the isoline style

    There are three possibilities for setting the line color indices (Ex):
	iso.linecolors=(22,33,44,55,66,77)	# Same as
	iso.linecolors=([22,33,44,55,66,77])	# Will set the isoline to a specific
		                              	#     color index
	iso.linecolors=None			# Turns off the line color index

    There are three possibilities for setting the line widths (Ex):
	iso.linewidths=(1,10,3,4,5,6,7,8)	# Same as
	iso.linewidths=([1,2,3,4,5,6,7,8])	# Will set the isoline to a specific
		                              	#     width size
	iso.linewidths=None			# Turns off the line width size

    There are three ways to specify the text or font number:
	iso.text=(1,2,3,4,5,6,7,8,9)     	# Font numbers are between 1 and 9
	iso.text=[9,8,7,6,5,4,3,2,1]
	iso.text=([1,3,5,6,9,2])
	iso.text=None		        	# Removes the text settings

    There are three possibilities for setting the text color indices (Ex.):
        iso.textcolors=([22,33,44,55,66,77])
	iso.textcolors=(16,19,33,44)
	iso.textcolors=None	        	# Turns off the text color index
"""
    colormap = VCS_validation_functions.colormap
    __slots__=[
         '__doc__',
         'colormap',
         '_colormap',
         'name',
         'g_name',
         'xaxisconvert',
         'yaxisconvert',
         'levels',
         'level',
         'label',
         'linecolors',
         'line',
         'linewidths',
         'text',
         'textcolors',
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
         'clockwise',
         'scale',
         'angle',
         'spacing',
         '_name',
         '_xaxisconvert',
         '_yaxisconvert',
         '_level',
         '_label',
         '_linecolors',
         '_line',
         '_linewidths',
         '_text',
         '_textcolors',
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
         '_clockwise',
         '_scale',
         '_angle',
         '_spacing',
         'info',
         ]
 
    def _getname(self):
         return self._name
    def _setname(self,value):
         value=VCS_validation_functions.checkname(self,'name',value)
         self._name=value
    name=property(_getname,_setname)

    def _getcalendar(self):
         return self._datawc_calendar
    def _setcalendar(self,value):
         value=VCS_validation_functions.checkCalendar(self,'datawc_calendar',value)
         self._datawc_calendar=value
    datawc_calendar=property(_getcalendar,_setcalendar)

    def _gettimeunits(self):
         return self._datawc_timeunits
    def _settimeunits(self,value):
         value=VCS_validation_functions.checkTimeUnits(self,'datawc_timeunits',value)
         self._datawc_timeunits=value
    datawc_timeunits=property(_gettimeunits,_settimeunits)

    def _getxaxisconvert(self):
         return self._xaxisconvert
    def _setxaxisconvert(self,value):
         value=VCS_validation_functions.checkAxisConvert(self,'xaxisconvert',value)
         self._xaxisconvert=value
    xaxisconvert=property(_getxaxisconvert,_setxaxisconvert)

    def _getyaxisconvert(self):
         return self._yaxisconvert
    def _setyaxisconvert(self,value):
         value=VCS_validation_functions.checkAxisConvert(self,'yaxisconvert',value)
         self._yaxisconvert=value
    yaxisconvert=property(_getyaxisconvert,_setyaxisconvert)

    def _getlevels(self):
         return self._level
    def _setlevels(self,value):
         value=VCS_validation_functions.checkIsolineLevels(self,'levels',value)
         self._level=value
    level=property(_getlevels,_setlevels)
    levels=property(_getlevels,_setlevels)

    def _getprojection(self):
         return self._projection
    def _setprojection(self,value):
         value=VCS_validation_functions.checkProjection(self,'projection',value)
         self._projection=value
    projection=property(_getprojection,_setprojection)

    def _getxticlabels1(self):
         return self._xticlabels1
    def _setxticlabels1(self,value):
         value=VCS_validation_functions.checkStringDictionary(self,'xticlabels1',value)
         self._xticlabels1=value
    xticlabels1=property(_getxticlabels1,_setxticlabels1)

    def _getxticlabels2(self):
         return self._xticlabels2
    def _setxticlabels2(self,value):
         value=VCS_validation_functions.checkStringDictionary(self,'xticlabels2',value)
         self._xticlabels2=value
    xticlabels2=property(_getxticlabels2,_setxticlabels2)

    def _getyticlabels1(self):
         return self._yticlabels1
    def _setyticlabels1(self,value):
         value=VCS_validation_functions.checkStringDictionary(self,'yticlabels1',value)
         self._yticlabels1=value
    yticlabels1=property(_getyticlabels1,_setyticlabels1)

    def _getyticlabels2(self):
         return self._yticlabels2
    def _setyticlabels2(self,value):
         value=VCS_validation_functions.checkStringDictionary(self,'yticlabels2',value)
         self._yticlabels2=value
    yticlabels2=property(_getyticlabels2,_setyticlabels2)

    def _getxmtics1(self):
         return self._xmtics1
    def _setxmtics1(self,value):
         value=VCS_validation_functions.checkStringDictionary(self,'xmtics1',value)
         self._xmtics1=value
    xmtics1=property(_getxmtics1,_setxmtics1)

    def _getxmtics2(self):
         return self._xmtics2
    def _setxmtics2(self,value):
         value=VCS_validation_functions.checkStringDictionary(self,'xmtics2',value)
         self._xmtics2=value
    xmtics2=property(_getxmtics2,_setxmtics2)

    def _getymtics1(self):
         return self._ymtics1
    def _setymtics1(self,value):
         value=VCS_validation_functions.checkStringDictionary(self,'ymtics1',value)
         self._ymtics1=value
    ymtics1=property(_getymtics1,_setymtics1)

    def _getymtics2(self):
         return self._ymtics2
    def _setymtics2(self,value):
         value=VCS_validation_functions.checkStringDictionary(self,'ymtics2',value)
         self._ymtics2=value
    ymtics2=property(_getymtics2,_setymtics2)

    def _getdatawc_x1(self):
          return self._datawc_x1
    def _setdatawc_x1(self,value):
         value2=VCS_validation_functions.checkDatawc(self,'datawc_x1',value)
         self._datawc_x1=value
    datawc_x1=property(_getdatawc_x1,_setdatawc_x1)

    def _getdatawc_x2(self):
              return self._datawc_x2
    def _setdatawc_x2(self,value):
         value2=VCS_validation_functions.checkDatawc(self,'datawc_x2',value)
         self._datawc_x2=value
    datawc_x2=property(_getdatawc_x2,_setdatawc_x2)
    
    def _getdatawc_y1(self):
              return self._datawc_y1
    def _setdatawc_y1(self,value):
         value2=VCS_validation_functions.checkDatawc(self,'datawc_y1',value)
         self._datawc_y1=value
    datawc_y1=property(_getdatawc_y1,_setdatawc_y1)

    def _getdatawc_y2(self):
              return self._datawc_y2
    def _setdatawc_y2(self,value):
         value2=VCS_validation_functions.checkDatawc(self,'datawc_y2',value)
         self._datawc_y2=value
    datawc_y2=property(_getdatawc_y2,_setdatawc_y2)
    
    def _getlinewidths(self):
         return self._linewidths
    def _setlinewidths(self,value):
         if not value is None:
              value = VCS_validation_functions.checkListOfNumbers(self,'linewidths',value,0,300)
         self._linewidths=value
    linewidths=property(_getlinewidths,_setlinewidths)
    
    def _getlinecolors(self):
         return self._linecolors
    def _setlinecolors(self,value):
         if not value is None:
              value = VCS_validation_functions.checkColorList(self,'linecolors',value)
         self._linecolors=value
    linecolors=property(_getlinecolors,_setlinecolors)
    
    def _getline(self):
         return self._line
    def _setline(self,value):
         if not value is None:
              value = VCS_validation_functions.checkLinesList(self,'line',value)
         self._line=value
    line=property(_getline,_setline)
    
    def _gettext(self):
         return self._text
    def _settext(self,value):
         if not value is None:
              value = VCS_validation_functions.checkTextsList(self,'text',value)
         self._text=value
    text=property(_gettext,_settext)

    def _gettextcolors(self):
         return self._textcolors
    def _settextcolors(self,value):
         if not value is None:
              value = VCS_validation_functions.checkColorList(self,'textcolors',value)
         self._textcolors=value
    textcolors=property(_gettextcolors,_settextcolors)

    def _getlabel(self):
         return self._label
    def _setlabel(self,value):
         value = VCS_validation_functions.checkYesNo(self,'label',value)
         self._label=value
    label=property(_getlabel,_setlabel)

    def _getspacing(self):
         return self._spacing
    def _setspacing(self,value):
         if not value is None:
              value = VCS_validation_functions.checkListOfNumbers(self,'spacing',value,0.)
         self._spacing=value
    spacing=property(_getspacing,_setspacing)
    def _getangle(self):
         return self._angle
    def _setangle(self,value):
         if not value is None:
              value = VCS_validation_functions.checkListOfNumbers(self,'angle',value,0.,90.)
         self._angle=value
    angle=property(_getangle,_setangle)
    def _getscale(self):
         return self._scale
    def _setscale(self,value):
         if not value is None:
              value = VCS_validation_functions.checkListOfNumbers(self,'scale',value,0.)
         self._scale=value
    scale=property(_getscale,_setscale)
    def _getclockwise(self):
         return self._clockwise
    def _setclockwise(self,value):
         if not value is None:
              value = VCS_validation_functions.checkListOfNumbers(self,'clockwise',value,-3,3,ints=True)
         self._clockwise=value
    clockwise=property(_getclockwise,_setclockwise)
    def __init__(self, Gi_name, Gi_name_src='default'):
	#                                                         #
        ###########################################################
	# Initialize the isoline class and its members            #
        #							  #
	# The getGimember function retrieves the values of the   #
        # isoline members in the C structure and passes back the  #
	# appropriate Python Object.                              #
        ###########################################################
	#                                                         #
        if not isinstance(Gi_name,str):
          raise ValueError,"Isoline name must be a string"
        if Gi_name in vcs.elements["isoline"].keys():
          raise ValueError,"isoline graphic method '%s' already exists" % Gi_name
        self._name = Gi_name
        self.g_name='Gi'
        if Gi_name=="default": 
          self._projection="linear"
          self._xticlabels1="*"
          self._xticlabels2="*"
          self._xmtics1=""
          self._xmtics2=""
          self._yticlabels1="*"
          self._yticlabels2="*"
          self._ymtics1=""
          self._ymtics2=""
          self._datawc_y1=1.e20
          self._datawc_y2=1.e20
          self._datawc_x1=1.e20
          self._datawc_x2=1.e20
          self._xaxisconvert='linear'
          self._yaxisconvert='linear'
          self._line = ['solid',]
          self._linecolors = [1,]
          self._linewidths = [1.,]
          self._text = None
          self._textcolors = None
          self._level=[[0., 1.0000000200408773e+20],]
          self._datawc_timeunits="days since 2000"
          self._datawc_calendar=135441
          self._clockwise = [0,]
          self._scale = [1.,]
          self._angle = [35.]
          self._spacing = [1.]
          self._label = 'n'
          self._colormap = None
        else:
          if isinstance(Gi_name_src,Gi):
            Gi_name_src=Gi_name_src.name
          if not Gi_name_src in vcs.elements["isoline"].keys():
            raise ValueError,"Isoline method '%s' does not exists" % Gi_name_src
          src =vcs.elements["isoline"][Gi_name_src]
          for att in ['label','colormap', 'projection' ,'xticlabels1' ,'xticlabels2' ,'xmtics1' ,'xmtics2' ,'yticlabels1' ,'yticlabels2' ,'ymtics1' ,'ymtics2' ,'datawc_y1' ,'datawc_y2' ,'datawc_x1' ,'datawc_x2' ,'xaxisconvert' ,'yaxisconvert' ,'level' ,'datawc_timeunits' ,'datawc_calendar',"line","linecolors","linewidths","text","textcolors","clockwise","scale","angle","spacing"]:
            setattr(self,att,getattr(src,att))
        self.info=AutoAPI.Info(self)
        self.info.expose=['ALL']
        #self.info.hide+=["fillareastyle","fillareaindices"]
        self.__doc__ = self.__doc__ % (xmldocs.graphics_method_core,xmldocs.linesdoc,xmldocs.textsdoc)
        vcs.elements["isoline"][Gi_name] = self

# 
# Doesn't make sense to inherit. This would mean more coding in C.
# I put this code back.                                 
#
    def xticlabels(self, xtl1='', xtl2=''):
##          specific_options_doc 
         self.xticlabels1= xtl1
         self.xticlabels2= xtl2
    xticlabels.__doc__ = xmldocs.xticlabelsdoc

    def xmtics(self,xmt1='', xmt2=''):
        self.xmtics1= xmt1
        self.xmtics2= xmt2
    xmtics.__doc__ = xmldocs.xmticsdoc

    def yticlabels(self, ytl1='', ytl2=''):
        self.yticlabels1= ytl1
        self.yticlabels2= ytl2
    yticlabels.__doc__ = xmldocs.yticlabelsdoc

    def ymtics(self, ymt1='', ymt2=''):
        self.ymtics1= ymt1
        self.ymtics2= ymt2
    ymtics.__doc__ = xmldocs.ymticsdoc

    def datawc(self, dsp1=1e20, dsp2=1e20, dsp3=1e20, dsp4=1e20):
        self.datawc_y1= dsp1
        self.datawc_y2= dsp2
        self.datawc_x1= dsp3
        self.datawc_x2= dsp4
    datawc.__doc__ = xmldocs.datawcdoc

    def xyscale(self, xat='', yat=''):
        self.xaxisconvert= xat
        self.yaxisconvert= yat
    xyscale.__doc__= xmldocs.xyscaledoc

    def list(self):
        if (self.name == '__removed_from_VCS__'):
           raise ValueError, 'This instance has been removed from VCS.'
        print "","----------Isoline (Gi) member (attribute) listings ----------"
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
        print "label = ", self.label
        print "line = ", self.line
        print "linecolors = ", self.linecolors
        print "linewidths = ", self.linewidths
        print "text = ", self.text
        print "textcolors = ", self.textcolors
        print "level = ", self.level
        print "clockwise = ",self.clockwise
        print "scale = ",self.scale
        print "angle = ",self.angle
        print "spacing = ",self.spacing
    list.__doc__ = xmldocs.listdoc

    #############################################################################
    #                                                                           #
    # Script out primary isoline graphics method in VCS to a file.              #
    #                                                                           #
    #############################################################################
    def script(self, script_filename, mode='a'):
        """
 %s
 Function:     script                           # Calls _vcs.scriptGi

 Description of Function:
       Saves out a isoline graphics method in Python and VCS script form to a
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
    iso=a.createisoline('temp')
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

           unique_name = '__Gi__' + self.name
           fp.write("#----------Isoline (Gi) member (attribute) listings ----------\n")
           fp.write("gi_list=v.listelements('isoline')\n")
           fp.write("if ('%s' in gi_list):\n" % self.name)
           fp.write("   %s = v.getisoline('%s')\n" % (unique_name, self.name))
           fp.write("else:\n")
           fp.write("   %s = v.createisoline('%s')\n" % (unique_name, self.name))
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
           # Unique attribute for isoline
           fp.write("%s.label = '%s'\n" % (unique_name, self.label))
           fp.write("%s.line = %s\n" % (unique_name, self.line))
           fp.write("%s.linecolors = %s\n" % (unique_name, self.linecolors))
           fp.write("%s.linewidths = %s\n" % (unique_name, self.linewidths))
           fp.write("%s.text = %s\n" % (unique_name, self.text))
           fp.write("%s.textcolors = %s\n" % (unique_name, self.textcolors))
           fp.write("%s.level = %s\n\n" % (unique_name, self.level))

           fp.write("%s.clockwise =  '%s'\n" % (unique_name,self.clockwise))
           fp.write("%s.scale =  '%s'\n" % (unique_name,self.scale))
           fp.write("%s.angle =  '%s'\n" % (unique_name,self.angle))
           fp.write("%s.spacing =  '%s'\n" % (unique_name,self.spacing))
           fp.write("%s.colormap = '%s'\n\n" % (unique_name, repr(self.colormap)))
        else:
          #Json type
          mode+="+"
          f = open(script_filename,mode)
          vcs.utils.dumpToJson(self,f)
          f.close()
    script.__doc__ = script.__doc__ % xmldocs.scriptdoc

#################################################################################
#        END OF FILE								#
#################################################################################
