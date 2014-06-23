"""
# Unification of all 1D gms
"""
###############################################################################
#                                                                             #
# Module:       yxvsx (GYx) module                                            #
#                                                                             #
# Copyright:    2000, Regents of the University of California                 #
#               This software may not be distributed to others without        #
#               permission of the author.                                     #
#                                                                             #
# Authors:      PCMDI Software Team                                           #
#               Lawrence Livermore NationalLaboratory:                        #
#               support@pcmdi.llnl.gov                                        #
#                                                                             #
# Description:  Python command wrapper for VCS's yxvsx graphics method.       #
#                                                                             #
# Version:      4.0                                                           #
#                                                                             #
###############################################################################
#
#
#
import VCS_validation_functions, cdtime
import AutoAPI
import xmldocs
import vcs

def process_src(nm,code,typ):
  """Takes VCS script code (string) as input and generates oneD gm from it"""
  onm = nm+""
  if typ=="GYx":
    nm+="_yxvsx"
  elif typ == "GXY":
    nm+="_xvsy"
  elif typ == "GXy":
    nm+="_xyvsy"
  elif typ == "GSp":
    nm+="_scatter"
  try:
    gm = G1d(nm)
  except:
    gm = vcs.elements["oned"][nm]
  ## process attributes with = as assignement
  for att in ["projection",
      "xticlabels#1","xticlabels#2",
      "xmtics#1","xmtics#2",
      "yticlabels#1","yticlabels#2",
      "ymtics#1","ymtics#2",
      "xaxisconvert","yaxisconvert",
      "datawc_tunits",
      "Tl",
      "Tm",
      "datawc_tunits",
      "datawc_calendar"]:
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
    if nm=="Tl":
      nm="line"
    elif nm=="Tm":
      nm="marker"
    try:
      #int will be converted
      setattr(gm,nm,int(sp[1]))
    except Exception,err:
      try:
        #int and floats will be converted
        setattr(gm,nm,eval(sp[1]))
      except Exception,err:
        # strings
        try:
          setattr(gm,nm,sp[1])
        except:
          pass # oh well we stick to default value
    #Datawc
    idwc = code.find(" datawc(")
    if idwc>-1:
      jdwc = code[idwc:].find(")")+idwc
      cd = code[idwc+8:jdwc]
      vals = cd.split(",")
      gm.datawc_x1 = float(vals[0])
      gm.datawc_y1 = float(vals[1])
      gm.datawc_x2 = float(vals[2])
      gm.datawc_y2 = float(vals[3])
    #idatawc
    idwc = code.find("idatawc(")
    if idwc>-1:
      jdwc = code[idwc:].find(")")+idwc
      cd = code[idwc+8:jdwc]
      vals = cd.split(",")
      if int(vals[0])==1:
        gm.datawc_x1 = cdtime.reltime(gm.datawc_x1,gm.datawc_timeunits).tocomp(gm.datawc_calendar)
      if int(vals[1])==1:
        gm.datawc_y1 = cdtime.reltime(gm.datawc_x2,gm.datawc_timeunits).tocomp(gm.datawc_calendar)
      if int(vals[2])==1:
        gm.datawc_x2 = cdtime.reltime(gm.datawc_y1,gm.datawc_timeunits).tocomp(gm.datawc_calendar)
      if int(vals[3])==1:
        gm.datawc_y2 = cdtime.reltime(gm.datawc_y2,gm.datawc_timeunits).tocomp(gm.datawc_calendar)
  if typ=="GYx":
    vcs.elements["yxvsx"][onm]=gm
  elif typ == "GXY":
    vcs.elements["xvsy"][onm]=gm
  elif typ == "GXy":
    gm.flip = True
    vcs.elements["xyvsy"][onm]=gm
  elif typ == "GSp":
    vcs.elements["scatter"][onm]=gm

class G1d(object,AutoAPI.AutoAPI):
    """
    Options:::
%s
%s
%s
%s
:::
 Class:	G1d			# General 1D plots 

 Description of G1d Class:
    This graphics method displays a line plot from 1D data array (i.e. a
    plot of Y(x), where y represents the 1D coordinate values, and x can be either Y's axis or another 1D arrays).
    The example below
    shows how to change line and marker attributes for the Yxvsx graphics method.

    This class is used to define an Yxvsx table entry used in VCS, or it can be
    used to change some or all of the Yxvsx attributes in an existing Yxvsx table
    entry.

 Example of Use:
    a=vcs.init()
    To Create a new instance of Yxvsx use:
     yxx=a.create1D('new','quick')    # Copies content of 'quick' to 'new'
     yxx=a.create1D('new')            # Copies content of 'default' to 'new'

    To Modify an existing Yxvsx use:
     yxx=a.get1D('AMIP_psl')

    yxx.list()                          # Will list all the Yxvsx attribute values
    yxx.projection='linear'             # Can only be 'linear'
    lon30={-180:'180W',-150:'150W',0:'Eq'}
    yxx.xticlabels1=lon30
    yxx.xticlabels2=lon30
    yxx.xticlabels(lon30, lon30)        # Will set them both
    yxx.xmtics1=''
    yxx.xmtics2=''
    yxx.xmtics(lon30, lon30)            # Will set them both
    yxx.yticlabels1=lat10
    yxx.yticlabels2=lat10
    yxx.yticlabels(lat10, lat10)        # Will set them both
    yxx.ymtics1=''
    yxx.ymtics2=''
    yxx.ymtics(lat10, lat10)            # Will set them both
    yxx.datawc_y1=-90.0
    yxx.datawc_y2=90.0
    yxx.datawc_x1=-180.0
    yxx.datawc_x2=180.0
    yxx.datawc(-90, 90, -180, 180)      # Will set them all
    yxx.xaxisconvert='linear'

    Specify the Yxvsx line type:
     yxx.line=0                         # same as yxx.line = 'solid'
     yxx.line=1                         # same as yxx.line = 'dash'
     yxx.line=2                         # same as yxx.line = 'dot'
     yxx.line=3                         # same as yxx.line = 'dash-dot'
     yxx.line=4                         # same as yxx.line = 'long-dash

    Specify the Yxvsx line color:
    yxx.linecolor=16    # color range: 16 to 230, default color is black
    yxx.linewidth=1     # width range: 1 to 100, default color is 1

    Specify the Yxvsx marker type:
     yxx.marker=1                       # Same as yxx.marker='dot'
     yxx.marker=2                       # Same as yxx.marker='plus'
     yxx.marker=3                       # Same as yxx.marker='star'
     yxx.marker=4                       # Same as yxx.marker='circle'
     yxx.marker=5                       # Same as yxx.marker='cross'
     yxx.marker=6                       # Same as yxx.marker='diamond'
     yxx.marker=7                       # Same as yxx.marker='triangle_up'
     yxx.marker=8                       # Same as yxx.marker='triangle_down'
     yxx.marker=9                       # Same as yxx.marker='triangle_left'
     yxx.marker=10                      # Same as yxx.marker='triangle_right'
     yxx.marker=11                      # Same as yxx.marker='square'
     yxx.marker=12                      # Same as yxx.marker='diamond_fill'
     yxx.marker=13                      # Same as yxx.marker='triangle_up_fill'
     yxx.marker=14                      # Same as yxx.marker='triangle_down_fill'
     yxx.marker=15                      # Same as yxx.marker='triangle_left_fill'
     yxx.marker=16                      # Same as yxx.marker='triangle_right_fill'
     yxx.marker=17                      # Same as yxx.marker='square_fill'
     yxx.marker=None                    # Draw no markers

    There are four possibilities for setting the marker color index (Ex):
     yxx.markercolors=22                # Same as below
     yxx.markercolors=(22)              # Same as below
     yxx.markercolors=([22])            # Will set the markers to a specific
                                          color index
     yxx.markercolors=None              # Color index defaults to Black

    To set the Yxvsx Marker sizie:
     yxx.markersize=5
     yxx.markersize=55
     yxx.markersize=100
     yxx.markersize=300
     yxx.markersize=None

"""
    colormap = VCS_validation_functions.colormap
    __slots__=[
         '__doc__',
         'name',
         'info',
         'colormap',
         '_colormap',
         'g_name',
         'xaxisconvert',
         'yaxisconvert',
         'linecolor',
         'line',
         'linewidth',
         'marker',
         'markersize',
         'markercolor',
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
         'flip',
         'smooth',
         '_name',
         '_xaxisconvert',
         '_yaxisconvert',
         '_linecolor',
         '_line',
         '_linewidth',
         '_marker',
         '_markersize',
         '_markercolor',
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
         '_flip',
         '_smooth',
         ]
    def _getname(self):
         return self._name
    def _setname(self,value):
         value=VCS_validation_functions.checkname(self,'name',value)
         if value is not None:
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
         value=VCS_validation_functions.checkDatawc(self,'datawc_x1',value)
         self._datawc_x1=value[0]
    datawc_x1=property(_getdatawc_x1,_setdatawc_x1)

    def _getdatawc_x2(self):
              return self._datawc_x2
    def _setdatawc_x2(self,value):
         value=VCS_validation_functions.checkDatawc(self,'datawc_x2',value)
         self._datawc_x2=value[0]
    datawc_x2=property(_getdatawc_x2,_setdatawc_x2)
    
    def _getdatawc_y1(self):
              return self._datawc_y1
    def _setdatawc_y1(self,value):
         value=VCS_validation_functions.checkDatawc(self,'datawc_y1',value)
         self._datawc_y1=value[0]
    datawc_y1=property(_getdatawc_y1,_setdatawc_y1)

    def _getdatawc_y2(self):
              return self._datawc_y2
    def _setdatawc_y2(self,value):
         value=VCS_validation_functions.checkDatawc(self,'datawc_y2',value)
         self._datawc_y2=value[0]
    datawc_y2=property(_getdatawc_y2,_setdatawc_y2)
    
    def _getlinewidth(self):
         return self._linewidth
    def _setlinewidth(self,value):
         if not value is None:
              value = VCS_validation_functions.checkNumber(self,'linewidth',value,0,300)
         self._linewidth=value
    linewidth=property(_getlinewidth,_setlinewidth)
    
    def _getlinecolor(self):
         return self._linecolor
    def _setlinecolor(self,value):
         if not value is None:
              value = VCS_validation_functions.checkColor(self,'linecolor',value)
         self._linecolor=value
    linecolor=property(_getlinecolor,_setlinecolor)
    
    def _getline(self):
         return self._line
    def _setline(self,value):
         if not value is None:
              value = VCS_validation_functions.checkLineType(self,'line',value)
         self._line=value
    line=property(_getline,_setline)
    
    def _getmarker(self):
         return self._marker
    def _setmarker(self,value):
         if not value is None:
              value = VCS_validation_functions.checkMarker(self,'marker',value)
         self._marker=value
    marker=property(_getmarker,_setmarker)

    def _getmarkersize(self):
         return self._markersize
    def _setmarkersize(self,value):
         if not value is None:
              value = VCS_validation_functions.checkInt(self,'markersize',value,1,300)
         self._markersize=value
    markersize=property(_getmarkersize,_setmarkersize)
    
    def _getmarkercolor(self):
         return self._markercolor
    def _setmarkercolor(self,value):
         if not value is None:
              value = VCS_validation_functions.checkColor(self,'markercolor',value)
         self._markercolor=value
    markercolor=property(_getmarkercolor,_setmarkercolor)
    def _getflip(self):
      return self._flip
    def _setflip(self,value):
      value = VCS_validation_functions.checkTrueFalse(self,'flip',value)
      self._flip = value
    flip=property(_getflip,_setflip)
    def _getsmooth(self):
      return self._smooth
    def _setsmooth(self,value):
      if value is not None:
        value = VCS_validation_functions.checkInt(self,"smooth",value,1)
      self._smooth=value
    smooth=property(_getsmooth,_setsmooth,None,"beta parameter for kaiser smoothing")

    def __init__(self, name, name_src='default'):
        #                                                         #
        ###########################################################
	    # Initialize the Yxvsx class and its members              #
        #							                              #
	    # The getGYxmember function retrieves the values of the   #
        # Yxvsx members in the C structure and passes back the    #
	    # appropriate Python Object.                              #
        ###########################################################
	    #                                                         #
        self.info=AutoAPI.Info(self)
        self.info.expose=['ALL']
        self.__doc__ = self.__doc__ % (xmldocs.graphics_method_core,xmldocs.xaxisconvert,xmldocs.linedoc,xmldocs.markerdoc)
        if name in vcs.elements["oned"]:
          raise ValueError,"The 1D method '%s' already exists"
        self.g_name='G1d'
        self._name = name
        if name == 'default':
            self._smooth=None
            self._flip = False
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
            self._xaxisconvert="linear"
            self._yaxisconvert="linear"
            self._line='solid'
            self._linecolor=241
            self._linewidth=1
            self._marker='dot'
            self._markercolor=241
            self._markersize=1
            self._datawc_timeunits="days since 2000"
            self._datawc_calendar=135441
            self._colormap = None
        else:
          if isinstance(name_src,G1d):
            name_src=name_src.name
          if not name_src in vcs.elements['oned']:
            raise ValueError, "The oneD method '%s' does not exists" % name_src
          src = vcs.elements["oned"][name_src]
          for att in ['projection' , 'colormap', 'xticlabels1' ,'xticlabels2' ,'xmtics1' ,'xmtics2' ,'yticlabels1' ,'yticlabels2' ,'ymtics1' ,'ymtics2' ,'datawc_y1' ,'datawc_y2' ,'datawc_x1' ,'datawc_x2' ,'xaxisconvert' ,'yaxisconvert' ,'line' ,'linecolor' ,'linewidth' ,'marker' ,'markercolor' ,'markersize' ,'datawc_timeunits' ,'datawc_calendar' ,'smooth', 'flip' ]:
           setattr(self,att,getattr(src,att)) 
        #Ok now we need to stick in the elements
        vcs.elements["oned"][name]=self
        #                                                         #
        ###########################################################
        # Find and set the Yxvsx structure in VCS C pointer       #
        # list. If the Yxvsx name does not exist, then use        #
        # default Yxvsx.                                          #
        ###########################################################
        #                                                         #

# 
# Doesn't make sense to inherit. This would mean more coding in C.
# I put this code back.                                 
#
    def xticlabels(self, xtl1='', xtl2=''):
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


    def list(self):
        if (self.name == '__removed_from_VCS__'):
           raise ValueError, 'This instance has been removed from VCS.'
        print "","----------Yxvsx (GYx) member (attribute) listings ----------"
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
        print "marker = ", self.marker
        print "markercolor = ", self.markercolor
        print "markersize = ", self.markersize
        print "flip = ",self.flip
    list.__doc__ = xmldocs.listdoc

    ###########################################################################
    #                                                                         #
    # Script out primary Yxvsx graphics method in VCS to a file.              #
    #                                                                         #
    ###########################################################################
    def script(self, script_filename, mode='a'):
        """
 %s
 Function:     script                           # Calls _vcs.scriptGYx

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

    a=vcs.init()
    Yx=a.createboxfill('temp')
    Yx.script('filename.py')         # Append to a Python file "filename.py"
    Yx.script('filename.scr')        # Append to a VCS file "filename.scr"
    Yx.script('filename','w')
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

           unique_name = '__OneD__' + self.name
           fp.write("#----------OneD (GOneD) member (attribute) listings ----------\n")
           fp.write("oned_list=v.listelements('oned')\n")
           fp.write("if ('%s' in oned_list):\n" % self.name)
           fp.write("   %s = v.getoned('%s')\n" % (unique_name, self.name))
           fp.write("else:\n")
           fp.write("   %s = v.createoneD('%s')\n" % (unique_name, self.name))
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
           # Unique attribute for yxvsx
           fp.write("%s.line = %s\n" % (unique_name, self.line))
           fp.write("%s.linecolor = %s\n" % (unique_name, self.linecolor))
           fp.write("%s.linewidth = %s\n" % (unique_name, self.linewidth))
           fp.write("%s.marker = %s\n" % (unique_name, self.marker))
           fp.write("%s.markercolor = %s\n" % (unique_name, self.markercolor))
           fp.write("%s.markersize = %s\n\n" % (unique_name, self.markersize))
           fp.write("%s.flip = '%s'\n\n" % (unique_name, repr(self.flip)))
           fp.write("%s.colormap = '%s'\n\n" % (unique_name, repr(self.colormap)))
        else:
          #Json type
          mode+="+"
          f = open(script_filename,mode)
          vcs.utils.dumpToJson(self,f)
          f.close()
    script.__doc__ = script.__doc__ % xmldocs.scriptdoc



###############################################################################
#        END OF FILE							      #
###############################################################################
