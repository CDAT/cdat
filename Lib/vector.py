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
import queries, vcs, VCS_validation_functions, cdtime
import Canvas

def load(nm,json_dict = {}):
  return

def process_src(nm,code):
  """Takes VCS script code (string) as input and generates vector gm from it"""
  try:
    gm = Gv(nm)
  except Exception,err:
    gm = vcs.elements["vector"][nm]
  ## process attributes with = as assignement
  for att in ["projection",
      "xticlabels#1","xticlabels#2",
      "xmtics#1","xmtics#2",
      "yticlabels#1","yticlabels#2",
      "ymtics#1","ymtics#2",
      "xaxisconvert","yaxisconvert",
      "datawc_tunits",
      "datawc_tunits",
      "datawc_calendar",
      "Tl","vector_scale",
      "vector_align","vector_type","ref_vector",
      ]:
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
    elif nm=="vector_scale":
      nm="scale"
    elif nm == "vector_align":
      nm="alignement"
      if sp[1]=="c":
        sp[1]="center"
      elif sp[1]=="h":
        sp[1]="head"
      elif sp[1]=="t":
        sp[1]="tail"
    elif nm=="ref_vector":
      nm="reference"
    elif nm=="vector_type":
      nm="type"
    elif nm=="datawc_tunits":
      nm = "datawc_timeunits"
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
	 a=vcs.init()			# Constructor
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
    a=vcs.init()
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
    __slots__=[
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
    
    def _getreference(self):
         return self._reference
    def _setreference(self,value):
         value = VCS_validation_functions.checkNumber(self,'reference',value)
         self._reference=value
    reference=property(_getreference,_setreference)
    
    def _getscale(self):
         return self._scale
    def _setscale(self,value):
         value = VCS_validation_functions.checkNumber(self,'scale',value)
         self._scale=value
    scale=property(_getscale,_setscale)
    
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
    
    def _gettype(self):
         return self._type
    def _settype(self,value):
         value = VCS_validation_functions.checkVectorType(self,'type',value)
         self._type=value
    type=property(_gettype,_settype)
    
    def _getalignment(self):
         return self._alignment
    def _setalignment(self,value):
         value = VCS_validation_functions.checkVectorAlignment(self,'alignment',value)
         self._alignment=value
    alignment=property(_getalignment,_setalignment)
    

    def __init__(self, Gv_name, Gv_name_src='default'):
	#                                                         #
        ###########################################################
	# Initialize the vector class and its members             #
        #							  #
	# The getGvmember function retrieves the values of the    #
        # vector members in the C structure and passes back the   #
	# appropriate Python Object.                              #
        ###########################################################
	#                                                         #
        if Gv_name in vcs.elements["vector"]:
          raise ValueError,"The vector method '%s' already exists" % Gv_name
        self.g_name='Gv'
        self._name = Gv_name
        if Gv_name == 'default':
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
          self._line=None
          self._linecolor=None
          self._linewidth=None
          self._scale=1.
          self._alignment="center"
          self._type="arrows"
          self._reference=1.e20
          self._datawc_timeunits="days since 2000"
          self._datawc_calendar=135441
        else:
          if isinstance(Gv_name_src,Gv):
            Gv_name_src=Gv_name_src.name
          if not Gv_name_src in vcs.elements['vector']:
            raise ValueError, "The vector method '%s' does not exists" % Gv_name_src
          src = vcs.elements["vector"][Gv_name_src]
          for att in ['projection' ,'xticlabels1' ,'xticlabels2' ,'xmtics1' ,'xmtics2' ,'yticlabels1' ,'yticlabels2' ,'ymtics1' ,'ymtics2' ,'datawc_y1' ,'datawc_y2' ,'datawc_x1' ,'datawc_x2' ,'xaxisconvert' ,'yaxisconvert' ,'line' ,'linecolor' ,'linewidth' ,'datawc_timeunits' ,'datawc_calendar' ,'scale','alignment','type','reference' ]:
           setattr(self,att,getattr(src,att)) 
        #Ok now we need to stick in the elements
        vcs.elements["vector"][Gv_name]=self
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
        print "","----------Vector (Gv) member (attribute) listings ----------"
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
 Function:     script                           # Calls _vcs.scriptGv

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

    a=vcs.init()
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
           print _vcs.scriptGv(self.name,script_filename,mode)
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
              fp.write("import vcs\n")
              fp.write("v=vcs.init()\n\n")

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
