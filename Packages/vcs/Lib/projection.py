"""
# Projection (Proj) module
"""
###############################################################################
#									      #
# Module:	projection (Proj) module				      #
#							      		      #
# Copyright:    2000, Regents of the University of California		      #
#               This software may not be distributed to others without	      #
#               permission of the author.				      #
#									      #
# Author:       PCMDI Software Team                                           #
#               Lawrence Livermore NationalLaboratory:                        #
#               support@pcmdi.llnl.gov                                        #
#								      	      #
# Description:	Python command wrapper for VCS's projection secondary method. #
#									      #
# Version:      4.0							      #
#									      #
###############################################################################
import VCS_validation_functions
import vcs

def process_src(nm,code):
  try:
    gm = Proj(nm)
  except Exception,err:
    gm = vcs.elements["projection"][nm]
  i=code.find("(")
  j=code.find(")")
  params=[]
  for v in code[i+1:j].split(","):
    params.append(float(v))
  i=code.find("=")
  gm.type=int(code[i+1:].split()[0])
  gm.parameters=params

#############################################################################
#                                                                           #
# Projection (Proj) secondary method Class.                                      #
#                                                                           #
#############################################################################
#class Proj(graphics_secondary method_core):
class Proj(object):
    """
 Class:	Proj                       	# Projection 
                                               
 Description of Proj Class:                          
    The projection secondary method (Proj) is used when plotting 2D data, and define
    how to project from lon/lat coord to another mapping system (lambert, mercator, mollweide, etc...)
                                                          
    This class is used to define a projection table entry used in VCS, or it
    can be used to change some or all of the attributes in an existing
    projection table entry.                                 

 Descprition of parameters (from USGS Documentation)
 
          Projection Transformation Package Projection Parameters

  -----------------------------------------------------------------------------
                          |       		Array Element		      |
   Code & Projection Id   |----------------------------------------------------
                          |   1  |   2  |  3   |  4   |   5   |    6    |7 | 8|
  -----------------------------------------------------------------------------
   0 Geographic           |      |      |      |      |       |         |  |  |
   1 U T M                |Lon/Z |Lat/Z |      |      |       |         |  |  |
   2 State Plane          |      |      |      |      |       |         |  |  |
   3 Albers Equal Area    |SMajor|SMinor|STDPR1|STDPR2|CentMer|OriginLat|FE|FN|
   4 Lambert Conformal C  |SMajor|SMinor|STDPR1|STDPR2|CentMer|OriginLat|FE|FN|
   5 Mercator             |SMajor|SMinor|      |      |CentMer|TrueScale|FE|FN|
   6 Polar Stereographic  |SMajor|SMinor|      |      |LongPol|TrueScale|FE|FN|
   7 Polyconic            |SMajor|SMinor|      |      |CentMer|OriginLat|FE|FN|
   8 Equid. Conic A       |SMajor|SMinor|STDPAR|      |CentMer|OriginLat|FE|FN|
     Equid. Conic B       |SMajor|SMinor|STDPR1|STDPR2|CentMer|OriginLat|FE|FN|
   9 Transverse Mercator  |SMajor|SMinor|Factor|      |CentMer|OriginLat|FE|FN|
  10 Stereographic        |Sphere|      |      |      |CentLon|CenterLat|FE|FN|
  11 Lambert Azimuthal    |Sphere|      |      |      |CentLon|CenterLat|FE|FN|
  12 Azimuthal            |Sphere|      |      |      |CentLon|CenterLat|FE|FN|
  13 Gnomonic             |Sphere|      |      |      |CentLon|CenterLat|FE|FN|
  14 Orthographic         |Sphere|      |      |      |CentLon|CenterLat|FE|FN|
  15 Gen. Vert. Near Per  |Sphere|      |Height|      |CentLon|CenterLat|FE|FN|
  16 Sinusoidal           |Sphere|      |      |      |CentMer|         |FE|FN|
  17 Equirectangular      |Sphere|      |      |      |CentMer|TrueScale|FE|FN|
  18 Miller Cylindrical   |Sphere|      |      |      |CentMer|         |FE|FN|
  19 Van der Grinten      |Sphere|      |      |      |CentMer|OriginLat|FE|FN|
  20 Hotin Oblique Merc A |SMajor|SMinor|Factor|      |       |OriginLat|FE|FN|
     Hotin Oblique Merc B |SMajor|SMinor|Factor|AziAng|AzmthPt|OriginLat|FE|FN|
  21 Robinson             |Sphere|      |      |      |CentMer|         |FE|FN|
  22 Space Oblique Merc A |SMajor|SMinor|      |IncAng|AscLong|         |FE|FN|
     Space Oblique Merc B |SMajor|SMinor|Satnum|Path  |       |         |FE|FN|
  23 Alaska Conformal     |SMajor|SMinor|      |      |       |         |FE|FN|
  24 Interrupted Goode    |Sphere|      |      |      |       |         |  |  |
  25 Mollweide            |Sphere|      |      |      |CentMer|         |FE|FN|
  26 Interrupt Mollweide  |Sphere|      |      |      |       |         |  |  |
  27 Hammer               |Sphere|      |      |      |CentMer|         |FE|FN|
  28 Wagner IV            |Sphere|      |      |      |CentMer|         |FE|FN|
  29 Wagner VII           |Sphere|      |      |      |CentMer|         |FE|FN|
  30 Oblated Equal Area   |Sphere|      |Shapem|Shapen|CentLon|CenterLat|FE|FN|
  -----------------------------------------------------------------------------

    Projection Transformation Package Projection Parameters elements 9-15
    continued

	 ----------------------------------------------------
       	                   	 |      Array Element	    |
	  Code & Projection Id	 |---------------------------
	  		         |  9  | 10 |  11 | 12 | 13 |  
	 ----------------------------------------------------
	  0 Geographic		 |     |    |     |    |    |
	  1 U T M		 |     |    |     |    |    |
	  2 State Plane	 	 |     |    |     |    |    |
	  3 Albers Equal Area	 |     |    |     |    |    |
	  4 Lambert Conformal C  |     |    |     |    |    |
	  5 Mercator		 |     |    |     |    |    |
	  6 Polar Stereographic  |     |    |     |    |    |
	  7 Polyconic		 |     |    |     |    |    |
	  8 Equid. Conic A       |zero |    |     |    |    |   
	    Equid. Conic B	 |one  |    |     |    |    |
	  9 Transverse Mercator  |     |    |     |    |    |
	 10 Stereographic	 |     |    |     |    |    |
	 11 Lambert Azimuthal    |     |    |     |    |    |    
	 12 Azimuthal            |     |    |     |    |    |    
	 13 Gnomonic 		 |     |    |     |    |    |
	 14 Orthographic	 |     |    |     |    |    |
	 15 Gen. Vert. Near Per  |     |    |     |    |    |
	 16 Sinusoidal           |     |    |     |    |    |
	 17 Equirectangular	 |     |    |     |    |    |
	 18 Miller Cylindrical   |     |    |     |    |    |
	 19 Van der Grinten	 |     |    |     |    |    |
	 20 Hotin Oblique Merc A |Long1|Lat1|Long2|Lat2|zero|   
 	    Hotin Oblique Merc B |     |    |     |    |one |
	 21 Robinson    	 |     |    |     |    |    |
	 22 Space Oblique Merc A |PSRev|LRat|PFlag|    |zero|    
	    Space Oblique Merc B |     |    |     |    |one |
	 23 Alaska Conformal     |     |    |     |    |    |
	 24 Interrupted Goode    |     |    |     |    |    |
	 25 Mollweide		 |     |    |     |    |    |
	 26 Interrupt Mollweide  |     |    |     |    |    |
	 27 Hammer		 |     |    |     |    |    |
	 28 Wagner IV		 |     |    |     |    |    |
	 29 Wagner VII		 |     |    |     |    |    |
	 30 Oblated Equal Area   |Angle|    |     |    |    |
	 ----------------------------------------------------

  where 
      Lon/Z 	Longitude of any point in the UTM zone or zero.  If zero,
		a zone code must be specified.
      Lat/Z	Latitude of any point in the UTM zone or zero.  If zero, a
		zone code must be specified.
      SMajor    Semi-major axis of ellipsoid.  If zero, Clarke 1866 in meters
		is assumed.
      SMinor    Eccentricity squared of the ellipsoid if less than zero,
		if zero, a spherical form is assumed, or if greater than
		zero, the semi-minor axis of ellipsoid.
      Sphere    Radius of reference sphere.  If zero, 6370997 meters is used.
      STDPAR    Latitude of the standard parallel
      STDPR1    Latitude of the first standard parallel
      STDPR2    Latitude of the second standard parallel
      CentMer   Longitude of the central meridian
      OriginLat Latitude of the projection origin
      FE    	False easting in the same units as the semi-major axis
      FN    	False northing in the same units as the semi-major axis
      TrueScale Latitude of true scale
      LongPol   Longitude down below pole of map
      Factor    Scale factor at central meridian (Transverse Mercator) or
		center of projection (Hotine Oblique Mercator)
      CentLon   Longitude of center of projection
      CenterLat Latitude of center of projection
      Height    Height of perspective point
      Long1	Longitude of first point on center line (Hotine Oblique
		Mercator, format A)
      Long2	Longitude of second point on center line (Hotine Oblique
		Mercator, format A)
      Lat1	Latitude of first point on center line (Hotine Oblique
		Mercator, format A)
      Lat2	Latitude of second point on center line (Hotine Oblique
		Mercator, format A)
      AziAng    Azimuth angle east of north of center line (Hotine Oblique
		Mercator, format B)
      AzmthPt   Longitude of point on central meridian where azimuth occurs
		(Hotine Oblique Mercator, format B)
      IncAng    Inclination of orbit at ascending node, counter-clockwise
		from equator (SOM, format A)
      AscLong   Longitude of ascending orbit at equator (SOM, format A)
      PSRev	Period of satellite revolution in minutes (SOM, format A)
      LRat	Landsat ratio to compensate for confusion at northern end
		of orbit (SOM, format A -- use 0.5201613)
      PFlag	End of path flag for Landsat:  0 = start of path,
		1 = end of path (SOM, format A)
      Satnum    Landsat Satellite Number (SOM, format B)
      Path	Landsat Path Number (Use WRS-1 for Landsat 1, 2 and 3 and
		WRS-2 for Landsat 4, 5 and 6.)  (SOM, format B)
      Shapem	Oblated Equal Area oval shape parameter m
      Shapen	Oblated Equal Area oval shape parameter n
      Angle	Oblated Equal Area oval rotation angle

                                   NOTES

   Array elements 14 and 15 are set to zero
   All array elements with blank fields are set to zero
   All angles (latitudes, longitudes, azimuths, etc.) are entered in packed
 	degrees/ minutes/ seconds (DDDMMMSSS.SS) format

   The following notes apply to the Space Oblique Mercator A projection.

      A portion of Landsat rows 1 and 2 may also be seen as parts of rows
   246 or 247.  To place these locations at rows 246 or 247, set the end of
   path flag (parameter 11) to 1--end of path.  This flag defaults to zero.

      When Landsat-1,2,3 orbits are being used, use the following values
   for the specified parameters:

      Parameter 4   099005031.2
      Parameter 5   128.87 degrees - (360/251 * path number) in packed
		    DMS format
      Parameter 9   103.2669323
      Parameter 10  0.5201613

      When Landsat-4,5 orbits are being used, use the following values
   for the specified parameters:

      Parameter 4   098012000.0
      Parameter 5   129.30 degrees - (360/233 * path number) in packed
		    DMS format
      Parameter 9   98.884119
      Parameter 10  0.5201613

 Note: In vcs angles can be entered either in DDDMMMSSS or regular angle format.

 Other Useful Functions:                    
               a=vcs.init()             # Constructor                       
               a.show('projection')        # Show predefined projection secondary methods
 Example of Use:                                               
    a=vcs.init()                                              
    To Create a new instance of projection use:                   
     p=a.createprojection('new','quick') # Copies content of 'quick' to 'new' 
     p=a.createprojection('new') 	# Copies content of 'default' to 'new'  

    To Modify an existing projection use:                    
     p=a.getprojection('lambert')                          

    p.list()  			# Will list all the projection attribute values
    p.type='lambert'
    p.parameters=[1.e20,1.e20,1.e20,1.e20,1.e20,1.e20,1.e20,1.e20,1.e20,1.e20,1.e20,1.e20,1.e20,1.e20,1.e20,]
    iso=x.createisoline('new')
    iso.projection=p
    #or
    iso.projection='lambert'
    
"""
    #############################################################################
    #                                                                           #
    # Initialize the projection attributes.                                        #
    #                                                                           #
    #############################################################################
    def __init__(self, Proj_name=None, Proj_name_src='default'):
	#                                                         #
    ###########################################################
	# Initialize the projection class and its members            #
    #							  #
	# The getProjmember function retrieves the values of the   #
    # projection members in the C structure and passes back the  #
	# appropriate Python Object.                              #
    ###########################################################
        if isinstance(Proj_name_src,Proj):
          Proj_name_src=Proj_name_src.name
        if Proj_name_src!="default" and not Proj_name_src in vcs.elements["projection"].keys():
          raise ValueError, "Projection '%s' does not exists" % Proj_name_src
        if (Proj_name == None):
          raise ValueError, 'Must provide a projection name.'
        else:
          if Proj_name in vcs.elements["projection"].keys():
            raise ValueError, "The projection '%s' already exists, use getprojection instead" % Proj_name
        self._name = Proj_name
        self.s_name = 'Proj'
	#                                                         #
        ###########################################################
        # Inherits core secondary method attributes.		  #
        ###########################################################
	#                                                         #
	# graphics_secondary method_core.__init__(self, parent)
        # Doesn't make sense to inherit. This would mean writing more code
        self._type = 0
        self._parameters = [1e+20, 1e+20, 1e+20, 1e+20, 1e+20, 1e+20, 1e+20, 1e+20, 1e+20, 1e+20, 1e+20, 1e+20, 1e+20, 1e+20, 1e+20]
        if Proj_name != "default":
          src = vcs.elements["projection"][Proj_name_src]
          self.type = src.type
          self.parameters = src.parameters
        vcs.elements["projection"][Proj_name]=self

    #############################################################################
    #                                                                           #
    # List out projection secondary method members (attributes).                    #
    #                                                                           #
    #############################################################################
    def list(self):
        if (self.name == '__removed_from_VCS__'):
           raise ValueError, 'This instance has been removed from VCS.'
        print ' ---------- Projection (Proj) member (attribute) listings ---------'
        print 'secondary method =',self.s_name
        print 'name =',self.name
        print 'type =',self.type
        #print 'parameters =',self.parameters
        p=[]
        if self._type in [3,4]:
             p.append('smajor')
             p.append('sminor')
             p.append('standardparallel1')
             p.append('standardparallel2')
             p.append('centralmeridian')
             p.append('originlatitude')
             p.append('falseeasting')
             p.append('falsenorthing')
        elif self._type==5:
             p.append('smajor')
             p.append('sminor')
             p.append('centralmeridian')
             p.append('truescale')
             p.append('falseeasting')
             p.append('falsenorthing')
        elif self._type==6:
             p.append('smajor')
             p.append('sminor')
             p.append('centerlongitude')
             p.append('truescale')
             p.append('falseeasting')
             p.append('falsenorthing')
        elif self._type==7:
             p.append('smajor')
             p.append('sminor')
             p.append('centralmeridian')
             p.append('originlatitude')
             p.append('falseeasting')
             p.append('falsenorthing')
        elif self._type==8:
             p.append('subtype')
             p.append('smajor')
             p.append('sminor')
             p.append('centralmeridian')
             p.append('originlatitude')
             p.append('falseeasting')
             p.append('falsenorthing')       
             if self.subtype==0:
                  p.append('standardparallel')
             else:
                  p.append('standardparallel1')
                  p.append('standardparallel2')
        elif self._type==9:
             p.append('smajor')
             p.append('sminor')
             p.append('factor')
             p.append('centralmeridian')
             p.append('originlatitude')
             p.append('falseeasting')
             p.append('falsenorthing')    
        elif self._type in [10,11,12,13,14]:
             p.append('sphere')
             p.append('centerlongitude')
             p.append('centerlatitude')
             p.append('falseeasting')
             p.append('falsenorthing')      
        elif self._type==15:
             p.append('sphere')
             p.append('height')
             p.append('centerlongitude')
             p.append('centerlatitude')
             p.append('falseeasting')
             p.append('falsenorthing')  
        elif self._type in [16,18,21,25,27,28,29]:
             p.append('sphere')
             p.append('centralmeridian')
             p.append('falseeasting')
             p.append('falsenorthing')     
        elif self._type==17:
             p.append('sphere')
             p.append('centralmeridian')
             p.append('truescale')
             p.append('falseeasting')
             p.append('falsenorthing')      
        elif self._type==19:
             p.append('sphere')
             p.append('centralmeridian')
             p.append('originlatitude')
             p.append('falseeasting')
             p.append('falsenorthing')        
        elif self._type==20:
             p.append('subtype')
             p.append('smajor')
             p.append('sminor')
             p.append('factor')
             p.append('originlatitude')
             p.append('falseeasting')
             p.append('falsenorthing')
             if self.subtype==0:
                  p.append('longitude1')
                  p.append('latitude1')
                  p.append('longitude2')
                  p.append('latitude2')
             else:
                  p.append('azimuthalangle')
                  p.append('azimuthallongitude')
        elif self._type==22:
             p.append('subtype')
             p.append('smajor')
             p.append('sminor')
             p.append('falseeasting')
             p.append('falsenorthing')
             if self.subtype==0:
                  p.append('orbitinclination')
                  p.append('orbitlongitude')
                  p.append('satelliterevolutionperiod')
                  p.append('landsatcompensationratio')
                  p.append('pathflag')
             else:
                  p.append('satellite')
                  p.append('path')
        elif self._type==23:
             p.append('smajor')
             p.append('sminor')
             p.append('falseeasting')
             p.append('falsenorthing')      
        elif self._type in [24,26]:
             p.append('sphere')
        elif self._type==30:
             p.append('sphere')
             p.append('shapem')
             p.append('shapen')
             p.append('centerlongitude')
             p.append('centerlatitude')
             p.append('falseeasting')
             p.append('falsenorthing')   
        for att in p:
             print att,'=',getattr(self,att)




        

    #############################################################################
    #                                                                           #
    # Script out primary projection secondary method in VCS to a file.              #
    #                                                                           #
    #############################################################################
    def script(self, script_filename=None, mode=None):
         """
         Function:     script				# Calls _vcs.scriptProj
         
         Description of Function:                                                      
         Saves out a projection secondary method in Python form to a
         designated file.
         
         Example of Use:                                                               
         script(scriptfile_name, mode)                                           
         where: scriptfile_name is the output name of the script file.
         mode is either "w" for replace or "a" for append.
         
         
         a=vcs.init()
         p=a.createprojection('temp')
         p.script('filename.py')         # Append to a file "filename.py"
         p.script('filename','w')
         """
         if (script_filename == None):
              raise ValueError, 'Error - Must provide an output script file name.'

         if (mode == None):
              mode = 'a'
         elif (mode not in ('w', 'a')):
              raise ValueError, 'Error - Mode can only be "w" for replace or "a" for append.'

         # By default, save file in python script mode
         scr_type = script_filename[len(script_filename)-4:len(script_filename)]
         if (script_filename == None):
              raise ValueError, 'Error - Must provide an output script file name.'
         
         if (mode == None):
              mode = 'a'
         elif (mode not in ('w', 'a')):
              raise ValueError, 'Error - Mode can only be "w" for replace or "a" for append.'
    
         if (scr_type == '.scr'):
              print _vcs.scriptProj(self.name,script_filename,mode)
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

              unique_name = '__Proj__' + self.name
              fp.write("#----------Projection (Proj) member (attribute) listings ----------\n")
              fp.write("proj_list=v.listelements('projection')\n")
              fp.write("if ('%s' in proj_list):\n" % self.name)
              fp.write("   %s = v.getprojection('%s')\n" % (unique_name, self.name))
              fp.write("else:\n")
              fp.write("   %s = v.createprojection('%s')\n" % (unique_name, self.name))
              # Common core secondary method attributes
              fp.write("%s.type = '%s'\n" % (unique_name, self.type))
              fp.write("%s.parameters = '%s'\n" % (unique_name, self.parameters))
              pass
    __slots__=[
         's_name',
         'smajor',
         'sminor',
         'centralmeridian',
         'truescale',
         'falseeasting',
         'falsenorthing',
         'factor',
         'originlatitude',
         'azimuthalangle',
         'azimuthlongitude',
         'longitude1',
         'longitude2',
         'latitude1',
         'latitude2',
         'subtype',
         'orbitinclination',
         'orbitlongitude',
         'satelliterevolutionperiod',
         'landsatcompensationratio',
         'pathflag',
         'path',
         'satellite',
         'sphere',
         'centerlongitude',
         'centerlatitude',
         'standardparallel1',
         'standardparallel2',
         'standardparallel',
         'height',
         'angle',
         'shapem',
         'shapen',
         'parent',
         'name',
         'type',
         'parameters',
         '_smajor',
         '_sminor',
         '_centralmeridian',
         '_truescale',
         '_falseeasting',
         '_falsenorthing',
         '_factor',
         '_originlatitude',
         '_azimuthalangle',
         '_azimuthlongitude',
         '_longitude1',
         '_longitude2',
         '_latitude1',
         '_latitude2',
         '_subtype',
         '_orbitinclination',
         '_orbitlongitude',
         '_satelliterevolutionperiod',
         '_landsatcompensationratio',
         '_pathflag',
         '_path',
         '_satellite',
         '_sphere',
         '_centerlongitude',
         '_centerlatitude',
         '_standardparallel1',
         '_standardparallel2',
         '_standardparallel',
         '_height',
         '_angle',
         '_shapem',
         '_shapen',
         '_name',
         '_type',
         '_parameters',
         ]


    def checkPP(self,name,value):
         value=VCS_validation_functions.setProjParameter(self,name,value)
         setattr(self,'_'+name,value)

    def _getsmajor(self):
         return self._smajor
    def _setsmajor(self,value):
         self.checkPP('smajor',value)
    smajor=property(_getsmajor,_setsmajor)
    
    def _getsminor(self):
         return self._sminor
    def _setsminor(self,value):
         self.checkPP('sminor',value)
    sminor=property(_getsminor,_setsminor)
    
    def _getcentralmeridian(self):
         return self._centralmeridian
    def _setcentralmeridian(self,value):
         self.checkPP('centralmeridian',value)
    centralmeridian=property(_getcentralmeridian,_setcentralmeridian)
    
    def _gettruescale(self):
         return self._truescale
    def _settruescale(self,value):
         self.checkPP('truescale',value)
    truescale=property(_gettruescale,_settruescale)
    
    def _getfalseeasting(self):
         return self._falseeasting
    def _setfalseeasting(self,value):
         self.checkPP('falseeasting',value)
    falseeasting=property(_getfalseeasting,_setfalseeasting)
    
    def _getfalsenorthing(self):
         return self._falsenorthing
    def _setfalsenorthing(self,value):
         self.checkPP('falsenorthing',value)
    falsenorthing=property(_getfalsenorthing,_setfalsenorthing)
    
    def _getfactor(self):
         return self._factor
    def _setfactor(self,value):
         self.checkPP('factor',value)
    factor=property(_getfactor,_setfactor)
    
    def _getoriginlatitude(self):
         return self._originlatitude
    def _setoriginlatitude(self,value):
         self.checkPP('originlatitude',value)
    originlatitude=property(_getoriginlatitude,_setoriginlatitude)
    
    def _getazimuthalangle(self):
         return self._azimuthalangle
    def _setazimuthalangle(self,value):
         self.checkPP('azimuthalangle',value)
    azimuthalangle=property(_getazimuthalangle,_setazimuthalangle)
    
    def _getazimuthallongitude(self):
         return self._azimuthallongitude
    def _setazimuthallongitude(self,value):
         self.checkPP('azimuthallongitude',value)
    azimuthallongitude=property(_getazimuthallongitude,_setazimuthallongitude)
    
    def _getlongitude1(self):
         return self._longitude1
    def _setlongitude1(self,value):
         self.checkPP('longitude1',value)
    longitude1=property(_getlongitude1,_setlongitude1)
    
    def _getlongitude2(self):
         return self._longitude2
    def _setlongitude2(self,value):
         self.checkPP('longitude2',value)
    longitude2=property(_getlongitude2,_setlongitude2)
    
    def _getlatitude1(self):
         return self._latitude1
    def _setlatitude1(self,value):
         self.checkPP('latitude1',value)
    latitude1=property(_getlatitude1,_setlatitude1)
    
    def _getlatitude2(self):
         return self._latitude2
    def _setlatitude2(self,value):
         self.checkPP('latitude2',value)
    latitude2=property(_getlatitude2,_setlatitude2)
    
    def _getsubtype(self):
         return self._subtype
    def _setsubtype(self,value):
         self.checkPP('subtype',value)
    subtype=property(_getsubtype,_setsubtype)
    
    def _getorbitinclination(self):
         return self._orbitinclination
    def _setorbitinclination(self,value):
         self.checkPP('orbitinclination',value)
    orbitinclination=property(_getorbitinclination,_setorbitinclination)
    
    def _getorbitlongitude(self):
         return self._orbitlongitude
    def _setorbitlongitude(self,value):
         self.checkPP('orbitlongitude',value)
    orbitlongitude=property(_getorbitlongitude,_setorbitlongitude)
    
    def _getsatelliterevolutionperiod(self):
         return self._satelliterevolutionperiod
    def _setsatelliterevolutionperiod(self,value):
         self.checkPP('satelliterevolutionperiod',value)
    satelliterevolutionperiod=property(_getsatelliterevolutionperiod,_setsatelliterevolutionperiod)
    
    def _getlandsatcompensationratio(self):
         return self._landsatcompensationratio
    def _setlandsatcompensationratio(self,value):
         self.checkPP('landsatcompensationratio',value)
    landsatcompensationratio=property(_getlandsatcompensationratio,_setlandsatcompensationratio)
    
    def _getpathflag(self):
         return self._pathflag
    def _setpathflag(self,value):
         self.checkPP('pathflag',value)
    pathflag=property(_getpathflag,_setpathflag)
    
    def _getpath(self):
         return self._path
    def _setpath(self,value):
         self.checkPP('path',value)
    path=property(_getpath,_setpath)
    
    def _getsatellite(self):
         return self._satellite
    def _setsatellite(self,value):
         self.checkPP('satellite',value)
    satellite=property(_getsatellite,_setsatellite)
    
    def _getsphere(self):
         return self._sphere
    def _setsphere(self,value):
         self.checkPP('sphere',value)
    sphere=property(_getsphere,_setsphere)
    
    def _getcenterlongitude(self):
         return self._centerlongitude
    def _setcenterlongitude(self,value):
         self.checkPP('centerlongitude',value)
    centerlongitude=property(_getcenterlongitude,_setcenterlongitude)
    
    def _getcenterlatitude(self):
         return self._centerlatitude
    def _setcenterlatitude(self,value):
         self.checkPP('centerlatitude',value)
    centerlatitude=property(_getcenterlatitude,_setcenterlatitude)
    
    def _getstandardparallel1(self):
         return self._standardparallel1
    def _setstandardparallel1(self,value):
         self.checkPP('standardparallel1',value)
    standardparallel1=property(_getstandardparallel1,_setstandardparallel1)
    
    def _getstandardparallel2(self):
         return self._standardparallel2
    def _setstandardparallel2(self,value):
         self.checkPP('standardparallel2',value)
    standardparallel2=property(_getstandardparallel2,_setstandardparallel2)
    
    def _getstandardparallel(self):
         return self._standardparallel
    def _setstandardparallel(self,value):
         self.checkPP('standardparallel',value)
    standardparallel=property(_getstandardparallel,_setstandardparallel)
    
    def _getheight(self):
         return self._height
    def _setheight(self,value):
         self.checkPP('height',value)
    height=property(_getheight,_setheight)
    
    def _getangle(self):
         return self._angle
    def _setangle(self,value):
         self.checkPP('angle',value)
    angle=property(_getangle,_setangle)
    
    def _getshapem(self):
         return self._shapem
    def _setshapem(self,value):
         self.checkPP('shapem',value)
    shapem=property(_getshapem,_setshapem)
    
    def _getshapen(self):
         return self._shapen
    def _setshapen(self,value):
         self.checkPP('shapen',value)
    shapen=property(_getshapen,_setshapen)
    
    def _getname(self):
         return self._name
    def _setname(self,value):
         value=VCS_validation_functions.checkname(self,'name',value)
         self._name=value
    name=property(_getname,_setname)
    
    def _settype(self,value):
         value=VCS_validation_functions.checkProjType(self,'type',value)
         self._type=value
         
    def _gettype(self):
         return VCS_validation_functions.getProjType(self)
    type=property(_gettype,_settype)

    def _getparameters(self):
         return self._parameters
    def _setparameters(self,value):
         value=VCS_validation_functions.checkProjParameters(self,'parameters',value)
         self._parameters=value
    parameters=property(_getparameters,_setparameters)
    
#################################################################################
#        END OF FILE								#
#################################################################################
