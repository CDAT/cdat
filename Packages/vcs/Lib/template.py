# Adapted for numpy/ma/cdms2 by convertcdms.py
"""
# Template (P) module
"""
###############################################################################
#                                                                             #
# Module:       template (P) module                                           #
#                                                                             #
# Copyright:    2000, Regents of the University of California                 #
#               This software may not be distributed to others without        #
#               permission of the author.                                     #
#                                                                             #
# Author:       PCMDI Software Team                                           #
#               Lawrence Livermore NationalLaboratory:                        #
#               support@pcmdi.llnl.gov                                        #
#                                                                             #
# Description:  Python command wrapper for VCS's template primary object.     #
#                                                                             #
# Version:      4.0                                                           #
#                                                                             #
###############################################################################
#
#
#
import copy, vcs, numpy
from Ptext import *
from Pformat import *
from Pxtickmarks import *
from Pytickmarks import *
from Pxlabels import *
from Pylabels import *
from Pboxeslines import *
from Plegend import *
from Pdata import *
from types import *
import inspect

## Following for class properties
def _getgen(self,name):
    return getattr(self,"_%s" % name)
def _setgen(self,name,cls,value):
    if self.name == "default":
      raise ValueError, "You cannot modify the default template"
    if not isinstance(value,cls):
      raise ValueError, "template attribute '%s' must be of type %s" % (name,cls)
    setattr(self,"_%s" % name, value)


## read .scr file
def process_src(nm,code):
  """Takes VCS script code (string) as input and generates boxfill gm from it"""
  try:
    t = P(nm)
  except Exception,err:
    t = vcs.elements["template"][nm]
  for sub in ["File","Function","LogicalMask","Transform","name","title","units","crdate","crtime","comment#1",
      "comment#2","comment#3","comment#4","xname","yname","zname","tname","xvalue","yvalue","zvalue","tvalue","xunits",
      "yunits","zunits","tunits","mean","min","max","xtic#1","xtic#2","xmintic#a","xmintic#b",
      "ytic#1","ytic#2","ymintic#a","ymintic#b","xlabel#1","xlabel#2","ylabel#1","ylabel#2",
      "box#1","box#2","box#3","box#4","line#1","line#2","line#3","line#4","legend","data"]:
    #isolate that segment
    i = code.find("%s(" % sub)
    if i==-1:
      #not set in this case
      continue
    sc=code[i+len(sub)+1:]
    j = sc.find(")")
    sc=sc[:j]
    #name on template object
    tnm=sub.lower().replace("#","")
    if tnm == "name":
      tnm="dataname"
    elif tnm [-4:]=="tica":
      tnm=tnm[:-4]+"tic1"
    elif tnm [-4:]=="ticb":
      tnm=tnm[:-4]+"tic2"
    elif tnm == "transform":
      tnm = "transformation"
    for S in sc.split(","): # all attributes are comman separated
      nm,val = S.split("=") # nm=val
      if nm=="p":
        nm="priority"
      elif nm=="Tl":
        nm="line"
      elif nm=="Tt":
        nm="texttable"
      elif nm=="To":
        nm = "textorientation"
      elif nm == "Th":
        nm = "format"
      tatt = getattr(t,tnm)
      try:
        setattr(tatt,nm,eval(val)) # int float should be ok here
      except:
        try:
          setattr(tatt,nm,val) # strings here
        except:
          #print "COULD NOT SET %s.%s.%s to %s" % (t.name,tnm,nm,val)
          pass
  i = code.find("Orientation(")
  t.orientation = int(code[i+12])


#############################################################################
#                                                                           #
# Template (P) graphics method Class.                                       #
#                                                                           #
#############################################################################
class P(object):
    """
 Class: P                             # Template

 Description of P Class:
    The template primary method (P) determines the location of each picture
    segment, the space to be allocated to it, and related properties relevant
    to its display.

 Other Useful Functions:
               a.show('template')   	# Show predefined templates
               a.show('texttable')   	# Show predefined text table methods
               a.show('textorientation')   # Show predefined text orientation methods
               a.show('line')   	# Show predefined line methods
               a.listelements('template') # Show templates as a Python list
               a.update()               # Updates the VCS Canvas at user's request
               a.mode=1, or 0           # If 1, then automatic update, else if
                                          0, then use the update function to
                                          update the VCS Canvas.

 Example of Use:
    a=vcs.init()
    To Create a new instance of boxfill use:
     box=a.createboxfill('new','quick') # Copies content of 'quick' to 'new'
     box=a.createboxfill('new')         # Copies content of 'default' to 'new'

    To Modify an existing boxfill use:
     box=a.getboxfill('AMIP_psl')

    To Create a new instance of template use:
     tpl=a.createtemplate('new','AMIP')  # Copies content of 'AMIP' to 'new'
     tpl=a.createtemplate('new')         # Copies content of 'default' to 'new'

    To Modify an existing template use:
     tpl=a.gettemplate('AMIP')
"""
    __slots__ = ["name","_name","_p_name","p_name","_orientation","_orientation","_file","file",
        "_function","function","_logicalmask","logicalmask","_transformation","transformation",
        "source","_source","dataname","_dataname","title","_title","units","_units","_crdate","crdate",
        "crtime","_crtime","_comment1","comment1","comment2","_comment2","_comment3","comment3",
        "_comment4","comment4",
        "xname","yname","zname","tname","xunits","yunits","zunits","tunits",
        "xvalue","zvalue","yvalue","tvalue","mean","min","max","xtic1","xtic2","xmintic1","xmintic2",
        "ytic1","ytic2","ymintic1","ymintic2","xlabel1","xlabel2","box1","box2","box3","box4",
        "ylabel1","ylabel2","line1","line2","line3","line4","legend","data",
        "_xname","_yname","_zname","_tname","_xunits","_yunits","_zunits","_tunits",
        "_xvalue","_zvalue","_yvalue","_tvalue","_mean","_min","_max","_xtic1","_xtic2","_xmintic1","_xmintic2",
        "_ytic1","_ytic2","_ymintic1","_ymintic2","_xlabel1","_xlabel2","_box1","_box2","_box3","_box4",
        "_ylabel1","_ylabel2","_line1","_line2","_line3","_line4","_legend","_data"]

    def _getName(self):
      return self._name
    name = property(_getName)
    def _getOrientation(self):
      return self._orientation
    def _setOrientation(self,value):
      if self.name == "default":
        raise ValueError, "You cannot change the default template"
      value = VCS_validation_functions.checkInt(self,"orientation",value)
      if not value in [0,1]:
        raise ValueError,"The orientation attribute must be an integer (i.e., 0 = landscape, 1 = portrait)."
      self._orientation = value
    orientation = property(_getOrientation,_setOrientation,"The orientation attribute must be an integer (i.e., 0 = landscape, 1 = portrait).")

    ###########################################################################
    #                                                                         #
    # Initialize the template attributes.                                     #
    #                                                                         #
    ###########################################################################
    def __init__(self, Pic_name=None, Pic_name_src='default'):
        #                                                         #
        ###########################################################
        # Initialize the template class and its members           #
        #                                                         #
        # The getPmember function retrieves the values of the     #
        # template members in the C structure and passes back the #
        # appropriate Python Object.                              #
        ###########################################################
        #                                                         #
        if (Pic_name == None):
              raise ValueError, 'Must provide a template name.'
        if Pic_name_src!="default" and Pic_name_src not in vcs.elements["template"]:
          raise "Invalid source template: %s" % Pic_name_src
        if isinstance(Pic_name_src,P):
          Pic_name_src = Pic_name_src.name
        if Pic_name in vcs.elements["template"].keys():
          raise ValueError, "Template %s already exists" % Pic_name

        self._name=Pic_name
        self.p_name='P'
        ## properties
        self.__class__.file=property(lambda x: _getgen(x,"file"),
                                  lambda x,v: _setgen(x,"file",Pt,v))
        self.__class__.function=property(lambda x: _getgen(x,"function"),
                                  lambda x,v: _setgen(x,"function",Pt,v))
        self.__class__.logicalmask=property(lambda x: _getgen(x,"logicalmask"),
                                  lambda x,v: _setgen(x,"logicalmask",Pt,v))
        self.__class__.transformation=property(lambda x: _getgen(x,"transformation"),
                                  lambda x,v: _setgen(x,"transformation",Pt,v))
        self.__class__.source=property(lambda x: _getgen(x,"source"),
                                  lambda x,v: _setgen(x,"source",Pt,v))
        self.__class__.dataname=property(lambda x: _getgen(x,"dataname"),
                                  lambda x,v: _setgen(x,"dataname",Pt,v))
        self.__class__.title=property(lambda x: _getgen(x,"title"),
                                  lambda x,v: _setgen(x,"title",Pt,v))
        self.__class__.units=property(lambda x: _getgen(x,"units"),
                                  lambda x,v: _setgen(x,"units",Pt,v))
        self.__class__.crdate=property(lambda x: _getgen(x,"crdate"),
                                  lambda x,v: _setgen(x,"crdate",Pt,v))
        self.__class__.crtime=property(lambda x: _getgen(x,"crtime"),
                                  lambda x,v: _setgen(x,"crtime",Pt,v))
        self.__class__.comment1=property(lambda x: _getgen(x,"comment1"),
                                  lambda x,v: _setgen(x,"comment1",Pt,v))
        self.__class__.comment2=property(lambda x: _getgen(x,"comment2"),
                                  lambda x,v: _setgen(x,"comment2",Pt,v))
        self.__class__.comment3=property(lambda x: _getgen(x,"comment3"),
                                  lambda x,v: _setgen(x,"comment3",Pt,v))
        self.__class__.comment4=property(lambda x: _getgen(x,"comment4"),
                                  lambda x,v: _setgen(x,"comment4",Pt,v))
        self.__class__.xname=property(lambda x: _getgen(x,"xname"),
                                  lambda x,v: _setgen(x,"xname",Pt,v))
        self.__class__.yname=property(lambda x: _getgen(x,"yname"),
                                  lambda x,v: _setgen(x,"yname",Pt,v))
        self.__class__.zname=property(lambda x: _getgen(x,"zname"),
                                  lambda x,v: _setgen(x,"zname",Pt,v))
        self.__class__.tname=property(lambda x: _getgen(x,"tname"),
                                  lambda x,v: _setgen(x,"tname",Pt,v))
        self.__class__.xunits=property(lambda x: _getgen(x,"xunits"),
                                  lambda x,v: _setgen(x,"xunits",Pt,v))
        self.__class__.yunits=property(lambda x: _getgen(x,"yunits"),
                                  lambda x,v: _setgen(x,"yunits",Pt,v))
        self.__class__.zunits=property(lambda x: _getgen(x,"zunits"),
                                  lambda x,v: _setgen(x,"zunits",Pt,v))
        self.__class__.tunits=property(lambda x: _getgen(x,"tunits"),
                                  lambda x,v: _setgen(x,"tunits",Pt,v))
        self.__class__.xvalue=property(lambda x: _getgen(x,"xvalue"),
                                  lambda x,v: _setgen(x,"xvalue",Pf,v))
        self.__class__.yvalue=property(lambda x: _getgen(x,"yvalue"),
                                  lambda x,v: _setgen(x,"yvalue",Pf,v))
        self.__class__.zvalue=property(lambda x: _getgen(x,"zvalue"),
                                  lambda x,v: _setgen(x,"zvalue",Pf,v))
        self.__class__.tvalue=property(lambda x: _getgen(x,"tvalue"),
                                  lambda x,v: _setgen(x,"tvalue",Pf,v))
        self.__class__.mean=property(lambda x: _getgen(x,"mean"),
                                  lambda x,v: _setgen(x,"mean",Pf,v))
        self.__class__.min=property(lambda x: _getgen(x,"min"),
                                  lambda x,v: _setgen(x,"min",Pf,v))
        self.__class__.max=property(lambda x: _getgen(x,"max"),
                                  lambda x,v: _setgen(x,"max",Pf,v))
        self.__class__.xtic1=property(lambda x: _getgen(x,"xtic1"),
                                  lambda x,v: _setgen(x,"xtic1",Pxt,v))
        self.__class__.xtic2=property(lambda x: _getgen(x,"xtic2"),
                                  lambda x,v: _setgen(x,"xtic2",Pxt,v))
        self.__class__.xmintic1=property(lambda x: _getgen(x,"xmintic1"),
                                  lambda x,v: _setgen(x,"xmintic1",Pxt,v))
        self.__class__.xmintic2=property(lambda x: _getgen(x,"xmintic2"),
                                  lambda x,v: _setgen(x,"xmintic2",Pxt,v))
        self.__class__.ytic1=property(lambda x: _getgen(x,"ytic1"),
                                  lambda x,v: _setgen(x,"ytic1",Pyt,v))
        self.__class__.ytic2=property(lambda x: _getgen(x,"ytic2"),
                                  lambda x,v: _setgen(x,"ytic2",Pyt,v))
        self.__class__.ymintic1=property(lambda x: _getgen(x,"ymintic1"),
                                  lambda x,v: _setgen(x,"ymintic1",Pyt,v))
        self.__class__.ymintic2=property(lambda x: _getgen(x,"ymintic2"),
                                  lambda x,v: _setgen(x,"ymintic2",Pyt,v))
        self.__class__.xlabel1=property(lambda x: _getgen(x,"xlabel1"),
                                  lambda x,v: _setgen(x,"xlabel1",Pxl,v))
        self.__class__.xlabel2=property(lambda x: _getgen(x,"xlabel2"),
                                  lambda x,v: _setgen(x,"xlabel2",Pxl,v))
        self.__class__.ylabel1=property(lambda x: _getgen(x,"ylabel1"),
                                  lambda x,v: _setgen(x,"ylabel1",Pyl,v))
        self.__class__.ylabel2=property(lambda x: _getgen(x,"ylabel2"),
                                  lambda x,v: _setgen(x,"ylabel2",Pyl,v))
        self.__class__.box1=property(lambda x: _getgen(x,"box1"),
                                  lambda x,v: _setgen(x,"box1",Pbl,v))
        self.__class__.box2=property(lambda x: _getgen(x,"box2"),
                                  lambda x,v: _setgen(x,"box2",Pbl,v))
        self.__class__.box3=property(lambda x: _getgen(x,"box3"),
                                  lambda x,v: _setgen(x,"box3",Pbl,v))
        self.__class__.box4=property(lambda x: _getgen(x,"box4"),
                                  lambda x,v: _setgen(x,"box4",Pbl,v))
        self.__class__.line1=property(lambda x: _getgen(x,"line1"),
                                  lambda x,v: _setgen(x,"line1",Pbl,v))
        self.__class__.line2=property(lambda x: _getgen(x,"line2"),
                                  lambda x,v: _setgen(x,"line2",Pbl,v))
        self.__class__.line3=property(lambda x: _getgen(x,"line3"),
                                  lambda x,v: _setgen(x,"line3",Pbl,v))
        self.__class__.line4=property(lambda x: _getgen(x,"line4"),
                                  lambda x,v: _setgen(x,"line4",Pbl,v))
        self.__class__.legend=property(lambda x: _getgen(x,"legend"),
                                  lambda x,v: _setgen(x,"legend",Pls,v))
        self.__class__.data=property(lambda x: _getgen(x,"data"),
                                  lambda x,v: _setgen(x,"data",Pds,v))
        #################################################
        # The following initializes the template's TEXT #
        #################################################
        if Pic_name == "default":
          self._orientation=0
          self._file=Pt('file')
          self._function=Pt('function')
          self._logicalmask=Pt('logicalmask')
          self._transformation=Pt('transformation')
          self._source=Pt('source')
          self._dataname=Pt('dataname')
          self._title=Pt('title')
          self._units=Pt('units')
          self._crdate=Pt('crdate')
          self._crtime=Pt('crtime')
          self._comment1=Pt('comment1')
          self._comment2=Pt('comment2')
          self._comment3=Pt('comment3')
          self._comment4=Pt('comment4')
          self._xname=Pt('xname')
          self._yname=Pt('yname')
          self._zname=Pt('zname')
          self._tname=Pt('tname')
          self._xunits=Pt('xunits')
          self._yunits=Pt('yunits')
          self._zunits=Pt('zunits')
          self._tunits=Pt('tunits')
          ####################################################
      # The following initializes the template's FORMATS #
          ####################################################
          self._xvalue=Pf('xvalue')
          self._yvalue=Pf('yvalue')
          self._zvalue=Pf('zvalue')
          self._tvalue=Pf('tvalue')
          self._mean=Pf('mean')
          self._min=Pf('min')
          self._max=Pf('max')
          #########################################################
      # The following initializes the template's X-TICK MARKS #
          #########################################################
          self._xtic1=Pxt('xtic1')
          self._xtic2=Pxt('xtic2')
          self._xmintic1=Pxt('xmintic1')
          self._xmintic2=Pxt('xmintic2')
          #########################################################
      # The following initializes the template's Y-TICK MARKS #
          #########################################################
          self._ytic1=Pyt('ytic1')
          self._ytic2=Pyt('ytic2')
          self._ymintic1=Pyt('ymintic1')
          self._ymintic2=Pyt('ymintic2')
          #####################################################
      # The following initializes the template's X-LABELS #
          #####################################################
          self._xlabel1=Pxl('xlabel1')
          self._xlabel2=Pxl('xlabel2')
          #####################################################
      # The following initializes the template's Y-LABELS #
          #####################################################
          self._ylabel1=Pyl('ylabel1')
          self._ylabel2=Pyl('ylabel2')
          ############################################################
      # The following initializes the template's BOXES and LINES #
          ############################################################
          self._box1=Pbl('box1')
          self._box2=Pbl('box2')
          self._box3=Pbl('box3')
          self._box4=Pbl('box4')
          self._line1=Pbl('line1')
          self._line2=Pbl('line2')
          self._line3=Pbl('line3')
          self._line4=Pbl('line4')
          #########################################################
      # The following initializes the template's LEGEND SPACE #
          #########################################################
          self._legend=Pls('legend')
          #######################################################
      # The following initializes the template's DATA SPACE #
          #######################################################
          self._data=Pds('data')
        else:
          if isinstance(Pic_name_src,P):
            Pic_name_src = P.name
          if not Pic_name_src in vcs.elements["template"].keys():
            raise ValueError, "The source template '%s' does not seem to exists" % Pic_name_src
          src = vcs.elements["template"][Pic_name_src]
          self.orientation=src.orientation
          self.file=copy.copy(src.file)
          self.function=copy.copy(src.function)
          self.logicalmask=copy.copy(src.logicalmask)
          self.transformation=copy.copy(src.transformation)
          self.source=copy.copy(src.source)
          self.dataname=copy.copy(src.dataname)
          self.title=copy.copy(src.title)
          self.units=copy.copy(src.units)
          self.crdate=copy.copy(src.crdate)
          self.crtime=copy.copy(src.crtime)
          self.comment1=copy.copy(src.comment1)
          self.comment2=copy.copy(src.comment2)
          self.comment3=copy.copy(src.comment3)
          self.comment4=copy.copy(src.comment4)
          self.xname=copy.copy(src.xname)
          self.yname=copy.copy(src.yname)
          self.zname=copy.copy(src.zname)
          self.tname=copy.copy(src.tname)
          self.xunits=copy.copy(src.xunits)
          self.yunits=copy.copy(src.yunits)
          self.zunits=copy.copy(src.zunits)
          self.tunits=copy.copy(src.tunits)
          ###################################################
      # The following initializes the template's FORMATS #
          ####################################################
          self.xvalue=copy.copy(src.xvalue)
          self.yvalue=copy.copy(src.yvalue)
          self.zvalue=copy.copy(src.zvalue)
          self.tvalue=copy.copy(src.tvalue)
          self.mean=copy.copy(src.mean)
          self.min=copy.copy(src.min)
          self.max=copy.copy(src.max)
          ########################################################
      # The folowing initializes the template's X-TICK MARKS #
          ########################################################
          self.xtic1=copy.copy(src.xtic1)
          self.xtic2=copy.copy(src.xtic2)
          self.xmintic1=copy.copy(src.xmintic1)
          self.xmintic2=copy.copy(src.xmintic2)
          ########################################################
      # The folowing initializes the template's Y-TICK MARKS #
          ########################################################
          self.ytic1=copy.copy(src.ytic1)
          self.ytic2=copy.copy(src.ytic2)
          self.ymintic1=copy.copy(src.ymintic1)
          self.ymintic2=copy.copy(src.ymintic2)
          ####################################################
      # The folowing initializes the template's X-LABELS #
          ####################################################
          self.xlabel1=copy.copy(src.xlabel1)
          self.xlabel2=copy.copy(src.xlabel2)
          ####################################################
      # The folowing initializes the template's Y-LABELS #
          ####################################################
          self.ylabel1=copy.copy(src.ylabel1)
          self.ylabel2=copy.copy(src.ylabel2)
          ###########################################################
      # The folowing initializes the template's BOXES and LINES #
          ###########################################################
          self.box1=copy.copy(src.box1)
          self.box2=copy.copy(src.box2)
          self.box3=copy.copy(src.box3)
          self.box4=copy.copy(src.box4)
          self.line1=copy.copy(src.line1)
          self.line2=copy.copy(src.line2)
          self.line3=copy.copy(src.line3)
          self.line4=copy.copy(src.line4)
          ########################################################
      # The folowing initializes the template's LEGEND SPACE #
          ########################################################
          self.legend=copy.copy(src.legend)
          ######################################################
      # The folowing initializes the template's DATA SPACE #
          #######################################################
          self.data=copy.copy(src.data)


        vcs.elements["template"][Pic_name]=self

    ###########################################################################
    #                                                                         #
    # List out template text members (attributes).                            #
    #                                                                         #
    ###########################################################################
    def list(self, single=None):
        if (self.name == '__removed_from_VCS__'):
           raise ValueError, 'This instance has been removed from VCS.'

        if (single == None):
           print "","----------Template (P) member (attribute) listings ----------"
           #print 'Canvas Mode =',self.mode
           print "method =", self.p_name
           print "name =", self.name
           print "orientation =", self.orientation
           self.file.list()
           self.function.list()
           self.logicalmask.list()
           self.transformation.list()
           self.source.list()
           self.dataname.list()
           self.title.list()
           self.units.list()
           self.crdate.list()
           self.crtime.list()
           self.comment1.list()
           self.comment2.list()
           self.comment3.list()
           self.comment4.list()
           self.xname.list()
           self.yname.list()
           self.zname.list()
           self.tname.list()
           self.xunits.list()
           self.yunits.list()
           self.zunits.list()
           self.tunits.list()
           self.xvalue.list()
           self.yvalue.list()
           self.zvalue.list()
           self.tvalue.list()
           self.mean.list()
           self.min.list()
           self.max.list()
           self.xtic1.list()
           self.xtic2.list()
           self.xmintic1.list()
           self.xmintic2.list()
           self.ytic1.list()
           self.ytic2.list()
           self.ymintic1.list()
           self.ymintic2.list()
           self.xlabel1.list()
           self.xlabel2.list()
           self.ylabel1.list()
           self.ylabel2.list()
           self.box1.list()
           self.box2.list()
           self.box3.list()
           self.box4.list()
           self.line1.list()
           self.line2.list()
           self.line3.list()
           self.line4.list()
           self.legend.list()
           self.data.list()
        elif ((single == 'text') or (single == 'Pt')):
           self.file.list()
           self.function.list()
           self.logicalmask.list()
           self.transformation.list()
           self.source.list()
           self.dataname.list()
           self.title.list()
           self.units.list()
           self.crdate.list()
           self.crtime.list()
           self.comment1.list()
           self.comment2.list()
           self.comment3.list()
           self.comment4.list()
           self.xname.list()
           self.yname.list()
           self.zname.list()
           self.tname.list()
           self.xunits.list()
           self.yunits.list()
           self.zunits.list()
           self.tunits.list()
        elif ((single == 'format') or (single == 'Pf')):
           self.xvalue.list()
           self.yvalue.list()
           self.zvalue.list()
           self.tvalue.list()
           self.mean.list()
           self.min.list()
           self.max.list()
        elif ((single == 'xtickmarks') or (single == 'Pxt')):
           self.xtic1.list()
           self.xtic2.list()
           self.xmintic1.list()
           self.xmintic2.list()
        elif ((single == 'ytickmarks') or (single == 'Pyt')):
           self.ytic1.list()
           self.ytic2.list()
           self.ymintic1.list()
           self.ymintic2.list()
        elif ((single == 'xlabels') or (single == 'Pxl')):
           self.xlabel1.list()
           self.xlabel2.list()
        elif ((single == 'ylabels') or (single == 'Pyl')):
           self.ylabel1.list()
           self.ylabel2.list()
        elif ((single == 'boxeslines') or (single == 'Pbl')):
           self.box1.list()
           self.box2.list()
           self.box3.list()
           self.box4.list()
           self.line1.list()
           self.line2.list()
           self.line3.list()
           self.line4.list()
        elif ((single == 'legend') or (single == 'Pls')):
           self.legend.list()
        elif ((single == 'data') or (single == 'Pds')):
           self.data.list()
        elif (single == 'file'):
           self.file.list()
        elif (single == 'function'):
           self.function.list()
        elif (single == 'logicalmask'):
           self.logicalmask.list()
        elif (single == 'transformation'):
           self.transformation.list()
        elif (single == 'source'):
           self.source.list()
        elif (single == 'name'):
           self.name.list()
        elif (single == 'title'):
           self.title.list()
        elif (single == 'units'):
           self.units.list()
        elif (single == 'crdate'):
           self.crdate.list()
        elif (single == 'crtime'):
           self.crtime.list()
        elif (single == 'comment1'):
           self.comment1.list()
        elif (single == 'comment2'):
           self.comment2.list()
        elif (single == 'comment3'):
           self.comment3.list()
        elif (single == 'comment4'):
           self.comment4.list()
        elif (single == 'xname'):
           self.xname.list()
        elif (single == 'yname'):
           self.yname.list()
        elif (single == 'zname'):
           self.zname.list()
        elif (single == 'tname'):
           self.tname.list()
        elif (single == 'xunits'):
           self.xunits.list()
        elif (single == 'yunits'):
           self.yunits.list()
        elif (single == 'zunits'):
           self.zunits.list()
        elif (single == 'tunits'):
           self.tunits.list()
        elif (single == 'xvalue'):
           self.xvalue.list()
        elif (single == 'yvalue'):
           self.yvalue.list()
        elif (single == 'zvalue'):
           self.zvalue.list()
        elif (single == 'tvalue'):
           self.tvalue.list()
        elif (single == 'mean'):
           self.mean.list()
        elif (single == 'min'):
           self.min.list()
        elif (single == 'max'):
           self.max.list()
        elif (single == 'xtic1'):
           self.xtic1.list()
        elif (single == 'xtic2'):
           self.xtic2.list()
        elif (single == 'xmintic1'):
           self.xmintic1.list()
        elif (single == 'xmintic2'):
           self.xmintic2.list()
        elif (single == 'ytic1'):
           self.ytic1.list()
        elif (single == 'ytic2'):
           self.ytic2.list()
        elif (single == 'ymintic1'):
           self.ymintic1.list()
        elif (single == 'ymintic2'):
           self.ymintic2.list()
        elif (single == 'xlabel1'):
           self.xlabel1.list()
        elif (single == 'xlabel2'):
           self.xlabel2.list()
        elif (single == 'ylabel1'):
           self.ylabel1.list()
        elif (single == 'ylabel2'):
           self.ylabel2.list()
        elif (single == 'box1'):
           self.box1.list()
        elif (single == 'box2'):
           self.box2.list()
        elif (single == 'box3'):
           self.box3.list()
        elif (single == 'box4'):
           self.box4.list()
        elif (single == 'line1'):
           self.line1.list()
        elif (single == 'line2'):
           self.line2.list()
        elif (single == 'line3'):
           self.line3.list()
        elif (single == 'line4'):
           self.line4.list()
        elif (single == 'legend'):
           self.legend.list()
        elif (single == 'data'):
           self.data.list()

    ###########################################################################
    #                                                                         #
    # Script out template object in VCS to a file.                            #
    #                                                                         #
    ###########################################################################
    def script(self, script_filename=None, mode=None):
        '''
 Function:     script                           # Calls _vcs.scriptP

 Description of Function:
       Saves out a template object in VCS or Python script form to a designated
       file.

 Example of Use:
    script(scriptfile_name, mode)
              where: scriptfile_name is the output name of the script file.
                     mode is either "w" for replace or "a" for append.

              Note: If the the filename has a ".py" at the end, it will produce a
                    Python script. If the filename has a ".scr" at the end, it will
                    produce a VCS script. If neither extensions are give, then by
                    default a Python script will be produced.

    a=vcs.init()
    templt=a.createtemplate('temp')   # create a template object
    templt.script('filename.py')      # Append to a Python file "filename.py"
    templt.script('filename.scr')     # Append to a VCS file "filename.scr"
    templt.script('filename','w')     # Create or overwrite to a Python file "filename.py"
'''
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

           unique_name = '__P__' + self.name
           fp.write("#----------Template (P) member (attribute) listings ----------\n")
           fp.write("p_list=v.listelements('template')\n")
           fp.write("if ('%s' in p_list):\n" % self.name)
           fp.write("   %s = v.gettemplate('%s')\n" % (unique_name, self.name))
           fp.write("else:\n")
           fp.write("   %s = v.createtemplate('%s')\n" % (unique_name, self.name))
           fp.write("orientation = '%d'\n" % self.orientation)
        # Write out the TEXT template
           j=0
           a=[self.file,self.function,self.logicalmask,self.transformation,self.source,self.dataname,self.title,self.units,self.crdate,self.crtime,self.comment1,self.comment2,self.comment3,self.comment4,self.xname,self.yname,self.zname,self.tname,self.xunits,self.yunits,self.zunits,self.tunits]
           for i in ('file','function','logicalmask','transformation','source','dataname','title','units','crdate','crtime','comment1','comment2','comment3','comment4','xname','yname','zname','tname','xunits','yunits','zunits','tunits'):
             fp.write("# member = %s\n" % i)
             fp.write("%s.%s.priority = %g\n" % (unique_name,i,a[j].priority))
             fp.write("%s.%s.x = %g\n" % (unique_name,i,a[j].x))
             fp.write("%s.%s.y = %g\n" % (unique_name,i,a[j].y))
             fp.write("%s.%s.texttable = '%s'\n" % (unique_name,i,a[j].texttable))
             fp.write("%s.%s.textorientation = '%s'\n\n" % (unique_name,i,a[j].textorientation))
             j=j+1

        # Write out the FORMAT template
           j=0
           a=[self.xvalue,self.yvalue,self.zvalue,self.tvalue,self.mean,self.min,self.max]
           for i in ('xvalue','yvalue','zvalue','tvalue','mean','min','max'):
             fp.write("# member = %s\n" % i)
             fp.write("%s.%s.priority = %g\n" % (unique_name,i,a[j].priority))
             fp.write("%s.%s.x = %g\n" % (unique_name,i,a[j].x))
             fp.write("%s.%s.y = %g\n" % (unique_name,i,a[j].y))
             fp.write("%s.%s.texttable = '%s'\n" % (unique_name,i,a[j].format))
             fp.write("%s.%s.texttable = '%s'\n" % (unique_name,i,a[j].texttable))
             fp.write("%s.%s.textorientation = '%s'\n\n" % (unique_name,i,a[j].textorientation))
             j=j+1

        # Write out the X-TICK template
           j=0
           a=[self.xtic1,self.xtic2,self.xmintic1,self.xmintic2]
           for i in ('xtic1','xtic2','xmintic1','xmintic2'):
             fp.write("# member = %s\n" % i)
             fp.write("%s.%s.priority = %g\n" % (unique_name,i,a[j].priority))
             fp.write("%s.%s.y1 = %g\n" % (unique_name,i,a[j].y1))
             fp.write("%s.%s.y2 = %g\n" % (unique_name,i,a[j].y2))
             fp.write("%s.%s.line = '%s'\n\n" % (unique_name,i,a[j].line))
             j=j+1

        # Write out the Y-TICK template
           j=0
           a=[self.ytic1,self.ytic2,self.ymintic1,self.ymintic2]
           for i in ('ytic1','ytic2','ymintic1','ymintic2'):
             fp.write("# member = %s\n" % i)
             fp.write("%s.%s.priority = %g\n" % (unique_name,i,a[j].priority))
             fp.write("%s.%s.x1 = %g\n" % (unique_name,i,a[j].x1))
             fp.write("%s.%s.x2 = %g\n" % (unique_name,i,a[j].x2))
             fp.write("%s.%s.line = '%s'\n\n" % (unique_name,i,a[j].line))
             j=j+1

        # Write out the X-LABELS template
           j=0
           a=[self.xlabel1, self.xlabel2]
           for i in ('xlabel1','xlabel2'):
             fp.write("# member = %s\n" % i)
             fp.write("%s.%s.priority = %g\n" % (unique_name,i,a[j].priority))
             fp.write("%s.%s.y = %g\n" % (unique_name,i,a[j].y))
             fp.write("%s.%s.texttable = '%s'\n" % (unique_name,i,a[j].texttable))
             fp.write("%s.%s.textorientation = '%s'\n\n" % (unique_name,i,a[j].textorientation))
             j=j+1

        # Write out the Y-LABELS template
           j=0
           a=[self.ylabel1, self.ylabel2]
           for i in ('ylabel1','ylabel2'):
             fp.write("# member = %s\n" % i)
             fp.write("%s.%s.priority = %g\n" % (unique_name,i,a[j].priority))
             fp.write("%s.%s.x = %g\n" % (unique_name,i,a[j].x))
             fp.write("%s.%s.texttable = '%s'\n" % (unique_name,i,a[j].texttable))
             fp.write("%s.%s.textorientation = '%s'\n\n" % (unique_name,i,a[j].textorientation))
             j=j+1

        # Write out the BOXES and LINES template
           j=0
           a=[self.box1, self.box2, self.box1, self.box2, self.line1, self.line2, self.line3, self.line4]
           for i in ('box1','box2','box3','box4','line1','line2','line3','line4'):
             fp.write("# member = %s\n" % i)
             fp.write("%s.%s.priority = %g\n" % (unique_name,i,a[j].priority))
             fp.write("%s.%s.x1 = %g\n" % (unique_name,i,a[j].x1))
             fp.write("%s.%s.y1 = %g\n" % (unique_name,i,a[j].y1))
             fp.write("%s.%s.x2 = %g\n" % (unique_name,i,a[j].x2))
             fp.write("%s.%s.y2 = %g\n" % (unique_name,i,a[j].y2))
             fp.write("%s.%s.line = '%s'\n\n" % (unique_name,i,a[j].line))
             j=j+1

        # Write out the LEGEND SPACE template
           fp.write("# member = %s\n" % 'legend')
           fp.write("%s.legend.priority = %g\n" % (unique_name,self.legend.priority))
           fp.write("%s.legend.x1 = %g\n" % (unique_name,self.legend.x1))
           fp.write("%s.legend.y1 = %g\n" % (unique_name,self.legend.y1))
           fp.write("%s.legend.x2 = %g\n" % (unique_name,self.legend.x2))
           fp.write("%s.legend.y2 = %g\n" % (unique_name,self.legend.y2))
           fp.write("%s.legend.line = '%s'\n" % (unique_name,self.legend.line))
           fp.write("%s.legend.texttable = '%s'\n" % (unique_name,self.legend.texttable))
           fp.write("%s.legend.textorientation = '%s'\n\n" % (unique_name,self.legend.textorientation))

        # Write out the DATA SPACE template
           fp.write("# member = %s\n" % 'data')
           fp.write("%s.data.priority = %g\n" % (unique_name,self.data.priority))
           fp.write("%s.data.x1 = %g\n" % (unique_name,self.data.x1))
           fp.write("%s.data.y1 = %g\n" % (unique_name,self.data.y1))
           fp.write("%s.data.x2 = %g\n" % (unique_name,self.data.x2))
           fp.write("%s.data.y2 = %g\n\n" % (unique_name,self.data.y2))
        else:
          #Json type
          mode+="+"
          f = open(script_filename,mode)
          vcs.utils.dumpToJson(self,f)
          f.close()
           
    ## Adding the drawing functionnality to plot all these attributes on the Canvas

    def drawTicks(self,slab,gm,x,axis,number,vp,wc,bg=0,X=None,Y=None,**kargs):
        """Draws the ticks for the axis x number number
        using the label passed by the graphic  method
        vp and wc are from the actual canvas, they have been reset when they get here...
        """
        kargs["donotstoredisplay"]=True
        if X is None:
          X=slab.getAxis(-1)
        if Y is None:
          Y=slab.getAxis(-2)
        displays = []
        # compute the spanning in x and y, and adjust for the viewport
        if gm.datawc_x1 > 9.E19 :
          wc[0]=X[:].min()
        else:
          wc[0] = gm.datawc_x1
        if gm.datawc_x2 > 9.E19 :
          wc[1]=X[:].max()
        else:
          wc[1] = gm.datawc_x2
        if gm.datawc_y1 > 9.E19 :
          wc[2]=Y[:].min()
        else:
          wc[2] = gm.datawc_y1
        if gm.datawc_y2 > 9.E19 :
          wc[3]=Y[:].max()
        else:
          wc[3] = gm.datawc_y2

        vp=[self.data.x1,self.data.x2,self.data.y1,self.data.y2]
        dx=wc[1]-wc[0]
        dy=wc[3]-wc[2]
        dx=dx/(vp[1]-vp[0])
        dy=dy/(vp[3]-vp[2])
        # get the actual labels
        loc=copy.copy(getattr(gm,axis+'ticlabels'+number))
        # Are they set or do we need to it ?
        if (loc is None or loc=='*'):
            # well i guess we have to do it !
            if axis=='x':
                 x1 = wc[0]
                 x2 = wc[1]
            else:
                 x1 = wc[2]
                 x2 = wc[3]
            loc=vcs.mkscale(x1,x2)
            loc=vcs.mklabels(loc)
            if number == '2':
                for t in loc.keys():
                    loc[t]=''
        if isinstance(loc,str):
          loc = vcs.elements["list"].get(loc,{})
        # Make sure the label passed are not outside the world coordinates
        dw1=1.E20
        dw2=1.E20
##         if getattr(gm,'datawc_'+axis+'1')<9.9E19:
##             dw1=getattr(gm,'datawc_'+axis+'1')
##             dw2=getattr(gm,'datawc_'+axis+'2')
##             for k in loc.keys():
##                 if getattr(gm,'datawc_'+axis+'2')>getattr(gm,'datawc_'+axis+'1'):
##                     if not (getattr(gm,'datawc_'+axis+'1')<=loc[k]<=getattr(gm,'datawc_'+axis+'2')):
##                         del(loc[k])
##                 else:
##                     if not (getattr(gm,'datawc_'+axis+'1')>=loc[k]>=getattr(gm,'datawc_'+axis+'2')):
##                         del(loc[k])

        if axis=='x':
             dw1,dw2=wc[0],wc[1]
        else:
             dw1,dw2=wc[2],wc[3]
        for k in loc.keys():
             if dw2>dw1:
                  if not(dw1<=k<=dw2):
                       del(loc[k])
             else:
                  if not (dw1>=k>=dw2):
                       del(loc[k])
        #The ticks
        obj=getattr(self,axis+'tic'+number)
        # the labels
        objlabl=getattr(self,axis+'label'+number)
        # the following to make sure we have a unique name,
        # i put them together assuming it would be faster
        ticks=x.createline(source=obj.line)
        tt=x.createtext(Tt_source=objlabl.texttable,To_source=objlabl.textorientation)
        ticks.priority=obj.priority
        tt.priority=objlabl.priority
        # initialize the list of values
        tstring=[]
        xs=[]
        ys=[]
        tys=[]
        txs=[]
        loc2=loc
        loc=getattr(gm,axis+'ticlabels'+number)
        if loc == '*' or loc is None:
             loc=loc2
        if isinstance(loc,str):
          loc = vcs.elements["list"].get(loc,{})
        # set the x/y/text values
        for l in loc.keys():
          if axis=='x':
               mn,mx = vcs.minmax(wc[0],wc[1])
               if mn<=l<=mx:
                    xs.append([(l-wc[0])/dx+vp[0],(l-wc[0])/dx+vp[0]])
                    ys.append([obj.y1,obj.y2])
                    txs.append((l-wc[0])/dx+vp[0])
                    tys.append(objlabl.y)
                    tstring.append(loc[l])
          elif axis=='y':
               mn,mx = vcs.minmax(wc[2],wc[3])
               if mn<=l<=mx:
                    ys.append([(l-wc[2])/dy+vp[2],(l-wc[2])/dy+vp[2]])
                    xs.append([obj.x1,obj.x2])
                    tys.append((l-wc[2])/dy+vp[2])
                    txs.append(objlabl.x)
                    tstring.append(loc[l])
        # now does the mini ticks
        if getattr(gm,axis+'mtics'+number)!='':
            obj=getattr(self,axis+'mintic'+number)
            for l in getattr(gm,axis+'mtics'+number).keys():
                a=getattr(gm,axis+'mtics'+number)[l]
                if axis=='x':
                    mn,mx = vcs.minmax(wc[0],wc[1])
                    if mn<=l<=mx:
                         xs.append([(l-wc[0])/dx+vp[0],(l-wc[0])/dx+vp[0]])
                         ys.append([obj.y1,obj.y2])
                         tstring.append(a)
                elif axis=='y':
                    mn,mx = vcs.minmax(wc[2],wc[3])
                    if mn<=l<=mx:
                         ys.append([(l-wc[2])/dy+vp[2],(l-wc[2])/dy+vp[2]])
                         xs.append([obj.x1,obj.x2])
                         tstring.append(a)

        if txs!=[]:
             tt.string=tstring
             tt.x=txs
             tt.y=tys
             displays.append(x.text(tt,bg=bg,**kargs))
        if xs!=[]:
             ticks.x=xs
             ticks.y=ys
             displays.append(x.line(ticks,bg=bg,**kargs))
        return displays
   


    def reset(self,sub_name,v1,v2,ov1=None,ov2=None):
         """
         this function reset all the attribute who have a sub attribute named name1 or name2 or name
         also respect how far from original position you are
         i.e you move to x1,x2 from old_x1, old_x2
         if your current x1 value is not == to old_x1_value, then respect how far from it you  were
         usage:
         reset(sub_name,v1,v2,ov1=None,ov2=None)
         Example:
              t.reset('x',x1,x2,t.data.x1,t.data.x2)
              #where t is a vcs template object
         """
         #attr=vars(self).keys()
         Attr = dir(self)
         attr = []
         for a in Attr:
           if a[0]!="_":
             attr.append(a)
         n=len(sub_name)
         # computes the ratio
         if ov1 is not None:
              odv=ov2-ov1
              ratio=(v2-v1)/odv
         else:
              ratio=1.
         for a in attr:
              v=getattr(self,a)
              try:
                   subattr=vars(v).keys()
              except:
                try:
                   subattr=v.__slots__
                   delta=0.
                   if sub_name+'1' in subattr:
                        ov=getattr(v,sub_name+'1')
                        if ov1 is not None:
                             delta=(ov-ov1)*ratio
                        setattr(v,sub_name+'1',v1+delta)
                   delta=0.
                   if sub_name+'2' in subattr:
                        ov=getattr(v,sub_name+'2')
                        if ov2 is not None:
                             delta=(ov-ov2)*ratio
                        setattr(v,sub_name+'2',v2+delta)
                   delta=0.
                   if sub_name in subattr:
                        ov=getattr(v,sub_name)
                        if ov1 is not None:
                             delta=(ov-ov1)*ratio
                        setattr(v,sub_name,v1+delta)
                        if a[-1]=='2':
                             ov=getattr(v,sub_name+'2')
                             if ov2 is not None:
                                  delta=(ov-ov2)*ratio
                             setattr(v,sub_name,v2+delta)
                except Exception,err:
                  pass

    def move(self,p,axis):
         """ move a template by p% along the axis 'x' or 'y'
         positive values of p mean movement toward right/top
         negative values of p mean movement toward left/bottom
         The reference point is t.data.x1/y1
         usage
         t.move(p,axis)
         example:
         t.move(.2,'x') # move everything right by 20%
         t.move(.2,'y') # move everything down by 20%
         """
         if not axis in ['x','y']:
              raise 'Error you can move the template only the x or y axis'
         #p/=100.
         ov1=getattr(self.data,axis+'1')
         ov2=getattr(self.data,axis+'2')
         v1=ov1+p
         v2=ov2+p
         self.reset(axis,v1,v2,ov1,ov2)
         
    def moveto(self,x,y):
         """ move a template along the axis 'x' or 'y' to p
         The reference point is t.data.x/y1
         usage
         t.moveto(x,y)
         example:
         t.moveto(.2,.2) # move everything so that data.x1=.2and data.y1=.2
         """
         #p/=100.
         ov1=getattr(self.data,'x1')
         ov2=getattr(self.data,'x2')
         v1=x
         v2=(ov2-ov1)+x
         self.reset('x',v1,v2,ov1,ov2)
         ov1=getattr(self.data,'y1')
         ov2=getattr(self.data,'y2')
         v1=y
         v2=(ov2-ov1)+y
         self.reset('y',v1,v2,ov1,ov2)
         
    def scale(self,scale,axis='xy',font=-1):
         """ scale a template along the axis 'x' or 'y' by scale
         positive values of p mean increase
         negative values of p mean decrease
         The reference point is t.data.x/y1
         usage
         t.scale(scale,axis='xy',font=-1)
         scale: any number
         axis can be 'x','y' or 'xy' (which means both)
         font can be 1/0/-1
                     0: means do not scale the fonts
                     1: means scale the fonts
                    -1: means do not scale the fonts unless axis='xy'
         Example:
         x=vcs.init()
         t=x.createtemplate('a_template')
         t.scale(.5)  # halves the template size
         t.scale(1.2) # upsize everything to 20% more than the original size
         t.scale(2,'x') # double the x axis

         reference is t.data.x1/y1
 
         """
         if not axis in ['x','y','xy']:
              raise 'Error you can move the template only the x or y axis'
         #p/=100.
         if axis=='xy':
              axis=['x','y']
         else:
              axis=[axis,]
         for ax in axis:
              ov1=getattr(self.data,ax+'1')
              ov2=getattr(self.data,ax+'2')
              v1=ov1
              v2=(ov2-ov1)*scale+v1
              self.reset(ax,v1,v2,ov1,ov2)
         if font==1 or (font==-1 and axis==['x','y']):
              self.scalefont(scale)
              
    def scalefont(self,scale):
         """
         Scales the tempate font by scale
         Usage:
         scalefont(scale)
         
         Example:
         x=vcs.init()
         t=x.createtemplate('a_template')
         t.scalefont(.5) # reduces the fonts size by 2
         """
         attr=vars(self).keys()
         for a in attr:
              v=getattr(self,a)
              try:
                   to=getattr(v,'textorientation')
                   tmp = vcs.init()
                   to=tmp.createtextorientation(source=to)
                   to.height=to.height*scale
                   setattr(v,'textorientation',to)
              except:
                   pass        
         
    def plot(self,x,slab,gm,bg=0,min=None,max=None,X=None,Y=None,**kargs):
        """ This plots the template stuff on the Canvas, it needs a slab and a graphic method
        returns a list containing all the displays used"""
        displays = []
        kargs["donotstoredisplay"]=True
        # now remembers the viewport and worldcoordinates in order to reset them later
        vp=x.viewport
        wc=x.worldcoordinate
        #m=x.mode
        # and resets everything to [0,1]
        x.viewport=[0,1,0,1]
        x.worldcoordinate=[0,1,0,1]
        #x.mode=0 # this should disable the replot but it doesn't work....
        # figures out the min and max and set them as atributes...
        smn,smx=vcs.minmax(slab)
        if min is None or max is None:
             mn,mx = smn,smx
        else:
             mn,mx=min,max

        attributes=['file','function','logicalmask','transformation',
                    'source','id','title','units','crdate','crtime',
                    'comment1','comment2','comment3','comment4','xname','yname',
                    'zname','tname','zunits','tunits','xvalue','yvalue','zvalue',
                    'tvalue','mean','min','max']
        if gm=='taylordiagram':
             attributes=attributes[:-3]

        for s in attributes: # loop through various section of the template object
            if hasattr(slab,s):
                if s=='id':
                    sub=self.dataname
                else:
                    sub=getattr(self,s)
                tt=x.createtext(None,sub.texttable,None,sub.textorientation)

                # Now for the min/max/mean add the name in front
                if s=='min':
                  tt.string='Min %g' % (smn)
                elif s=='max':
                  tt.string='Max %g' % smx
                elif s=='mean':
                    if not inspect.ismethod(getattr(slab,'mean')):
                         tt.string='Mean '+str(getattr(slab,s))
                    else:
                         tt.string='Mean %f'%slab.mean()
                else :
                    tt.string=str(getattr(slab,s))
                tt.x=[sub.x]
                tt.y=[sub.y]
                tt.priority=sub.priority
                displays.append(x.text(tt,bg=bg,**kargs))


        # Do the tickmarks/labels
        if gm!='taylordiagram':
             displays+=self.drawTicks(slab,gm,x,axis='x',number='1',vp=vp,wc=wc,bg=bg,X=X,Y=Y,**kargs)
             displays+=self.drawTicks(slab,gm,x,axis='x',number='2',vp=vp,wc=wc,bg=bg,X=X,Y=Y,**kargs)
             displays+=self.drawTicks(slab,gm,x,axis='y',number='1',vp=vp,wc=wc,bg=bg,X=X,Y=Y,**kargs)
             displays+=self.drawTicks(slab,gm,x,axis='y',number='2',vp=vp,wc=wc,bg=bg,X=X,Y=Y,**kargs)

        # Do the boxes and lines
        b=self.box1
        if b.priority!=0:
             l=x.createline(source=b.line)
             l.x=[b.x1,b.x2,b.x2,b.x1,b.x1]
             l.y=[b.y1,b.y1,b.y2,b.y2,b.y1]
             l.priority=b.priority
             displays.append(x.line(l,bg=bg,**kargs))

        b=self.box2
        if b.priority!=0:
             l=x.createline(source=b.line)
             l.x=[b.x1,b.x2,b.x2,b.x1,b.x1]
             l.y=[b.y1,b.y1,b.y2,b.y2,b.y1]
             l.priority=b.priority
             displays.append(x.line(l,bg=bg,**kargs))

        b=self.box3
        if b.priority!=0:
             l=x.createline(source=b.line)
             l.x=[b.x1,b.x2,b.x2,b.x1,b.x1]
             l.y=[b.y1,b.y1,b.y2,b.y2,b.y1]
             l.priority=b.priority
             displays.append(x.line(l,bg=bg,**kargs))

        b=self.box4
        if b.priority!=0:
             l=x.createline(source=b.line)
             l.x=[b.x1,b.x2,b.x2,b.x1,b.x1]
             l.y=[b.y1,b.y1,b.y2,b.y2,b.y1]
             l.priority=b.priority
             displays.append(x.line(l,bg=bg,**kargs))

        b=self.line1
        if b.priority!=0:
             l=x.createline(source=b.line)
             l.x=[b.x1,b.x2]
             l.y=[b.y1,b.y2]
             l.priority=b.priority
             displays.append(x.line(l,bg=bg,**kargs))

        b=self.line2
        if b.priority!=0:
             l=x.createline(source=b.line)
             l.x=[b.x1,b.x2]
             l.y=[b.y1,b.y2]
             l.priority=b.priority
             displays.append(x.line(l,bg=bg,**kargs))

        b=self.line3
        if b.priority!=0:
             l=x.createline(source=b.line)
             l.x=[b.x1,b.x2]
             l.y=[b.y1,b.y2]
             l.priority=b.priority
             displays.append(x.line(l,bg=bg,**kargs))

        b=self.line4
        if b.priority!=0:
             l=x.createline(source=b.line)
             l.x=[b.x1,b.x2]
             l.y=[b.y1,b.y2]
             l.priority=b.priority
             displays.append(x.line(l,bg=bg,**kargs))
        #x.mode=m
        # I think i have to use dict here because it's a valid value
        # (obviously since i got it from the object itself and didn't touch it
        # but Dean doesn't allow to set it back to some of these values (None)!
        x.viewport=vp
        x.worldcoordinate=wc
        return displays

    def drawColorBar(self,colors,levels,legend=None,ext_1='n',ext_2='n',x=None,bg=0,priority=None,cmap=None,**kargs):
         """
         This function, draws the colorbar, it needs:
         colors : The colors to be plotted
         levels : The levels that each color represent
         legend : To overwrite, saying just draw box at certain values and display some specific text instead of the value
         ext_1 and ext_2: to draw the arrows
         x : the canvas where to plot it
         bg: background mode ?
         returns a list of displays used
         """
         kargs["donotstoredisplay"]=True
         displays=[]
         #
         # Create legend
         #

         ## Are levs contiguous?
         if isinstance(levels[0],(list,tuple)):
           levs2=[]
           cont = True
           for i in range(len(levels)-1):
             if levels[i][1] == levels[i+1][0]:
               levs2.append(levels[i][0])
             else: # Ok not contiguous
               cont = False
               break
           if cont:
             levs2.append(levels[-1][0])
             levs2.append(levels[-1][1])
             levels = levs2

         # Now sets the priority value
         if priority is None:
              priority=self.legend.priority
         # Now resets the viewport and worldcoordinate
         vp=x.viewport  # preserve for later restore
         wc=x.worldcoordinate # preserve for later restore
         x.viewport=[0.,1.,0.,1.]
         x.worldcoordinate=[0.,1.,0.,1.]
         # Ok first determine the orientation of the legend (bottom to top  or left to right)
         dX=self.legend.x2-self.legend.x1
         dY=self.legend.y2-self.legend.y1
         nbox=len(colors)
         if isinstance(levels[0],list):
           l0 = levels[0][0]
           l1 = levels[-1][1]
         else:
           l0 = levels[0]
           l1 = levels[-1]
         if l0<-1.e19:
           ext_1='y'
         if l1>1.e19:
           ext_2='y'
         levels=list(levels)
         # Now figure out the typical length of a box
         if abs(dX)>=abs(dY):
              isH=1
              dLong=dX
              dlong = dX/nbox
              dshrt = dY
              startlong = self.legend.x1
              startshrt = self.legend.y1
         else:
              isH=0
              dLong=dY
              dlong = dY/nbox
              dshrt = dX
              startlong = self.legend.y1
              startshrt = self.legend.x1
         # initialize the fillarea coordinates
         L=[]
         S=[]
         # computes the fillarea coordinates
         iext=0 # To know if we changed the dims
         minarrow=.02 # % of the legend that the arrow must use (at least)
         if (ext_1=='y' or ext_2=='y' ) and dlong<minarrow*dLong:
              iext=1 # one mins changed ext_1
              if ext_1=='y' and ext_2=='y':
                   dlong=dLong*(1.-2.*minarrow)/(nbox-2.)
                   iext=3 # changed both side
              else:
                   dlong=dLong*(1.-minarrow)/(nbox-1.)
                   if ext_2=='y': iext=2
                   
         for i in range(nbox):
              if ext_1=='y' and i==0:
                   if iext==1 or iext==3: # did the chage the size of the arrow
                        dlongarrow=minarrow*dLong
                   else:
                        dlongarrow=dlong
                   # Draws the little arrow at the begining
                   L.append([startlong,          startlong+dlongarrow, startlong+dlongarrow])
                   S.append([startshrt+dshrt/2., startshrt+dshrt     , startshrt      ])
                   # Now readjust startlong if necessary
                   if iext==1 or iext==3:
                        startlong=startlong-dlong+minarrow*dLong
              elif ext_2=='y' and i==nbox-1:
                   if iext>1: # we changed the size of the arrow at the end
                        dlongarrow=dLong*minarrow
                   else:
                        dlongarrow=dlong
                   # Draws the little arrow at the end
                   L.append([startlong+dlong*i+dlongarrow, startlong+dlong*i, startlong+dlong*i])
                   S.append([startshrt+dshrt/2.   , startshrt+dshrt  , startshrt      ])
              else:
                   # Draws a normal box
                   #print i,dlong,dshrt,startlong,startshrt
                   L.append([startlong+dlong*i, startlong+dlong*(i+1), startlong+dlong*(i+1), startlong+dlong*i])
                   S.append([startshrt        , startshrt            , startshrt+dshrt      , startshrt+dshrt])
                    
         fa=x.createfillarea()
         fa.color=colors
         fa.style='solid'
         fa.priority=priority
         if cmap is not None:
           fa.colormap = cmap
         if isH:
              fa.x=L
              fa.y=S
         else:
              fa.x=S
              fa.y=L
##          fa.list()
         displays.append(x.fillarea(fa,bg=bg,**kargs))
         # Now draws the legend
         # Fisrt of all make sure we draw the arrows
         Sl=[]
         Ll=[]
         St=[]
         Lt=[]
         Tt=[]
         dD=dLong # length of the levels area
         if ext_1=='y':
              startlong=startlong+dlong
              Sl.append(S[0])
              Ll.append(L[0])
              # we need to close the arrow
              Sl[0].append(Sl[0][0])
              Ll[0].append(Ll[0][0])
              levels.pop(0)
              if iext==1 or iext==3:
                   dD=dD-dLong*minarrow
              else:
                   dD=dD-dlong
         if ext_2=='y':
              Sl.append(S[-1])
              Ll.append(L[-1])
              # we need to close the arrow
              Sl[0].append(Sl[0][0])
              Ll[0].append(Ll[0][0])
              levels.pop(-1)
              if iext>1:
                   dD=dD-dLong*minarrow
              else:
                   dD=dD-dlong
         # adds the coordinate for the box around the legend
         Sl.append([startshrt      , startshrt         , startshrt+dshrt   , startshrt+dshrt, startshrt])
         Ll.append([startlong      , startlong+dD      , startlong+dD      , startlong      , startlong])
         # Now make sure we have a legend
         if isinstance(levels[0],list):
           ## Ok these are nono contiguous levels, we will use legend only if it's a perfect match
           for i,l in enumerate(levels):
             lt = l[0]
             lb = l[1]
             loc = i*dlong + startlong
             Ll.append([loc,loc])
             Sl.append([startshrt,startshrt+dshrt])
             if legend is not None:
               lt=legend.get(lt,None)
               lb=legend.get(lb,None)
             if lt is not None:
               loct = startlong + (i+.5)*dlong
               Tt.append(str(lt)) 
               Lt.append(loct)
               St.append(startshrt+dshrt*1.4)
             if lb is not None:
               loct = startlong + (i+.5)*dlong
               Tt.append(str(lb)) 
               Lt.append(loct)
               St.append(startshrt-dshrt*.6)

         else:
           if legend is None:
                legend=vcs.mklabels(levels)
           if levels[0]<levels[1]:
                ecompfunc=numpy.less_equal
                compfunc=numpy.less             
           else:
                ecompfunc=numpy.greater_equal
                compfunc=numpy.greater
           for l in legend.keys():
                if not compfunc(l,levels[0]) and not compfunc(levels[-1],l):
                     for i in range(len(levels)-1):
                          if ecompfunc(levels[i],l) and ecompfunc(l,levels[i+1]):
                               # Ok we're between 2 levels, let's add the legend
                               # first let's figure out where to put it
                               loc=i*dlong # position at beginnig of level
                               loc+=(l-levels[i])/(levels[i+1]-levels[i])*dlong # Adds the distance from beginnig of level box
                               loc+=startlong ## Figures out the begining
  ##                              loc=((l-levels[0])/(levels[-1]-levels[0]))*dD+startlong
                               Ll.append([loc,loc])
                               Sl.append([startshrt,startshrt+dshrt])
                               Lt.append(loc)
                               St.append(startshrt+dshrt*1.4)
                               Tt.append(legend[l])
                               break
         # ok now creates the line object and text object
         ln=x.createline(source=self.legend.line)
         txt=x.createtext(To_source=self.legend.textorientation,Tt_source=self.legend.texttable)
         ln.priority=priority+1
         txt.priority=priority+1
         txt.string=Tt
         if isinstance(legend,list):
           if isH:
             txt.halign = "center"
           else:
             txt.valign = "half"
         if isH:
              ln.x=Ll
              ln.y=Sl
              txt.x=Lt
              txt.y=St
         else:
              ln.x=Sl
              ln.y=Ll
              txt.x=St
              txt.y=Lt
         
         # Now reset the viewport and worldcoordiantes
         displays.append(x.line(ln,bg=bg,**kargs))
         displays.append(x.text(txt,bg=bg,**kargs))
         x.viewport=vp
         x.worldcoordinate=wc
         return displays

    def ratio_linear_projection(self,lon1,lon2,lat1,lat2,Rwished=None,Rout=None,box_and_ticks=0,x=None):
         '''
         Computes ratio to shrink the data area of a template in order that the overall area
         has the least possible deformation in linear projection

         Version: 1.1
         Notes: Thanks to Karl Taylor for the equation of "optimal" ratio

         Necessary arguments:
           lon1, lon2: in degrees_east  : Longitude spanned by plot
           lat1, lat2: in degrees_north : Latitude  spanned by plot

         Optional arguments:
           Rwished: Ratio y/x wished, None=automagic
           Rout: Ratio of output (default is US Letter=11./8.5)
                 Also you can pass a string: "A4","US LETTER", "X"/"SCREEN", the latest uses the window information
           box_and_ticks: Also redefine box and ticks to the new region
         Returned:
           vcs template object

         Usage example:
           ## USA
           t.ratio_linear_projection(-135,-50,20,50)
         '''
         

         # Converts lat/lon to rad
         Lat1=lat1/180.*numpy.pi
         Lat2=lat2/180.*numpy.pi
         Lon1=lon1/180.*numpy.pi
         Lon2=lon2/180.*numpy.pi

         if (Lon1==Lon2) or (Lat1==Lat2):
              return
         
         
         if Rwished is None:
             Rwished=float(2*(numpy.sin(Lat2)-numpy.sin(Lat1))/(Lon2-Lon1)/(1.+(numpy.sin(2*Lat2)-numpy.sin(2*Lat1))/2./(Lat2-Lat1)))
         self.ratio(Rwished,Rout,box_and_ticks,x)
         return

    
    def ratio(self,Rwished,Rout=None,box_and_ticks=0,x=None):
         '''
         Computes ratio to shrink the data area of a template to have an y/x ratio of Rwished
         has the least possible deformation in linear projection

         Version: 1.1

         Necessary arguments:
           Rwished: Ratio y/x wished
         Optional arguments:
           Rout: Ratio of output (default is US Letter=11./8.5)
                 Also you can pass a string: "A4","US LETTER", "X"/"SCREEN", the latest uses the window information
           box_and_ticks: Also redefine box and ticks to the new region
         Returned:
           vcs template object

         Usage example:
           ## y is twice x
           t.ratio(2)
         '''
         if x is None:
           x=vcs.init()
         if isinstance(Rout,str):
              if Rout.lower()=='a4':
                   Rout=29.7/21.
                   if x.isportrait():
                        Rout=1./Rout
              elif Rout.lower() in ['us letter','letter','us_letter','usletter']:
                   Rout=11./8.5
                   if x.isportrait():
                        Rout=1./Rout
              elif Rout.lower() in ['x','x11','screen']:
                   if x.iscanvasdisplayed(): # do we have the canvas opened ?
                        info=x.canvasinfo()
                        Rout=float(info['width'])/float(info['height'])
                   else: # Not opened yet, assuming default size: 959/728
                        Rout=1./.758800507
                        if x.isportrait():
                             Rout=1./Rout
         elif Rout is None:
           try:
             info = x.canvasinfo()
             Rout=float(info['width'])/float(info['height'])
           except Exception,err:
              Rout=1./.758800507
              if x.isportrait():
                   Rout=1./Rout

         t=x.createtemplate(source=self.name)

         # Computes the template ratio
         Rt=(self.data.y2-self.data.y1)/(self.data.x2-self.data.x1)

         # Actual ratio template and output format combined
         Ra=Rt/Rout
         #Ra=(self.data.y2-self.data.y1)/(self.data.x2-self.data.x1)
         if Rwished>Ra:
              t.scale(Ra/Rwished,axis='x')
         else:
              t.scale(Rwished/Ra,axis='y')
         ndx=t.data.x2-t.data.x1
         ndy=t.data.y2-t.data.y1

         odx=self.data.x2-self.data.x1
         ody=self.data.y2-self.data.y1

         self.data.x1=t.data.x1
         self.data.x2=t.data.x2
         self.data.y1=t.data.y1
         self.data.y2=t.data.y2
     ##     print odx,ndx
         if odx!=ndx:
             self.data.x1+=(odx-ndx)/2.
             self.data.x2+=(odx-ndx)/2.
         else:
             self.data.y1+=(ody-ndy)/2.
             self.data.y2+=(ody-ndy)/2.

         if box_and_ticks:
              # Box1 resize
              self.box1.x1=self.data.x1
              self.box1.x2=self.data.x2
              self.box1.y1=self.data.y1
              self.box1.y2=self.data.y2
              # xLabel distance save
              dY1=self.xlabel1.y-self.xtic1.y1
              dY2=self.xlabel2.y-self.xtic2.y1
              # xLabel distance save
              dX1=self.ylabel1.x-self.ytic1.x1
              dX2=self.ylabel2.x-self.ytic2.x1
              # X tic
              dy=self.xtic1.y2-self.xtic1.y1
              self.xtic1.y1=self.data.y1
              self.xtic1.y2=self.xtic1.y1+dy
              dy=self.xtic2.y2-self.xtic2.y1
              self.xtic2.y1=self.data.y2
              self.xtic2.y2=self.xtic2.y1+dy
              # Xmin tic
              dy=self.xmintic1.y2-self.xmintic1.y1
              self.xmintic1.y1=self.data.y1
              self.xmintic1.y2=self.xtic1.y1+dy
              dy=self.xmintic2.y2-self.xmintic2.y1
              self.xmintic2.y1=self.data.y2
              self.xmintic2.y2=self.xmintic2.y1+dy
              # Y tic
              dx=self.ytic1.x2-self.ytic1.x1
              self.ytic1.x1=self.data.x1
              self.ytic1.x2=self.ytic1.x1+dx
              dx=self.ytic2.x2-self.ytic2.x1
              self.ytic2.x1=self.data.x2
              self.ytic2.x2=self.ytic2.x1+dx
              # Ymin tic
              dx=self.ymintic1.x2-self.ymintic1.x1
              self.ymintic1.x1=self.data.x1
              self.ymintic1.x2=self.ymintic1.x1+dx
              dx=self.ymintic2.x2-self.ymintic2.x1
              self.ymintic2.x1=self.data.x2
              self.ymintic2.x2=self.ymintic2.x1+dx
              # Xlabels
              self.xlabel1.y=self.xtic1.y1+dY1
              self.xlabel2.y=self.xtic2.y1+dY2
              # Ylabels
              self.ylabel1.x=self.ytic1.x1+dX1
              self.ylabel2.x=self.ytic2.x1+dX2
              self.data._ratio = -Rwished
         else:
              self.data._ratio = Rwished
              
              
         return

         


###############################################################################
#                                                                             #
#        END OF FILE                                                          #
#                                                                             #
###############################################################################

