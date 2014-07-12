# Adapted for numpy/ma/cdms2 by convertcdms.py
import vcs,cdtime,queries
import numpy
import warnings

class PPE(Exception):
     def __init__ (self, parameter,type):
          self.parameter=parameter
          self.type=type
     def __str__(self):
          return 'Projection Parameter Error: Parameter "'+self.parameter+'" is not settable for projection type:'+str(self.type)

## def checkPythonOnly(name):
##      if name in []:
##           return 1
##      else:
##           return 0


def color2vcs(col):
   if isinstance(col,unicode):
     col = str(value)
   if isinstance(col,str):
       r,g,b=vcs.colors.str2rgb(col)
       if r is None :
         raise ValueError,"Invalid color: %s" % col
            #r,g,b=[0,0,0] # black by default

            # Now calls the function that matches the closest color in the the colormap
       color=matchVcsColor(r/2.55,g/2.55,b/2.55)
   else:
        color = col
   return color

def matchVcsColor(r,g,b,colormap="default"):
   rmsmin=100000000.
   color=None
   cmap = vcs.elements["colormap"][colormap]
   for i in range(256):
       r2,g2,b2 = cmap.index[i]
       rms=numpy.sqrt((r2-r)**2+(g2-g)**2+(b2-b)**2)
       if rms<rmsmin :
           rmsmin=rms
           color=i
   return color

def checkElements(self,name,value,function):
   if not isinstance(value,list):
       raise ValueError,'Error type for %s, you must pass a list' % name
   for i in range(len(value)):
       try:
            value[i] = function(self,name,value[i])
       except Exception,err:
            raise ValueError, '%s failed while checking validity of element: %s\nError message was: %s' % (name, repr(value[i]),err)
   return value

def checkContType(self,name,value):
     checkName(self,name,value)
     checkInt(self,name,value,minvalue=0)
     return value
def checkLine(self,name,value):
     checkName(self,name,value)
     if isinstance(value,unicode):
          value = str(value)
     if not isinstance(value,(str,vcs.line.Tl)):
          raise ValueError, name+' must be an line primitive or the name of an exiting one.'
     if isinstance(value,str):
          if not value in vcs.listelements('line'):
               raise ValueError, name+' is not an existing line primitive'
          return value
     else:
       return value.name


## def checkIsoline(self,name,value):
##      checkName(self,name,value)
##      if not isinstance(value,(str,vcs.isoline.Gi)):
##           raise ValueError, name+' must be an isoline graphic method or the name of an exiting one.'
##      if isinstance(value,str):
##           if not value in self.x.listelements('isoline'):
##                raise ValueError, name+' is not an existing isoline graphic method'
##           value=self.x.getisoline(value)
##      return value

## def checkIsofill(self,name,value):
##      checkName(self,name,value)
##      if not isinstance(value,(str,vcs.isofill.Gfi)):
##           raise ValueError, name+' must be an isofill graphic method or the name of an exiting one.'
##      if isinstance(value,str):
##           if not value in self.x.listelements('isofill'):
##                raise ValueError, name+' is not an existing isofill graphic method'
##           value=self.x.getisofill(value)
##      return value

def isNumber(value,min=None,max=None):
     """ Checks if value is a Number, optionaly can check if min<value<max
     """
     try:
          value=value.tolist() # converts MA/MV/numpy
     except:
          pass
     if not isinstance(value,(int,long,float,numpy.floating)):
          return False
     if min is not None and value<min:
          return -1
     if max is not None and value>max:
          return -2
     return True

def checkNumber(self,name,value,minvalue=None,maxvalue=None):
     checkName(self,name,value)
     try:
          value=value.tolist() # converts MA/MV/numpy
     except:
          pass
     n=isNumber(value,min=minvalue,max=maxvalue)
     if n is False:
          raise ValueError, name+' must be a number'
     if n==-1:
          raise ValueError, name+' values must be at least '+str(minvalue)
     if n==-2:
          raise ValueError, name+' values must be at most '+str(maxvalue)
     return value
     
def checkInt(self,name,value,minvalue=None,maxvalue=None):
     checkName(self,name,value)
     n=checkNumber(self,name,value,minvalue=minvalue,maxvalue=maxvalue)
     if not isinstance(n,int):
          raise ValueError, name+' must be an integer'
     return n
          
def checkListOfNumbers(self,name,value,minvalue=None,maxvalue=None,minelements=None,maxelements=None,ints=False):
     checkName(self,name,value)
     if not isinstance(value,(list,tuple)):
          raise ValueError, name+' must be a list or tuple'
     n=len(value)
     if minelements is not None and n<minelements:
          raise ValueError, name+' must have at least '+str(minelements)+' elements'
     if maxelements is not None and n>maxelements:
          raise ValueError, name+' must have at most '+str(maxelements)+' elements'
     for v in value:
          if ints:
               checkInt(self,name,v,minvalue=minvalue,maxvalue=maxvalue)
          else:
               checkNumber(self,name,v,minvalue=minvalue,maxvalue=maxvalue)
     return list(value)
          

def checkFont(self,name,value):
     if (value == None):
          pass
     elif isNumber(value,min=1):
          value=int(value)
          # try to see if font exists
          nm = vcs.getfontname(value)
     elif isinstance(value,str):
          value = vcs.getfontnumber(value)
     else:
          nms = vcs.listelements("font")
          raise ValueError, 'Error for attribute %s: The font attribute values must be a valid font number or a valid font name. valid names are: %s' % (name,', '.join(nms))
     return value


def checkMarker(self,name,value):
     checkName(self,name,value)
     oks = [None, 'dot', 'plus', 'star', 'circle', 'cross', 'diamond', 'triangle_up', 'triangle_down', 'triangle_down', 'triangle_left', 'triangle_right', 'square', 'diamond_fill', 'triangle_up_fill', 'triangle_down_fill', 'triangle_left_fill', 'triangle_right_fill', 'square_fill', 'hurricane']
     for i in range(103):
         oks.append('w%.2i' % i)
     for i in range(19):
         oks.append(i)
     for i in range(100,203):
         oks.append(i)
     if ((value in oks) or (queries.ismarker(value)==1)):
          if value in (None, 0):
               value=None
          elif value in ('dot', 1):
               value='dot'
          elif value in ('plus', 2):
               value='plus'
          elif value in ('star', 3):
               value='star'
          elif value in ('circlet', 4):
               value='circle'
          elif value in ('cross', 5):
               value='cross'
          elif value in ('diamond', 6):
               value='diamond'
          elif value in ('triangle_up', 7):
               value='triangle_up'
          elif value in ('triangle_down', 8):
               value='triangle_down'
          elif value in ('triangle_left', 9):
               value='triangle_left'
          elif value in ('triangle_right', 10):
               value='triangle_right'
          elif value in ('square', 11):
               value='square'
          elif value in ('diamond_fill', 12):
               value='diamond_fill'
          elif value in ('triangle_up_fill', 13):
               value='triangle_up_fill'
          elif value in ('triangle_down_fill', 14):
               value='triangle_down_fill'
          elif value in ('triangle_left_fill', 15):
               value='triangle_left_fill'
          elif value in ('triangle_right_fill', 16):
               value='triangle_right_fill'
          elif value in ('square_fill', 17):
               value='square_fill'
          elif value in ('hurricane', 18):
               value='hurricane'
          elif isinstance(value,str) and value[0]=="w" and int(value[1:]) in range(103):
              value = value
          elif value in range(100,203):
              value = "w%.2i" % (value-100)
          elif (queries.ismarker(value)==1):
               value=value.name
     else:
         raise ValueError, 'The '+name+' value must be in : %s' % (oks)
     return value

def checkMarkersList(self,name,value):
     checkName(self,name,value)
     if isinstance(value,int):
          value=list(value)
     value=checkListTuple(self,name,value)
     hvalue = []
     for v in value:
          hvalue.append(checkMarker(self,name,v))
     return hvalue

def checkListElements(self,name,value,function):
     checkName(self,name,value)
     if not isinstance(value,(list,tuple)):
          raise ValueError, "Attribute %s must be a list" % name
     for v in value:
          try:
               v = function(self,name,v)
          except Exception,err:
               raise ValueError, "element %s of attribute %s list  failed type compliance\n error was: %s" % (repr(v),name,err)
     return value

def isListorTuple(value):
    if isinstance(value,(list,tuple)):
        return 1
    return 0

def checkName(self, name, value):
     if hasattr(self,'name'):
          if (self.name == '__removed_from_VCS__'):
               raise ValueError, 'This instance has been removed from VCS.'
          if (self.name == 'default'):
               raise ValueError, 'You cannot modify the default'
     
def checkname(self,name,value):
     checkName(self,name,value)
     if isinstance(value,str):
          if value!='__removed_from_VCS__':
               self.rename(self.name, value)
               return value
          else:
               self._name=value
               return None
     else:
          raise ValueError, 'The name attribute must be a string.'

def checkString(self,name,value):
     checkName(self,name,value)
     if isinstance(value,str):
          return value
     elif isinstance(value,unicode):
          return str(value)
     else:
          raise ValueError, 'The '+name+' attribute must be a string.'
     
def checkCallable(self,name,value):
     checkName(self,name,value)
     if callable(value):
          return value
     else:
          raise ValueError, 'The '+name+' attribute must be callable.'
     
     ## def checkFillAreaStyle(self,name,value):
##      checkName(self,name,value)
##      if not isinstance(value,str):
##           raise ValueError,'The fillarea attribute must be a string'
##      if not value.lower() in ['solid','hatch','pattern']:
##           raise ValueError, 'The fillarea attribute must be either solid, hatch, or pattern.'
##      return value
def checkFillAreaStyle(self,name,value):
     checkName(self,name,value)
     if ((value in ('solid', 'hatch', 'pattern', 'hallow', 0, 1, 2, 3)) or
                 (queries.isfillarea(value)==1)):
         if value in ('solid', 0):
              value='solid'
         elif value in ('hatch', 1):
              value='hatch'
         elif value in ('pattern', 2):
              value='pattern'
         elif value in ('hallow', 3):
              value='hallow'
         elif (queries.isfillarea(value)==1):
              value=value.name
     else:
          raise ValueError, 'The '+name+' attribute must be either solid, hatch, or pattern.'
     return value
     
def checkAxisConvert(self,name,value):
     checkName(self,name,value)
     if isinstance(value,unicode):
       value = str(value)
     if isinstance(value,str) and (value.lower() in ('linear', 'log10', 'ln','exp','area_wt')):
          return value.lower()
     else:
          raise ValueError, 'The '+name+' attribute must be either: linear, log10, ln, exp, or area_wt'
     
def checkBoxfillType(self,name,value):
     checkName(self,name,value)
     if isinstance(value,unicode):
       value = str(value)
     if isinstance(value,str) and (value.lower() in ('linear', 'log10', 'custom')):
          return value.lower()
     elif value in [0,1,2]:
       return ["linear","log10","custom"][value]
     else:
          raise ValueError, 'The '+name+' attribute must be either: linear, log10 or custom'
     
def checkIntFloat(self,name,value):
     try:
          value=value.tolist() # converts MA/MV/numpy
     except:
          pass
     if isinstance(value,(int,float,numpy.floating)):
          return float(value)
     else:
          raise ValueError, 'The '+name+' attribute must be either an integer or a float value.'
     
## def checkInt(self,name,value):
##      checkName(self,name,value)
##      if isinstance(value,int):
##           return value
##      else:
##           raise ValueError, 'The '+name+' attribute must be either an integer or a float value.'
     
def checkTrueFalse(self,name,value):
  checkName(self,name,value)
  if value in [True,False,1,0]:
    return value==True
  else:
    raise ValueError, "The '%s' attribute must be True or False" % name

def checkOnOff(self,name,value,return_string=0):
     checkName(self,name,value)
     if isinstance(value,unicode):
       value = str(value)
     if value is None:
          value = 0
     elif isinstance(value,str):
          if value.lower() in ['on','1','y','yes']:
               value = 1
          elif value.lower() in ['off','0','n','no']:
               value = 0
          else:
               raise ValueError, "The "+name+" attribute must be either 1/0, 'on'/'off', 'y'/'n' or 'yes'/'no'"
     elif isNumber(value):
          if value==0. or value==1.:
               value = int(value)
          else:
               raise ValueError, "The "+name+" attribute must be either 1/0, 'on'/'off', 'y'/'n' or 'yes'/'no'"
     else:
          raise ValueError, "The "+name+" attribute must be either 1/0, 'on'/'off', 'y'/'n' or 'yes'/'no'"
     if return_string:
          if value:
               return 'y'
          else:
               return 'n'
     elif return_string!=0:
          if value:
               return 'on'
          else:
               return 'off'
     else:
          return value
     
def checkYesNo(self,name,value):
     checkName(self,name,value)
     if isinstance(value,unicode):
       value = str(value)
     if value is None:
          value = 'n'
     elif isinstance(value,str):
          if value.lower() in ['on','1','y','yes']:
               value = 'y'
          elif value.lower() in ['off','0','n','no']:
               value = 'n'
          else:
               raise ValueError, "The "+name+" attribute must be either 1/0, 'on'/'off', 'y'/'n' or 'yes'/'no'"
     elif isNumber(value):
          if value==0.:
               value='n'
          elif value==1.:
               value = 'y'
          else:
               raise ValueError, "The "+name+" attribute must be either 1/0, 'on'/'off', 'y'/'n' or 'yes'/'no'"
     else:
          raise ValueError, "The "+name+" attribute must be either 1/0, 'on'/'off', 'y'/'n' or 'yes'/'no'"
     return value
          
def checkWrap(self,name,value):
      checkName(self,name,value)
      if isinstance(value,tuple) : value=list(value)
      if value is None: value = [0,0]
      if isinstance(value,list):
          return value
      else:
          raise ValueError, 'The '+name+' attribute must be either None or a list.'
          
def checkListTuple(self,name,value):
     checkName(self,name,value)
     if isinstance(value,list) or isinstance(value,tuple):
          return list(value)
     else:
          raise ValueError, 'The '+name+' attribute must be either a list or a tuple.'
     
def checkColor(self,name,value):
     checkName(self,name,value)
     if isinstance(value,unicode):
       value = str(value)
     if isinstance(value,str):
          value = color2vcs(value)
     if isinstance(value,int) and value in range(0,256):
          return value
     else:
          raise ValueError, 'The '+name+' attribute must be an integer value within the range 0 to 255.'
     
def checkColorList(self,name,value):
     checkName(self,name,value)
     value=checkListTuple(self,name,value)
     for v in value:checkColor(self,name+'_list_value',v)
     return value
     
def checkIsolineLevels(self,name,value):
     checkName(self,name,value)
     value=checkListTuple(self,name,value)
     hvalue=[]
     for v in value:
          if isinstance(v,(list,tuple)):
               if (len(v) == 2):
                    hvalue.append(list(v))
               else:
                    for j in range(len(v)):
                         hvalue.append([v[j],0])
          elif isNumber(v):
               hvalue.append([float(v),0])
          
     return hvalue

def checkIndex(self,name,value):
     checkName(self,name,value)
     if ((value not in range(1,21)) and
         (queries.isfillarea(value)==0)):
          raise ValueError, 'The '+name+' values must be in the range 1 to 18 or provide one or more fillarea objects.'
     elif (queries.isfillarea(value)==1):
          value=value.name
     return value
          
     
def checkIndicesList(self,name,value):
     checkName(self,name,value)
     value=checkListTuple(self,name,value)
     hvalue=[]
     for v in value:
          v=checkIndex(self,name,v)
          hvalue.append(v)
          
     return hvalue

def checkVectorType(self,name,value):
     checkName(self,name,value)
     if value in ('arrows', 0):
          hvalue = 'arrows'
     elif value in ('barbs', 1):
          hvalue = 'barbs'
     elif value in ('solidarrows', 2):
          hvalue = 'solidarrows'
     else:
          raise ValueError, 'The '+name+' can either be ("arrows", "barbs", "solidarrows") or (0, 1, 2).'
     return hvalue

def checkVectorAlignment(self,name,value):
     checkName(self,name,value)
     if value in ('head', 0):
          hvalue = 'head'
     elif value in ('center', 1):
          hvalue = 'center'
     elif value in ('tail', 2):
          hvalue = 'tail'
     else:
          raise ValueError, 'The '+name+' can either be ("head", "center", "tail") or (0, 1, 2).'
     return hvalue

def checkLineType(self,name,value):
     checkName(self,name,value)
     if value in ('solid', 0):
          hvalue = 'solid'
     elif value in ('dash', 1):
          hvalue = 'dash'
     elif value in ('dot', 2):
          hvalue = 'dot'
     elif value in ('dash-dot', 3):
          hvalue = 'dash-dot'
     elif value in ('long-dash', 4):
          hvalue = 'long-dash'
     elif (queries.isline(value)==1):
          hvalue = value.name
     else:
          raise ValueError, 'The '+name+' can either be ("solid", "dash", "dot", "dash-dot", "long-dash"), (0, 1, 2, 3, 4), or a line object.'
     return hvalue

def checkLinesList(self,name,value):
     checkName(self,name,value)
     if isinstance(value,int):
          value=list(value)
     value=checkListTuple(self,name,value)
     hvalue = []
     for v in value:
          hvalue.append(checkLineType(self,name,v))
     return hvalue

def checkTextTable(self,name,value):
     checkName(self,name,value)
     if isinstance(value,unicode):
       value = str(value)
     if isinstance(value,str):
          if not value in vcs.listelements("texttable"):
               raise ValueError,"Error : not a valid texttable"
     elif not isinstance(value,vcs.texttable.Tt):
          raise ValueError,"Error you must pass a texttable objector a texttable name"
     else:
          return value.name
     return value
def checkTextOrientation(self,name,value):
     checkName(self,name,value)
     if isinstance(value,unicode):
       value = str(value)
     if isinstance(value,str):
          if not value in vcs.listelements("textorientation"):
               raise ValueError,"Error : not a valid textorientation"
     elif not isinstance(value,vcs.textorientation.To):
          raise ValueError,"Error you must pass a textorientation objector a textorientation name"
     else:
          return value.name
     return value
def checkTextsList(self,name,value):
     checkName(self,name,value)
     if isinstance(value,int):
          value=list(value)
     value=checkListTuple(self,name,value)
     hvalue = []
     for v in value:
          if v in range(1,10):
               hvalue.append(v)
          elif ((queries.istexttable(v)==1) and
                (queries.istextorientation(v)==0)):
               name='__Tt__.'+ v.name
               hvalue.append(name)
          elif ((queries.istexttable(v)==0) and
                (queries.istextorientation(v)==1)):
               name='__To__.'+ v.name
               hvalue.append(name)
          elif (queries.istextcombined(v)==1):
               name=v.Tt_name + '__' + v.To_name
               hvalue.append(name)
     return hvalue

def checkLegend(self,name,value):
     checkName(self,name,value)
     if isinstance(value,dict):
          return value
     elif isNumber(value):
          try:
               value= value.tolist()
          except:
               pass
          return {value:repr(value)}
     elif isinstance(value,(list,tuple)):
          ret={}
          for v in value:
               ret[v]=repr(v)
          return ret
     elif value is None:
          return None
     else:
          raise ValueError, 'The '+name+' attribute should be a dictionary !'
     
## def checkListTupleDictionaryNone(self,name,value):
##      checkName(self,name,value)
##      if isinstance(value,int) or isinstance(value,float) \
##         or isinstance(value,list) or isinstance(value,tuple) or isinstance(value,dict):
##           if isinstance(value,int) or isinstance(value,float):
##                value=list((value,))
##           elif isinstance(value,list) or isinstance(value,tuple):
##                value=list(value)
##           if isinstance(value,list):
##                d={}
##                for i in range(len(value)):
##                     d[value[i]]=repr(value[i])
##           else:
##                d=value
##           return d
##      elif value is None:
##           return value
##      else:
##           raise ValueError, 'The '+name+' attribute must be a List, Tuple, Dictionary, or None'
  
def checkExt(self,name,value):
     checkName(self,name,value)
     if isinstance(value,unicode):
       value = str(value)
     if isinstance(value,str):
          if value.strip().lower() in ('y', "yes",):
              return True
          elif value.strip().lower() in ('n', "no",):
              return False
          else:
               raise ValueError, "The '%s' attribute must be either n or y." % name
     elif value is None:
       return False
     elif value in [True,False]:
       return value
     elif value==0:
       return False
     elif value==1:
       return True
     else:
          raise ValueError, 'The '+name+' attribute must be a "yes"/"no" string or one of 0,1,True,False'
     
def checkProjection(self,name,value):
     checkName(self,name,value)
     if isinstance(value,vcs.projection.Proj):
          value=value.name
     if isinstance(value,str):
        if not value in vcs.elements["projection"].keys():
           raise ValueError, 'The '+value+' projection does not exist'
        return value

def checkTicks(self,name,value):
  value = checkStringDictionary(self,name,value)
  if isinstance(value,str):
    if value.strip() in ["","*"]:
      return value.strip()
    if not value in vcs.elements["list"]:
      raise ValueError, "You are trying to use the vcs list: '%s' which does not exist" % value
  else:
    return value
def checkStringDictionary(self,name,value):
     checkName(self,name,value)
     if isinstance(value,unicode):
       return str(value)
     elif isinstance(value,str) or isinstance(value,dict):
          return value
     else:
          raise ValueError, 'The '+name+' attribute must be either a string or a dictionary'

def deg2DMS(val):
     """ converts degrees to DDDMMMSSS.ss format"""
     ival=int(val)
     out=float(ival)
     ftmp=val-out
     out=out*1000000.
     itmp=int(ftmp*60.)
     out=out+float(itmp)*1000.
     ftmp=ftmp-float(itmp)/60.
     itmp=ftmp*3600.
     out=out+float(itmp)
     ftmp=ftmp-float(itmp)/3600.
     out=out+ftmp
     return out

def DMS2deg(val):
     """converts DDDMMMSSS to degrees"""
     if val>1.e19:
          return val
     ival=int(val)
     s=str(ival).zfill(9)
     deg=float(s[:3])
     mn=float(s[3:6])
     sec=float(s[6:9])
     r=val-ival
##      print deg,mn,sec,r
     return deg+mn/60.+sec/3600.+r/3600.

def checkProjParameters(self,name,value):
     if not (isinstance(value,list) or isinstance(value,tuple)):
          raise ValueError, "Error Projection Parameters must be a list or tuple"
     if not(len(value))==15:
          raise ValueError, "Error Projection Parameters must be of length 15 (see doc)"
     for i in range(2,6):
          if abs(value[i])<10000:
               if (not(i==3 and (self.type in [9,15,20,22,30]))
                   and
                   (not(i==4 and (self.type==20 or (self.type==22 and value[12]==1) or self.type==30)))
                   ):
##                     print i,value[i]
                    value[i]=deg2DMS(value[i])
     for i in range(8,12):
          if self.type in [20,30] and abs(value[i])<10000 :
##                print i,value[i]
               value[i]=deg2DMS(value[i])
     return value
               
def checkCalendar(self,name,value):
     checkName(self,name,value)
     if not isinstance(value,(int,long)):
          raise ValueError,'cdtime calendar value must be an integer'
     if not value in [cdtime.Calendar360,cdtime.ClimCalendar,cdtime.ClimLeapCalendar,cdtime.DefaultCalendar,
                      cdtime.GregorianCalendar,cdtime.JulianCalendar,cdtime.MixedCalendar,cdtime.NoLeapCalendar,
                      cdtime.StandardCalendar]:
          raise ValueError,str(value)+' is not a valid cdtime calendar value'

     return value

def checkTimeUnits(self,name,value):
     checkName(self,name,value)
     if isinstance(value,unicode):
       value = str(value)
     if not isinstance(value,str):
          raise ValueError, 'time units must be a string'
     a=cdtime.reltime(1,'days since 1900')
     try:
          a.torel(value)
     except:
          raise ValueError, value+' is invalid time units'
     sp=value.split('since')[1]
     b=cdtime.s2c(sp)
     if b==cdtime.comptime(0,1):
          raise ValueError, sp+' is invalid date'
     return value

     
     
def checkDatawc(self,name,value):
     checkName(self,name,value)
     if isNumber(value):
          value = float(value), 0
     elif isinstance(value,str):
          t=cdtime.s2c(value)
          if t!=cdtime.comptime(0,1):
               t=t.torel(self.datawc_timeunits,self.datawc_calendar)
               value = float(t.value), 1
          else:
               raise ValueError, 'The '+name+' attribute must be either an integer or a float value or a date/time.' 
     elif type(value) in [type(cdtime.comptime(1900)),type(cdtime.reltime(0,'days since 1900'))]:
          value = value.torel(self.datawc_timeunits,self.datawc_calendar).value, 1
     else:
              raise ValueError, 'The '+name+' attribute must be either an integer or a float value or a date/time.'
     return value
     
def checkInStringsListInt(self,name,value,values):
     """ checks the line type"""
     checkName(self,name,value)
     val=[]
     str1=name + ' can either be ('
     str2=' or ('
     i=0
     for v in values:
          if not v=='': # skips the invalid/non-contiguous values
               str2=str2+str(i)+', '
               if isinstance(v,list) or isinstance(v,tuple):
                    str1=str1+"'"+v[0]+"', "
                    for v2 in v:
                         val.append(v2)
               else:
                    val.append(v)
                    str1=str1+"'"+v+"', "
               i=i+1
     err=str1[:-2]+')'+str2[:-2]+')'
     if isinstance(value,unicode):
       value = str(value)
     if isinstance(value,str):
          value=value.lower()
          if not value in val:
               raise ValueError, err
          i=0
          for v in values:
               if isinstance(v,list) or isinstance(v,tuple):
                    if value in v:
                         return i
               elif value==v:
                    return i
               i=i+1
     elif isNumber(value) and int(value)==value:
          if not int(value) in range(len(values)):
               raise ValueError, err
          else:
               return int(value)
     else:
          raise ValueError, err


def checkProjType(self,name,value):
    """set the projection type """
    checkName(self,name,value)
    if vcs.queries.isprojection(value):
      value = value.type
    if isinstance(value,unicode):
      value = str(value)
    if isinstance(value,str):
         value=value.strip().lower()
         if value in ['utm','state plane']:
              raise ValueError, "Projection Type: "+value+" not supported yet"
    if -3<=value<0:
         return value

    if self._type==-3 and (value=='polar' or value==6 or value=="polar (non gctp)"):
         return -3
    
    if self._type==-1 and (value=='robinson' or value=='robinson (non gctp)' or value==21):
         return -1
    if self._type==-2 and (value=='mollweide' or value=='mollweide (non gctp)' or value==25):
         return -2
    checkedvalue= checkInStringsListInt(self,name,value,
                          ["linear",
                           "utm",
                           "state plane",
                           ["albers equal area","albers"],
                           ["lambert","lambert conformal c","lambert conformal conic"],
                           "mercator",
                           ["polar","polar stereographic"],
                           "polyconic",
                           ["equid conic a","equid conic","equid conic b"],
                           "transverse mercator",
                           "stereographic",
                           "lambert azimuthal",
                           "azimuthal",
                           "gnomonic",
                           "orthographic",
                           ["gen. vert. near per","gen vert near per",],
                           
                           "sinusoidal",
                           "equirectangular",
                           ["miller","miller cylindrical"],
                           "van der grinten",
                           ["hotin","hotin oblique", "hotin oblique merc","hotin oblique merc a","hotin oblique merc b", "hotin oblique mercator","hotin oblique mercator a","hotin oblique mercator b",],
                           "robinson",
                           ["space oblique","space oblique merc","space oblique merc a","space oblique merc b",],
                           ["alaska","alaska conformal"],
                           ["interrupted goode","goode"],
                           "mollweide",
                           ["interrupted mollweide","interrupt mollweide",],
                           "hammer",
                           ["wagner iv","wagner 4","wagner4"],
                           ["wagner vii","wagner 7","wagner7"],
                           ["oblated","oblated equal area"],
                           ]
                          )
    
    self._type=checkedvalue
    p=self.parameters
    if self._type in [3,4]:
         self.smajor=p[0]
         self.sminor=p[1]
         self.standardparallel1=DMS2deg(p[2])
         self.standardparallel2=DMS2deg(p[3])
         self.centralmeridian=DMS2deg(p[4])
         self.originlatitude=DMS2deg(p[5])
         self.falseeasting=p[6]
         self.falsenorthing=p[7]
    elif self._type==5:
         self.smajor=p[0]
         self.sminor=p[1]
         self.centralmeridian=DMS2deg(p[4])
         self.truescale=DMS2deg(p[5])
         self.falseeasting=p[6]
         self.falsenorthing=p[7]
    elif self._type==6:
         self.smajor=p[0]
         self.sminor=p[1]
         self.centerlongitude=DMS2deg(p[4])
         self.truescale=DMS2deg(p[5])
         self.falseeasting=p[6]
         self.falsenorthing=p[7]
    elif self._type==7:
         self.smajor=p[0]
         self.sminor=p[1]
         self.centralmeridian=DMS2deg(p[4])
         self.originlatitude=DMS2deg(p[5])
         self.falseeasting=p[6]
         self.falsenorthing=p[7]
    elif self._type==8:
         self.smajor=p[0]
         self.sminor=p[1]
         self.centralmeridian=DMS2deg(p[4])
         self.originlatitude=DMS2deg(p[5])
         self.falseeasting=p[6]
         self.falsenorthing=p[7]         
         if (p[8]==0 or p[8]>9.9E19):
              self.subtype=0
              self.standardparallel=DMS2deg(p[2])
         else:
              self.subtype=1
              self.standardparallel1=DMS2deg(p[2])
              self.standardparallel2=DMS2deg(p[3])
    elif self._type==9:
         self.smajor=p[0]
         self.sminor=p[1]
         self.factor=p[2]
         self.centralmeridian=DMS2deg(p[4])
         self.originlatitude=DMS2deg(p[5])
         self.falseeasting=p[6]
         self.falsenorthing=p[7]         
    elif self._type in [10,11,12,13,14]:
         self.sphere=p[0]
         self.centerlongitude=DMS2deg(p[4])
         self.centerlatitude=DMS2deg(p[5])
         self.falseeasting=p[6]
         self.falsenorthing=p[7]         
    elif self._type==15:
         self.sphere=p[0]
         self.height=p[2]
         self.centerlongitude=DMS2deg(p[4])
         self.centerlatitude=DMS2deg(p[5])
         self.falseeasting=p[6]
         self.falsenorthing=p[7]         
    elif self._type in [16,18,21,25,27,28,29]:
         self.sphere=p[0]
         self.centralmeridian=DMS2deg(p[4])
         self.falseeasting=p[6]
         self.falsenorthing=p[7]         
    elif self._type==17:
         self.sphere=p[0]
         self.centralmeridian=DMS2deg(p[4])
         self.truescale=DMS2deg(p[5])
         self.falseeasting=p[6]
         self.falsenorthing=p[7]    
    elif self._type==19:
         self.sphere=p[0]
         self.centralmeridian=DMS2deg(p[4])
         self.originlatitude=DMS2deg(p[5])
         self.falseeasting=p[6]
         self.falsenorthing=p[7]         
    elif self._type==20:
         self.smajor=p[0]
         self.sminor=p[1]
         self.factor=p[2]
         self.originlatitude=DMS2deg(p[5])
         self.falseeasting=p[6]
         self.falsenorthing=p[7]
         if (p[12]==0 or p[12]>9.9E19):
              self.subtype=0
              self.longitude1=DMS2deg(p[8])
              self.latitude1=DMS2deg(p[9])
              self.longitude2=DMS2deg(p[10])
              self.latitude2=DMS2deg(p[11])
         else:
              self.subtype=1
              self.azimuthalangle=DMS2deg(p[3])
              self.azimuthallongitude=DMS2deg(p[4])
    elif self._type==22:
         self.smajor=p[0]
         self.sminor=p[1]
         self.falseeasting=p[6]
         self.falsenorthing=p[7]
         if (p[12]==0 or p[12]>9.9E19):
              self.subtype=0
              self.orbitinclination=DMS2deg(p[3])
              self.orbitlongitude=DMS2deg(p[4])
              self.satelliterevolutionperiod=p[8]
              self.landsatcompensationratio=p[9]
              self.pathflag=p[10]
         else:
              self.subtype=1
              self.satellite=p[2]
              self.path=p[3]
    elif self._type==23:
         self.smajor=p[0]
         self.sminor=p[1]
         self.falseeasting=p[6]
         self.falsenorthing=p[7]         
    elif self._type in [24,26]:
         self.sphere=p[0]
    elif self._type==30:
         self.sphere=p[0]
         self.shapem=p[2]
         self.shapen=p[3]
         self.centerlongitude=DMS2deg(p[4])
         self.centerlatitude=DMS2deg(p[5])
         self.falseeasting=p[6]
         self.falsenorthing=p[7]
         
    return checkedvalue
        
def getProjType(self):
    """get the projection type """
    dic={0:"linear",
         1:"utm",
         2:"state plane",
         3:"albers equal area",
         4:"lambert conformal c",
         5:"mercator",
         6:"polar stereographic",
         7:"polyconic",
         8:"equid conic",
         9:"transverse mercator",
         10:"stereographic",
         11:"lambert azimuthal",
         12:"azimuthal",
         13:"gnomonic",
         14:"orthographic",
         15:"gen. vert. near per",
         16:"sinusoidal",
         17:"equirectangular",
         18:"miller cylindrical",
         19:"van der grinten",
         20:"hotin oblique merc",
         21:"robinson",
         22:"space oblique merc",
         23:"alaska conformal",
         24:"interrupted goode",
         25:"mollweide",
         26:"interrupt mollweide",
         27:"hammer",
         28:"wagner iv",
         29:"wagner vii",
         30:"oblated equal area",
         }
    value=self._type
    if 0<=value<=30:
         return dic[value]
    elif value==-1:
         return "robinson (non gctp)"
    elif value==-2:
         return "mollweide (non gctp)"
    elif value==-3:
         return "polar (non gctp)"
                          
proj_ok_parameters={
    'smajor':[[3,4,5,6,7,8,9,20,22,23],0,[]],
    'sminor':[[3,4,5,6,7,8,9,20,22,23],1,[]],
    'sphere':[[10,11,12,13,14,15,16,17,18,19,21,24,25,26,27,28,29,30],0,[]],
    'centralmeridian':[[3,4,5,7,8,9,16,17,18,19,21,25,27,28,29],4,[]],
    'centerlongitude':[[6,10,11,12,13,14,15,30],4,[]],
    'standardparallel1':[[3,4,8],2,[]],
    'standardparallel2':[[3,4,8],3,[]],
    'originlatitude':[[3,4,7,8,9,19,20],5,[]],
    'falseeasting':[[3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,25,27,28,29,30],6,[]],
    'falsenorthing':[[3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,25,27,28,29,30],7,[]],          
    'truescale':[[5,6,17,],5,[]],
    'standardparallel':[[8,],2,[]],
    'factor':[[9,20,],2,[]],
    'centerlatitude':[[10,11,12,13,14,15,30],5,[]],
    'height':[[15,],2,[]],
    'azimuthalangle':[[20,],3,[]],
    'azimuthallongitude':[[20,],4,[]],
    'orbitinclination':[[22,],3,[]],
    'orbitlongitude':[[22,],4,[]],
    'satelliterevolutionperiod':[[22,],8,[]],
    'landsatcompensationratio':[[22,],9,[]],
    'pathflag':[[22,],10,[]],
    'path':[[22,],3,[]],
    'satellite':[[22,],2,[]],
    'shapem':[[30,],2,[]],
    'shapen':[[30,],3,[]],
    'subtype':[[8,20,22,],12,[]],
    'longitude1':[[20,],8,[]],
    'latitude1':[[20,],9,[]],
    'longitude2':[[20,],10,[]],
    'latitude2':[[20,],11,[]],
    'angle':[[30,],8,[]],
    }
def setProjParameter(self,name,value):
     """ Set an individual paramater for a projection """
     checkName(self,name,value)
     param=self.parameters
     ok = proj_ok_parameters
     for nm in ok.keys():
          vals=ok[nm]
          oktypes=vals[0]
          position=vals[1]
          nms=vals[2]
          nms.insert(0,nm)
          if name in nms:
               if not self._type in oktypes:
                    raise PPE(name,self._type)
               param[position]=value
               ## Subtype is parameter 8 not 12 for projection type 8
               if nm=='subtype' and self._type==8:
                    param[position]=1.e20
                    param[8]=value
               ## Now raise error when wrong subtype
               if nm in ['longitude1','longitude2','latitude1','latitude2',
                         'satelliterevolutionperiod','landsatcompensationratio',
                         'pathflag','orbitinclination'] and self.parameters[12]==1:
                    raise PPE(name,str(self.type)+' subtype 1')
               if nm in ['azimuthalangle','azimuthallongitude','satellite','path',] and (self.parameters[12]==0. or self.parameters[12]==1.e20):
                     raise PPE(name,str(self.type)+' subtype 0')
               if nm=='standardparallel' and self.parameters[8]==1:
                    raise PPE(name,str(self.type)+' subtype 1')
               if nm in ['standardparallel1','standardparallel2'] and (self.parameters[8]==0 or self.parameters[8]==1.e20) and self.type==8:
                    raise PPE(name,str(self.type)+' subtype 0')
               self.parameters=param
               return value
     raise PPE(name,'Unknow error...')

def _getpriority(self):
  return self._priority
def _setpriority(self,value):
  self._priority = checkInt(self,"priority",value)
priority = property(_getpriority,_setpriority)
def _getX1(self):
  return self._x1
def _setX1(self,value):
  self._x1 = checkNumber(self,"x1",value,0.,1.)
x1 = property(_getX1,_setX1,"x1 position in % of page")
def _getX2(self):
  return self._x2
def _setX2(self,value):
  self._x2 = checkNumber(self,"x2",value,0.,1.)
x2 = property(_getX2,_setX2,"x position in % of page")
def _getX(self):
  return self._x
def _setX(self,value):
  self._x = checkNumber(self,"x",value,0.,1.)
x = property(_getX,_setX,"x position in % of page")
def _getY1(self):
  return self._y1
def _setY1(self,value):
  self._y1 = checkNumber(self,"y1",value,0.,1.)
y1 = property(_getY1,_setY1,"y1 position in % of page")
def _getY2(self):
  return self._y2
def _setY2(self,value):
  self._y2 = checkNumber(self,"y2",value,0.,1.)
y2 = property(_getY2,_setY2,"y2 position in % of page")
def _getY(self):
  return self._y
def _setY(self,value):
  self._y = checkNumber(self,"y",value,0.,1.)
y = property(_getY,_setY,"y position in % of page")
def _gettexttable(self):
  return self._texttable
def _settextable(self,value):
  self._texttable = checkTextTable(self,"textable",value)
texttable = property(_gettexttable,_settextable,"texttable")
def _gettextorientation(self):
  return self._textorientation
def _settexorientation(self,value):
  self._textorientation = checkTextOrientation(self,"texorientation",value)
textorientation = property(_gettextorientation,_settexorientation,"textorientation")
def _getLine(self):
  return self._line
def _setLine(self,value):
  self._line = checkLine(self,"line",value)
line = property(_getLine,_setLine,"line properties")

def _getcolormap(self):
  return self._colormap
def _setcolormap(self,value):
  if value is None:
    self._colormap = None
    return
  if isinstance(value,vcs.colormap.Cp):
    value = value.name
  if isinstance(value,unicode):
    value = str(value)
  if not isinstance(value,str):
    raise "colormap attribute must be a colormap object or a string"
  if not value in vcs.elements["colormap"]:
    raise "The colormap '%s' does not exists" % value
  self._colormap = value
colormap = property(_getcolormap,_setcolormap)

def _getlevels(self):
     return self._levels
def _setlevels(self,value):
     if value ==  ([1.0000000200408773e+20, 1.0000000200408773e+20],):
      self._levels = value
      return

     value=list(checkListTuple(self,'levels',value))
     if len(value)==1 and isinstance(value[0],(list,tuple)) and len(value[0])>2:
       value = list(value[0])

     if (value[0]<-9.9E19):
         self._ext_1='y'
     else:
         self._ext_1='n'

     if (value[-1]>9.9E19):
        self._ext_2='y'
     else:
        self._ext_2='n'
     self._levels=tuple(value)
levels=property(_getlevels,_setlevels)
