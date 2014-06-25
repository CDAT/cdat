# Adapted for numpy/ma/cdms2 by convertcdms.py
import numpy
import cdtime
import warnings
import vcs
import boxfill
import isofill
import isoline
import taylor
import projection
import fillarea
import template
import texttable
import textorientation
import line
import unified1D
import vector
import marker
import colormap
import json

def dumpToDict(obj,skipped,must):
  dic = {}
  for a in obj.__slots__:
    if (not a in skipped) and (a[0]!="_" or a in must):
      try:
        val = getattr(obj,a)
      except:
        continue
      if not isinstance(val,(str,tuple,list,int,long,float)) and val is not None:
        val = dumpToDict(val,skipped,must)
      dic[a] = val
  return dic

def dumpToJson(obj,fileout,skipped = ["info","member"], must = []):
  dic = dumpToDict(obj,skipped,must)
  if fileout is not None:
    if isinstance(fileout,str):
      f=open(fileout,"a+")
    else:
      f = fileout
      fileout = f.name
    try:
      D = json.load(f)
    except Exception,err:
      print "Error reading json:",fileout,err
      D = {}
    f.close()
    f=open(fileout,"w")
    for N in ["g_name","s_name","p_name"]:
      if dic.has_key(N):
        nm = dic[N]
        del(dic[N])
        break
    d = D.get(nm,{})
    nm2 = dic["name"]
    del(dic["name"])
    d[nm2]=dic
    D[nm]=d
    json.dump(D,f,sort_keys=True)
    if isinstance(fileout,str):
      f.close()
  else:
    return json.dumps(dic,sort_keys=True)

def getfontname(number):
  if not number in vcs.elements["fontNumber"]:
    raise Exception,"Error font number not existing %i" % number
  return vcs.elements["fontNumber"][number]

def getfontnumber(name):
  for i in vcs.elements["fontNumber"]:
    if vcs.elements["fontNumber"][i]==name:
      return i
  raise Exception,"Font name not existing! %s" % name

def process_src_element(code):
  i = code.find("_")
  typ = code[:i]
  code=code[i+1:]
  i = code.find("(")
  nm=code[:i]
  code=code[i+1:-1]
  #try:
  if 1:
    if typ == "Gfb":
      boxfill.process_src(nm,code)
    elif typ == "Gfi":
      isofill.process_src(nm,code)
    elif typ == "Gi":
      isoline.process_src(nm,code)
    elif typ == "L":
      dic = {}
      sp = code.split(",")
      for i in range(0,len(sp),2):
        dic[eval(sp[i])]=eval(sp[i+1])
      vcs.elements["list"][nm]=dic
    elif typ == "Gtd":
      taylor.process_src(nm,code)
    elif typ=="Proj":
      projection.process_src(nm,code)
    elif typ=="Tf":
      fillarea.process_src(nm,code)
    elif typ=="P":
      template.process_src(nm,code)
    elif typ=="Tt":
      texttable.process_src(nm,code)
    elif typ=="To":
      textorientation.process_src(nm,code)
    elif typ=="Tl":
      line.process_src(nm,code)
    elif typ in ["GXy","GYx","GXY","GSp"]:
      unified1D.process_src(nm,code,typ)
    elif typ=="Gv":
      vector.process_src(nm,code)
    elif typ=="Tm":
      marker.process_src(nm,code)
    elif typ=="C":
      colormap.process_src(nm,code)

  #except Exception,err:
  #  print "Processing error for %s,%s: %s" % (nm,typ,err)

def listelements(typ):
  if not typ in vcs.elements.keys():
    raise Exception,"Error: '%s' is not a valid vcs element\nValid vcs elements are: %s" % (typ,vcs.elements.keys())
  return vcs.elements[typ].keys()

def script_run_src(script,canvas=None):
  # Now does the python Graphic methods
  f=open(script,'r')
  # browse through the file to look for taylordiagram/python graphics methods
  processing=False # found a taylor graphic method
  for l in f.xreadlines():
    if l[:6]=="color(" and canvas is not None:
      canvas.setcolormap(l.strip()[6:-1])
    elif l[:2] in ["P_","L_","C_"] or l[:3] in ["Tm_","Gv_","Gi_","Tl_","To_","Tt_","Tf_",] or l[:4] in ['GXy_','GYx_','GXY_','GSp_','Gtd_','Gfb_',"Gfm_","Gfi_"] or l[:5] in ["Proj_",] :
      #We found a graphic method
      processing = True
      opened = 0
      closed = 0
      s=""
    if processing:
      s+=l.strip()
      opened+=l.count("(")
      closed+=l.count(")")
      if closed == opened:
        # ok we read the whole Graphic method
        vcs.process_src_element(s)
        processing = False
  f.close()
  ## Ok now we need to double check the isolines
  gd = vcs.elements["isoline"]["default"]
  for g in vcs.elements["isoline"].values():
    if g.name == "default":
      continue
    for att in ["line","textcolors","text"]:
      try:
        setattr(g,att,getattr(g,att))
      except Exception,err:
        lst = []
        if att == "line":
          for e in g.line:
            if e in vcs.elements["line"]:
              lst.append(vcs.elements["line"][e])
            else:
              lst.append(e)
        elif att == "text":
          for e in g.line:
            if e in vcs.elements["textorientation"]:
              lst.append(vcs.elements["line"][e])
            elif e in vcs.elements["text"]:
              lst.append(vcs.elements["line"][e])
            else:
              lst.append(e)
        elif att == "textcolors":
          for e in g.line:
            if e in vcs.elements["texttable"]:
              lst.append(vcs.elements["line"][e])
            elif e in vcs.elements["text"]:
              lst.append(vcs.elements["line"][e])
            else:
              lst.append(e)
        try:
          setattr(g,att,lst)
        except Exception,err:
          setattr(g,att,getattr(gd,att))
                          
#############################################################################
#                                                                           #
# Import old VCS file script commands into CDAT.                            
def scriptrun(script):
  if script.split(".")[-1] == "scr":
    script_run_scr(script) 
  elif script.split(".")[-1] == "py":
    execfile(script)
  else:
    pass  
  return

def return_display_names():
  warnings.warn("PLEASE IMPLEMENT return_display_names!!!! (in utils.py)")
  return [""],[""]

def getdotdirectory():
  return ".uvcdat","UVCDAT_DIR"

class VCSUtilsError (Exception):
    def __init__ (self, args=None):
        """Create an exception"""
        self.args = args
    def __str__(self):
        """Calculate the string representation"""
        return str(self.args)
    __repr__ = __str__


def minmax(*data) :
  '''
  Function : minmax
  Description of Function
    Return the minimum and maximum of a serie of array/list/tuples (or combination of these)
    Values those absolute value are greater than 1.E20, are masked
    You can combined list/tuples/... pretty much any combination is allowed
    
  Examples of Use
  >>> s=range(7)
  >>> vcs.minmax(s)
  (0.0, 6.0)
  >>> vcs.minmax([s,s])
  (0.0, 6.0)
  >>> vcs.minmax([[s,s*2],4.,[6.,7.,s]],[5.,-7.,8,(6.,1.)])
  (-7.0, 8.0)
  '''
  mx=-1.E77
  mn=1.E77
  if len(data)==1 : data=data[0]
  global myfunction
  
  def myfunction(d,mx,mn):
    if d is None:
        return mx,mn
    from numpy.ma import maximum,minimum,masked_where,absolute,greater,count
    try:
      d=masked_where(greater(absolute(d),9.9E19),d)
      if count(d)==0 : return mx,mn
      mx=float(maximum(mx,float(maximum(d))))
      mn=float(minimum(mn,float(minimum(d))))
    except:
      for i in d:
        mx,mn=myfunction(i,mx,mn)
    return mx,mn
  mx,mn=myfunction(data,mx,mn)
  if mn==1.E77 and mx==-1.E77 :mn,mx=1.E20,1.E20
  return mn,mx

def mkevenlevels(n1,n2,nlev=10):
    '''
  Function : mkevenlevels
  
  Description of Function:
    Return a serie of evenly spaced levels going from n1 to n2
    by default 10 intervals will be produced

  Examples of use:
    >>> vcs.mkevenlevels(0,100)
    [0.0, 10.0, 20.0, 30.0, 40.0, 50.0, 60.0, 70.0, 80.0, 90.0, 100.0]
    >>> vcs.mkevenlevels(0,100,nlev=5)
    [0.0, 20.0, 40.0, 60.0, 80.0, 100.0]
    >>> vcs.mkevenlevels(100,0,nlev=5)
    [100.0, 80.0, 60.0, 40.0, 20.0, 0.0]
  '''
    import numpy.ma
    lev=numpy.ma.arange(nlev+1,dtype=numpy.float)
    factor=float(n2-n1)/nlev
    lev=factor*lev
    lev=lev+n1
    return list(lev)

def mkscale(n1,n2,nc=12,zero=1,ends=False):
  '''
  Function: mkscale

  Description of function:
  This function return a nice scale given a min and a max

  option:
  nc # Maximum number of intervals (default=12)
  zero # Not all implemented yet so set to 1 but values will be:
         -1: zero MUST NOT be a contour
          0: let the function decide # NOT IMPLEMENTED
          1: zero CAN be a contour  (default)
          2: zero MUST be a contour
  ends # True/False force the 2 values to be part of the returned labels
  Examples of Use:
  >>> vcs.mkscale(0,100)
  [0.0, 10.0, 20.0, 30.0, 40.0, 50.0, 60.0, 70.0, 80.0, 90.0, 100.0]
  >>> vcs.mkscale(0,100,nc=5)
  [0.0, 20.0, 40.0, 60.0, 80.0, 100.0]
  >>> vcs.mkscale(-10,100,nc=5)
  [-25.0, 0.0, 25.0, 50.0, 75.0, 100.0]
  >>> vcs.mkscale(-10,100,nc=5,zero=-1)
  [-20.0, 20.0, 60.0, 100.0]
  >>> vcs.mkscale(2,20)
  [2.0, 4.0, 6.0, 8.0, 10.0, 12.0, 14.0, 16.0, 18.0, 20.0]
  >>> vcs.mkscale(2,20,zero=2)
  [0.0, 2.0, 4.0, 6.0, 8.0, 10.0, 12.0, 14.0, 16.0, 18.0, 20.0]
  '''
  if n1==n2 : return [n1]
  import numpy
  nc=int(nc)   
  cscale=0  # ???? May be later
  min, max=minmax(n1,n2)
  if zero>1.:
    if min>0. : min=0.
    if max<0. : max=0.
  rg=float(max-min)  # range
  delta=rg/nc # basic delta
  # scale delta to be >10 and <= 100
  lg=-numpy.log10(delta)+2.
  il=numpy.floor(lg)
  delta=delta*(10.**il)
  max=max*(10.**il)
  min=min*(10.**il)
  if zero>-0.5:
    if   delta<=20.:
      delta=20
    elif delta<=25. :
      delta=25
    elif delta<=40. :
      delta=40
    elif delta<=50. :
      delta=50
    elif delta<=101. :
      delta=100
    first = numpy.floor(min/delta)-1.
  else:
    if   delta<=20.:
      delta=20
    elif delta<=40. :
      delta=40
    elif delta<=60. :
      delta=60
    elif delta<=101. :
      delta=100
    first=numpy.floor(min/delta)-1.5
    

  if ends:
    scvals=n1+delta*numpy.arange(2*nc)
  else:
    scvals=delta*(numpy.arange(2*nc)+first)
    
  a=0
  for j in range(len(scvals)):
    if scvals[j]>min :
      a=j-1
      break
  b=0
  for j in range(len(scvals)):
    if scvals[j]>=max :
      b=j+1
      break
  if cscale==0:
    cnt=scvals[a:b]/10.**il
  else:
    #not done yet...
    raise VCSUtilsError,'ERROR scale not implemented in this function'
  return list(cnt)

def __split2contiguous(levels):
  """ Function __split2contiguous(levels)
  takes list of split intervals and make it contiguous if possible
  """
  tmplevs=[]
  for il in range(len(levels)):
    lv=levels[il]
    if not (isinstance(lv,list) or isinstance(lv,tuple)):
      raise VCSUtilsError,"Error levels must be a set of intervals"
    if not len(lv)==2: raise VCSUtilsError,"Error intervals can only have 2 elements"
    if il!=0:
      lv2=levels[il-1]
      if lv2[1]!=lv[0]:
        raise VCSUtilsError,"Error intervals are NOT contiguous from "+str(lv2[1])+" to "+str(lv[0])
    tmplevs.append(lv[0])
  tmplevs.append(levels[-1][1])
  return tmplevs
        
def mklabels(vals,output='dict'):
  '''
  Function : mklabels

  Description of Function:
    This function gets levels and output strings for nice display of the levels values, returns a dictionary unless output="list" specified

  Examples of use:
  >>> a=vcs.mkscale(2,20,zero=2)
  >>> vcs.mklabels (a)
  {20.0: '20', 18.0: '18', 16.0: '16', 14.0: '14', 12.0: '12', 10.0: '10', 8.0: '8', 6.0: '6', 4.0: '4', 2.0: '2', 0.0: '0'}
  >>> vcs.mklabels ( [5,.005])
  {0.0050000000000000001: '0.005', 5.0: '5.000'}
  >>> vcs.mklabels ( [.00002,.00005])
  {2.0000000000000002e-05: '2E-5', 5.0000000000000002e-05: '5E-5'}
  >>> vcs.mklabels ( [.00002,.00005],output='list')
  ['2E-5', '5E-5']
  '''
  import string,numpy.ma
  if isinstance(vals[0],list) or isinstance(vals[0],tuple):
    vals=__split2contiguous(vals)
  vals=numpy.ma.asarray(vals)
  nvals=len(vals)
  ineg=0
  ext1=0
  ext2=0
  # Finds maximum number to write
  amax=float(numpy.ma.maximum(numpy.ma.absolute(vals)))
  if amax==0 :
    if string.lower(output[:3])=='dic' :
      return {0:'0'}
    else:
      return ['0']
  amin,amax=minmax(numpy.ma.masked_equal(numpy.ma.absolute(vals),0))
  ratio=amax/amin
  if int(numpy.ma.floor(numpy.ma.log10(ratio)))+1>6:
    lbls=[]
    for i in range(nvals):
      if vals[i]!=0:
        lbls.append(mklabels([vals[i]],output='list')[0])
      else:
        lbls.append('0')
    if string.lower(output[:3])=='dic':
      dic={}
      for i in range(len(vals)):
        dic[float(vals[i])]=lbls[i]
      return dic
    else:
      return lbls
  tmax=float(numpy.ma.maximum(vals))
  if tmax<0. :
    ineg=1
    vals=-vals
    amax=float(numpy.ma.maximum(vals))
  #  Number of digit on the left of decimal point
  idigleft=int(numpy.ma.floor(numpy.ma.log10(amax)))+1
  # Now determine the number of significant figures
  idig=0
  for i in range(nvals):
    aa=numpy.ma.power(10.,-idigleft)
    while abs(round(aa*vals[i])-aa*vals[i])>.000001 : aa=aa*10.
    idig=numpy.ma.maximum(idig,numpy.ma.floor(numpy.ma.log10(aa*numpy.ma.power(10.,idigleft))))
  idig=int(idig)
  # Now does the writing part
  lbls=[]
  # First if we need an E format
  if idigleft>5 or idigleft<-2:
    if idig==1:
      for i in range(nvals):
        aa=int(round(vals[i]/numpy.ma.power(10.,idigleft-1)))
        lbls.append(str(aa)+'E'+str(idigleft-1))
    else:
      for i in range(nvals):
        aa=str(vals[i]/numpy.ma.power(10.,idigleft-1))
        ii=1
        if vals[i]<0. : ii=2
        aa=string.ljust(aa,idig+ii)
        aa=string.replace(aa,' ','0')
        lbls.append(aa+'E'+str(idigleft-1))
  elif idigleft>0 and idigleft>=idig:  #F format
    for i in range(nvals):
      lbls.append(str(int(round(vals[i]))))
  else:
    for i in range(nvals):
      ii=1
      if vals[i]<0.: ii=2
      ndig=idig+ii
      rdig=idig-idigleft
      if idigleft<0 : ndig=idig-idigleft+1+ii
      aa='%'+str(ndig)+'.'+str(rdig)+'f'
      aa=aa % vals[i]
      lbls.append(aa)
  if ineg:
    vals=-vals
    for i in range(len(lbls)):
      lbls[i]='-'+lbls[i]
  if string.lower(output[:3])=='dic':
    dic={}
    for i in range(len(vals)):
      dic[float(vals[i])]=str(lbls[i])
    return dic
  else:
    return lbls


def getcolors(levs,colors=range(16,240),split=1,white=240):
 '''
 Function : getcolors(levs,colors=range(16,240),split=1,white=240)

 Description of Function:
   For isofill/boxfill purposes
   Given a list of levels this function returns the colors that would best spread a list of "user-defined" colors (default is 16 to 239 , i.e 224 colors), always using the first and last color. Optionally the color range can be split into 2 equal domain to represent <0 and >0 values.
   If the colors are split an interval goes from <0 to >0 then this is assigned the "white" color
 Usage:
   levs : levels defining the color ranges
   colors (default= range(16,240) ) : A list/tuple of the of colors you wish to use
   split # parameter to split the colors between 2 equal domain:
   one for positive values and one for  negative values
         0 : no split 
         1 : split if the levels go from <0 to >0
         2 : split even if all the values are positive or negative
   white (=240) # If split is on and an interval goes from <0 to >0 this color number will be used within this interval (240 is white in the default VCS palette color)
   
   Examples of Use:
   >>> a=[0.0, 2.0, 4.0, 6.0, 8.0, 10.0, 12.0, 14.0, 16.0, 18.0, 20.0]
   >>> vcs.getcolors (a)
   [16, 41, 66, 90, 115, 140, 165, 189, 214, 239] 
   >>> vcs.getcolors (a,colors=range(16,200))
   [16, 36, 57, 77, 97, 118, 138, 158, 179, 199]
   >>> vcs.getcolors(a,colors=[16,25,15,56,35,234,12,11,19,32,132,17])
   [16, 25, 15, 35, 234, 12, 11, 32, 132, 17]
   >>> a=[-6.0, -2.0, 2.0, 6.0, 10.0, 14.0, 18.0, 22.0, 26.0]
   >>> vcs.getcolors (a,white=241)
   [72, 241, 128, 150, 172, 195, 217, 239]
   >>> vcs.getcolors (a,white=241,split=0)
   [16, 48, 80, 112, 143, 175, 207, 239] 
'''

 import string
 
 if len(levs)==1: return [colors[0]]
 if isinstance(levs[0],list) or isinstance(levs[0],tuple):
   tmplevs=[levs[0][0]]
   for i in range(len(levs)):
     if i!=0:
       if levs[i-1][1]*levs[i][0]<0.:
         tmplevs[-1]=0.
     tmplevs.append(levs[i][1])       
   levs=tmplevs
 # Take care of the input argument split
 if isinstance(split,str):
     if split.lower()=='no' :
         split=0
     elif split.lower()=='force' :
         split=2
     else :
         split=1
 # Take care of argument white
 if isinstance(white,str): white=string.atoi(white)
 
 # Gets first and last value, and adjust if extensions
 mn=levs[0]
 mx=levs[-1]
 # If first level is < -1.E20 then use 2nd level for mn
 if levs[0]<=-9.E19 and levs[1]>0. : mn=levs[1]
 # If last level is > 1.E20 then use 2nd to last level for mx
 if levs[-1]>=9.E19 and levs[-2]<0. : mx=levs[-2]
 # Do we need to split the palette in 2 ?
 sep=0
 if mx*mn<0. and split==1 : sep=1
 if split==2 : sep=1
 # Determine the number of colors to use
 nc=len(levs)-1

 ## In case only 2 levels, i.e only one color to return
 if nc==1:
     if split>0 and levs[0]*levs[1]<=0: # Change of sign
             return white
     else:
         return colors[0]

         
 # Number of colors passed
 ncols=len(colors)
 
 k=0  #???
 col=[]
 # Counts the number of negative colors
 nn=0 # initialize
 #if (mn<=0.) and (levs[0]<=-9.E19) : nn=nn+1 # Ext is one more <0 box
 zr=0  # Counter to know if you stop by zero or it is included in a level
 for i in range(nc):
     if levs[i]<0.: nn=nn+1 # Count nb of <0 box
     if levs[i]==0.: zr=1    # Do we stop at zero ?
 np=nc-nn # Nb of >0 box is tot - neg -1 for the blank box
 
 if mx*mn<0. and zr==0 :nn=nn-1  # we have a split cell bet + and - so remove a -
 # Determine the interval (in colors) between each level
 cinc=(ncols-1.)/float(nc-1.)
 
 # Determine the interval (in colors) between each level (neg)
 cincn=0.
 if nn!=0 and nn!=1 : cincn=(ncols/2.-1.)/float(nn-1.)
 
 # Determine the interval (in colors) between each level (pos)
 cincp=0
 isplit=0
 if np!=0 and np!=1 : cincp=(ncols/2.-1.)/float(np-1.)
 if sep!=1:
     for i in xrange(nc):
         cv=i*cinc
         col.append(colors[int(round(cv))])
 else:
     colp=[]
     coln=[]
     col=[]
     for i in xrange(nc):
      if levs[i] < 0 :
          cv=i*cincn
         # if nn==1 : cv=len(colors)/4.   # if only 1 neg then use the middle of the neg colors
          if (levs[i])*(levs[i+1])<0 :
              col.append(white)
              isplit=1
          else:
              col.append(colors[int(round(cv))])
      else:
          if np==1 : cv=3*len(colors)/4. # if only 1 pos then use the middle of the pos colors
          cv=ncols/2.+(i-nn-isplit)*cincp
          col.append(colors[int(round(cv))])
 if col[0]==white and levs[0]<-9.E19: col[0]=colors[0]
 return col


def generate_time_labels(d1,d2,units,calendar=cdtime.DefaultCalendar):
    """ generate_time_labels(d1,d2,units,calendar=cdtime.DefaultCalendar)
    returns a dictionary of time labels for an interval of time, in a user defined units system
    d1 and d2 must be cdtime object, if not they will be assumed to be in "units"

    Example:
    lbls = generate_time_labels(cdtime.reltime(0,'months since 2000'),
                                cdtime.reltime(12,'months since 2000'),
                                'days since 1800',
                                )
    This generated a dictionary of nice time labels for the year 2000 in units of 'days since 1800'

    lbls = generate_time_labels(cdtime.reltime(0,'months since 2000'),
                                cdtime.comptime(2001),
                                'days since 1800',
                                )
    This generated a dictionary of nice time labels for the year 2000 in units of 'days since 1800'

    lbls = generate_time_labels(0,
                                12,
                                'months since 2000',
                                )
    This generated a dictionary of nice time labels for the year 2000 in units of 'months since 2000'

    """
    if isinstance(d1,(int,long,float)):
        d1=cdtime.reltime(d1,units)
    if isinstance(d2,(int,long,float)):
        d2=cdtime.reltime(d2,units)
    d1r=d1.torel(units,calendar)
    d2r=d2.torel(units,calendar)
    d1,d2=minmax(d1r.value,d2r.value)
    u=units.split('since')[0].strip().lower()
    dic={}
    if u in ['month','months']:
        delta=(d2-d1)*30
    elif u in ['year','years']:
        delta=(d2-d1)*365
    elif u in ['hours','hour']:
        delta=(d2-d1)/24.
    elif u in ['minute','minutes']:
        delta=(d2-d1)/24./60.
    elif u in ['second','seconds']:
        delta=(d2-d1)/24./60.
    else:
        delta=d2-d1
    if delta<.042: # less than 1 hour
        levs=mkscale(d1,d2)
        for l in levs:
            dic[l]=str(cdtime.reltime(l,units).tocomp(calendar))
    elif delta<1: # Less than a day put a label every hours
        d1=d1r.torel('hours since 2000').value
        d2=d2r.torel('hours since 2000').value
        d1,d2=minmax(d1,d2)
        levs=mkscale(d1,d2)
        for l in levs:
            t=cdtime.reltime(l,'hours since 2000').tocomp(calendar)
            if t.minute>30:
                t=t.add(1,cdtime.Hour)
            t.minute=0
            t.second=0
            tr=t.torel(units,calendar)
            dic[tr.value]=str(t).split(':')[0]
    elif delta<90: # Less than 3 month put label every day
        d1=d1r.torel('days since 2000').value
        d2=d2r.torel('days since 2000').value
        d1,d2=minmax(d1,d2)
        levs=mkscale(d1,d2)
        for l in levs:
            t=cdtime.reltime(l,'days since 2000').tocomp(calendar)
            if t.hour>12:
                t=t.add(1,cdtime.Day)
            t.hour=0
            t.minute=0
            t.second=0
            tr=t.torel(units,calendar)
            dic[tr.value]=str(t).split(' ')[0]
    elif delta<800: # ~ Less than 24 month put label every month
        d1=d1r.torel('months since 2000').value
        d2=d2r.torel('months since 2000').value
        d1,d2=minmax(d1,d2)
        levs=mkscale(d1,d2)
        for l in levs:
            t=cdtime.reltime(l,'months since 2000').tocomp(calendar)
            if t.day>15:
                t=t.add(1,cdtime.Month)
            t.day=1
            t.hour=0
            t.minute=0
            t.second=0
            tr=t.torel(units,calendar)
            dic[tr.value]='-'.join(str(t).split('-')[:2])
    else: # ok lots of years, let auto decide but always puts at Jan first
        d1=d1r.torel('years since 2000').value
        d2=d2r.torel('years since 2000').value
        d1,d2=minmax(d1,d2)
        levs=mkscale(d1,d2)
        for l in levs:
            t=cdtime.reltime(l,'years since 2000').tocomp(calendar)
            if t.month>6:
                t=t.add(1,cdtime.Year)
            t.month=1
            t.day=1
            t.hour=0
            t.minute=0
            t.second=0
            tr=t.torel(units,calendar)
            dic[tr.value]=str(t).split('-')[0]
    return dic

def prettifyAxisLabels(ticks,axis):
    for k in ticks.keys():
        if len(ticks[k])==0:
            continue
        if axis=="longitude":
            K = k % 360
            if K>180:
                if int(K)==float(K):
                  ticks[k]="%iW" % (360-K)
                else:
                  ticks[k]="%.2fW" % (360-K)
            elif K<180:
                if numpy.allclose(K,0.):
                  ticks[k]="0"
                elif int(K)==float(K):
                  ticks[k]="%iE" % (K)
                else:
                  ticks[k]="%.2fE" % (K)
            else:
              if k==-180.:
                ticks[k]="180W"
              else:
                ticks[k]="180E"
        elif axis=="latitude":
            if k<0:
                if len(ticks[k])>4:
                  ticks[k]="%.1f" % eval(ticks[k][1:])+"S"
                else:
                  ticks[k]=ticks[k][1:]+"S"
            elif k>0:
              if len(ticks[k])>4:
                ticks[k]="%.1f" % eval(ticks[k])+"N"
              else:
                ticks[k]=ticks[k]+"N"
            else:
                ticks[0]="Eq"
    return ticks

def setTicksandLabels(gm,datawc_x1,datawc_x2,datawc_y1,datawc_y2,x=None,y=None):
    """ Sets the labels and ticks for a graphics method made in python
    Usage setTicksandLabels(gm,datawc_x1,datawc_x2,datawc_y1,datawc_y2,x=None,y=None)
    datawc are world coordinates
    
    """
    if isinstance(gm,vcs.taylor.Gtd):
        return
    # Now the template stuff
    # first create the dictionary to remember which ones are changed
    dic={}
    for i in ('xticlabels1','xmtics1','xticlabels2','xmtics2','yticlabels1','ymtics1','yticlabels2','ymtics2'):
        dic[i]=False
    #xticklabels1
    if gm.xticlabels1 is None or gm.xticlabels1=='*':
        if x=="longitude" and abs(datawc_x2-datawc_x1)>30:
          ticks="lon30"
        else:
          ticks=vcs.mkscale(datawc_x1,datawc_x2)
          ticks=prettifyAxisLabels(vcs.mklabels(ticks),x)
        ## for k in ticks.keys() : # make sure you're in the range
        ##     if k<numpy.minimum(datawc_x1,datawc_x2) or k>numpy.maximum(datawc_x2,datawc_x1):
        ##         del(ticks[k])
        setattr(gm,'xticlabels1',ticks)
        dic['xticlabels1']=True
    #xmtics1
    if gm.xmtics1 is None or gm.xmtics1=='*':
        ticks=vcs.mkscale(datawc_x1,datawc_x2)
        tick2=[]
        for i in range(len(ticks)-1):
            tick2.append((ticks[i]+ticks[i+1])/2.)
        ticks=prettifyAxisLabels(vcs.mklabels(tick2),x)
        ## for k in ticks.keys() : # make sure you're in the range
        ##     if k<numpy.minimum(datawc_x1,datawc_x2) or k>numpy.maximum(datawc_x2,datawc_x1):
        ##         del(ticks[k])
        setattr(gm,'xmtics1',ticks)
        dic['xmtics1']=True
    #xticklabels2
    if  hasattr(gm,"xticlabels2") and (gm.xticlabels2 is None or gm.xticlabels2=='*'):
        ticks=vcs.mkscale(datawc_x1,datawc_x2)
        ticks=prettifyAxisLabels(vcs.mklabels(ticks),x)
        ## for k in ticks.keys():
        ##     ticks[k]=''
        ##     if k<numpy.minimum(datawc_x1,datawc_x2) or k>numpy.maximum(datawc_x2,datawc_x1):
        ##         del(ticks[k])
        setattr(gm,'xticlabels2',ticks)
        dic['xticlabels2']=True
    #xmtics2
    if hasattr(gm,"xmtics2") and (gm.xmtics2 is None or gm.xmtics2=='*'):
        ticks=vcs.mkscale(datawc_x1,datawc_x2)
        tick2=[]
        for i in range(len(ticks)-1):
            tick2.append((ticks[i]+ticks[i+1])/2.)
        ticks=prettifyAxisLabels(vcs.mklabels(tick2),x)
        ## for k in ticks.keys() : # make sure you're in the range
        ##     if k<numpy.minimum(datawc_x1,datawc_x2) or k>numpy.maximum(datawc_x2,datawc_x1):
        ##         del(ticks[k])
        setattr(gm,'xmtics2',ticks)
        dic['xmtics2']=True
    #yticklabels1
    if gm.yticlabels1 is None or gm.yticlabels1=='*':
        if y=="latitude" and abs(datawc_y2-datawc_y1)>20:
          ticks="lat20"
        else:
          ticks=vcs.mkscale(datawc_y1,datawc_y2)
          ticks=prettifyAxisLabels(vcs.mklabels(ticks),y)
        ## for k in ticks.keys() : # make sure you're in the range
        ##     if k<numpy.minimum(datawc_y1,datawc_y2) or k>numpy.maximum(datawc_y2,datawc_y1):
        ##         del(ticks[k])
        setattr(gm,'yticlabels1',ticks)
        dic['yticlabels1']=True
    #ymtics1
    if gm.ymtics1 is None or gm.ymtics1=='*':
        ticks=vcs.mkscale(datawc_y1,datawc_y2)
        tick2=[]
        for i in range(len(ticks)-1):
            tick2.append((ticks[i]+ticks[i+1])/2.)
        ticks=prettifyAxisLabels(vcs.mklabels(tick2),y)
        ## for k in ticks.keys() : # make sure you're in the range
        ##     if k<numpy.minimum(datawc_y1,datawc_y2) or k>numpy.maximum(datawc_y2,datawc_y1):
        ##         del(ticks[k])
        setattr(gm,'ymtics1',ticks)
        dic['ymtics1']=True
    #yticklabels2
    if hasattr(gm,"yticlabels2") and (gm.yticlabels2 is None or gm.yticlabels2=='*'):
        ticks=vcs.mkscale(datawc_y1,datawc_y2)
        ticks=prettifyAxisLabels(vcs.mklabels(ticks),y)
        ## for k in ticks.keys():
        ##     ticks[k]=''
        ##     if k<numpy.minimum(datawc_y1,datawc_y2) or k>numpy.maximum(datawc_y2,datawc_y1):
        ##         del(ticks[k])
        setattr(gm,'yticlabels2',ticks)
        dic['yticlabels2']=True
    #ymtics2
    if hasattr(gm,"ymtics2") and (gm.ymtics2 is None or gm.ymtics2=='*'):
        ticks=vcs.mkscale(datawc_y1,datawc_y2)
        tick2=[]
        for i in range(len(ticks)-1):
            tick2.append((ticks[i]+ticks[i+1])/2.)
        ticks=prettifyAxisLabels(vcs.mklabels(tick2),y)
        ## for k in ticks.keys() : # make sure you're in the range
        ##     if k<numpy.minimum(datawc_y1,datawc_y2) or k>numpy.maximum(datawc_y2,datawc_y1):
        ##         del(ticks[k])
        setattr(gm,'ymtics2',ticks)
        dic['ymtics2']=True
    return dic

def match_color(color,colormap=None):
    """
Function: cmatch_color                          # Returns the color in the colormap that is closet from the required color
Description of Function:
       Given a color (defined as rgb values -0/100 range- or a string name) and optionally a colormap name,
       returns the color number that is closet from the requested color
       (using rms difference between rgb values)
       if colormap is not map use the currently used colormap
Example of use:
       a=vcs.init()
       print vcs.match_color('salmon')
       print vcs.match_color('red')
       print vcs.match_color([0,0,100],'defaullt') # closest color from blue

"""
    # First gets the rgb values 
    if type(color)==type(''):
        vals=genutil.colors.str2rgb(color)
        vals[0]/=2.55
        vals[1]/=2.55
        vals[2]/=2.55
    else:
        vals=color

    # Now gets the colormap to look in
    if colormap is None: colormap=vcs.getcolormapname()
    cmap=vcs.getcolormap(colormap)

    # Now tries determines the min rms diff
    rmsmin=2.E40
    match=None
    for i in cmap.index.keys():
        col=cmap.index[i]
        rms=numpy.sqrt((vals[0]-col[0])**2+\
                         (vals[1]-col[1])**2+\
                         (vals[2]-col[2])**2 \
                         )
        if rms<rmsmin:
            rmsmin=rms
            match=i
    return match
