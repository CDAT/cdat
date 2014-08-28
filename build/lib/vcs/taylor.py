# Adapted for numpy/ma/cdms2 by convertcdms.py
import vcs,numpy.ma,sys,string,colors,numpy,cdms2,types,VCS_validation_functions
import MV2

def process_src(name,code):
  # Now break the string
  # now gets the name and prepare the graphics method
  if name!='default' : # we cannot change default
      try:
          td=Gtd()
          td._name = name
      except Exception,err:
          print "Err:",err
          td=vcs.elements["taylordiagram"][name]
      sp=code.split(';') # breaks the thing into different attributes
      imark=0
      for a in sp : # the last one is ')'
          sp2=a.split('=')
          if sp2[0].strip()=='Marker' : imark=1
          if len(sp2)==2:
              if imark:
                  setattr(td.Marker,sp2[0].strip(),eval(sp2[1]))
              else:
                  setattr(td,sp2[0].strip(),eval(sp2[1]))

def createnewvcsobj(canvas,type,basenm,src='default',src2='default'):
    ii=0
    nm=basenm+str(ii)
    if type!='text':
        nms=canvas.listelements(type)
    else:
        nms=canvas.listelements('texttable')
    while nm in nms:
        ii+=1
        nm=basenm+str(ii)
    else:
        if type!='text':
            exec('out=canvas.create'+type+'("'+nm+'","'+src+'")')
        else:
            exec('out=canvas.create'+type+'("'+nm+'","'+src+'","'+nm+'","'+src2+'")')
            
    if type=='text':
        tmptt=canvas.gettexttable(src)
        tmpto=canvas.gettextorientation(src2)
        out.halign=tmpto.halign
        out.valign=tmpto.valign
        out.height=tmpto.height
        out.angle=tmpto.angle
        out.path=tmpto.path
        out.font=tmptt.font
        out.spacing=tmptt.spacing
        out.expansion=tmptt.expansion
        out.color=tmptt.color
    elif type=='line':
        ln=canvas.getline(src)
        out.type=ln.type
        out.width=ln.width
        out.color=ln.color
    return out

class TDMarker(object):
    """class"""
##     __slots__ = [
##         '_parent',
##         'status',
##         'line',
##         'id',
##         'id_size',
##         'id_color',
##         'id_font',
##         'symbol',
##         'color',
##         'size',
##         'xoffset',
##         'yoffset',
##         'line_color',
##         'line_size',
##         'line_type',
##         'number'
##         '_parent',
##         '_status',
##         '_line',
##         '_id',
##         '-id_size',
##         '_id_color',
##         '_id_font',
##         '_symbol',
##         '-color',
##         '_size',
##         '-xoffset',
##         '_yoffset',
##         '_line_color',
##         '_line_size',
##         '_line_type',
##         '_number',
##         ]
    
    def __init__(self):
        self.name='marker'
        self._status=[]
        self._line=[]
        self._id=[]
        self._id_size=[]
        self._id_color=[]
        self._id_font=[]
        self._symbol=[]
        self._color=[]
        self._size=[]
        self._xoffset=[]
        self._yoffset=[]
        self._line_color=[]
        self._line_size=[]
        self._line_type=[]
        self._number=0

    def __len__(self):
        self.equalize()
        return self._number

    def checklineconnectingtype(self,dummy,name,v):
        if isinstance(v,str):
            if not v.lower() in [ 'line', 'tail','head','none']:
                raise ValueError,"Error you can only set line to None (0), 'tail' (1), 'line' (2) or 'head' (3) "+v+'is not valid'
            else:
                v = string.lower(v)
                if v=='none':
                    return None
                else:
                    return v
        elif isinstance(v,int):
            if v>3:
                raise ValueError,"Error you can only set line to None (0), 'tail' (1), 'line' (2) or 'head' (3)"
            else:
                if v==0 :
                    return None
                elif v==1:
                    return 'tail'
                elif v==2:
                    return 'line'
                else:
                    return 'head'
        elif not v is None:
            raise ValueError,"Error you can only set line to None (0), 'tail' (1), 'line' (2) or 'head' (3)"
        
    def _getstatus(self):
        return self._status
    def _setstatus(self,value):
        self._status=VCS_validation_functions.checkElements(self,'status',value,VCS_validation_functions.checkOnOff)
    status=property(_getstatus,_setstatus)

    def _getline(self):
        return self._line
    def _setline(self,value):
         self._line=VCS_validation_functions.checkElements(self,'line',value,self.checklineconnectingtype)
    line=property(_getline,_setline)

    def _getid(self):
        return self._id
    def _setid(self,value):
         self._id=VCS_validation_functions.checkElements(self,'id',value,VCS_validation_functions.checkString)
    id=property(_getid,_setid)

    def _getid_color(self):
        return self._id_color
    def _setid_color(self,value):
         self._id_color=VCS_validation_functions.checkElements(self,'id_color',value,VCS_validation_functions.checkColor)
    id_color=property(_getid_color,_setid_color)

    def _getid_size(self):
         return self._id_size
    def _setid_size(self,value):
         value=VCS_validation_functions.checkListOfNumbers(self,'id_size',value,minvalue=1,ints=True)
         if value is not None:
              self._id_size=value

    id_size=property(_getid_size,_setid_size)
    
    def _getid_font(self):
         return self._id_font
    def _setid_font(self,value):
         value=VCS_validation_functions.checkListOfNumbers(self,'id_font',value,minvalue=1,maxvalue=9,ints=True)
         if value is not None:
              self._id_font=value
    id_font=property(_getid_font,_setid_font)
        
    def _getsymbol(self):
        return self._symbol
    def _setsymbol(self,value):
         self._symbol=VCS_validation_functions.checkElements(self,'symbol',value,VCS_validation_functions.checkMarker)
    symbol=property(_getsymbol,_setsymbol)

    def _getcolor(self):
        return self._color
    def _setcolor(self,value):
         self._color=VCS_validation_functions.checkElements(self,'color',value,VCS_validation_functions.checkColor)
    color=property(_getcolor,_setcolor)

    def _getsize(self):
         return self._size
    def _setsize(self,value):
         value=VCS_validation_functions.checkListOfNumbers(self,'size',value,minvalue=1,ints=True)
         if value is not None:
              self._size=value
    size=property(_getsize,_setsize)

    def _getxoffset(self):
         return self._xoffset
    def _setxoffset(self,value):
         value=VCS_validation_functions.checkListOfNumbers(self,'xoffset',value)
         if value is not None:
              self._xoffset=value
    xoffset=property(_getxoffset,_setxoffset)

    def _getyoffset(self):
         return self._yoffset
    def _setyoffset(self,value):
         value=VCS_validation_functions.checkListOfNumbers(self,'yoffset',value)
         if value is not None:
              self._yoffset=value
    yoffset=property(_getyoffset,_setyoffset)

    def _getline_color(self):
        return self._line_color
    def _setline_color(self,value):
         self._line_color=VCS_validation_functions.checkElements(self,'line_color',value,VCS_validation_functions.checkColor)
    line_color=property(_getline_color,_setline_color)

    def _getline_size(self):
        return self._line_size
    def _setline_size(self,value):
         value=VCS_validation_functions.checkListOfNumbers(self,'line_size',value,minvalue=1)
         if value is not None:
              self._line_size=value
    line_size=property(_getline_size,_setline_size)

    def _getline_type(self):
        return self._line_type
    def _setline_type(self,value):
         self._line_type=VCS_validation_functions.checkElements(self,'line_type',value,VCS_validation_functions.checkLineType)
    line_type=property(_getline_type,_setline_type)            
                
    def _getnumber(self):
        return self._number
    def _setnumber(self,value):
         self._number=VCS_validation_functions.checkInt(self,'number',value)
    number=property(_getnumber,_setnumber)            
                
    def pop(self,i):
        self.status.pop(i)
        self.line.pop(i)
        self.id.pop(i)
        self.id_size.pop(i)
        self.id_color.pop(i)
        self.id_font.pop(i)
        self.symbol.pop(i)
        self.color.pop(i)
        self.size.pop(i)
        self.xoffset.pop(i)
        self.yoffset.pop(i)
        self.line_color.pop(i)
        self.line_size.pop(i)
        self.line_type.pop(i)
        
    def list(self):
        print '    status = ',self.status
        print '    line = ',self.line
        print '    id = ',self.id
        print '    id_size = ',self.id_size
        print '    id_color = ',self.id_color
        print '    id_font = ',self.id_font
        print '    symbol = ',self.symbol
        print '    color = ',self.color
        print '    size = ',self.size
        print '    xoffset = ',self.xoffset
        print '    yoffset = ',self.yoffset
        print '    line_color = ',self.line_color
        print '    line_size = ',self.line_size
        print '    line_type = ',self.line_type

    def insert(self,i):
        self.status.insert(i,self.status[i])
        self.line.insert(i,self.line[i])
        self.id.insert(i,self.id[i])
        self.id_size.insert(i,self.id_size[i])
        self.id_color.insert(i,self.id_color[i])
        self.id_font.insert(i,self.id_font[i])
        self.symbol.insert(i,self.symbol[i])
        self.color.insert(i,self.color[i])
        self.size.insert(i,self.size[i])
        self.xoffset.insert(i,self.xoffset[i])
        self.yoffset.insert(i,self.yoffset[i])
        self.line_color.insert(i,self.line_color[i])
        self.line_size.insert(i,self.line_size[i])
        self.line_type.insert(i,self.line_type[i])

    def addMarker(self,status='on',line=None,
                  id='',id_size=None,id_color=None,id_font=None,symbol=None,
                  color=None,size=None,xoffset=0.,yoffset=0.,
                  line_color=None,line_size=None,line_type=None):
        
        if self._number==0: # first marker ever !
            if symbol is None:
                symbol='dot'
            if color is None:
                color='black'
            if size is None:
                size=5
            if line_color is None:
                line_color='black'
            if line_size is None:
                line_size=1.
            if line_type is None:
                line_type='solid'
            if id_size is None:
                id_size=30
            if id_color is None:
                id_color=color
            if id_font is None:
                id_font=1
        else:
            if line is None:
                line=self.line[-1]
            if symbol is None:
                symbol=self.symbol[-1]
            if color is None:
                color=self.color[-1]
            if size is None:
                size=self.size[-1]
            if line_color is None:
                line_color=self.line_color[-1]
            if line_size is None:
                line_size=self.line_size[-1]
            if line_type is None:
                line_type=self.line_type[-1]
            if id_size is None:
                id_size=self.id_size[-1]
            if id_color is None:
                id_color=color
            if id_font is None:
                id_font=self.id_font[-1]
                
        a=self.status
        a.append(status)
        self.status=a
        a=self.line
        a.append(line)
        self.line=a
        a=self.id
        a.append(id)
        self.id=a
        a=self.symbol
        a.append(symbol)
        self.symbol=a
        a=self.color
        a.append(color)
        self.color=a
        a=self.size
        a.append(size)
        self.size=a
        a=self.xoffset
        a.append(xoffset)
        self.xoffset=a
        a=self.yoffset
        a.append(yoffset)
        self.yoffset=a
        a=self.line_color
        a.append(line_color)
        self.line_color=a
        a=self.line_size
        a.append(line_size)
        self.line_size=a
        a=self.line_type
        a.append(line_type)
        self.line_type=a
        a=self.id_size
        a.append(id_size)
        self.id_size=a
        a=self.id_color
        a.append(id_color)
        self.id_color=a
        a=self.id_font
        a.append(id_font)
        self.id_font=a
        self._number+=1
        if self._number!=0 : self.equalize()
        return

    def eq(self,attr,n):
        tmp=getattr(self,attr)
        if len(tmp)>=n:
            return
        while (len(tmp))<n:
            tmp.append(tmp[-1])
        else:
            setattr(self,attr,tmp)
            
    def equalize(self):
        """ Make sure that we have the same amount of everything
        usage self.equalize()
        Also updates self.number
        """
        if self._number>0:
            n=max(len(self.status),len(self.line),len(self.id),
                  len(self.symbol),len(self.color),len(self.size),len(self.id_size),len(self.id_color),
                  len(self.xoffset),len(self.yoffset),len(self.id_font),
                  len(self.line_color),len(self.line_size),len(self.line_type))

            for attr in ['status','line','id','symbol','color','size','id_size','id_color',
                         'xoffset','yoffset','id_font','line_color','line_size','line_type']:
                self.eq(attr,n)
            self._number=n
            
        

        
class Gtd(object):
    """ class"""    
##     __slots__ = [
##         '_template',
##         'max',
##         'quadrans',
##         'preserveaspectratio',
##         'skillValues',
##         'skillDrawLabels',
##         'skillColor',
##         'skillCoefficient',
##         'outtervalue',
##         'detail',
##         'referencevalue',
##         'Marker',
##         'arrowlength',
##         'arrowangle',
##         'name',
##         'g_name',
##         '_x',
##         'yticlabels1',
##         'xticlabels1',
##         'cticlabels1',
##         'ymtics1',
##         'xmtics1',
##         'cmtics1',
##         '_max',
##         '_quadrans',
##         '_preserveaspectratio',
##         '_skillValues',
##         '_skillDrawLabels',
##         '_skillColor',
##         '_skillCoefficient',
##         '_outtervalue',
##         '_detail',
##         '_referencevalue',
##         'Marker',
##         '-arrowlength',
##         '_arrowangle',
##         '_name',
##         '-yticlabels1',
##         '-xticlabels1',
##         '_cticlabels1',
##         '-ymtics1',
##         '-xmtics1',
##         '_cmtics1',
##         ]
            
    def __init__(self):
        self.init()


    def init(self):

        self.template=None
        self._max=None # maximum value of the standard deviaton, copied to the value of the outter circle
        self._quadrans=1
        self.preserveaspectratio='y'
        self._skillValues=[.1,.2,.3,.4,.5,.6,.7,.8,.9,.95]
        self._skillDrawLabels='y'
        self._skillColor='grey'
        self._skillCoefficient=[1.,1.,1.,]
        self.outtervalue=None # where to draw the outter circle 
        self._detail=75 # for precision of skill dots
        self._referencevalue=1. # inner circle
##         self._referencecolor='black'
        self._Marker=TDMarker()
        self._arrowlength=.05
        self._arrowangle=20.
        self._arrowbase=.75
        self._name='default'
        self.g_name='Gtd'
        self._x=None
        self._yticlabels1='*'
        self._xticlabels1='*'
        self._cticlabels1='*'
        self._ymtics1='*'
        self._xmtics1='*'
        self._cmtics1='*'
        self.displays = []

    def _getname(self):
         return self._name
    def _setname(self,value):
        value=VCS_validation_functions.checkname(self,'name',value)
        if value is not None:
            self._name=value
    name=property(_getname,_setname)

    def _getmax(self):
         return self._max
    def _setmax(self,value):
        if value is not None:
            value=VCS_validation_functions.checkNumber(self,'max',value,minvalue=0)
        self._max=value
    max=property(_getmax,_setmax)

    def _getquadrans(self):
         return self._quadrans
    def _setquadrans(self,value):
         value=VCS_validation_functions.checkInt(self,'quadrans',value,minvalue=1,maxvalue=2)
         if value is not None:
              self._quadrans=value
    quadrans=property(_getquadrans,_setquadrans)

    def _getskillvalues(self):
         return self._skillValues
    def _setskillvalues(self,value):
         value=VCS_validation_functions.checkListOfNumbers(self,'skillValues',value)
         if value is not None:
              self._skillValues=value
    skillValues=property(_getskillvalues,_setskillvalues)

    def _getskillcolor(self):
         return self._skillColor
    def _setskillcolor(self,value):
        if value is not None:
            value=VCS_validation_functions.checkColor(self,'skillColor',value)
        self._skillColor=value
    skillColor=property(_getskillcolor,_setskillcolor)

    def _getskillcoefficient(self):
         return self._skillCoefficient
    def _setskillcoefficient(self,value):
         value=VCS_validation_functions.checkListOfNumbers(self,'skillCoefficient',value)
         if value is not None:
              self._skillCoefficient=value
    skillCoefficient=property(_getskillcoefficient,_setskillcoefficient)

    def _getdetail(self):
         return self._detail
    def _setdetail(self,value):
         value=VCS_validation_functions.checkInt(self,'detail',value,minvalue=0)
         if value is not None:
              self._detail=value
    detail=property(_getdetail,_setdetail)

    def _getreferencevalue(self):
         return self._referencevalue
    def _setreferencevalue(self,value):
         value=VCS_validation_functions.checkNumber(self,'referencevalue',value,minvalue=0)
         if value is not None:
              self._referencevalue=value
    referencevalue=property(_getreferencevalue,_setreferencevalue)

    def _getskillcolor(self):
         return self._skillColor
    def _setskillcolor(self,value):
         value=VCS_validation_functions.checkColor(self,'skillColor',value)
         if value is not None:
              self._skillColor=value
    skillColor=property(_getskillcolor,_setskillcolor)

    def _getMarker(self):
        return self._Marker
    def _setMarker(self,value):
        if not isinstance(value,TDMarker):
            raise ValueError,'Error Marker must be an instance of TDMarker'
        else:
            self._Marker=value
    Marker=property(_getMarker,_setMarker)

    def _getarrowlength(self):
         return self._arrowlength
    def _setarrowlength(self,value):
         value=VCS_validation_functions.checkNumber(self,'arrowlength',value,minvalue=0)
         if value is not None:
              self._arrowlength=value
    arrowlength=property(_getarrowlength,_setarrowlength)

    def _getarrowangle(self):
         return self._arrowangle
    def _setarrowangle(self,value):
         value=VCS_validation_functions.checkNumber(self,'arrowangle',value,minvalue=-360.,maxvalue=360.)
         if value is not None:
              self._arrowangle=value
    arrowangle=property(_getarrowangle,_setarrowangle)
    
    def _getarrowbase(self):
         return self._arrowbase
    def _setarrowbase(self,value):
         value=VCS_validation_functions.checkNumber(self,'arrowbase',value,minvalue=0.,maxvalue=1.)
         if value is not None:
              self._arrowbase=value
    arrowbase=property(_getarrowbase,_setarrowbase)

    def _getskillDrawLabels(self):
         return self._skillDrawLabels

    def _setskillDrawLabels(self,value):
         value=VCS_validation_functions.checkYesNo(self,'skillDrawLabels',value)
         if value is not None:
              self._skillDrawLabels=value
    skillDrawLabels=property(_getskillDrawLabels,_setskillDrawLabels)

    def _getxticlabels1(self):
         return self._xticlabels1
    def _setxticlabels1(self,value):
         value=VCS_validation_functions.checkStringDictionary(self,'xticlabels1',value)
         self._xticlabels1=value
    xticlabels1=property(_getxticlabels1,_setxticlabels1)
    
    def _getxmtics1(self):
         return self._xmtics1
    def _setxmtics1(self,value):
         value=VCS_validation_functions.checkStringDictionary(self,'xmtics1',value)
         self._xmtics1=value
    xmtics1=property(_getxmtics1,_setxmtics1)

    def _getyticlabels1(self):
         return self._yticlabels1
    def _setyticlabels1(self,value):
         value=VCS_validation_functions.checkStringDictionary(self,'yticlabels1',value)
         self._yticlabels1=value
    yticlabels1=property(_getyticlabels1,_setyticlabels1)
    
    def _getymtics1(self):
         return self._ymtics1
    def _setymtics1(self,value):
         value=VCS_validation_functions.checkStringDictionary(self,'ymtics1',value)
         self._ymtics1=value
    ymtics1=property(_getymtics1,_setymtics1)
    
    def _getcticlabels1(self):
         return self._cticlabels1
    def _setcticlabels1(self,value):
         value=VCS_validation_functions.checkStringDictionary(self,'cticlabels1',value)
         self._cticlabels1=value
    cticlabels1=property(_getcticlabels1,_setcticlabels1)
    
    def _getcmtics1(self):
         return self._cmtics1
    def _setcmtics1(self,value):
         value=VCS_validation_functions.checkStringDictionary(self,'cmtics1',value)
         self._cmtics1=value
    cmtics1=property(_getcmtics1,_setcmtics1)
            
    def defaultSkillFunction(self,s,R):
        """ Default skillfunction"""
        alpha = self.skillCoefficient[0] # default is 1.
        beta  = self.skillCoefficient[1] # default is 1.
        R2    = self.skillCoefficient[2] # default is 1.
        sigma = s/self.referencevalue
        
        try:
            return numpy.exp(-alpha*(R2-R) - 0.5*beta*(sigma + 1.0/sigma - 2.0))
        except:
            return 1.E20

    
    def __mkblk(self,obj):
        lst=dir(obj)
        try:
            setattr(obj,'priority',0)
        except:
            pass
        for l in lst:
            o=getattr(obj,l)
            try:
                setattr(o,'priority',0)
            except:
                pass
            if type(o)==types.InstanceType and type(o)!=type(obj):
                self.__mkblk(o)
                
            

    def drawSkill(self,canvas,values,function=None):
        """
        Draw a skill score, default skill score provide in defaultSkill
        from Karl taylor, see PCMDI report series 55
        """
        if function is None:
            return
        color=self.skillColor
        a=MV2.ones((self.detail,self.detail),typecode=MV2.float)
        a=MV2.masked_equal(a,1)
        v1=[]
        v2=[]
        for i in range(self.detail):
            x=float(i)/self.detail*self.stdmax*self.quadrans-self.stdmax*(self.quadrans-1)
            v1.append(x)
            for j in range(self.detail):
                y=float(j)/self.detail*self.stdmax
                if i==0: v2.append(y)
                std=numpy.sqrt(float(x)**2.+float(y)**2.)                    
                if 0<std<self.stdmax:
                    cor=x/std
                    a[j,i]=function(std,cor)
        iso=createnewvcsobj(canvas,'isoline','td_new_')
        cols=[]
        c=VCS_validation_functions.color2vcs(color)
        for i in range(len(values)):
            cols.append(c)

        ttt=createnewvcsobj(canvas,'text','sktay')
##         else:
##             ttt=canvas.gettext('skilltaylor','skilltaylor')
        ttt.color=c
        iso.text=[ttt,]*len(cols)
        iso.level=values
        iso.line=['solid']
        iso.label=self.skillDrawLabels
        iso.linecolors=cols
        wc=self.worldcoordinate
        iso.datawc_x1=wc[0]
        iso.datawc_x2=wc[1]
        iso.datawc_y1=wc[2]
        iso.datawc_y2=wc[3]
        a=MV2.masked_greater_equal(a,1.E20)
        av=cdms2.createAxis(v1)
        av2=cdms2.createAxis(v2)
        a.setAxis(0,av2)
        a.setAxis(1,av)
        
        self.__dict__['tmpl']=createnewvcsobj(canvas,'template','tdtempl','deftaylor')
                
        self.__mkblk(self.tmpl)
        self.tmpl.data.priority=1
        self.tmpl.data.x1=self.template.data.x1
        self.tmpl.data.x2=self.template.data.x2
        self.tmpl.data.y1=self.template.data.y1
        self.tmpl.data.y2=self.template.data.y2
        self.displays.append(canvas.plot(a,iso,self.tmpl,bg=self.bg))

    def list(self):
        print ' ----------Taylordiagram (Gtd) member (attribute) listings ----------'
        print 'graphic method = Gtd'
        print 'name =',self.name
        print 'detail =',self.detail
        print 'max =',self.max # maximum value of the standard deviaton, copied to the value of the outter circle
        print 'quadrans =',self.quadrans
        print 'skillValues =',self.skillValues
        print 'skillColor =',self.skillColor
        print 'skillDrawLabels =',self.skillDrawLabels
        print 'skillCoefficient =',self.skillCoefficient
        print 'referencevalue =',self.referencevalue
##         print 'referencecolor =',self.referencecolor
        print 'arrowlength =',self.arrowlength
        print 'arrowangle =',self.arrowangle
        print 'arrowbase =',self.arrowbase
        print "xticlabels1 =", self.xticlabels1
        print "xmtics1 =", self.xmtics1
        print "yticlabels1 =", self.yticlabels1
        print "ymtics1 =", self.ymtics1
        print "cticlabels1 =", self.cticlabels1
        print "cmtics1 =", self.cmtics1
        print 'Marker'
        self.Marker.list()
        
    def script(self,file,option='a'):
        self.Marker.equalize()
        if not option in ['a','w']:
            raise Exception,'Error valid write option are w or a'
        if option=='a' : option='r+'
        try:
            f=open(file,option)
            a=f.readlines()
        except:
            f=open(file,'w')
        f.write('Gtd_'+self.name+'(\n')
        f.write( 'detail = '+repr(self.detail)+';\n')
        f.write( 'max = '+repr(self.max)+';\n') # maximum value of the standard deviaton, copied to the value of the outter circle
        f.write( 'quadrans = '+repr(self.quadrans)+';\n')
        f.write( 'skillValues = '+repr(self.skillValues)+';\n')
        f.write( 'skillColor = '+repr(self.skillColor)+';\n')
        f.write( 'skillDrawLabels = '+repr(self.skillDrawLabels)+';\n')
        f.write( 'skillCoefficient = '+repr(self.skillCoefficient)+';\n')
        f.write( 'referencevalue = '+repr(self.referencevalue)+';\n')
##         f.write( 'referencecolor = '+repr(self.referencecolor)+';\n')
        f.write( 'arrowlength = '+repr(self.arrowlength)+';\n')
        f.write( 'arrowangle = '+repr(self.arrowangle)+';\n')
        f.write( 'arrowbase = '+repr(self.arrowbase)+';\n')
        f.write( 'Marker;\n')
        f.write('    status = '+repr(self.Marker.status)+';\n')
        f.write('    line = '+repr(self.Marker.line)+';\n')
        f.write('    id = '+repr(self.Marker.id)+';\n')
        f.write('    id_size = '+repr(self.Marker.id_size)+';\n')
        f.write('    id_color = '+repr(self.Marker.id_color)+';\n')
        f.write('    id_font = '+repr(self.Marker.id_font)+';\n')
        f.write('    symbol = '+repr(self.Marker.symbol)+';\n')
        f.write('    color = '+repr(self.Marker.color)+';\n')
        f.write('    size = '+repr(self.Marker.size)+';\n')
        f.write('    xoffset = '+repr(self.Marker.xoffset)+';\n')
        f.write('    yoffset = '+repr(self.Marker.yoffset)+';\n')
        f.write('    line_color = '+repr(self.Marker.line_color)+';\n')
        f.write('    line_size = '+repr(self.Marker.line_size)+';\n')
        f.write('    line_type = '+repr(self.Marker.line_type)+';\n')
        f.write(')\n')
        f.close()
    def addMarker(self,status='on',line=None,
                  id='',id_size=None,id_color=None,id_font=None,symbol=None,
                  color=None,size=None,xoffset=0.,yoffset=0.,
                  line_color=None,line_size=None,line_type=None):
        M=self.Marker
        M.addMarker(status,line,id,id_size,id_color,id_font,symbol,color,size,xoffset,yoffset,line_color,line_size,line_type)
        return

    def draw(self,canvas,data):
        # Now makes sure we defined enough Markers
        n=data.shape[0]
        # Now draws the markers, and lineion
        # First group same markers together
        nmarker=0
        markers=[]
        self.Marker.equalize()
        while self.Marker._number<n:
            self.addMarker()
        for i in range(n):
            nmarker+=1
            d0=float(data[i][0])
            d1=float(data[i][1])
                
            s=int(self.Marker.size[i])
            t=self.Marker.symbol[i]
            c=VCS_validation_functions.color2vcs(self.Marker.color[i])
            m=createnewvcsobj(canvas,'marker','TD_')
            m.worldcoordinate=self.worldcoordinate
            m.viewport=self.viewport
            markers.append(m)
            markers[-1].size=s
            markers[-1].type=t
            markers[-1].color=VCS_validation_functions.color2vcs(c)
            markers[-1].x=[d0*d1,]
            markers[-1].y=[float(d0*numpy.ma.sin(numpy.ma.arccos(d1))),]

        # Plot markers before marker ids
        for i in range(nmarker):
            markers[i].priority=2
            self.displays.append(canvas.plot(markers[i],bg=self.bg))

        for i in range(n):
            try:
                d0=data[i][0].astype('d')
            except:
                d0=float(data[i][0])
            try:
                d1=data[i][1].astype('d')
            except:
                d1=float(data[i][1])

            if self.Marker.id[i]!='' and self.Marker.id[i]!='None':
                t=createnewvcsobj(canvas,'text','id')
                t.worldcoordinate=self.worldcoordinate
                t.viewport=self.viewport
                t.string=self.Marker.id[i]
                t.height=int(self.Marker.id_size[i])
                t.halign='center'
                t.priority=4
                t.color=VCS_validation_functions.color2vcs(self.Marker.id_color[i])
                t.font=self.Marker.id_font[i]
##                 t.x=[d1*(d0+self.stdmax*self.Marker.xoffset[i]/100.)]
                t.x=[d1*d0+self.stdmax*self.Marker.xoffset[i]/100.,]
##                 t.y=[numpy.ma.sin(numpy.ma.arccos(d1))*(d0+self.stdmax*self.Marker.yoffset[i]/100.)]
                t.y=[float(numpy.ma.sin(numpy.ma.arccos(d1))*d0+self.stdmax*self.Marker.yoffset[i]/100.),]
                self.displays.append(canvas.plot(t,bg=self.bg))

            if not self.Marker.line[i] is None:
                l=createnewvcsobj(canvas,'line','TD_li')
                l.worldcoordinate=self.worldcoordinate
                l.viewport=self.viewport
                x1=d1*d0
                y1=numpy.ma.sin(numpy.ma.arccos(d1))*d0
                l.x=[x1,x1]
                l.y=[y1,y1]
                if i != self.Marker.number-1 and self.Marker.line[i]!='head':
                    try:
                        x2=data[i+1][1].astype('d')*data[i+1][0].astype('d')
                    except:
                        x2=float(data[i+1][1]*data[i+1][0])
                    try:
                        y2=numpy.ma.sin(numpy.ma.arccos(data[i+1][1].astype('d')))*data[i+1][0].astype('d')
                    except:
                        y2=numpy.ma.sin(numpy.ma.arccos(float(data[i+1][1])))*float(data[i+1][0])
  
                    l.x=[x1,x2]
                    l.y=[y1,y2]
                l.type=self.Marker.line_type[i]
                l.width=int(self.Marker.line_size[i])
                l.color=VCS_validation_functions.color2vcs(self.Marker.line_color[i])
                if self.Marker.line[i]=='tail':
                    self.drawarrow(canvas,x1,y1,x1,y1,x2,y2,l.color)
                elif self.Marker.line[i]=='head':
                    try:
                        dd1=data[i-1][1].astype('d')
                    except:
                        dd1=float(data[i-1][1])
                    try:
                        dd0=data[i-1][0].astype('d')
                    except:
                        dd0=float(data[i-1][0])
                    
                    self.drawarrow(canvas,x1,y1,dd1*dd0,numpy.ma.sin(numpy.ma.arccos(dd1))*dd0,x1,y1,VCS_validation_functions.color2vcs(self.Marker.line_color[i-1]))
                self.displays.append(canvas.plot(l,bg=self.bg))

    def drawarrow(self,canvas,xloc,yloc,x1,y1,x2,y2,color):
        # The head
        xs=[xloc]
        ys=[yloc]
        # first determine the angle:
        alpha=numpy.arctan((y2-y1)/(x2-x1))
        alpha2=self.arrowangle/180.*numpy.pi
        Alpha=alpha+alpha2
        # First point behind
        if x2-x1>0:
            yx=yloc-self.arrowlength*numpy.sin(Alpha)*self.outtervalue
            xx=xloc-self.arrowlength*numpy.cos(Alpha)*self.outtervalue
        else:
            yx=yloc+self.arrowlength*numpy.sin(Alpha)*self.outtervalue
            xx=xloc+self.arrowlength*numpy.cos(Alpha)*self.outtervalue
        xs.append(xx)
        ys.append(yx)
        Alpha=alpha
        # This should be the middle point
        if x2-x1>0:
            ### If think this is wrong (in any case it should be in radians), it should be alpha
            ### Now why was it 1-cos ?
##             yx=yloc-self.arrowlength*numpy.sin(Alpha)*self.outtervalue*self.arrowbase*(1.-numpy.cos(self.arrowangle))
##             xx=xloc-self.arrowlength*numpy.cos(Alpha)*self.outtervalue*self.arrowbase*(1.-numpy.cos(self.arrowangle))
            yx=yloc-self.arrowlength*numpy.sin(Alpha)*self.outtervalue*self.arrowbase*numpy.cos(alpha2)
            xx=xloc-self.arrowlength*numpy.cos(Alpha)*self.outtervalue*self.arrowbase*numpy.cos(alpha2)
        else:
            yx=yloc+self.arrowlength*numpy.sin(Alpha)*self.outtervalue*self.arrowbase*numpy.cos(alpha2)
            xx=xloc+self.arrowlength*numpy.cos(Alpha)*self.outtervalue*self.arrowbase*numpy.cos(alpha2)
        xs.append(xx)
        ys.append(yx)
        # Second  point behind
        Alpha=alpha-alpha2
        if x2-x1>0:
            yx=yloc-self.arrowlength*numpy.sin(Alpha)*self.outtervalue
            xx=xloc-self.arrowlength*numpy.cos(Alpha)*self.outtervalue
        else:
            yx=yloc+self.arrowlength*numpy.sin(Alpha)*self.outtervalue
            xx=xloc+self.arrowlength*numpy.cos(Alpha)*self.outtervalue
        xs.append(xx)
        ys.append(yx)
        f=createnewvcsobj(canvas,'fillarea','tdfill')
        f.worldcoordinate=self.worldcoordinate
        f.viewport=self.viewport
        f.priority=3
        # Back to the begining
        xs.append(xs[0])
        ys.append(ys[0])
        f.x=xs
        f.y=ys
        f.color=color
        f.style='solid'
        self.displays.append(canvas.plot(f,bg=self.bg))

    def getArc(self,value,val1=0.,val2=90.,convert=True):
        """ Return coordinates to draw an arc from 0 to 90 degrees
        val1 and val2 can limit the arc (in angles)
        """
        xs=[]
        ys=[]
        val1=val1*numpy.pi/180.
        val2=val2*numpy.pi/180.
        deltaangle=(val2-val1)/self.detail
        for i in range(self.detail+1):
            a=val1+i*deltaangle
            xs.append(value*numpy.cos(a))
            ys.append(value*numpy.sin(a))
            if convert:
                xs[-1]=self.convert(xs[-1],'x')
                ys[-1]=self.convert(ys[-1],'y')
        return xs,ys
            
##         Cx.append(self.outtervalue*numpy.cos(self.quadrans/2.*numpy.pi))
##         Cy.append(self.outtervalue*numpy.sin(self.quadrans/2.*numpy.pi))
        
    def drawFrame(self,canvas,data):
        viewport=[self.template.data.x1,self.template.data.x2,
                  self.template.data.y1,self.template.data.y2]
        self.viewport=viewport
        max=self.stdmax*1.15
        max=self.stdmax
        if self.quadrans==1:
            preserve=0.
            X=max*preserve/2.
            wc=[-X*1.2,max+X,-X*1.2,max+X]
        else:
            preserve=0.05
            X=max*preserve/2.
            wc=[-max-X*5,max,0,max]
        self.worldcoordinate=wc
        if self.preserveaspectratio=='y':
            page=canvas.orientation()
            try:
                canvasinfo=canvas.canvasinfo()
                pr=float(canvasinfo['width'])/float(canvasinfo['height'])
            except:
                if page=='portrait':
                    pr=1./1.29381443299
                else:
                    pr=1.29381443299
            xr=self.viewport[1]-self.viewport[0]
            yr=self.viewport[3]-self.viewport[2]
            vr=xr/yr
            wxr=wc[1]-wc[0]
            wyr=wc[3]-wc[2]
            wr=wxr/wyr
            r=pr*vr/wr
            vp=self.viewport
            if r>1: # xs are bigger
                self.worldcoordinate=[wc[0],wc[0]+(wc[1]-wc[0])*r,
                                        wc[2],wc[3]]
            else:   # ys are bigger
                self.worldcoordinate=[wc[0],wc[1],
                                        wc[2],wc[2]+(wc[3]-wc[2])/r]
        O=createnewvcsobj(canvas,'line','tdiag_',self.template.line2.line)
        frame=createnewvcsobj(canvas,'line','tdiag_f',self.template.line1.line)
        xtic1=createnewvcsobj(canvas,'line','tdgxt1',self.template.xtic1.line)
        xtic2=createnewvcsobj(canvas,'line','tdgxt2',self.template.xtic2.line)
        ytic1=createnewvcsobj(canvas,'line','tdgxt2',self.template.ytic1.line)
        ytic2=createnewvcsobj(canvas,'line','tdgyt2',self.template.ytic2.line)
        xmtic1=createnewvcsobj(canvas,'line','tdxt1',self.template.xmintic1.line)
        ymtic1=createnewvcsobj(canvas,'line','tdxt2',self.template.ymintic1.line)
        ymtic2=createnewvcsobj(canvas,'line','tdyt2',self.template.ymintic2.line)
        xmtic2=createnewvcsobj(canvas,'line','tdxt2',self.template.xmintic2.line)
        stdticks=createnewvcsobj(canvas,'text','tic',self.template.xlabel1.texttable,self.template.xlabel1.textorientation)
        stdticks2=createnewvcsobj(canvas,'text','tc2',self.template.ylabel1.texttable,self.template.ylabel1.textorientation)


        O.priority=self.template.line1.priority
        frame.priority=self.template.line2.priority
        xtic1.priority=self.template.xtic1.priority
        xtic2.priority=self.template.xtic2.priority
        ytic1.priority=self.template.ytic1.priority
        ytic2.priority=self.template.ytic2.priority
        xmtic1.priority=self.template.xmintic1.priority
        ymtic1.priority=self.template.ymintic1.priority
        ymtic2.priority=self.template.ymintic2.priority
        xmtic2.priority=self.template.xmintic2.priority
        stdticks.priority=self.template.xlabel1.priority
        stdticks2.priority=self.template.ylabel1.priority


##         if type(self.referencecolor) == types.StringType:
##             O.color=self.color2vcs(canvas,self.referencecolor)
##         else:
##             O.color=self.referencecolor
##         else:
##             O=canvas.getline('tdiag_O')
##             frame=canvas.getline('tdiag_frame')
##             stdticks=canvas.gettext('stdtic','stdtic')
##             stdticks2=canvas.gettext('stdtic2','stdtic2')

##         O.list()
        O.worldcoordinate=self.worldcoordinate
        frame.worldcoordinate=self.worldcoordinate
        O.viewport=self.viewport
        frame.viewport=self.viewport
            
        if self.quadrans==1:
            fx=[[0.,self.outtervalue]]
            fy=[[0.,0.]]
        else:
            fx=[[-self.outtervalue,self.outtervalue]]
            fy=[[0.,0.]]
        fx.append([0.,0.])
        fy.append([0.,self.outtervalue])
        xtic1x=[]
        xtic1y=[]
        xtic2x=[]
        xtic2y=[]
        ytic1x=[]
        ytic1y=[]
        ytic2x=[]
        ytic2y=[]
        xmtic1x=[]
        xmtic1y=[]
        xmtic2x=[]
        xmtic2y=[]
        ymtic1x=[]
        ymtic1y=[]
        ymtic2x=[]
        ymtic2y=[]
        Cx=[]
        Cy=[]
        Ox=[]
        Oy=[]
        Ox,Oy=self.getArc(self.referencevalue,val2=90.*self.quadrans,convert=False)
        Cx,Cy=self.getArc(self.outtervalue,val2=90.*self.quadrans,convert=False)
        fx.append(Cx)
        fy.append(Cy)
        O.x=Ox
        O.y=Oy
##         if self.outtervalue<1:
##             ticklength=.03*self.outtervalue
##         elif self.outtervalue>10 :
##             ticklength=.03*numpy.log(self.outtervalue)
##         else:
        ticstr=[]
        ticxs=[]
        ticys=[]
        ticstr2=[]
        ticxs2=[]
        ticys2=[]
        extension=.03
        subextension=extension/2.
        ticklength=extension*self.outtervalue
        sticklength=ticklength/2.
        ## Ok figures out if we have defined the labels/tics
        if isinstance(self.yticlabels1,str): # Ok we want automatic
            vals=vcs.mkscale(0,self.outtervalue,20)
            tmp=vals[::2]
            if tmp[-1]!=vals[-1]: tmp.append(vals[-1])
            levs=vcs.mklabels(tmp)
        else:
            levs=self.yticlabels1
        for v in levs.keys():
            if wc[0]<v<min(wc[1],wc[3]):       
                ticxs2.append(self.template.ylabel1.x)
                ticys2.append(self.convert(v,'y'))
                ticstr2.append(levs[v]+' ')
                ytic1y.append([self.convert(v,axis='y'),self.convert(v,axis='y')])
                ytic1x.append([self.template.ytic1.x1,self.template.ytic1.x2])
        ytic1.x=ytic1x
        ytic1.y=ytic1y
        if ytic1.x!=[]:
            self.displays.append(canvas.plot(ytic1,bg=self.bg))
            
##                 fx.append([0.,ticklength])
##                 fy.append([v,v])
        if isinstance(self.ymtics1,str): # Ok we want automatic
            vals=vcs.mkscale(0.,self.outtervalue,20)[1:-1:2]
            levs=vcs.mklabels(vals)
        else:
            levs=self.ymtics1
        for v in levs.keys():
            if wc[0]<v<min(wc[1],wc[3]):       
                ymtic1x.append([self.template.ymintic1.x1,self.template.ymintic1.x2])
                ymtic1y.append([self.convert(v,axis='y'),self.convert(v,axis='y')])
                pass
        ymtic1.x=ymtic1x
        ymtic1.y=ymtic1y
        if ymtic1.x!=[]:
            self.displays.append(canvas.plot(ymtic1,bg=self.bg))

        if isinstance(self.xmtics1,str): # Ok we want automatic
            vals=vcs.mkscale(0.,self.outtervalue,20)[1:-1:2]
            if self.quadrans==2:
                tmp=vals
                vals=[]
                for v in tmp:
                    vals.append(v)
                    vals.insert(0,-v)
            levs=vcs.mklabels(vals)
            # ok need to remove potential negative values, std is always >0
            for k in levs.keys():
                if k<0:
                    levs[k]=levs[k][1:]
                if k<-wc[1]:
                    del(levs[k])
        else:
            levs=self.xmtics1

        delta=abs(self.template.data.y2-self.template.data.y1)
        val1=(self.template.xmintic2.y1-self.template.data.y1)/delta*90.
        val2=(self.template.xmintic2.y2-self.template.data.y1)/delta*90.
        for v in levs.keys():
            if wc[0]<v<min(wc[1],wc[3]):       
                  tx,ty=self.getArc(v,val1=val1,val2=val2)
                  xmtic1x.append([self.convert(v,axis='x'),self.convert(v,axis='x')])
                  xmtic1y.append([self.template.xmintic1.y1,self.template.xmintic1.y2])
                  xmtic2x.append(tx)
                  xmtic2y.append(ty)
        xmtic2.x=xmtic2x
        xmtic2.y=xmtic2y
        xmtic1.x=xmtic1x
        xmtic1.y=xmtic1y
        if xmtic1.x!=[]:
            self.displays.append(canvas.plot(xmtic1,bg=self.bg))
        if xmtic2.x!=[]:
            self.displays.append(canvas.plot(xmtic2,bg=self.bg))
            
        if isinstance(self.xticlabels1,str): # Ok we want automatic
            vals=vcs.mkscale(0,self.outtervalue,20)
            tmp=vals[::2]
            if tmp[-1]!=vals[-1]: tmp.append(vals[-1])
            if self.quadrans==2:
                vals=[]
                for v in tmp:
                    vals.append(v)
                    vals.insert(0,-v)
                tmp=vals
            levs=vcs.mklabels(tmp)
            # ok need to remove potential negative values, std is always >0
            for k in levs.keys():
                if k<0:
                    levs[k]=levs[k][1:]
                if k<-wc[1]:
                    del(levs[k])
        else:
            levs=self.xticlabels1

        val1=(self.template.xtic2.y1-self.template.data.y1)/delta*90.
        val2=(self.template.xtic2.y2-self.template.data.y1)/delta*90      

        for v in levs.keys():
            if wc[0]<v<min(wc[3],wc[1]):
                V=self.convert(v,'x')                
                if v >= 0:
                    # v is +ve value. So standard deviaton arc should be in 
                    # 1st quadrant.
                    tx,ty=self.getArc(v,val1=val1,val2=val2)
                else:           
                    # v is -ve value. So standard deviaton arc should be in 
                    # 2nd quadrant. 
                    # Here we change the -ve v value into +ve v value, by abs 
                    # function. And add 90 degree to both val1 and val2.
                    # i.e. standard deviaton arcs should start from 90 degree
                    # to 180 degree.
                    tx,ty=self.getArc(abs(v), val1=val1+90, val2=val2+90)
                xtic2x.append(tx)
                xtic2y.append(ty)
                ticxs.append(V)
                ticys.append(self.template.xlabel1.y)
                ticstr.append(levs[v])
                xtic1x.append([V,V])
                xtic1y.append([self.template.xtic1.y1,self.template.xtic1.y2])
        xtic1.x=xtic1x
        xtic1.y=xtic1y
        xtic2.x=xtic2x
        xtic2.y=xtic2y
        if xtic1.x!=[]:
            self.displays.append(canvas.plot(xtic1,bg=self.bg))
        if xtic2.x!=[]:
            self.displays.append(canvas.plot(xtic2,bg=self.bg))

        stdticks.string=ticstr
        stdticks.x=ticxs
        stdticks.y=ticys
        
        stdticks2.string=ticstr2
        stdticks2.x=ticxs2
        stdticks2.y=ticys2

        if isinstance(self.cticlabels1,str): # Ok we want default
            levs={}
            for i in range(10):
                if i!=0:
                    levs[-i/10.]='%.2f' % (-i/10.)
                    levs[i/10.]='%.2f' % (i/10.)
                else:
                    levs[0.]='0'
            levs[.95]='0.95'
            levs[.99]='0.99'
            levs[-.95]='-0.95'
            levs[-.99]='-0.99'
        else:
            levs=self.cticlabels1
            
        dx2=1.+self.template.ytic2.x2-self.template.data.x2 #
        dx1=1.-(self.template.ytic2.x2-self.template.ytic2.x1)/(self.template.data.x2-self.template.data.x1)
        for v in levs.keys():
            if 0.<=v<=1. or (-1.<=v and self.quadrans==2):
                x1=v
                y1=numpy.sin(numpy.arccos(x1))
                tic=createnewvcsobj(canvas,'text','cor_',self.template.ylabel2.texttable,self.template.ylabel2.textorientation)
                tic.priority=self.template.ylabel2.priority
                dx=1.+self.template.ylabel2.x-self.template.data.x2 # How far from data.x2 ?
                tic.x=[self.convert(x1*min(wc[1],wc[3])*dx,axis='x')]
                tic.y=[self.convert(y1*min(wc[1],wc[3])*dx,axis='y')]
                tic.string=levs[v]
                if x1>0:
                    tic.angle=360-int(numpy.arccos(x1)/numpy.pi*180.)
                elif x1==0 and self.quadrans==2:
                    tic.angle=0
                else:
                    tic.angle=180-int(numpy.arccos(x1)/numpy.pi*180.)
                if v>0:
                    tic.halign='left'
                elif v==0 and self.quadrans==2:
                    tic.halign='center'
                    tic.valign='bottom'
                else:
                    tic.halign='right'
                self.displays.append(canvas.plot(tic,bg=self.bg))
                ytic2x.append([self.convert(x1*min(wc[1],wc[3])*dx1,axis='x'),self.convert(x1*min(wc[1],wc[3])*dx2,axis='x')])
                ytic2y.append([self.convert(y1*min(wc[1],wc[3])*dx1,axis='y'),self.convert(y1*min(wc[1],wc[3])*dx2,axis='y')])
        ytic2.x=ytic2x
        ytic2.y=ytic2y
        if ytic2.x!=[]:
            self.displays.append(canvas.plot(ytic2,bg=self.bg))
        if isinstance(self.cmtics1,str): # Ok we want automatic
            levs={}
            for i in range(9):
                levs[i/10.+.05]=''
            levs[.91]=''
            levs[.92]=''
            levs[.93]=''
            levs[.94]=''
            levs[.96]=''
            levs[.97]=''
            levs[.98]=''
            ks=levs.keys()
            for l in ks:
                levs[-l]=''
        else:
            levs=self.cmtics1
            
        dx2=1.+self.template.ymintic2.x2-self.template.data.x2 #
        dx1=1.-(self.template.ymintic2.x2-self.template.ymintic2.x1)/(self.template.data.x2-self.template.data.x1)
        for v in levs.keys():
            if 0.<=v<=1. or (-1.<=v and self.quadrans==2):
                x1=v
                y1=numpy.sin(numpy.arccos(x1))
                ymtic2x.append([self.convert(x1*min(wc[1],wc[3])*dx1,axis='x'),self.convert(x1*min(wc[1],wc[3])*dx2,axis='x')])
                ymtic2y.append([self.convert(y1*min(wc[1],wc[3])*dx1,axis='y'),self.convert(y1*min(wc[1],wc[3])*dx2,axis='y')])
        ymtic2.x=ymtic2x
        ymtic2.y=ymtic2y
        if ymtic2.x!=[]:
            self.displays.append(canvas.plot(ymtic2,bg=self.bg))

        x1=numpy.cos(45/180.*numpy.pi)
        y1=numpy.sin(numpy.arccos(x1))
        tic=createnewvcsobj(canvas,'text','cor__',self.template.yname.texttable,self.template.yname.textorientation)
        tic.priority=self.template.yname.priority
        ddx=1.+self.template.yname.x-self.template.data.x1 # How far are we from yaxis ?
        tic.x=[self.convert(x1*self.outtervalue*ddx,axis='x')]
        tic.y=[self.convert(y1*self.outtervalue*ddx,axis='y')]
        tic.string=['Correlation']
##         tic.height=int(40.*self.stdmax)+1
        tic.angle=45
        tic.halign='center'
        tic.valign='bottom'
        self.displays.append(canvas.plot(tic,bg=self.bg))
##         canvas.removeobject(tic)
        frame.x=fx
        frame.y=fy
        stdaxis=createnewvcsobj(canvas,'text','stax',self.template.xname.texttable,self.template.xname.textorientation)
##         stdaxis.worldcoordinate=self.worldcoordinate
##         stdaxis.viewport=self.viewport
##         stdaxis.list()

        ddy=1.-abs(self.template.xname.y-self.template.data.y1)
        stdaxis.x = [self.template.data.x1 - (self.template.data.y1-self.template.xname.y)]
        stdaxis.y=[abs(self.template.data.y2+self.template.data.y1)/2.]
        stdstring='Standard Deviation'
        if hasattr(data,'units'):
            if string.strip(data.units)!='':
                stdstring=stdstring+' ( '+str(data.units)+' )'
        stdaxis.string=[stdstring]
        stdaxis.angle=stdaxis.angle+270
        stdaxis.priority=self.template.xname.priority
##         stdaxis.halign='center'
##         stdaxis.valign='top'
##         stdaxis.height=int(40.*self.stdmax)+1
        
        xstdaxis=createnewvcsobj(canvas,'text','xstax',self.template.xname.texttable,self.template.xname.textorientation)
##         xstdaxis.worldcoordinate=self.worldcoordinate
##         xstdaxis.viewport=self.viewport
        xstdaxis.y=[self.template.xname.y]
        xstdaxis.x=[abs(self.template.data.x2+self.template.data.x1)/2.]
        xstdaxis.string=[stdstring]
        xstdaxis.priority=self.template.xname.priority
        
        self.displays.append(canvas.plot(stdaxis,bg=self.bg))
        self.displays.append(canvas.plot(xstdaxis,bg=self.bg))
        self.displays.append(canvas.plot(frame,bg=self.bg))
        if self.referencevalue/self.outtervalue>.05 : self.displays.append(canvas.plot(O,bg=self.bg))
        self.displays.append(canvas.plot(stdticks,bg=self.bg))
        self.displays.append(canvas.plot(stdticks2,bg=self.bg))

    def convert(self,value,axis):
        if axis=='y':
            vp1,vp2=self.viewport[2:]
            V1,V2=self.worldcoordinate[2:]
        else:
            vp1,vp2=self.viewport[:2]
            V1,V2=self.worldcoordinate[:2]
        tmp=(value-V1)/(V2-V1)
        out=vp1+(vp2-vp1)*(value-V1)/(V2-V1)
        return out
             
           
    
    def plot(self,data,template='deftaylor',skill=None,bg=0,canvas=None):
        """plots"""
        self.__dict__['bg']=bg
        self.displays=[]
        if canvas is None:
            canvas=vcs.init()
            
        savedmode=canvas.mode
        canvas.mode=0
        if isinstance(template,str):
            self.__dict__['template']=canvas.gettemplate(template)
        elif vcs.istemplate(template):
            self.__dict__['template']=template
        else:
            raise Exception,'Error you passed an invalid template'
        
        canvas.pause_time=0
        #canvas.open()
##         canvas.clear()
        savedstdmax=getattr(self,'stdmax',None)
        if self.max is None:
            self.__dict__['stdmax']=1.2*numpy.ma.maximum(data[...,0])
        else:            
            self.__dict__['stdmax']=self.max
        resetoutter=0
        if self.outtervalue is None:
            if self.referencevalue<self.stdmax:
                self.__dict__['outtervalue']=self.stdmax
            else:
                self.__dict__['outtervalue']=self.referencevalue*1.2
            resetoutter=1
        self.drawFrame(canvas,data=data)
        self.drawSkill(canvas,values=self.skillValues,function=skill)
        self.draw(canvas,data)
        ## Ok now draws the little comment/source, etc
        self.displays+=self.template.plot(data,'taylordiagram',bg=bg)
        if resetoutter:
            self.__dict__['outtervalue']=None
        if savedstdmax is not None:
            self.__dict__['stdmax']=savedstdmax
        else:
            delattr(self,'stdmax')
        canvas.mode=savedmode
        return

        




