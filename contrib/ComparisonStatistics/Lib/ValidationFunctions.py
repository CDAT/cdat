# Adapted for numpy/ma/cdms2 by convertcdms.py
import string
import types
import numpy
import numpy.ma
import cdms2
import os
import Compall

def checkStringOrNone(self,name,value):
    if not type(value) in [types.StringType,types.NoneType]:
        raise ValueError,name+' must be a string or None'
    self._basic_set(name,value)

def isNumber(value,min=None,max=None):
     """ Checks if value is a Number, optionaly can check if min<value<max
     """
     if not isinstance(value,(int,long,float,numpy.floating)):
          return False
     if min is not None and value<min:
          return -1
     if max is not None and value>max:
          return -2
     return True

def checkNumber(self,name,value,minvalue=None,maxvalue=None):
     checkName(self,name,value)
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
          
def checkListNumbers(self,name,value,minvalue=None,maxvalue=None,minelements=None,maxelements=None,ints=False):
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
          
    
def setSlab(self,name,value):
    if isinstance (value,numpy.ndarray ) or numpy.ma.isMA(value):
        self._basic_set('data',value)
    elif type(value) == types.StringType:
        if os.path.exists(value):
            self._basic_set('file',value)
        else:
            raise ValueError, value+" : file does not exist...."
    elif type(value) == types.NoneType:
        self._basic_set(name,value)
    else:
        raise ValueError, name+" must be a slab, a file name or None"

def checkAxisType(self,name,value):
    checkInStringsListInt(self,name,value,[
                          ['uniform','rect','linear'],
                          'gaussian',
                          ['equal','equal area','equalarea','equal-area'],]
                          )
    
def checkAction(self,name,value):
    checkInStringsListInt(self,name,value,['select','mask'])
    
def  setDataSetGrid(self,name,value):
    if isinstance(value,Compall.Grid):
        self._basic_set(name,value)
    else:
        self.grid.grid=value
    
def setGrid(self,name,value):
    if isinstance(value,cdms2.grid.AbstractGrid):
        self._basic_set(name,value)
    elif value is None:
        self.var=None
        self.file=None
        self.longitude.__init__()
        self.latitude.__init__()
        self.mask=None
        self._basic_set('grid',None)
    else:
        raise ValueError, name+" must be a grid object or None"
        
def setSlabOnly(self,name,value):
    if isinstance (value,numpy.ndarray ) or numpy.ma.isMA(value):
        self._basic_set(name,value)
    elif type(value) == types.NoneType:
        self._basic_set(name,value)
    else:
        raise ValueError, name+" must be a slab or None"
    
def getSlab(self,name):
    value=self._basic_get(name)
    try:
        times=self._basic_get('times')
        times_type=self._basic_get('times_type')
    except:
        times=None
        times_type=''
    if times_type == 'indices':
        times=slice(times[0],times[1])
        
    if isinstance (value,numpy.ndarray ) or numpy.ma.isMA(value):
        return value
    elif type(value)==types.StringType:
        f=cdms2.open(value)
        if not times is None:
            v=f(self.var,time=times)
        else:
            v=f(self.var)
        f.close()
        return v
    else:
        return None

def checkNumberOrNone(self,name,value):
    if not type(value) in [types.IntType,types.FloatType,types.LongType,types.NoneType]:
        raise ValueError,name+' must be an integer, a float, or None'
    self._basic_set(name,value)

def checkIntOrNone(self,name,value):
    if not type(value) in [types.IntType,types.LongType,types.NoneType]:
        raise ValueError,name+' must be an integer or None'
    self._basic_set(name,value)



    

    
    
def checkInStringsList(self,name,value,values):
    """ check if value is in values"""
    if not type(value)==types.StringType:
        raise ValueError, name + 'must be a string'
    elif not string.lower(value) in values:
        err=name+" must be in ('"+values[0]
        for v in values[1:-1]:
            err=err+", '"+v+"'"
        err=err+" or '"+values[-1]+"')"
        raise ValueError, err
    self._basic_set(name,string.lower(value))

def checkInStringsListInt(self,name,value,values):
    """ checks the line type"""
    val=[]
    str1=name + ' can either be ('
    str2=' or ('
    i=0
    for v in values:
        if not v=='': # skips the invalid/non-contiguous values
            str2=str2+str(i)+', '
            if type(v) in [types.ListType,types.TupleType]:
                str1=str1+"'"+v[0]+"', "
                for v2 in v:
                    val.append(v2)
            else:
                val.append(v)
                str1=str1+"'"+v+"', "
            i=i+1
    err=str1[:-2]+')'+str2[:-2]+')'
    if type(value)==types.StringType:
        value=string.lower(value)
        if not value in val:
            raise ValueError, err
        i=0
        for v in values:
            if type(v) in [types.ListType,types.TupleType]:
                if value in v:
                    self._basic_set(name,i)
                    return
            elif value==v:
                self._basic_set(name,i)
                return
            i=i+1
    elif type(value)==types.IntType or (type(value)==types.FloatType and int(value)==value):
        if not value in range(len(values)):
            raise ValueError, err
        else:
            self._basic_set(name,int(value))
            return
    else:
        raise ValueError, err

 
def checkPositiveInt(self,name,value):
    if not type(value) in [types.IntType,types.FloatType,types.LongType]:
        raise ValueError,name+' must be an integer'
    elif (not type(value)==types.IntType) and (not  int(value)==value):
        raise ValueError,name+' must be an integer'
    elif value<0:
        raise ValueError,name+' must be positve'
    self._basic_set(name,value)

def checkPositiveNumber(self,name,value):
    if not type(value) in [types.IntType,types.FloatType,types.LongType]:
        raise ValueError,name+' must be an integer or a float'
    elif value<0:
        raise ValueError,name+' must be positve'
    self._basic_set(name,value)
def checkNumber(self,name,value):
    if not type(value) in [types.IntType,types.FloatType,types.LongType]:
        raise ValueError,name+' must be an integer or a float'
    self._basic_set(name,value)

def checkRGB(self,name,value):
    if not type(value) in [types.ListType,types.TupleType]:
        raise ValueError, name+' must be a list or a tuple'
    elif len(value)!=3:
        raise ValueError, name+' must have exactly 3 arguments'
    else:
        for i in range(3):
            if type(value[i])!=types.IntType:
                if int(value[i])==value[i]:
                    value[i]=int(value[i])
                else:
                    raise ValueError, name+' values must be integer'
            if value[i]<0 or value[i]>255:
                raise ValueError, name+' values must be between 0 and 255'
    self._basic_set(name,value)

def checkOnOff(self,name,value):
    checkInStringsList(self,name,value,['on','off'])

def checkLinestyle(self,name,value):
    """set the line style """
    checkInStringsListInt(self,name,value,[
        "none",
        "solid",
        "dot",
        "dash",
        "long-dash",
        ["dot-dash","dash-dot","dashdot","dotdash"],
        ["dot-long-dash","long-dash-dot"],
        "dot-dot-dash",
        "dot-dash-dash",
        "dot-long-dash"])
def checkFont(self,name,value):
    """set the font """
    checkInStringsListInt(self,name,value,[
        ["times","times-roman"],
        "times-italic",
        "times-bold",
        "times-bolditalic",
        "helvetica",
        "helvetica-oblique",
        "helvetica-bold",
        "helvetica-boldoblique",
        "courier",
        "courier-oblique",
        "courier-bold",
        "courier-boldoblique",
        "symbol",
        "zapfdingbats"])
    
def checkSide(self,name,value):
    """ check the side """
    checkInStringsList(self,name,value,['normal','both','opposite'])
    
def checkLoc(self,name,value):
    """ check the loc (auto) or a location """
    if not ( 
        ((type(value)==types.StringType) and (string.lower(str(value))=='auto'))
        or 
        type(value) in [types.ListType, types.TupleType]
        ):
        raise ValueError, name + 'must be a "auto" or a tuple/list'
    elif type(value) in [types.ListType, types.TupleType]:
        if not len(value)!=2:
            raise ValueError, name +'tuple length must be 2 x/y coordinate'
        else:
            for v in value:
                if not type(v) in [types.IntType,types.longType,types.Float]:
                    raise ValueError, name +' must contain numbers only'
    if type(value)==types.StringType:
        self._basic_set(name,string.lower(value))
    else:
        self._basic_set(name,value)

def checkLayout(self,name,value):
    """ check the layout para/perp """
    if not type(value)==types.StringType:
        raise ValueError, name + 'must be a string'
    elif not string.lower(value[:4]) in ['para','perp']:
        raise ValueError, name + 'must be either "para" or "perp"'
    self._basic_set(name,string.lower(value[:4]))

def checkTickType(self,name,value):
    """ check the type of special ticks """
    if not type(value)==types.StringType:
        raise ValueError, name + 'must be a string'
    elif not string.lower(value) in ['none','both','ticks']:
        raise ValueError, name +' must be either "none", "both" or "ticks"'
    self._basic_set(name,string.lower(value))

def checkTicksType(self,name,value):
    """ check the type of special ticks """
    if not type(value)==types.StringType:
        raise ValueError, name + 'must be a string'
    elif not string.lower(value) in ['auto','spec','zmean']:
        raise ValueError, name +' must be either "auto", "spec" or "zmean"'
    self._basic_set(name,string.lower(value))
    if string.lower(value)=='zmean':
        self.spec.values={-1.:'90S',-0.866:'60S',-0.5:'30S',0.:'Eq',0.5:'30N',0.866:'60N',1.:'90N'}

def checkTickLocation(self,name,value):
    """check that the locations are ok"""
    if not type(value) in [types.ListType, types.TupleType,types.DictionaryType]:
        raise ValueError,name+' must be a list/tuple or dictionary'
    elif type(value) in [types.ListType, types.TupleType]:
        val=value
    else:
        val=value.keys()
    # Now checks that the location are actually numbers
    for v in val:
        if not type(v) in [types.IntType,types.LongType,types.FloatType]:
            raise ValueError,name + ' locations position must be numbers !'
    # Now we're ok let's set the attributes
    if type(value) in [types.ListType, types.TupleType]:
        self._basic_set(name,value)
    else:
        loc=value.keys()
        val=[]
        for l in loc:
            k=value[l]
            if not type(k) in [types.StringType,types.IntType,types.LongType,types.FloatType]:
                raise ValueError,name + ' values must be string or number'
            else:
                val.append(str(k))
        self._basic_set('loc',loc)
        self._basic_set('values',val)
        
def checkTickValues(self,name,value):
    """check that the locations are ok"""
    if not type(value) in [types.ListType, types.TupleType,types.DictionaryType]:
        raise ValueError,name+' must be a list/tuple or dictionary'
    elif type(value) in [types.ListType, types.TupleType]:
        val=[]
    else:
        val=value.keys()
    # Now checks that the location are actually numbers
    for v in val:
        if not type(v) in [types.IntType,types.LongType,types.FloatType]:
            raise ValueError,name + ' locations position must be numbers !'
    # Now we're ok let's set the attributes
    if type(value)==types.DictionaryType:
        self._basic_set('loc',val)
        vals=[]
        for v in value.keys():
            vals.append(value[v])
        value=vals
    val=[]
    for k in value:
        if not type(k) in [types.StringType,types.IntType,types.LongType,types.FloatType]:
            raise ValueError,name + ' values must be string or number'
        else:
            val.append(str(k))
    self._basic_set('values',val)

    
def checkFormat(self,name,value):
    """checks for the format"""
    checkInStringsList(self,name,value,[
        'general',
        'decimal','exponential','power','scientific','engineering',
        'ddmmyy','mmddyy','yymmdd','mmyy','mmdd',
        'monthday','daymonth','months','monthsy','monthl',
        'dayofweekl','dayofweeks','dayofyear',
        'hms','mmddhms','mmddyyhms','yymmddhms',
        'degreeslon','degreesmmlon', 'degreesmmsslon','mmsslon',
        'degreeslat','degreesmmlat', 'degreesmmsslat','mmsslat',])
    
def checkAuto(self,name,value):
    """ check for 'auto' or a value """
    if not (
        ((type(value)==types.StringType) and (string.lower(value)=='auto'))\
        or
        type(value) in [types.IntType,types.LongType,types.FloatType]
        ):
        raise ValueError, name + 'must be a "auto" or a number'
    if type(value)==types.StringType:
        self._basic_set(name,'auto')
    else:
        self._basic_set(name,value)
        
def checkTrueFalse(self,name,value):
    """ check for 'true' or 'false' """
    checkInStringsList(self,name,value,['true','false'])
    
def checkInOut(self,name,value):
    """ check in or out """
    checkInStringsList(self,name,value,['in','out','both'])

def checkScale(self,name,value):
    """ check the scale """
    checkInStringsList(self,name,value,['normal','logarithmic','reciprocal','logit'])
    
def checkPattern(self,name,value):
    """ check the pattern """
    if not isinstance(value, types.IntType):
        raise ValueError, name + 'must be an integer'
    elif not 0<=value<32:
        raise ValueError, name + 'must be an integer between 0 and 31'
    self._basic_set(name,value)

def checkLoctype(self,name,value):
    """ check the location type (view/world)"""
    checkInStringsList(self,name,value,['view','world','viewport'])

    
def checkFrameType(self,name,value):
    """set the frame style """
    checkInStringsListInt(self,name,value,[
        'closed',
        ['half','halfopen','half open','half-open'],
        ['breaktop','break-top','break top'],
        ['breakbottom','break-bottom','break bottom'],
        ['breakleft','break-left','break left'],
        ['breakright','break-right','break right']])
    
def checkGraphType(self,name,value):
    """ check the graph type"""
    checkInStringsList(self,name,value,['xy','chart','polar','smith','fixed','pie'])
    
                       
    
def checkList2(self,name,value):
    """ check for a list of 2 number"""
    if not type(value) in [types.ListType,types.TupleType]:
        raise ValueError, name + ' must be a list/tuple'
    if not len(value)==2:
        raise ValueError, name + ' list/tuple must be of length 2'
    for v in value:
        if not type(v) in [types.IntType,types.LongType,types.Float]:
            raise ValueError, name + ' list/tuple elements must be numbers'
    self._basic_set(name,value)
    
def checkList4(self,name,value):
    """ check for a list of 4 number"""
    if not type(value) in [types.ListType,types.TupleType]:
        raise ValueError, name + ' must be a list/tuple'
    if not len(value)==4:
        raise ValueError, name + ' list/tuple must be of length 4'
    for v in value:
        if not type(v) in [types.IntType,types.LongType,types.FloatType]:
            raise ValueError, name + ' list/tuple elements must be numbers'
    self._basic_set(name,value)
    
def checkPercent(self,name,value):
    """ check to see if a number is between 0 and 1"""
    if not type(value) in [types.IntType,types.LongType,types.FloatType]:
        raise ValueError, name + ' must be number'
    if not 0.<=value<=1.:
        raise ValueError, name + ' must be between 0 and 1'
    self._basic_set(name,value)

def checkSymbol(self,name,value):
    """ check the symbol"""
    checkInStringsListInt(self,name,value,[
        'none',
        'circle',
        'square',
        'diamond',
        ['triangle up','triangleup','triangle-up','triangle_up'],
        ['triangle left','triangleleft','triangle-left','triangle_left'],
        ['triangle down','triangledown','triangle-down','triangle_down'],
        ['triangle right','triangleright','triangle-right','triangle_right'],
        ['plus','+'],
        'x',
        ['star','*'],
        ['char','character']])

def checkChar(self,name,value):
    """ checks the character for symbol"""
    if type(value)==types.StringType:
        if len(string.strip(value))!=1:
            raise ValueError, name + " must be a single character"
        self._basic_set(name,ord(string.strip(value)))
    elif type(value)==types.IntType:
        if not value in range(256):
            raise ValueError, name +' not in range(256)'
        self._basic_set(name,value)
    else:
        raise ValueError, name +' must be a single character or an integer less than 256'
        
       
def checkLineType(self,name,value):
    """ checks the line type"""
    checkInStringsListInt(self,name,value,[
        'none',
        'straight',
        'left',
        'right',
        ['segments','seg'],
        ['3seg','3segments']])
    
def checkBaseLineType(self,name,value):
    """ checks the baseline type"""
    checkInStringsListInt(self,name,value,[
        'zero',
        ['setmin','set min'],
        ['setmax','set max'],
        ['graphmin','graph min'],
        ['graphmax','graph max'],
        ['setaverage','set average','setavg','setaverage','set-average','set-avg']])
        
def checkFillType(self,name,value):
    """ checks the fill type"""
    checkInStringsListInt(self,name,value,[
        'none',
        ['polygon','as polygon','as-polygone','aspoly','poly','aspolygon','as-poly'],
        ['baseline','base','tobase','to baseline','tobaseline','to-base','to-baseline']])

def checkRule(self,name,value):
    """ checks the rule"""
    checkInStringsListInt(self,name,value,['winding',['even-odd','evenodd']])

def checkOffset(self,name,value):
    """ check the offset """
    if not type(value) in [types.ListType,types.TupleType]:
        raise ValueError, name +' must be a list/tuple'
    if not len(value)==2:
        raise ValueError, name +' must be of length 2'
    self.xoffset=value[0]
    self.yoffset=value[1]
        
def getOffset(self,name):
    """ get the offset """
    return [self.xoffset,self.yoffset]

def checkXY(self,name,value):
    """ check xy """
    if not type(value) in [types.ListType,types.TupleType]:
        raise ValueError, name +' must be a list/tuple'
    if not len(value)==2:
        raise ValueError, name +' must be of length 2'
    self.xoffset=value[0]
    self.yoffset=value[1]
    
def checkStringXY(self,name,value):
    """ check xy """
    if not type(value) in [types.ListType,types.TupleType]:
        raise ValueError, name +' must be a list/tuple'
    if not len(value)==2:
        raise ValueError, name +' must be of length 2'
    self.x=value[0]
    self.y=value[1]

def getXY(self,name):
    """ get the offset """
    return [self.x,self.y]

def getStringXY(self,name):
    """ get the offset """
    return [self.x,self.y]

def checkAngle(self,name,value):
    """ checks angles in degrees """
    if not type(value) in [types.IntType,types.FloatType]:
        raise ValuerError, name +' must be a number between 0. and 360.'
    if not 0<=value<=360.:
        raise ValuerError, name +' must be a number between 0. and 360.'
    self._basic_set(name,value)
    
def checkDSetType(self,name,value):
    """ checks the dataset type"""
    checkInStringsList(self,name,value,[
        'xy','xydx','xydy','xydxdx','xydydy','xydxdy','xydxdxdydy','bar','bardy','bardydy',
        'xyhilo','xyz','xyr','xysize','xycolor','xycolpat','xyvmap','xyboxplot'])
        
def checkJustification(self,name,value):
    """ checks the justification"""
    if type(value) in [types.IntType,types.FloatType]:
        if value==3 or value==7 or value==11 :
            raise ValueError,name+' wrong value: '+str(value)
        else:
            self._basic_set(name,value)
    elif type(value)==types.StringType:
        if value=='':raise ValueError,name+' cannot be an empty string'
    else:
        checkInStringsListInt(self,name,value,[
            ['left-justify','lj','left','leftjustify','left justify'],
            ['right-justify','rj','right','rightjustify','right justify'],
            ['center-justify','cj','center','centerjustify','center justify'],
            '',
            ['left-bottom','lb','left bottom','leftbottom'],
            ['right-bottom','rb','right bottom','rightbottom'],
            ['center-bottom','cb','center bottom','centerbottom'],
            '',
            ['left-top','lt','left top','lefttop'],
            ['right-top','rt','right top','righttop'],
            ['center-top','ct','center top','centertop'],
            '',
            ['left-half','lh','left half','lefthalf'],
            ['right-half','rh','right half','righthalf'],
            ['center-half','ch','center half','centerhalf']
            ])
        
def checkOnOffInt(self,name,value):
    """ checks for on/off and set to 1/0"""
    checkInStringsListInt(self,name,value,['off','on'])
    
def checkArrowType(self,name,value):
    """ checks for arrow type"""
    checkInStringsListInt(self,name,value,[
        'line',
        ['filled','fill'],
        'opaque'
        ])
    
def checkArrowPosition(self,name,value):
    """ checks for arrow type"""
    checkInStringsListInt(self,name,value,[
        'line',
        ['filled','fill'],
        'opaque'
        ])
    
def checkAvalueType(self,name,value):
    """ checks for arrow type"""
    checkInStringsListInt(self,name,value,[
        'none',
        'x',
        'y',
        ['xy','x,y','x, y'],
        ['string','s','str'],
        'z'
        ])
    
def getX1X2Y1Y2(self,name,value):
    """ returns x1 x2 y1 y2"""
    return [self.x1,self.x2,self.y1,self.y2]

def checkX1X2Y1Y2(self,name,value):
    """ returns x1 x2 y1 y2"""
    if not type(value) in [types.ListType,types.TupleType]:
        raise ValueError, name +' must be a list or a tuple: [x1value,x2value,y1value,y2value]'
    elif len(value)!=4:
        raise ValueError, name +' must be of length 4: [x1value,x2value,y1value,y2value]'
    else:
        for v in value:
            if not type(value) in [types.IntType,types.LongType,types.FloatType]:
               raise ValueError, name +' elements must be numbers: [x1value,x2value,y1value,y2value]'
    self.x1=value[0]
    self.x2=value[1]
    self.y1=value[2]
    self.y2=value[3]
    
def checkRegionXy(self,name,value):
    if value is None: # is it None ?
        if not self.type in ['polyi','polyo']: # if yes are we a poly type region ?
            self._basic_set(name,value) # No then that's ok to set it to None
        else:
            raise ValueError, name+' type is poly therefore xy can not be None'
    elif not self.type in ['polyi','polyo']: # Are we of the right type (poly)
        raise ValueError,'type is not poly you can not set xy'
    # now it's not None and it's the right type
    elif not type(value) in [types.ListType, types.TupleType]: # right type of data ?
        raise ValueError, name+' must be a list or tuple'
    for v in value:
        if not type(value) in [types.ListType, types.TupleType]:
            raise ValueError, name+' must be a list or tuple of such'
        elif len(v)!=2:
            raise ValueError, name+' tuples/list inside must be of length 2'
        for vv in v:
            if not type(vv) in [types.FloatType,types.IntType,types.LongType]:
                raise ValueError, name+' values must be numbers'
    self._basic_set(name,value)

def checkRegionLine(self,name,value):
    if value is None: # is it None ?
        if self.type in ['polyi','polyo']: # if yes are we a poly type region ?
            self._basic_set(name,value) # Yes then that's ok to set it to None
        else:
            raise ValueError, name+' type is not poly therefore line can not be None'
    elif self.type in ['polyi','polyo']: # Are we of the right type (poly)
        raise ValueError,'type is poly you can not set line'
    # now it's not None and it's the right type
    elif not type(value) in [types.ListType, types.TupleType]: # right type of data ?
        raise ValueError, name+' must be a list or tuple'
    for v in value:
        if not type(value) in [types.ListType, types.TupleType]:
            raise ValueError, name+' must be a list or tuple of such'
        elif len(v)!=4:
            raise ValueError, name+' tuples/list inside must be of length 4'
        for vv in v:
            if not type(vv) in [types.FloatType,types.IntType,types.LongType]:
                raise ValueError, name+' values must be numbers'
    self._basic_set(name,value)

def checkRegionType(self,name,value):
    if not string.lower(value) in ['polyi','polyo',
                                   'above','below',
                                   'left','right',
                                   'horizi','horizo',
                                   'verti','verto']:
        raise ValueError, name+" must be  'polyi','polyo'," + \
                                   "'above','below'," + \
                                   "'left','right'," + \
                                   "'horizi','horizo'," + \
                                   "'verti' or 'verto'"
    self._basic_set(name,string.lower(value))
def setAxesMinMax(self,name,value):
    setattr(self.graph,self._secretname+name,value)
    
def checkColor(self,name,value):
    if type(value) is types.StringType:
        for c in self.parent.Color: # goes through the user defined colors
            if c.name==value:
                self._basic_set(name,c.id)
                return
        # if we reach that then the name wasn't a user defined name !
        # Let's check with the xmgrace defined names
        if not string.lower(value) in [
            'white','black','red','green','blue','yellow','brown','grey',
            'violet','cyan','magenta','orange','indigo','maroon',
            'turquoise','green']:
            raise ValueError,name+' color: '+value+' is not user defined or xmgrace default name'
        else:
            checkInStringsListInt(self,name,value,['white','black','red','green',
                                                   'blue','yellow','brown','grey',
                                                   'violet','cyan','magenta','orange',
                                                   'indigo','maroon',
                                                   'turquoise','green'])
                
    else:
        checkPositiveInt(self,name,value)


def changeGraph(self,name,value):
    self.parent.Graph[value].Set.append(self.parent.Graph[self.graph].Set.pop(self.id))
    for g in self.parent.Graph:
        g.nset=len(g.Set)
    
    self._basic_set(name,value)

