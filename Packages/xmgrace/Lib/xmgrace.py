# Adapted for numpy/ma/cdms2 by convertcdms.py
import MV2
import os,signal
"""
## XMGR Module 
## Author: Charles Doutriaux 
## Email:  doutriaux1@llnl.gov 

## Third Party 
## See end of file 
## Thanks to: Michael Haggerty <mhagger@blizzard.harvard.edu> for the original piping capability 

## History of changes 
## version 1.1.0
## change change class xmgr name to init 
## added a postscript function 
## version 1.1.1 
## added a time delay to the postscript function so that it actually works ! 
## changed the way to set tickmarks from xaxis.tick.type=[list] 
## to x.axis.tick.spec.values=[list] 
## and x.axis.tick.spec.type='both' (by default but accept: ticks and none)
## version 1.1.2
## Changed the output functions, now handles any available format 
## does not need any temporary file anymore 
## currently available output: PS,EPS,PDF,JPEG,PNM,PNG,Metafile,SVG,MIF 
## version 1.1.2b 
## June 28, 2000: Added the read_parameter function 
## version 1.1.3
## September 19/20th, 2000:
##    All graph x/y reference are now in % of page
##    added function objecthelp()
##    cleaned up the del part
##    Graph class changed, xmin,xmax,ymin,ymax still exist but it is better to set  Graph.xaxis.min,...,Graph.yaxis.max
##    added the x.update()
## version 1.1.3b
## October 9th, 2000:
## tick marks (x)axis.tick.spec.values can now be a dictionnary, no need to fill .loc and .values
## x.orientation(),x.portrait(),x.landscape() added
## January 18th, 2001
## tick marks (x)axis.tick.spec.loc can now be a dictionnary as well, no need to fill .loc and .values
## January 22nd, 2001
## added the capabilty to set the device option when calling the function
## example x.ps('tmp.ps',color='grayscale')
## September 14th, 2001
## Added list() function, validation of EVERY attributes.... some doc strings
## version 1.2
## remove dependency to PropertiedClasses from Nmueric instead gets it from object
## Validation Functions andslots are still available but now in 'standard' python

## Things to do/add
__doc__ files, and __str__
"""
import ValidationFunctions
import time
import types
from genutil import colors
import atexit

## class Error(Exception):
##     """All exceptions thrown by this class are descended from Error."""
##     pass

## class Disconnected(Error):
##     """Thrown when xmgrace unexpectedly disconnects from the pipe.

##     This exception is thrown on an EPIPE error, which indicates that
##     xmgrace has stopped reading the pipe that is used to communicate
##     with it.  This could be because it has been closed (e.g., by
##     clicking on the exit button), crashed, or sent an exit command."""
##     pass

class FONT(object):
    __slots___=['name','_name']
    def __init__(self,name):
        self.name=name
        return
    def _getname(self):
        return self._name
    def _setname(self,value):
        self._name=ValidationFunctions.checkString(self,'name',value)
    name=property(_getname,_setname)
    
class COLOR(object):
    __slots__=['name','rgb','id','_name','_rgb','_id']
    def __init__(self,name='Black',rgb=None,id=16):
        self.name=name
        if rgb is None:
            self.rgb=self.__conv_color(self.name)
        else:
            self.rgb=rgb
        if id<16:
            raise ValueError,'Color id less than 16 are predefined and cannot be reset !'
        self.id=id
        
    def change(self,nm):
        # Change rgb values to name
        self.rgb=self.__conv_color(nm)
        self.name=nm
        
    def list(self):
        """list the attributes"""
        print 'id =',self.id
        print 'name =',self.name
        print 'rgb =',self.rgb
        
    def __conv_color(self,col):
        ret=colors.str2rgb(col)
        if ret is [None,None,None] or ret==[None,None,None]: ret=[0,0,0]
        return ret
    
    def _getname(self):
        return self._name
    def _setname(self,value):
        self._name=ValidationFunctions.checkString(self,'name',value)
    name=property(_getname,_setname)
    
    def _getid(self):
        return self._id
    def _setid(self,value):
        self._id=ValidationFunctions.checkPositiveInt(self,'id',value)
    id=property(_getid,_setid)
    
    def _getrgb(self):
        return self._rgb
    def _setrgb(self,value):
        self._rgb=ValidationFunctions.checkRGB(self,'rgb',value)
    rgb=property(_getrgb,_setrgb)
    
class REGION(object):
    """REGION CLASS"""
    def __init__(self,parent):
        """Default values"""
        self.parent=parent
        self.status='off'
        self.type='above'
        # Types: above,bellow,left,right,polyi,polyo,horizi,horizo,verti,verto
        self.linestyle=parent.linestyle
        self.linewidth=parent.linewidth
        self.color=parent.color
        self.link=0
        self.xy=None
        self.line=[0,0,0,0]
        
    def list(self):
        """list the attributes"""
        print 'status =',self.status
        print 'type =',self.type
        print 'linestyle =',self.linestyle
        print 'linewidth =',self.linewidth
        print 'color =',self.color
        print 'link =',self.link
        if not xy is None:
            print 'xy =',self.xy
        if not line is None:
            print 'line =',self.line

    __slots__=['status','type','linestyle','linewidth','color','xy','line','link','parent',
               '_status','_type','_linestyle','_linewidth','_color','_xy','_line','_link']
        
    def _getstatus(self):
        return self._status
    def _setstatus(self,value):
        self._status=ValidationFunctions.checkOnOff(self,'status',value)
    status=property(_getstatus,_setstatus)
    
    def _gettype(self):
        return self._type
    def _settype(self,value):
        self._type=ValidationFunctions.checkRegionType(self,'type',value)
    type=property(_gettype,_settype)
    
    def _getlinestyle(self):
        return self._linestyle
    def _setlinestyle(self,value):
        self._linestyle=ValidationFunctions.checkLinestyle(self,'linestyle',value)
    linestyle=property(_getlinestyle,_setlinestyle)
    
    def _getlinewidth(self):
        return self._linewidth
    def _setlinewidth(self,value):
        self._linewidth=ValidationFunctions.checkPositiveNumber(self,'linewidth',value)
    linewidth=property(_getlinewidth,_setlinewidth)
    
    def _getcolor(self):
        return self._color
    def _setcolor(self,value):
        self._color=ValidationFunctions.checkColor(self,'color',value)
    color=property(_getcolor,_setcolor)
    
    def _getlink(self):
        return self._link
    def _setlink(self,value):
        self._link=ValidationFunctions.checkPositiveInt(self,'link',value)
    link=property(_getlink,_setlink)

    def _getxy(self):
        return self._xy
    def _setxy(self,value):
        self._xy=ValidationFunctions.checkRegionXy(self,'xy',value)
    xy=property(_getxy,_setxy)

    def _getline(self):
        return self._line
    def _setline(self,value):
        self._line=ValidationFunctions.checkRegionLine(self,'line',value)
    line=property(_getline,_setline)

class GRAPH(object):
    def __init__(self,parent,ymin=0.,ymax=1.,xmin=0.,xmax=1.):
        ## Since version 1.1.3 the 4 following in % of page
        self.nset=0
        self.Set=[]
        self.xmin=0.
        self.xmax=1.
        self.ymin=0.
        self.ymax=1.
        self.vxmin=.15
        self.vxmax=.85
        self.vymin=.15
        self.vymax=.85
        self.status='on'
        self.hidden='false'
        self.type='XY'
        self.stacked='false'
        self.stack_world=[0,0,0,0]
        self.bar_hgap=0.
        self.znorm=1.
        self.title=''
        self.stitle=''
        class Fixedpoint(object):
            def __init__(self):
                self.status='off'
                self.type=0
                self.xy=[0.,0.]
                self.format='general'
                self.prec=6
                
            __slots__=['status', 'type', 'xy', 'format', 'prec',
                       '_status','_format','_prec']
                
            def list(self):
                print '\t status =',self.status
                print '\t type =',self.type
                print '\t xy =',self.xy
                print '\t format',self.format
                print '\t prec',self.prec
            
            def _getstatus(self):
                return self._status
            def _setstatus(self,value):
                self._status=ValidationFunctions.checkOnOff(self,'status',value)
            status=property(_getstatus,_setstatus)
       
            def _getprec(self):
                return self._prec
            def _setprec(self,value):
                self._prec=ValidationFunctions.checkPositiveInt(self,'prec',value)
            prec=property(_getprec,_setprec)
       
            def _getformat(self):
                return self._format
            def _setformat(self,value):
                self._format=ValidationFunctions.checkFormat(self,'format',value)
            format=property(_getformat,_setformat)
                       
        self.fixedpoint=Fixedpoint()
        class Title(object):
            __slots__=['font','size','color','parent','_font','_size','_color']
            
            def __init__(self,parent,size):
                self.parent=parent
                self.font=parent.font
                self.size=size
                self.color=parent.color
                
            def list(self):
                print '\t font =',self.font
                print '\t size =',self.size
                print '\t color =',self.color
        
            def _getfont(self):
                return self._font
            def _setfont(self,value):
                self._font=ValidationFunctions.checkFont(self,'font',value)
            font=property(_getfont,_setfont)
               
            def _getsize(self):
                return self._size
            def _setsize(self,value):
                self._size=ValidationFunctions.checkPositiveNumber(self,'size',value)
            size=property(_getsize,_setsize)
                
            def _getcolor(self):
                return self._color
            def _setcolor(self,value):
                self._color=ValidationFunctions.checkColor(self,'color',value)
            color=property(_getcolor,_setcolor)
                
        self.tit=Title(parent,1.5)
        self.stit=Title(parent,1.)
        
        class Axe(object):                
            def __init__(self,parent,graph):
                self.graph=graph
                self.scale='Normal'  #! Logarithmic , Reciprocal
                self.invert='off'
                self.status='on'
                self.zero='false'
                self.xoffset=0.
                self.yoffset=0.
                class Bar(object):
                    def __init__(self,parent):
                        self.parent=parent
                        self.status='on'
                        self.color=parent.color
                        self.linestyle=parent.linestyle
                        self.linewidth=parent.linewidth
                    def list(self):
                        print '\t\t status =',self.status
                        print '\t\t color =',self.color
                        print '\t\t linestyle =',self.linestyle
                        print '\t\t linewidth =',self.linewidth
                        
                    __slots__=['status','linestyle','linewidth','color','parent',
                               '_status','_linestyle','_linewidth','_color']
                    
                    def _getcolor(self):
                        return self._color
                    def _setcolor(self,value):
                        self._color=ValidationFunctions.checkColor(self,'color',value)
                    color=property(_getcolor,_setcolor)
                    
                    def _getstatus(self):
                        return self._status
                    def _setstatus(self,value):
                        self._status=ValidationFunctions.checkOnOff(self,'status',value)
                    status=property(_getstatus,_setstatus)
                    
                    def _getlinestyle(self):
                        return self._linestyle
                    def _setlinestyle(self,value):
                        self._linestyle=ValidationFunctions.checkLinestyle(self,'linestyle',value)
                    linestyle=property(_getlinestyle,_setlinestyle)
                        
                    def _getlinewidth(self):
                        return self._linewidth
                    def _setlinewidth(self,value):
                        self._linewidth=ValidationFunctions.checkPositiveNumber(self,'linewidth',value)
                    linewidth=property(_getlinewidth,_setlinewidth)
                            
                self.bar=Bar(parent)
                self.label=''
                class Label(object):
                    def __init__(self,parent):
                        self.parent=parent
                        self.layout='para'  # perp
                        self.char_size=parent.char_size
                        self.font=parent.font
                        self.color=parent.color
                        class Place(object):
                            def __init__(self):
                                self.loc='auto' # if not pass list with loc
                                self.side='Normal' # opposite, both
                            def list(self):
                                print '\t\t\t loc =',self.loc
                                print '\t\t\t side =',self.side
                            __slots__=['loc','side','_loc','_side']
                            def _getloc(self):
                                return self._loc
                            def _setloc(self,value):
                                self._loc=ValidationFunctions.checkLoc(self,'loc',value)
                            loc=property(_getloc,_setloc)
                            def _getside(self):
                                return self._side
                            def _setside(self,value):
                                self._side=ValidationFunctions.checkSide(self,'side',value)
                            side=property(_getside,_setside)
                        self.place=Place()
                    def list(self):
                        print '\t\t layout =',self.layout
                        print '\t\t char_size =',self.char_size
                        print '\t\t font =',self.font
                        print '\t\t color =',self.color
                        print '\t\t member = place'
                        self.place.list()
                    __slots__=['layout','char_size','font','color','place','parent',
                               '_layout','_char_size','_font','_color']
                    def _getlayout(self):
                        return self._layout
                    def _setlayout(self,value):
                        self._layout=ValidationFunctions.checkLayout(self,'layout',value)
                    layout=property(_getlayout,_setlayout)
                    
                    def _getchar_size(self):
                        return self._char_size
                    def _setchar_size(self,value):
                        self._char_size=ValidationFunctions.checkPositiveNumber(self,'char_size',value)
                    char_size=property(_getchar_size,_setchar_size)
                    
                    def _getfont(self):
                        return self._font
                    def _setfont(self,value):
                        self._font=ValidationFunctions.checkFont(self,'font',value)
                    font=property(_getfont,_setfont)
                    
                    def _getcolor(self):
                        return self._color
                    def _setcolor(self,value):
                        self._color=ValidationFunctions.checkColor(self,'color',value)
                    color=property(_getcolor,_setcolor)
                    
                self.lbl=Label(parent)
                class Tick(object):
                    def __init__(self,parent):
                        self.type='auto'
                        self.status='on'
                        self.inc=.5
                        self.nsub=6
                        self.minor_ticks=1
                        self.place_rounded='true'
                        self.orientation='in' # 'out' 'both'
                        self.place='Normal'
                        class Spec(object):
                            def __init__(self):
                                self.loc=[]
                                self.values=[]
                                self.type='both'
                            def list(self):
                                print '\t\t\t loc = ',self.loc
                                print '\t\t\t values =',self.values
                                print '\t\t\t type =',self.type

                            __slots__=['loc','values','type','_loc','_values','_type']
                            
                            def _getloc(self):
                                return self._loc
                            def _setloc(self,value):
                                ValidationFunctions.checkTickLocation(self,'loc',value)
                            loc=property(_getloc,_setloc)
                            
                            def _getvalues(self):
                                return self._values
                            def _setvalues(self,value):
                                ValidationFunctions.checkTickValues(self,'values',value)
                            values=property(_getvalues,_setvalues)
                            
                            def _gettype(self):
                                return self._type
                            def _settype(self,value):
                                self._type=ValidationFunctions.checkTickType(self,'type',value)
                            type=property(_gettype,_settype)
                            
                        self.spec=Spec()
                        class STick(object):
                            def __init__(self,parent,size):
                                self.parent=parent
                                self.size=size
                                self.color=parent.color
                                self.linewidth=parent.linewidth
                                self.linestyle=parent.linestyle
                                self.grid='off'
                            def list(self):
                                print '\t\t\t size =',self.size
                                print '\t\t\t color =',self.color
                                print '\t\t\t linewidth =',self.linewidth
                                print '\t\t\t linestyle =',self.linestyle
                                print '\t\t\t grid =',self.grid

                            __slots__=['size','color','linewidth','linestyle','grid','parent',
                                       '_size','_color','_linewidth','_linestyle','_grid']
                            
                            def _getcolor(self):
                                return self._color
                            def _setcolor(self,value):
                                self._color=ValidationFunctions.checkColor(self,'color',value)
                            color=property(_getcolor,_setcolor)

                            def _getsize(self):
                                return self._size
                            def _setsize(self,value):
                                self._size=ValidationFunctions.checkPositiveNumber(self,'size',value)
                            size=property(_getsize,_setsize)

                            def _getlinestyle(self):
                                return self._linestyle
                            def _setlinestyle(self,value):
                                self._linestyle=ValidationFunctions.checkLinestyle(self,'linestyle',value)
                            linestyle=property(_getlinestyle,_setlinestyle)

                            def _getlinewidth(self):
                                return self._linewidth
                            def _setlinewidth(self,value):
                                self._linewidth=ValidationFunctions.checkPositiveNumber(self,'linewidth',value)
                            linewidth=property(_getlinewidth,_setlinewidth)
                            
                            def _getgrid(self):
                                return self._grid
                            def _setgrid(self,value):
                                self._grid=ValidationFunctions.checkOnOff(self,'grid',value)
                            grid=property(_getgrid,_setgrid)
                        
                        self.major=STick(parent,1.)
                        self.minor=STick(parent,0.5)
                        
                        class Tlbl(object):
                            def __init__(self,parent):
                                self.parent=parent
                                self.status='on'
                                self.prec=5
                                self.format='general'
                                self.append=''
                                self.prepend=''
                                self.angle=0
                                self.skip=0
                                self.stagger=0
                                self.place='Normal'
                                self.offset='auto' # if not pass list
                                self.start='auto' # if not pass value
                                self.stop='auto'  # if not pass value
                                self.char_size=parent.char_size
                                self.font=parent.font
                                self.color=parent.color
                                
                            def list(self):
                                print '\t\t\t status =',self.status
                                print '\t\t\t prec =',self.prec
                                print '\t\t\t format =',self.format
                                print '\t\t\t append =',self.append
                                print '\t\t\t prepend =',self.prepend
                                print '\t\t\t angle =',self.angle
                                print '\t\t\t skip =',self.skip
                                print '\t\t\t stagger =',self.stagger
                                print '\t\t\t place =',self.place
                                print '\t\t\t offset =',self.offset
                                print '\t\t\t start =',self.start
                                print '\t\t\t stop =',self.stop
                                print '\t\t\t char_size =',self.char_size
                                print '\t\t\t font =',self.font                                
                                print '\t\t\t color =',self.color                               

                            __slots__=['status','prec','format','append','prepend','angle','skip','stagger',
                                            'place','offset','start','stop','char_size','font',
                                            'color','parent',
                                       '_status','_prec','_format','_append','_prepend','_angle','_skip','_stagger',
                                            '_place','_offset','_start','_stop','_char_size','_font',
                                            '_color']
                                
                            def _getcolor(self):
                                return self._color
                            def _setcolor(self,value):
                                self._color=ValidationFunctions.checkColor(self,'color',value)
                            color=property(_getcolor,_setcolor)

                            def _getstatus(self):
                                return self._status
                            def _setstatus(self,value):
                                self._status=ValidationFunctions.checkOnOff(self,'status',value)
                            status=property(_getstatus,_setstatus)
                            
                            def _getprec(self):
                                return self._prec
                            def _setprec(self,value):
                                self._prec=ValidationFunctions.checkPositiveInt(self,'prec',value)
                            prec=property(_getprec,_setprec)
                            
                            def _getskip(self):
                                return self._skip
                            def _setskip(self,value):
                                self._skip=ValidationFunctions.checkPositiveInt(self,'skip',value)
                            skip=property(_getskip,_setskip)
                            
                            def _getstagger(self):
                                return self._stagger
                            def _setstagger(self,value):
                                self._stagger=ValidationFunctions.checkPositiveInt(self,'stagger',value)
                            stagger=property(_getstagger,_setstagger)

                            def _getformat(self):
                                return self._format
                            def _setformat(self,value):
                                self._format=ValidationFunctions.checkFormat(self,'format',value)
                            format=property(_getformat,_setformat)
                            
                            def _getappend(self):
                                return self._append
                            def _setappend(self,value):
                                self._append=ValidationFunctions.checkString(self,'append',value)
                            append=property(_getappend,_setappend)
                            
                            def _getprepend(self):
                                return self._prepend
                            def _setprepend(self,value):
                                self._prepend=ValidationFunctions.checkString(self,'prepend',value)
                            prepend=property(_getprepend,_setprepend)
                            
                            def _getangle(self):
                                return self._angle
                            def _setangle(self,value):
                                self._angle=ValidationFunctions.checkAngle(self,'angle',value)
                            angle=property(_getangle,_setangle)
                            
                            def _getplace(self):
                                return self._place
                            def _setplace(self,value):
                                self._place=ValidationFunctions.checkSide(self,'place',value)
                            place=property(_getplace,_setplace)
                            
                            def _getoffset(self):
                                return self._offset
                            def _setoffset(self,value):
                                self._offset=ValidationFunctions.checkLoc(self,'offset',value)
                            offset=property(_getoffset,_setoffset)
                            
                            def _getstart(self):
                                return self._start
                            def _setstart(self,value):
                                self._start=ValidationFunctions.checkAuto(self,'start',value)
                            start=property(_getstart,_setstart)
                            
                            def _getstop(self):
                                return self._stop
                            def _setstop(self,value):
                                self._stop=ValidationFunctions.checkAuto(self,'stop',value)
                            stop=property(_getstop,_setstop)
                            def _getchar_size(self):
                                return self._char_size
                            def _setchar_size(self,value):
                                self._char_size=ValidationFunctions.checkPositiveNumber(self,'char_size',value)
                            char_size=property(_getchar_size,_setchar_size)

                            def _getfont(self):
                                return self._font
                            def _setfont(self,value):
                                self._font=ValidationFunctions.checkFont(self,'font',value)
                            font=property(_getfont,_setfont)
                                
                        self.label=Tlbl(parent)
                    # End of __init__
                    
                    def list(self):
                        print '\t\t status =',self.status
                        print '\t\t inc =',self.inc
                        print '\t\t nsub =',self.nsub
                        print '\t\t minor_ticks =',self.minor_ticks
                        print '\t\t place_rounded =',self.place_rounded
                        print '\t\t orientation =',self.orientation
                        print '\t\t place =',self.place
                        print '\t\t member = spec'
                        self.spec.list()
                        print '\t\t member = major'
                        self.major.list()
                        print '\t\t member = minor'
                        self.minor.list()
                        print '\t\t member = label'
                        self.label.list()
                        print '\t\t type =',self.type

                    __slots__=['type','status','inc','nsub','minor_ticks','place_rounded',
                                    'orientation','place','spec','major','minor','label',
                               '_type','_status','_inc','_nsub','_minor_ticks','_place_rounded',
                                    '_orientation','_place']
                    
                    def _gettype(self):
                        return self._type
                    def _settype(self,value):
                        self._type=ValidationFunctions.checkTicksType(self,'type',value)
                    type=property(_gettype,_settype)
                    
                    def _getstatus(self):
                        return self._status
                    def _setstatus(self,value):
                        self._status=ValidationFunctions.checkOnOff(self,'status',value)
                    status=property(_getstatus,_setstatus)
                    
                    def _getinc(self):
                        return self._inc
                    def _setinc(self,value):
                        self._inc=ValidationFunctions.checkPositiveNumber(self,'inc',value)
                    inc=property(_getinc,_setinc)
                    
                    def _getnsub(self):
                        return self._nsub
                    def _setnsub(self,value):
                        self._nsub=ValidationFunctions.checkPositiveInt(self,'nsub',value)
                    nsub=property(_getnsub,_setnsub)
                    
                    def _getminor_ticks(self):
                        return self._minor_ticks
                    def _setminor_ticks(self,value):
                        self._minor_ticks=ValidationFunctions.checkPositiveInt(self,'minor_ticks',value)
                    minor_ticks=property(_getminor_ticks,_setminor_ticks)
                    
                    def _getplace_rounded(self):
                        return self._place_rounded
                    def _setplace_rounded(self,value):
                        self._place_rounded=ValidationFunctions.checkTrueFalse(self,'place_rounded',value)
                    place_rounded=property(_getplace_rounded,_setplace_rounded)
                    
                    def _getorientation(self):
                        return self._orientation
                    def _setorientation(self,value):
                        self._orientation=ValidationFunctions.checkInOut(self,'orientation',value)
                    orientation=property(_getorientation,_setorientation)
                    
                    def _getplace(self):
                        return self._place
                    def _setplace(self,value):
                        self._place=ValidationFunctions.checkSide(self,'place',value)
                    place=property(_getplace,_setplace)
               # end of class Tick
                self.tick=Tick(parent)
            # end of __init__
            def list(self):
                print '\t scale =',self.scale
                print '\t invert =',self.invert
                print '\t status =',self.status
                print '\t zero =',self.zero
                print '\t xoffset =',self.xoffset
                print '\t yoffset =',self.yoffset
                print '\t member = bar'
                self.bar.list()
                print '\t label =',self.label
                print '\t member = lbl'
                self.lbl.list()
                print '\t member = tick'
                self.tick.list()
            __slots__=['scale','invert','status','zero','xoffset','yoffset','offset',
                            'min','max','label','bar','lbl','tick','graph',
                       '_scale','_invert','_status','_zero','_xoffset','_yoffset',
                            '_min','_max','_label','_secretname']
            
            def _getscale(self):
                return self._scale
            def _setscale(self,value):
                self._scale=ValidationFunctions.checkScale(self,'scale',value)
            scale=property(_getscale,_setscale)
            
            def _getinvert(self):
                return self._invert
            def _setinvert(self,value):
                self._invert=ValidationFunctions.checkOnOff(self,'invert',value)
            invert=property(_getinvert,_setinvert)
            
            def _getstatus(self):
                return self._status
            def _setstatus(self,value):
                self._status=ValidationFunctions.checkOnOff(self,'status',value)
            status=property(_getstatus,_setstatus)
            
            def _getzero(self):
                return self._zero
            def _setzero(self,value):
                self._zero=ValidationFunctions.checkTrueFalse(self,'zero',value)
            zero=property(_getzero,_setzero)
            
            def _getoffset(self):
                return [self.xoffset,self.yoffset]
            def _setoffset(self,value):
                ValidationFunctions.checkOffset(self,'offset',value)
            offset=property(_getoffset,_setoffset)
            
            def _getxoffset(self):
                return self._xoffset
            def _setxoffset(self,value):
                self._xoffset=ValidationFunctions.checkNumber(self,'xoffset',value)
            xoffset=property(_getxoffset,_setxoffset)
            
            def _getyoffset(self):
                return self._yoffset
            def _setyoffset(self,value):
                self._yoffset=ValidationFunctions.checkNumber(self,'yoffset',value)
            yoffset=property(_getyoffset,_setyoffset)
           
            def _getmin(self):
                return self._min
            def _setmin(self,value):
                self._min=ValidationFunctions.setAxesMinMax(self,'min',value)
            min=property(_getmin,_setmin)
           
            def _getmax(self):
                return self._max
            def _setmax(self,value):
                self._max=ValidationFunctions.setAxesMinMax(self,'max',value)
            max=property(_getmax,_setmax)
           
            def _getlabel(self):
                return self._label
            def _setlabel(self,value):
                self._label=ValidationFunctions.checkString(self,'label',value)
            label=property(_getlabel,_setlabel)
           
        # end of class Axe
        self.xaxis=Axe(parent,self)
        self.xaxis._secretname='x'
        self.yaxis=Axe(parent,self)
        self.yaxis._secretname='y'
        self.altxaxis=Axe(parent,self)
        self.altxaxis._secretname='x'
        self.altyaxis=Axe(parent,self)
        self.altyaxis._secretname='y'
        self.altxaxis.status='off'
        self.altyaxis.status='off'
        class Legend(object):                
            def __init__(self,parent):
                self.parent=parent
                self.status='on'
                self.loctype='view'
                self.x=.85
                self.y=.8
                class Box(object):
                    def __init__(self,parent):
                        self.parent=parent
                        self.color=parent.color
                        self.pattern=parent.pattern
                        self.linewidth=parent.linewidth
                        self.linestyle=parent.linestyle
                        self.fcolor=0
                        self.fpattern=parent.pattern
                    def list(self):
                        print '\t\t color =',self.color
                        print '\t\t pattern =',self.pattern
                        print '\t\t linewidth =',self.linewidth
                        print '\t\t linestyle =',self.linestyle
                        print '\t\t fcolor =',self.fcolor
                        print '\t\t fpattern =',self.fpattern
                    __slots__=['color','pattern','linewidth','linestyle','fcolor','fpattern','parent',
                               '_color','_pattern','_linewidth','_linestyle','_fcolor','_fpattern']
                    def _getcolor(self):
                        return self._color
                    def _setcolor(self,value):
                        self._color=ValidationFunctions.checkColor(self,'color',value)
                    color=property(_getcolor,_setcolor)
                    
                    def _getpattern(self):
                        return self._pattern
                    def _setpattern(self,value):
                        self._pattern=ValidationFunctions.checkPattern(self,'pattern',value)
                    pattern=property(_getpattern,_setpattern)
                    
                    def _getlinestyle(self):
                        return self._linestyle
                    def _setlinestyle(self,value):
                        self._linestyle=ValidationFunctions.checkLinestyle(self,'linestyle',value)
                    linestyle=property(_getlinestyle,_setlinestyle)
                        
                    def _getlinewidth(self):
                        return self._linewidth
                    def _setlinewidth(self,value):
                        self._linewidth=ValidationFunctions.checkPositiveNumber(self,'linewidth',value)
                    linewidth=property(_getlinewidth,_setlinewidth)

                    def _getfcolor(self):
                        return self._fcolor
                    def _setfcolor(self,value):
                        self._fcolor=ValidationFunctions.checkColor(self,'fcolor',value)
                    fcolor=property(_getfcolor,_setfcolor)
                    
                    def _getfpattern(self):
                        return self._fpattern
                    def _setfpattern(self,value):
                        self._fpattern=ValidationFunctions.checkPattern(self,'fpattern',value)
                    fpattern=property(_getfpattern,_setfpattern)
                self.box=Box(parent)
                self.font=parent.font
                self.char_size=parent.char_size
                self.color=parent.color
                self.length=4
                self.vgap=1
                self.hgap=1
                self.invert='false'
            def list(self):
                print '\t status =',self.status
                print '\t loctype =',self.loctype
                print '\t x =',self.x
                print '\t y =',self.y
                print '\t member = box'
                self.box.list()
                print '\t font =',self.font
                print '\t char_size =',self.char_size
                print '\t color =',self.color
                print '\t length =',self.length
                print '\t vgap =',self.vgap
                print '\t hgap =',self.vgap
                print '\t invert =',self.vgap
            __slots__=['status','loctype','x','y','font','char_size','color',
                            'length','vgap','hgap','invert','box','parent',
                       '_status','_loctype','_x','_y','_font','_char_size','_color',
                            '_length','_vgap','_hgap','_invert']

            def _getstatus(self):
                return self._status
            def _setstatus(self,value):
                self._status=ValidationFunctions.checkOnOff(self,'status',value)
            status=property(_getstatus,_setstatus)
            
            def _getloctype(self):
                return self._loctype
            def _setloctype(self,value):
                self._loctype=ValidationFunctions.checkLoctype(self,'loctype',value)
            loctype=property(_getloctype,_setloctype)
            
            def _getx(self):
                return self._x
            def _setx(self,value):
                self._x=ValidationFunctions.checkNumber(self,'x',value)
            x=property(_getx,_setx)
            
            def _gety(self):
                return self._y
            def _sety(self,value):
                self._y=ValidationFunctions.checkNumber(self,'y',value)
            y=property(_gety,_sety)
            
            def _getfont(self):
                return self._font
            def _setfont(self,value):
                self._font=ValidationFunctions.checkFont(self,'font',value)
            font=property(_getfont,_setfont)
            
            def _getchar_size(self):
                return self._char_size
            def _setchar_size(self,value):
                self._char_size=ValidationFunctions.checkPositiveNumber(self,'char_size',value)
            char_size=property(_getchar_size,_setchar_size)
            
            def _getcolor(self):
                return self._color
            def _setcolor(self,value):
                self._color=ValidationFunctions.checkColor(self,'color',value)
            color=property(_getcolor,_setcolor)
        
            def _getlength(self):
                return self._length
            def _setlength(self,value):
                self._length=ValidationFunctions.checkPositiveInt(self,'length',value)
            length=property(_getlength,_setlength)
            
            def _getvgap(self):
                return self._vgap
            def _setvgap(self,value):
                self._vgap=ValidationFunctions.checkPositiveInt(self,'vgap',value)
            vgap=property(_getvgap,_setvgap)
            
            def _gethgap(self):
                return self._hgap
            def _sethgap(self,value):
                self._hgap=ValidationFunctions.checkPositiveInt(self,'hgap',value)
            hgap=property(_gethgap,_sethgap)
            
            def _getinvert(self):
                return self._invert
            def _setinvert(self,value):
                self._invert=ValidationFunctions.checkTrueFalse(self,'invert',value)
            invert=property(_getinvert,_setinvert)
        self.legend=Legend(parent)
        class Frame(object):
            def __init__(self,parent):
                self.parent=parent
                self.type=0
                self.linestyle=parent.linestyle
                self.linewidth=parent.linewidth
                self.color=parent.color
                self.pattern=parent.pattern
                self.background_color=parent.background_color
                self.background_pattern=0
            def list(self):
                print '\t type =',self.type
                print '\t color =',self.color
                print '\t pattern =',self.pattern
                print '\t linewidth =',self.linewidth
                print '\t linestyle =',self.linestyle
                print '\t background_color =',self.background_color
                print '\t background_pattern =',self.background_pattern
            __slots__=['type','color','pattern','linewidth','linestyle',
                       'background_color','background_pattern','parent',
                       '_type','_color','_pattern','_linewidth','_linestyle',
                       '_background_color','_background_pattern']
            def _gettype(self):
                return self._type
            def _settype(self,value):
                self._type=ValidationFunctions.checkFrameType(self,'type',value)
            type=property(_gettype,_settype)
            def _getcolor(self):
                return self._color
            def _setcolor(self,value):
                self._color=ValidationFunctions.checkColor(self,'color',value)
            color=property(_getcolor,_setcolor)
            def _getpattern(self):
                return self._pattern
            def _setpattern(self,value):
                self._pattern=ValidationFunctions.checkPattern(self,'pattern',value)
            pattern=property(_getpattern,_setpattern)
            def _getlinestyle(self):
                return self._linestyle
            def _setlinestyle(self,value):
                self._linestyle=ValidationFunctions.checkLinestyle(self,'linestyle',value)
            linestyle=property(_getlinestyle,_setlinestyle)

            def _getlinewidth(self):
                return self._linewidth
            def _setlinewidth(self,value):
                self._linewidth=ValidationFunctions.checkPositiveNumber(self,'linewidth',value)
            linewidth=property(_getlinewidth,_setlinewidth)

            def _getbackground_color(self):
                return self._background_color
            def _setbackground_color(self,value):
                self._background_color=ValidationFunctions.checkColor(self,'background_color',value)
            background_color=property(_getbackground_color,_setbackground_color)

            def _getbackground_pattern(self):
                return self._background_pattern
            def _setbackground_pattern(self,value):
                self._background_pattern=ValidationFunctions.checkPattern(self,'background_pattern',value)
            background_pattern=property(_getbackground_pattern,_setbackground_pattern)
        self.frame=Frame(parent)
    #end of __init__
    def list(self):
        print 'xmin =',self.xmin
        print 'xmax =',self.xmax
        print 'ymin =',self.ymin
        print 'ymax =',self.ymax
        print 'vxmin =',self.vxmin
        print 'vxmax =',self.vxmax
        print 'vymin =',self.vymin
        print 'vymax =',self.vymax
        print 'status =',self.status
        print 'hidden =',self.hidden
        print 'type = ',self.type
        print 'stacked = ',self.stacked
        print 'stack_world =',self.stack_world
        print 'bar_hgap =',self.bar_hgap
        print 'znorm =',self.znorm
        print 'title =',self.title
        print 'member = tit'
        self.tit.list()
        print 'stitle =',self.stitle
        print 'member = stit'
        self.stit.list()
        print 'member = fixedpoint'
        self.fixedpoint.list()
        print 'member = xaxis'
        self.xaxis.list()
        print 'member = yaxis'
        self.yaxis.list()
        print 'member = altxaxis'
        self.altxaxis.list()
        print 'member = altyaxis'
        self.altyaxis.list()
        print 'member = legend'
        self.legend.list()
        print 'member = frame'
        self.frame.list()
    __slots__=['vxmin','vxmax','vymin','vymax','status','hidden',
               'type','stacked','stack_world','bar_hgap','znorm',
               'title','stitle','stit','tit','fixedpoint',
               'xaxis','yaxis','altxaxis','altyaxis',
               'legend','frame','xmin','xmax','ymin','ymax',
               'nset','Set',
               '_vxmin','_vxmax','_vymin','_vymax','_status','_hidden',
               '_type','_stacked','_stack_world','_bar_hgap','_znorm',
               '_title','_stitle','_nset','_xmin','_xmax','_ymin','_ymax']

    def _getnset(self):
        return self._nset
    def _setnset(self,value):
        self._nset=ValidationFunctions.checkPositiveInt(self,'nset',value)
    nset=property(_getnset,_setnset)
    
    def _getxmin(self):
        return self._xmin
    def _setxmin(self,value):
        self._xmin=ValidationFunctions.checkNumber(self,'xmin',value)
    xmin=property(_getxmin,_setxmin)
    
    def _getxmax(self):
        return self._xmax
    def _setxmax(self,value):
        self._xmax=ValidationFunctions.checkNumber(self,'xmax',value)
    xmax=property(_getxmax,_setxmax)
    
    def _getymin(self):
        return self._ymin
    def _setymin(self,value):
        self._ymin=ValidationFunctions.checkNumber(self,'ymin',value)
    ymin=property(_getymin,_setymin)
    
    def _getymax(self):
        return self._ymax
    def _setymax(self,value):
        self._ymax=ValidationFunctions.checkNumber(self,'ymax',value)
    ymax=property(_getymax,_setymax)

    def _getvxmin(self):
        return self._vxmin
    def _setvxmin(self,value):
        self._vxmin=ValidationFunctions.checkPositiveNumber(self,'vxmin',value)
    vxmin=property(_getvxmin,_setvxmin)

    def _getvxmax(self):
        return self._vxmax
    def _setvxmax(self,value):
        self._vxmax=ValidationFunctions.checkPositiveNumber(self,'vxmax',value)
    vxmax=property(_getvxmax,_setvxmax)

    def _getvymin(self):
        return self._vymin
    def _setvymin(self,value):
        self._vymin=ValidationFunctions.checkPositiveNumber(self,'vymin',value)
    vymin=property(_getvymin,_setvymin)

    def _getvymax(self):
        return self._vymax
    def _setvymax(self,value):
        self._vymax=ValidationFunctions.checkPositiveNumber(self,'vymax',value)
    vymax=property(_getvymax,_setvymax)

    def _getstatus(self):
        return self._status
    def _setstatus(self,value):
        self._status=ValidationFunctions.checkOnOff(self,'status',value)
    status=property(_getstatus,_setstatus)

    def _gethidden(self):
        return self._hidden
    def _sethidden(self,value):
        self._hidden=ValidationFunctions.checkTrueFalse(self,'hidden',value)
    hidden=property(_gethidden,_sethidden)

    def _gettype(self):
        return self._type
    def _settype(self,value):
        self._type=ValidationFunctions.checkGraphType(self,'type',value)
    type=property(_gettype,_settype)

    def _getstacked(self):
        return self._stacked
    def _setstacked(self,value):
        self._stacked=ValidationFunctions.checkTrueFalse(self,'stacked',value)
    stacked=property(_getstacked,_setstacked)

    def _getstack_world(self):
        return self._stack_world
    def _setstack_world(self,value):
        self._stack_world=ValidationFunctions.checkList4(self,'stack_world',value)
    stack_world=property(_getstack_world,_setstack_world)

    def _getbar_hgap(self):
        return self._bar_hgap
    def _setbar_hgap(self,value):
        self._bar_hgap=ValidationFunctions.checkPercent(self,'bar_hgap',value)
    bar_hgap=property(_getbar_hgap,_setbar_hgap)

    def _getznorm(self):
        return self._znorm
    def _setznorm(self,value):
        self._znorm=ValidationFunctions.checkNumber(self,'znorm',value)
    znorm=property(_getznorm,_setznorm)

    def _gettitle(self):
        return self._title
    def _settitle(self,value):
        self._title=ValidationFunctions.checkString(self,'title',value)
    title=property(_gettitle,_settitle)

    def _getstitle(self):
        return self._stitle
    def _setstitle(self,value):
        self._stitle=ValidationFunctions.checkString(self,'stitle',value)
    stitle=property(_getstitle,_setstitle)

class DSET(object):        
    def __init__(self,parent,color,graph):
        self._graph=graph
        self.parent=parent
        self.id=parent.Graph[graph].nset-1
        if color==-1 : color=parent.color       
        self.hidden='false'
        self.type='xy'
        class Symbol(object):
            def __init__(self,parent,color):
                self.parent=parent
                self.type=0
                self.size=parent.symbol_size
                self.color=parent.background_color
                self.pattern=parent.pattern
                self.fcolor=color
                self.fpattern=parent.pattern
                self.linestyle=parent.linestyle
                self.linewidth=parent.linewidth
                self.char=65
                self.char_font=parent.font
                self.skip=0
            def list(self):
                print '\t type =',self.type
                print '\t size =',self.size
                print '\t color =',self.color
                print '\t pattern =',self.pattern
                print '\t fcolor =',self.fcolor
                print '\t fpattern =',self.fpattern
                print '\t linestyle =',self.linestyle
                print '\t linewidth =',self.linewidth
                print '\t char =',self.char
                print '\t char_font =',self.char_font
                print '\t skip =',self.skip
                
            __slots__=['type','size','color','pattern','fcolor','parent',
                       'fpattern','linestyle','linewidth','char',
                       'char_font','skip',
                       '_type','_size','_color','_pattern','_fcolor',
                       '_fpattern','_linestyle','_linewidth','_char',
                       '_char_font','_skip']
            
            def _gettype(self):
                return self._type
            def _settype(self,value):
                self._type=ValidationFunctions.checkSymbol(self,'type',value)
            type=property(_gettype,_settype)
            
            def _getsize(self):
                return self._size
            def _setsize(self,value):
                self._size=ValidationFunctions.checkPositiveNumber(self,'size',value)
            size=property(_getsize,_setsize)
            
            def _getcolor(self):
                return self._color
            def _setcolor(self,value):
                self._color=ValidationFunctions.checkColor(self,'color',value)
            color=property(_getcolor,_setcolor)
            
            def _getpattern(self):
                return self._pattern
            def _setpattern(self,value):
                self._pattern=ValidationFunctions.checkPattern(self,'pattern',value)
            pattern=property(_getpattern,_setpattern)
            
            def _getfcolor(self):
                return self._fcolor
            def _setfcolor(self,value):
                self._fcolor=ValidationFunctions.checkColor(self,'fcolor',value)
            fcolor=property(_getfcolor,_setfcolor)
            
            def _getfpattern(self):
                return self._fpattern
            def _setfpattern(self,value):
                self._fpattern=ValidationFunctions.checkPattern(self,'fpattern',value)
            fpattern=property(_getfpattern,_setfpattern)
            
            def _getlinestyle(self):
                return self._linestyle
            def _setlinestyle(self,value):
                self._linestyle=ValidationFunctions.checkLinestyle(self,'linestyle',value)
            linestyle=property(_getlinestyle,_setlinestyle)
            
            def _getlinewidth(self):
                return self._linewidth
            def _setlinewidth(self,value):
                self._linewidth=ValidationFunctions.checkPositiveNumber(self,'linewidth',value)
            linewidth=property(_getlinewidth,_setlinewidth)

            def _getchar(self):
                return self._char
            def _setchar(self,value):
                self._char=ValidationFunctions.checkChar(self,'char',value)
            char=property(_getchar,_setchar)

            def _getchar_font(self):
                return self._char_font
            def _setchar_font(self,value):
                self._char_font=ValidationFunctions.checkFont(self,'char_font',value)
            char_font=property(_getchar_font,_setchar_font)

            def _getskip(self):
                return self._skip
            def _setskip(self,value):
                self._skip=ValidationFunctions.checkPositiveInt(self,'skip',value)
            skip=property(_getskip,_setskip)

        self.symbol=Symbol(parent,color)
        
        class Line(object):
            def __init__(self,color,parent):
                self.parent=parent
                self.type=1
                self.linestyle=parent.linestyle
                self.linewidth=parent.linewidth
                self.color=color
                self.pattern=parent.pattern
            def list(self):
                print '\t type =',self.type
                print '\t linestyle =',self.linestyle
                print '\t linewidth =',self.linewidth
                print '\t color =',self.color
                print '\t pattern =',self.pattern
            __slots__=['type','color','pattern',
                       'linestyle','linewidth','parent',
                       '_type','_color','_pattern',
                       '_linestyle','_linewidth']
            
            def _gettype(self):
                return self._type
            def _settype(self,value):
                self._type=ValidationFunctions.checkLineType(self,'type',value)
            type=property(_gettype,_settype)
            
            def _getcolor(self):
                return self._color
            def _setcolor(self,value):
                self._color=ValidationFunctions.checkColor(self,'color',value)
            color=property(_getcolor,_setcolor)
            
            def _getpattern(self):
                return self._pattern
            def _setpattern(self,value):
                self._pattern=ValidationFunctions.checkPattern(self,'pattern',value)
            pattern=property(_getpattern,_setpattern)
            
            def _getlinestyle(self):
                return self._linestyle
            def _setlinestyle(self,value):
                self._linestyle=ValidationFunctions.checkLinestyle(self,'linestyle',value)
            linestyle=property(_getlinestyle,_setlinestyle)
            
            def _getlinewidth(self):
                return self._linewidth
            def _setlinewidth(self,value):
                self._linewidth=ValidationFunctions.checkPositiveNumber(self,'linewidth',value)
            linewidth=property(_getlinewidth,_setlinewidth)

        self.line=Line(color,parent)
        self.dropline='off'
        
        class Baseline(object):
            def list(self):
                print '\t type =',self.type
                print '\t status =',self.status
            def __init__(self):
                self.type=0
                self.status='off'
                
            __slots__=['type','status','_type','_status']
            
            def _getstatus(self):
                return self._status
            def _setstatus(self,value):
                self._status=ValidationFunctions.checkOnOff(self,'status',value)
            status=property(_getstatus,_setstatus)
            
            def _gettype(self):
                return self._type
            def _settype(self,value):
                self._type=ValidationFunctions.checkBaseLineType(self,'type',value)
            type=property(_gettype,_settype)
            
        self.baseline=Baseline()
        class Fill(object):
            def __init__(self,color,parent):
                self.parent=parent
                self.type=0
                self.rule=0
                self.color=color
                self.pattern=parent.pattern
            def list(self):
                print '\t type =',self.type
                print '\t rule =',self.rule
                print '\t color =',self.color
                print '\t pattern =',self.pattern
            __slots__=['type','rule','color','pattern','parent',
                       '_type','_rule','_color','_pattern']
            
            def _gettype(self):
                return self._type
            def _settype(self,value):
                self._type=ValidationFunctions.checkFillType(self,'type',value)
            type=property(_gettype,_settype)
            
            def _getrule(self):
                return self._rule
            def _setrule(self,value):
                self._rule=ValidationFunctions.checkRule(self,'rule',value)
            rule=property(_getrule,_setrule)
            
            def _getcolor(self):
                return self._color
            def _setcolor(self,value):
                self._color=ValidationFunctions.checkColor(self,'color',value)
            color=property(_getcolor,_setcolor)
            
            def _getpattern(self):
                return self._pattern
            def _setpattern(self,value):
                self._pattern=ValidationFunctions.checkPattern(self,'pattern',value)
            pattern=property(_getpattern,_setpattern)
            
        self.fill=Fill(color,parent)
        class Avalue(object):               
            def __init__(self,color,parent):
                self.parent=parent
                self.status='off'
                self.type=2
                self.char_size=parent.char_size
                self.font=parent.font
                self.color=color
                self.rot=0
                self.format='general'
                self.prec=3
                self.prepend=''
                self.append=''
                self.xoffset=0.
                self.yoffset=0.
            def list(self):
                print '\t status =',self.status
                print '\t type =',self.type
                print '\t char_size =',self.char_size
                print '\t font =',self.font
                print '\t color =',self.color
                print '\t rot =',self.rot
                print '\t format =',self.format
                print '\t prec =',self.prec
                print '\t prepend =',self.prepend
                print '\t append =',self.append
                print '\t xoffset =',self.xoffset
                print '\t yoffset =',self.yoffset
            __slots__=['status','type','char_size','font','color','rot','format','parent',
                       'prec','prepend','append','offset','xoffset','yoffset',
                       '_status','_type','_char_size','_font','_color','_rot','_format',
                       '_prec','_prepend','_append','_offset','_xoffset','_yoffset']
            def _getstatus(self):
                return self._status
            def _setstatus(self,value):
                self._status=ValidationFunctions.checkOnOff(self,'status',value)
            status=property(_getstatus,_setstatus)
            
            def _gettype(self):
                return self._type
            def _settype(self,value):
                self._type=ValidationFunctions.checkAvalueType(self,'type',value)
            type=property(_gettype,_settype)
            
            def _getchar_size(self):
                return self._char_size
            def _setchar_size(self,value):
                self._char_size=ValidationFunctions.checkPositiveNumber(self,'char_size',value)
            char_size=property(_getchar_size,_setchar_size)
            
            def _getfont(self):
                return self._font
            def _setfont(self,value):
                self._font=ValidationFunctions.checkFont(self,'font',value)
            font=property(_getfont,_setfont)
            
            def _getcolor(self):
                return self._color
            def _setcolor(self,value):
                self._color=ValidationFunctions.checkColor(self,'color',value)
            color=property(_getcolor,_setcolor)
            
            def _getrot(self):
                return self._rot
            def _setrot(self,value):
                self._rot=ValidationFunctions.checkAngle(self,'rot',value)
            rot=property(_getrot,_setrot)
            
            def _getformat(self):
                return self._format
            def _setformat(self,value):
                self._format=ValidationFunctions.checkFormat(self,'format',value)
            format=property(_getformat,_setformat)
            
            def _getprec(self):
                return self._prec
            def _setprec(self,value):
                self._prec=ValidationFunctions.checkPositiveInt(self,'prec',value)
            prec=property(_getprec,_setprec)
            
            def _getprepend(self):
                return self._prepend
            def _setprepend(self,value):
                self._prepend=ValidationFunctions.checkString(self,'prepend',value)
            prepend=property(_getprepend,_setprepend)
            
            def _getappend(self):
                return self._append
            def _setappend(self,value):
                self._append=ValidationFunctions.checkString(self,'append',value)
            append=property(_getappend,_setappend)
            
            def _getoffset(self):
                return [self.xoffset,self.yoffset]
            def _setoffset(self,value):
                ValidationFunctions.checkOffset(self,'offset',value)
            offset=property(_getoffset,_setoffset)
            
            def _getxoffset(self):
                return self._xoffset
            def _setxoffset(self,value):
                self._xoffset=ValidationFunctions.checkNumber(self,'xoffset',value)
            xoffset=property(_getxoffset,_setxoffset)
            
            def _getyoffset(self):
                return self._yoffset
            def _setyoffset(self,value):
                self._yoffset=ValidationFunctions.checkNumber(self,'yoffset',value)
            yoffset=property(_getyoffset,_setyoffset)
        self.avalue=Avalue(color,parent)
        class Error(object):
            def __init__(self,parent):
                self.parent=parent
                self.status='off'
                self.place='both'
                self.size=1.
                self.linewidth=parent.linewidth
                self.linestyle=parent.linestyle
                self.color=parent.color
                self.pattern=parent.pattern
                class Riser(object):
                   def __init__(self,parent):
                        self.linewidth=parent.linewidth
                        self.linestyle=parent.linestyle
                        class Clip(object):
                            def __init__(self):
                                self.status='off'
                                self.length=.1
                            def list(self):
                                print '\t\t\t status =',self.status
                                print '\t\t\t length =',self.length
                            __slots__=['status','length','_status','_length']
                            def _getstatus(self):
                                return self._status
                            def _setstatus(self,value):
                                self._status=ValidationFunctions.checkOnOff(self,'status',value)
                            status=property(_getstatus,_setstatus)
                            def _getlength(self):
                                return self._length
                            def _setlength(self,value):
                                self._length=ValidationFunctions.checkPositiveNumber(self,'length',value)
                            length=property(_getlength,_setlength)
                        self.clip=Clip()
                   def list(self):
                       print '\t\t linestyle =',self.linestyle
                       print '\t\t linewidth =',self.linewidth
                       print '\t\t member = clip'
                       self.clip.list()
                   __slots__=['linestyle','linewidth','clip','_linestyle','_linewidth']
                   def _getlinestyle(self):
                       return self._linestyle
                   def _setlinestyle(self,value):
                       self._linestyle=ValidationFunctions.checkLinestyle(self,'linestyle',value)
                   linestyle=property(_getlinestyle,_setlinestyle)
                   
                   def _getlinewidth(self):
                       return self._linewidth
                   def _setlinewidth(self,value):
                       self._linewidth=ValidationFunctions.checkPositiveNumber(self,'linewidth',value)
                       
                   linewidth=property(_getlinewidth,_setlinewidth)
                self.riser=Riser(parent)
            def list(self):
                print '\t status =',self.status
                print '\t place =',self.place
                print '\t size =',self.size
                print '\t linestyle =',self.linestyle
                print '\t linewidth =',self.linewidth
                print '\t color =',self.linewidth
                print '\t pattern =',self.pattern
                print '\t member = riser'
                self.riser.list()
            __slots__=['status','place','size','linestyle','linewidth','riser','color','pattern','parent',
                       '_status','_place','_size','_linestyle','_linewidth','_riser','_color','_pattern']
            def _getlinestyle(self):
                return self._linestyle
            def _setlinestyle(self,value):
                self._linestyle=ValidationFunctions.checkLinestyle(self,'linestyle',value)
            linestyle=property(_getlinestyle,_setlinestyle)

            def _getlinewidth(self):
                return self._linewidth
            def _setlinewidth(self,value):
                self._linewidth=ValidationFunctions.checkPositiveNumber(self,'linewidth',value)
            linewidth=property(_getlinewidth,_setlinewidth)
            
            def _getstatus(self):
                return self._status
            def _setstatus(self,value):
                self._status=ValidationFunctions.checkOnOff(self,'status',value)
            status=property(_getstatus,_setstatus)
            
            def _getplace(self):
                return self._place
            def _setplace(self,value):
                self._place=ValidationFunctions.checkSide(self,'place',value)
            place=property(_getplace,_setplace)
            
            def _getsize(self):
                return self._size
            def _setsize(self,value):
                self._size=ValidationFunctions.checkPositiveNumber(self,'size',value)
            size=property(_getsize,_setsize)
            
            def _getcolor(self):
                return self._color
            def _setcolor(self,value):
                self._color=ValidationFunctions.checkColor(self,'color',value)
            color=property(_getcolor,_setcolor)
            
            def _getpattern(self):
                return self._pattern
            def _setpattern(self,value):
                self._pattern=ValidationFunctions.checkPattern(self,'pattern',value)
            pattern=property(_getpattern,_setpattern)
            
        self.errorbar=Error(parent)
        self.comment=''
        self.legend=''
    # end of __init__
    def list(self):
        print 'graph =',self.graph
        print 'hidden =',self.hidden
        print 'type =',self.type
        print 'member = symbol'
        self.symbol.list()
        print 'member = line'
        self.line.list()
        print 'dropline =',self.dropline
        print 'member = baseline'
        self.baseline.list()
        print 'member = fill'
        self.fill.list()
        print 'member = avalue'
        self.avalue.list()
        print 'member = errorbar'
        self.errorbar.list()
        print 'comment =',self.comment
        print 'legend =',self.legend
    __slots__=['graph','hidden','type','dropline','comment','legend',
               'symbol','line','baseline','fill','avalue','errorbar',
               'id','parent',
               '_graph','_hidden','_type','_dropline','_comment','_legend']
    def _getgraph(self):
        return self._graph
    def _setgraph(self,value):
        self._graph=ValidationFunctions.changeGraph(self,'graph',value)
    graph=property(_getgraph,_setgraph)
    
    def _gethidden(self):
        return self._hidden
    def _sethidden(self,value):
        self._hidden=ValidationFunctions.checkTrueFalse(self,'hidden',value)
    hidden=property(_gethidden,_sethidden)
    
    def _gettype(self):
        return self._type
    def _settype(self,value):
        self._type=ValidationFunctions.checkDSetType(self,'type',value)
    type=property(_gettype,_settype)
    
    def _getdropline(self):
        return self._dropline
    def _setdropline(self,value):
        self._dropline=ValidationFunctions.checkOnOff(self,'dropline',value)
    dropline=property(_getdropline,_setdropline)
        
    def _getcomment(self):
        return self._comment
    def _setcomment(self,value):
        self._comment=ValidationFunctions.checkString(self,'comment',value)
    comment=property(_getcomment,_setcomment)
        
    def _getlegend(self):
        return self._legend
    def _setlegend(self,value):
        self._legend=ValidationFunctions.checkString(self,'legend',value)
    legend=property(_getlegend,_setlegend)
    
class STRING(object):
    def __init__(self,parent,x,y,text,color,char_size,font,rot,just,graph=0):
        self.parent=parent
        self.status='on'
        self.loctype='view'
        self.color=color
        self.rot=rot
        self.x=x
        self.y=y
        self.xy=[x,y]
        self.font=font
        self.just=just
        self.char_size=char_size
        self.text=text
        self.graph=graph
    def list(self):
        print 'status =',self.status
        print 'loctype =',self.loctype
        print 'graph =',self.graph
        print 'color =',self.color
        print 'rot =',self.rot
        print 'x =',self.x
        print 'y =',self.y
        print 'font =',self.font
        print 'just =',self.just
        print 'char_size =',self.char_size
        print 'text =',self.text
    __slots__=['status','loctype','color','font','just','rot',
                    'x','y','xy','char_size','text','parent','graph',
               '_status','_loctype','_color','_font','_just','_rot',
                    '_x','_y','_xy','_char_size','_text']
    def _getstatus(self):
        return self._status
    def _setstatus(self,value):
        self._status=ValidationFunctions.checkOnOff(self,'status',value)
    status=property(_getstatus,_setstatus)
        
    def _getloctype(self):
        return self._loctype
    def _setloctype(self,value):
        self._loctype=ValidationFunctions.checkLoctype(self,'loctype',value)
    loctype=property(_getloctype,_setloctype)
        
    def _getcolor(self):
        return self._color
    def _setcolor(self,value):
        self._color=ValidationFunctions.checkColor(self,'color',value)
    color=property(_getcolor,_setcolor)
        
    def _getrot(self):
        return self._rot
    def _setrot(self,value):
        self._rot=ValidationFunctions.checkAngle(self,'rot',value)
    rot=property(_getrot,_setrot)
        
    def _getx(self):
        return self._x
    def _setx(self,value):
        self._x=ValidationFunctions.checkNumber(self,'x',value)
    x=property(_getx,_setx)
    
    def _gety(self):
        return self._y
    def _sety(self,value):
        self._y=ValidationFunctions.checkNumber(self,'y',value)
    y=property(_gety,_sety)
        
    def _getxy(self):
        return ValidationFunctions.getStringXY(self,'xy')
    def _setxy(self,value):
        self._xy=ValidationFunctions.checkStringXY(self,'xy',value)
    xy=property(_getxy,_setxy)
    
    def _getfont(self):
        return self._font
    def _setfont(self,value):
        self._font=ValidationFunctions.checkFont(self,'font',value)
    font=property(_getfont,_setfont)
    
    def _getjust(self):
        return self._just
    def _setjust(self,value):
        self._just=ValidationFunctions.checkJustification(self,'just',value)
    just=property(_getjust,_setjust)
    
    def _getchar_size(self):
        return self._char_size
    def _setchar_size(self,value):
        self._char_size=ValidationFunctions.checkPositiveNumber(self,'char_size',value)
    char_size=property(_getchar_size,_setchar_size)
    
    def _gettext(self):
        return self._text
    def _settext(self,value):
        self._text=ValidationFunctions.checkString(self,'text',value)
    text=property(_gettext,_settext)
    
class LINE(object):        
    def __init__(self,parent,x1,y1,x2,y2,color,lwidth,lstyl,pos,atyp,algth,alyo,graph=0):
        self.parent=parent
        self.status='on'
        self.loctype='view'
        self.color=color
        self.linewidth=lwidth
        self.linestyle=lstyl
        self.graph=graph
        class Arrow(object):
            def __init__(self,pos,atyp,algth,alyo):
                self.position=pos
                self.type=atyp
                self.length=algth
                self.layout=alyo
            def list(self):
                print '\t position =',self.position
                print '\t type =',self.type
                print '\t length =',self.length
                print '\t layout =',self.layout
            __slots__=['position','type','length','layout',
                       '_position','_type','_length','_layout']
            def _gettype(self):
                return self._type
            def _settype(self,value):
                self._type=ValidationFunctions.checkArrowType(self,'type',value)
            type=property(_gettype,_settype)
            
            def _getposition(self):
                return self._position
            def _setposition(self,value):
                self._position=ValidationFunctions.checkArrowPosition(self,'position',value)
            position=property(_getposition,_setposition)
            
            def _getlength(self):
                return self._length
            def _setlength(self,value):
                self._length=ValidationFunctions.checkPositiveNumber(self,'length',value)
            length=property(_getlength,_setlength)
            
            def _getlayout(self):
                return self._layout
            def _setlayout(self,value):
                self._layout=ValidationFunctions.checkList2(self,'layout',value)
            layout=property(_getlayout,_setlayout)
            
        self.arrow=Arrow(pos,atyp,algth,alyo)           
        self.x1=x1
        self.y1=y1
        self.x2=x2
        self.y2=y2
    def list(self):
        print 'status =',self.status
        print 'loctype =',self.loctype
        print 'graph =',self.graph
        print 'color =',self.color
        print 'linewidth =',self.linewidth
        print 'linestyle =',self.linestyle
        print 'member = arrow'
        self.arrow.list()
        print 'x1 =',self.x1
        print 'x2 =',self.x2
        print 'y1 =',self.y1
        print 'y2 =',self.y2
    __slots__=['status','loctype','color','linewidth','linestyle',
                    'x1','x2','y1','y2','coord','parent',
                    'arrow','graph',
               '_status','_loctype','_color','_linewidth','_linestyle',
                    '_x1','_x2','_y1','_y2','_coord']
    
    def _getlinestyle(self):
        return self._linestyle
    def _setlinestyle(self,value):
        self._linestyle=ValidationFunctions.checkLinestyle(self,'linestyle',value)
    linestyle=property(_getlinestyle,_setlinestyle)

    def _getlinewidth(self):
        return self._linewidth
    def _setlinewidth(self,value):
        self._linewidth=ValidationFunctions.checkPositiveNumber(self,'linewidth',value)
    linewidth=property(_getlinewidth,_setlinewidth)

    def _getstatus(self):
        return self._status
    def _setstatus(self,value):
        self._status=ValidationFunctions.checkOnOff(self,'status',value)
    status=property(_getstatus,_setstatus)

    def _getcolor(self):
        return self._color
    def _setcolor(self,value):
        self._color=ValidationFunctions.checkColor(self,'color',value)
    color=property(_getcolor,_setcolor)

    def _getloctype(self):
        return self._loctype
    def _setloctype(self,value):
        self._loctype=ValidationFunctions.checkLoctype(self,'loctype',value)
    loctype=property(_getloctype,_setloctype)

    def _getx1(self):
        return self._x1
    def _setx1(self,value):
        self._x1=ValidationFunctions.checkNumber(self,'x1',value)
    x1=property(_getx1,_setx1)
            
    def _getx2(self):
        return self._x2
    def _setx2(self,value):
        self._x2=ValidationFunctions.checkNumber(self,'x2',value)
    x2=property(_getx2,_setx2)
            
    def _gety1(self):
        return self._y1
    def _sety1(self,value):
        self._y1=ValidationFunctions.checkNumber(self,'y1',value)
    y1=property(_gety1,_sety1)
            
    def _gety2(self):
        return self._y2
    def _sety2(self,value):
        self._y2=ValidationFunctions.checkNumber(self,'y2',value)
    y2=property(_gety2,_sety2)
            
    def _getcoord(self):
        return ValidationFunctions.getX1X2Y1Y2(self,'coord')
    def _setcoord(self,value):
        ValidationFunctions.checkX1X2Y1Y2(self,'coord',value)
    coord=property(_getcoord,_setcoord)
            
class FILL(object):
    def __init__(self,parent,fillcolor,fillpattern):
        self.parent=parent
        self.color=fillcolor
        self.pattern=fillpattern
    def list(self):
        print '\t pattern =',self.pattern
        print '\t color =',self.color
        
    __slots__=['_color','_pattern','parent','color','pattern']
    
    def _getcolor(self):
        return self._color
    def _setcolor(self,value):
        self._color=ValidationFunctions.checkColor(self,'color',value)
    color=property(_getcolor,_setcolor)
    def _getpattern(self):
        return self._pattern
    def _setpattern(self,value):
        self._pattern=ValidationFunctions.checkPattern(self,'pattern',value)
    pattern=property(_getpattern,_setpattern)

class BOX_ELLIPSE(object):        
    def __init__(self,parent,x1,y1,x2,y2,color,lwidth,lstyl,fillcolor,fillpattern,graph=0):
        self.parent=parent
        self.status='on'
        self.loctype='view'
        self.graph=graph
        self.color=color
        self.linewidth=lwidth
        self.linestyle=lstyl
        self.fill=FILL(parent,fillcolor,fillpattern)
        self.x1=x1
        self.y1=y1
        self.x2=x2
        self.y2=y2
    def list(self):
        print 'status =',self.status
        print 'loctype =',self.loctype
        print 'graph =',self.graph
        print 'color =',self.color
        print 'linewidth =',self.linewidth
        print 'linestyle =',self.linestyle
        print 'member = fill'
        self.fill.list()
        print 'x1 =',self.x1
        print 'x2 =',self.x2
        print 'y1 =',self.y1
        print 'y2 =',self.y2
    __slots__=['status','loctype','color','linewidth','linestyle',
                    'x1','x2','y1','y2','coord','fill','parent','graph',
               '_status','_loctype','_color','_linewidth','_linestyle',
                    '_x1','_x2','_y1','_y2']
    def _getlinestyle(self):
        return self._linestyle
    def _setlinestyle(self,value):
        self._linestyle=ValidationFunctions.checkLinestyle(self,'linestyle',value)
    linestyle=property(_getlinestyle,_setlinestyle)

    def _getlinewidth(self):
        return self._linewidth
    def _setlinewidth(self,value):
        self._linewidth=ValidationFunctions.checkPositiveNumber(self,'linewidth',value)
    linewidth=property(_getlinewidth,_setlinewidth)

    def _getstatus(self):
        return self._status
    def _setstatus(self,value):
        self._status=ValidationFunctions.checkOnOff(self,'status',value)
    status=property(_getstatus,_setstatus)

    def _getcolor(self):
        return self._color
    def _setcolor(self,value):
        self._color=ValidationFunctions.checkColor(self,'color',value)
    color=property(_getcolor,_setcolor)

    def _getloctype(self):
        return self._loctype
    def _setloctype(self,value):
        self._loctype=ValidationFunctions.checkLoctype(self,'loctype',value)
    loctype=property(_getloctype,_setloctype)

    def _getx1(self):
        return self._x1
    def _setx1(self,value):
        self._x1=ValidationFunctions.checkNumber(self,'x1',value)
    x1=property(_getx1,_setx1)
            
    def _getx2(self):
        return self._x2
    def _setx2(self,value):
        self._x2=ValidationFunctions.checkNumber(self,'x2',value)
    x2=property(_getx2,_setx2)
            
    def _gety1(self):
        return self._y1
    def _sety1(self,value):
        self._y1=ValidationFunctions.checkNumber(self,'y1',value)
    y1=property(_gety1,_sety1)
            
    def _gety2(self):
        return self._y2
    def _sety2(self,value):
        self._y2=ValidationFunctions.checkNumber(self,'y2',value)
    y2=property(_gety2,_sety2)
            
    def _getcoord(self):
        return ValidationFunctions.getX1X2Y1Y2(self,'coord')
    def _setcoord(self,value):
        ValidationFunctions.checkX1X2Y1Y2(self,'coord',value)
    coord=property(_getcoord,_setcoord)
    
class init(object):
##     ininit=1
    def __str__(self):
        out='XMGRACE Object\n'
        out=out+'--------------\n\n'
        out=out+str(self.ngraph)+' Graph and '+str(self.nset)+' Set defined\n'
        out=out+'Layout (X/Y): '+str(self.page.x)+'/'+str(self.page.y)+'\n'
        for i in range(self.ngraph):
            out=out+'\nGraph: '+str(i)+' (.Graph['+str(i)+'])\n\n'
            out=out+'     World Coordinate   | View Coordinate\n'
            out=out+'xmin      '+str(self.Graph[i].xmin)+'             '
            out=out+str(self.Graph[i].vxmin)+'\n'
            out=out+'xmax      '+str(self.Graph[i].xmax)+'             '
            out=out+str(self.Graph[i].vxmax)+'\n'
            out=out+'ymin      '+str(self.Graph[i].ymin)+'             '
            out=out+str(self.Graph[i].vymin)+'\n'
            out=out+'ymax      '+str(self.Graph[i].ymax)+'             '
            out=out+str(self.Graph[i].vymax)
        return(out)

    def clean_exit(self):
        self.close()
        try:
            os.remove(self.pipe_file)
        except Exception, err:
            pass
    def __init__(self,xmgrace_args='',pipe_file=None,new_pipe=1,clean_on_exit=True,color=1,font=0,linewidth=1., pattern=1,char_size=1):
        self.pipe_file=pipe_file
        self.new_pipe=new_pipe
        if clean_on_exit:
            atexit.register(self.clean_exit)
        class Page(object):
            def __init__(self):
                self.x=792
                self.y=612
                self.scroll=5
                self.inout=5
                self.background_fill='on'
            def list(self):
                print '\t x =',self.x
                print '\t y =',self.y
                print '\t scroll =',self.scroll
                print '\t inout =',self.inout
                print '\t background_fill =',self.background_fill
                
            __slots__=['x','y','scroll','inout','background_fill',
                       '_x','_y','_scroll','_inout','_background_fill']
            def _getx(self):
                return self._x
            def _setx(self,value):
                self._x=ValidationFunctions.checkPositiveInt(self,'x',value)
            x=property(_getx,_setx)
            
            def _gety(self):
                return self._y
            def _sety(self,value):
                self._y=ValidationFunctions.checkPositiveInt(self,'y',value)
            y=property(_gety,_sety)

            def _getscroll(self):
                return self._scroll
            def _setscroll(self,value):
                self._scroll=ValidationFunctions.checkPositiveInt(self,'scroll',value)
            scroll=property(_getscroll,_setscroll)
           
            def _getinout(self):
                return self._inout
            def _setinout(self,value):
                self._inout=ValidationFunctions.checkPositiveInt(self,'inout',value)
            inout=property(_getinout,_setinout)
           
            def _getbackground_fill(self):
                return self._background_fill
            def _setbackground_fill(self,value):
                self._background_fill=ValidationFunctions.checkOnOff(self,'background_fill',value)
            background_fill=property(_getbackground_fill,_setbackground_fill)
           
        self.page=Page()
        class Date(object):
            def __init__(self):
                self.reference=0
                class Wrap(object):
                    def __init__(self):
                        self.status='off'
                        self.year=1950
                    def list(self):
                        print '\t\t status =',self.status
                        print '\t\t year =',self.year
                    __slots__=['status','year','_status','_year']
                    
                    def _getstatus(self):
                        return self._status
                    def _setstatus(self,value):
                        self._status=ValidationFunctions.checkOnOff(self,'status',value)
                    status=property(_getstatus,_setstatus)
                    def _getyear(self):
                        return self._year
                    def _setyear(self,value):
                        self._year=ValidationFunctions.checkPositiveInt(self,'year',value)
                    year=property(_getyear,_setyear)
                self.wrap=Wrap()
            def list(self):
                print '\t reference =',self.reference
                print '\t member = wrap'
                self.wrap.list()
            __slots__=['reference','wrap','_reference']
            def _getreference(self):
                return self._reference
            def _setreference(self,value):
                self._reference=ValidationFunctions.checkPositiveInt(self,'reference',value)
            reference=property(_getreference,_setreference)
            
        self.date=Date()
        self.ininit=1
        self.title=''
        self.stitle=''
        self.Graph=[]
        self.ngraph=0
        self.Set=[]
        self.nset=0
        self.Font=[]
        self.nfont=0
        self.Color=[]
        self.ncolor=0
        self.String=[]
        self.nstring=0
        self.Line=[]
        self.nline=0
        self.Box=[]
        self.nbox=0
        self.Ellipse=[]
        self.nellipse=0
        self._version='50102'
        self._pyversion='1.2'
        self.link_page='off'
        self.linewidth=float(linewidth)
        self.linestyle=1
        self.color=color
        self.pattern=pattern
        self.font=font
        self.char_size=char_size
        self.symbol_size=1
        self.sformat='"%16.8g"'
        self.background_color=0
        self.R=[]
        self.nr=0
        class Timestamp(object):
            def __init__(self,parent):
                self.parent=parent
                self.status='off'
                self.x=0.03
                self.y=0.03
                self.color=parent.color
                self.rot=0.
                self.font=parent.font
                self.char_size=parent.char_size
                self.default='"'+time.ctime()+'"' # set to time to time of creation
            def list(self):
                print '\t status =',self.status
                print '\t x =',self.x
                print '\t y =',self.y
                print '\t color =',self.color
                print '\t rot =',self.rot
                print '\t font =',self.font
                print '\t char_size =',self.char_size
                print '\t default =',self.default
            __slots__=['status','x','y','xy','color','rot',
                            'font','char_size','default','parent',
                       '_status','_x','_y','_color','_rot',
                            '_font','_char_size','_default']
            def _getstatus(self):
                return self._status
            def _setstatus(self,value):
                self._status=ValidationFunctions.checkOnOff(self,'status',value)
            status=property(_getstatus,_setstatus)
            def _getx(self):
                return self._x
            def _setx(self,value):
                self._x=ValidationFunctions.checkNumber(self,'x',value)
            x=property(_getx,_setx)
            def _gety(self):
                return self._y
            def _sety(self,value):
                self._y=ValidationFunctions.checkNumber(self,'y',value)
            y=property(_gety,_sety)
            def _getxy(self):
                return ValidationFunctions.getXY(self,'xy')
            def _setxy(self,value):
                ValidationFunctions.checkXY(self,'xy',value)
            xy=property(_getxy,_setxy)
            def _getcolor(self):
                return self._color
            def _setcolor(self,value):
                self._color=ValidationFunctions.checkColor(self,'color',value)
            color=property(_getcolor,_setcolor)
            def _getrot(self):
                return self._rot
            def _setrot(self,value):
                self._rot=ValidationFunctions.checkAngle(self,'rot',value)
            rot=property(_getrot,_setrot)
            def _getfont(self):
                return self._font
            def _setfont(self,value):
                self._font=ValidationFunctions.checkFont(self,'font',value)
            font=property(_getfont,_setfont)
            def _getchar_size(self):
                return self._char_size
            def _setchar_size(self,value):
                self._char_size=ValidationFunctions.checkPositiveNumber(self,'char_size',value)
            char_size=property(_getchar_size,_setchar_size)
            def _getdefault(self):
                return self._default
            def _setdefault(self,value):
                self._default=ValidationFunctions.checkString(self,'default',value)
            default=property(_getdefault,_setdefault)                
        self.timestamp=Timestamp(self)
        self.add_font('Times-Roman')
        self.add_font('Times-Italic')
        self.add_font('Times-Bold')
        self.add_font('Times-BoldItalic')
        self.add_font('Helvetica')
        self.add_font('Helvetica-Oblique')
        self.add_font('Helvetica-Bold')
        self.add_font('Helvetica-BoldOblique')
        self.add_font('Courier')
        self.add_font('Courier-Oblique')
        self.add_font('Courier-Bold')
        self.add_font('Courier-BoldOblique')
        self.add_font('Symbol')
        self.add_font('ZapfDingbats')
        # Set the default colors
##         self.add_color('white',0)
##         self.add_color('black',1)
##         self.add_color('red',2)
##         self.add_color('green',3)
##         self.add_color('blue',4)
##         self.add_color('yellow',5)
##         self.add_color('brown',6)
##         self.add_color('grey',7)
##         self.add_color('violet',8)
##         self.add_color('cyan',9)
##         self.add_color('magenta',10)
##         self.add_color('orange',11)
##         self.add_color('indigo',12)
##         self.add_color('maroon',13)
##         self.add_color('turquoise',14)
##         self.add_color('green4',15)
        for c in self.Color:
            c.list()
        self.add_graph()
        self._start_pipe(xmgrace_args)
    # end of __init__


    def _start_pipe(self,xmgrace_args):
        """Starts the pipe to xmgrace, extra args can be passed (since 1.2)"""
        OPEN_MAX = 64 # copied from C header file sys/syslimits.h ###
        import os
        #xmgrace_args='adummy '+xmgrace_args
        cmd = tuple(xmgrace_args.split())
        # First of all figures out the version of xmgrace
        # if before 5.1.8 no need for -nosafe arg
        nosafe=1
        if self.new_pipe:
            try:
                xmv=os.popen('xmgrace  -version').readlines()
                for l in xmv:
                    if l[:6]=='Grace-':
                        ver=l[6:].split('.')
                        major=int(ver[0])
                        minor=int(ver[1])
                        try:
                            minor_xtra=int(ver[2])
                        except:
                            minor_xtra=0
                        if major+minor/10.<5.1:
                            nosafe=0
                        elif major+minor/10.==5.1:
                            if minor_xtra<8:
                                nosafe=0
            except Exception,err:
                print 'Error trying to identify version\nerror:',err
                pass


        cmd = ('-noask',) + cmd 
        if nosafe: cmd = ('-nosafe',) + cmd
        # Don't exit when our child "grace" exits (which it could if
        # the user clicks on `exit'):
        signal.signal(signal.SIGCHLD, signal.SIG_IGN)
        (self.fd_r, fd_w) = os.pipe()
        if self.pipe_file is None:
            cmd = cmd + ('-dpipe', `self.fd_r`)
        else:
            cmd = cmd + ('-npipe',self.pipe_file)

	# looks like first agr is ignored needs to add a dummy
	cmd = ('dummy',) + cmd
        if self.new_pipe:
            if self.pipe_file is not None:
#                print 'Opening file pipe:',self.pipe_file
                self.pipe=open(self.pipe_file,'w',1)
            self.pid = os.fork()
            if self.pid == 0:
                try:
                    # This whole thing is within a try block to make sure
                    # the child can't escape.
                    for i in range(OPEN_MAX):
                        # close everything except stdin, stdout, stderr
                        # and the read part of the pipe
                        if i not in (self.fd_r,0,1,2):
                            try:
                                import os
                                os.close(i)
                            except OSError:
                                pass
                    try:
                        os.execvp('xmgrace', cmd)
                    except Exception,err:
                        import sys
                        # we have to be careful in the child process.  We
                        # don't want to throw an exception because that would
                        # allow two threads to continue running.
                        sys.stderr.write('GraceProcess: Could not start xmgrace\n')
                        os._exit(1) # exit this forked process but not the parent
                except:
                    import sys
                    sys.stderr.write('Unexpected exception in child!\n')
                    os.exit()
                    os._exit(2) # exit child but not parent

            # We are the parent -> keep only the writeable side of the pipe
            os.close(self.fd_r)
            
        # turn the writeable side into a buffered file object:
        if self.pipe_file is None:
            self.pipe = os.fdopen(fd_w, 'w', 0)
        else:
 #           print 'Opening file pipe 2:',self.pipe_file
            self.pipe=open(self.pipe_file,'w+',1)

        self.ininit=0
        return
        
    def add_r(self):
        self.R.append(REGION(self))
        self.nr=len(self.R)
        return

    def list_font(self):
        lst=[]
        for i in self.Font: lst.append(i.name)
        return lst

    def add_font(self,name):
        self.Font.append(FONT(name))
        self.nfont=len(self.Font)
        return
    
    def add_color(self,name,rgb=None,id=None):
        if id is None:
            id=self.ncolor+16
        self.Color.append(COLOR(name,rgb,id))
        self.ncolor=len(self.Color)
        return
        
    def add_graph(self,ymin=0.,ymax=1.,xmin=0.,xmax=1.):
        self.Graph.append(GRAPH(self,ymin,ymax,xmin,xmax))
        self.add_set(self.ngraph)
        self.ngraph=len(self.Graph)
        return

    def creategraph(self,ymin=0.,ymax=1.,xmin=0.,xmax=1.):
        self.add_graph(ymin=0.,ymax=1.,xmin=0.,xmax=1.)
        return

    def add_set(self,graph=0,color=None):
        self.ngraph=len(self.Graph)
        for i in range(self.ngraph):
            self.Graph[i].nset=len(self.Graph[i].Set)
        if color==None: color=self.Graph[graph].nset+1
        self.Graph[graph].Set.append(DSET(self,color,graph))
        self.Graph[graph].nset=len(self.Graph[graph].Set)
        self.Set.append(self.Graph[graph].Set[-1])
        self.nset=len(self.Set)
        return

    def add_string(self,x,y,text,color=-1,char_size=-1,font=-1,rot=0,just=14):
        if color==-1 : color=self.color
        if char_size==-1 : char_size=self.char_size
        if font==-1 : font=self.font
        self.String.append(STRING(self,x,y,text,color,char_size,font,rot,just))
        self.nstring=len(self.String)
        return
    
    def add_line(self,x1,y1,x2,y2,color=-1,lwidth=-1,lstyl=-1,position=0,atyp=0,algth=2,alyo=[1.,1.]):
        if color==-1 : color=self.color
        if lwidth==-1 : lwidth=self.linewidth
        if lstyl==-1 : lstyl=self.linestyle
        self.Line.append(LINE(self,x1,y1,x2,y2,color,lwidth,lstyl,position,atyp,algth,alyo))
        self.nline=len(self.Line)
        return

    def add_box(self,x1,y1,x2,y2,
                color=-1,lwidth=-1,lstyl=-1,
                fillcolor=-1,fillpattern=-1):
        if color==-1: color=self.color
        if lwidth==-1 : lwidth=self.linewidth
        if lstyl==-1 : lstyl=self.linestyle
        if fillcolor==-1: fillcolor=self.color
        if fillpattern==-1: fillpattern=self.pattern
        
        self.Box.append(BOX_ELLIPSE(self,x1,y1,x2,y2,color,lwidth,lstyl,fillcolor,fillpattern))
        self.nbox=len(self.Box)
        return

    def add_ellipse(self,x1,y1,x2,y2,
                color=-1,lwidth=-1,lstyl=-1,
                fillcolor=-1,fillpattern=-1):
        
        if color==-1: color=self.color
        if lwidth==-1 : lwidth=self.linewidth
        if lstyl==-1 : lstyl=self.linestyle
        if fillcolor==-1: fillcolor=self.color
        if fillpattern==-1: fillpattern=self.pattern
        
        self.Ellipse.append(BOX_ELLIPSE(self,x1,y1,x2,y2,color,lwidth,lstyl,fillcolor,fillpattern))
        self.nellipse=len(self.Box)
        return

    def whichsets(self,*args):
        og=[]
##         print args
        if len(args)==0: args=range(len(self.Graph))
        for ig in args:
            og.append([])
            for iset in range(len(self.Set)):
                if self.Set[iset].graph==ig: og[-1].append(iset)
        if len(og)==1 : og=og[0]
        return og

    def read_parameter(self,parameterfile):
        f=open(parameterfile)
        ln=f.readlines()
        self.pipe.writelines(ln)
        self.flush()
        self('redraw')
        return
    
    def make_parameter(self):
        ln=[]
        ln.append(' version '+self.version+'\n')
        ln.append(' page size '+str(self.page.x)+', '+str(self.page.y)+'\n')
        ln.append(' page scroll '+str(self.page.scroll)+'%\n')
        ln.append(' page inout '+str(self.page.inout)+'%\n')
        ln.append(' link page '+str(self.link_page)+'\n')
	for i in range(self.nfont):
	    nm=self.Font[i].name
	    ln.append(' map font '+str(i)+' to "'+nm+'", "'+nm+'"\n')
	for i in range(self.ncolor):
	    rgb=self.Color[i].rgb
	    r,g,b=str(rgb[0]),str(rgb[1]),str(rgb[2])
            ln.append(' map color '+str(self.Color[i].id)+' to ('+r+', '+g+', '+b+'), "'+self.Color[i].name+'"\n')
        ln.append(' default linewidth '+str(self.linewidth)+'\n')
        ln.append(' default linestyle '+str(self.linestyle)+'\n')
        ln.append(' default color '+self.col(self.color)+'\n')
        ln.append(' default pattern '+str(self.pattern)+'\n')
        ln.append(' default font '+str(self.font)+'\n')
        ln.append(' default char size '+str(self.char_size)+'\n')
        ln.append(' default symbol size '+str(self.symbol_size)+'\n')
        ln.append(' background color '+self.col(self.background_color)+'\n')
        ln.append(' page background fill '+str(self.page.background_fill)+'\n')
        ln.append(' timestamp '+self.timestamp.status+'\n')
        ln.append(' timestamp '+str(self.timestamp.x)+', '+str(self.timestamp.y)+'\n')
        ln.append(' timestamp color '+self.col(self.timestamp.color)+'\n')
        ln.append(' timestamp rot '+str(self.timestamp.rot)+'\n')
        ln.append(' timestamp font '+str(self.timestamp.font)+'\n')
        ln.append(' timestamp char size '+str(self.timestamp.char_size)+'\n')
        ln.append(' timestamp def '+self.timestamp.default+'\n')
        # Adds the strings
	for i in range(self.nstring):
	    s=self.String[i]
	    ln.append(' with string\n')
	    ln.append('  string '+s.status+'\n')
	    ln.append('  string loctype '+s.loctype+'\n')
            x,y=str(s.x),str(s.y)
            ## Warning now the dim are in % of page,
            ## therefore there's a conversion here to make it look ok
            if s.loctype!='world':
                ratio=float(self.page.x)/float(self.page.y)
                if ratio<1.:
                    y=str(eval(y)/ratio)
                else:
                    x=str(eval(x)*ratio)
            else:
                ln.append('  string g'+str(s.graph)+'\n')
	    ln.append('  string '+x+', '+y+'\n')
	    ln.append('  string color '+self.col(s.color)+'\n')
	    ln.append('  string rot '+str(s.rot)+'\n')
	    ln.append('  string font '+str(s.font)+'\n')
	    ln.append('  string just '+str(s.just)+'\n')
	    ln.append('  string char size '+str(s.char_size)+'\n')
	    ln.append('  string def "'+s.text+'"\n')
            
        # Adds the lines
	for i in range(self.nline):
	    s=self.Line[i]
	    ln.append(' with line\n')
	    ln.append('  line '+s.status+'\n')
	    ln.append('  line loctype '+s.loctype+'\n')
            x1,y1,x2,y2=str(s.x1),str(s.y1),str(s.x2),str(s.y2)
            ## Warning now the dim are in % of page,
            ## therefore there's a conversion here to make it look ok
            ratio=float(self.page.x)/float(self.page.y)
            if s.loctype!='world':
                if ratio<1.:
                    y1=str(eval(y1)/ratio)
                    y2=str(eval(y2)/ratio)
                else:
                    x1=str(eval(x1)*ratio)
                    x2=str(eval(x2)*ratio)
            else:
                ln.append('  line g'+str(s.graph)+'\n')
	    ln.append('  line '+x1+', '+y1+', '+x2+', '+y2+'\n')
	    ln.append('  line linewidth '+str(s.linewidth)+'\n')
	    ln.append('  line linestyle '+str(s.linestyle)+'\n')
	    ln.append('  line color '+self.col(s.color)+'\n')
	    ln.append('  line arrow  '+str(s.arrow.position)+'\n')
	    ln.append('  line arrow type '+str(s.arrow.type)+'\n')
	    ln.append('  line arrow length '+str(s.arrow.length)+'\n')
	    ln.append('  line arrow layout '+str(s.arrow.layout[0])+', '+str(s.arrow.layout[1])+'\n')
	    ln.append('  line def \n')
            
        # Adds the boxes
	for i in range(self.nbox):
	    s=self.Box[i]
	    ln.append(' with box\n')
	    ln.append('  box '+s.status+'\n')
	    ln.append('  box loctype '+s.loctype+'\n')
            x1,y1,x2,y2=str(s.x1),str(s.y1),str(s.x2),str(s.y2)
            ## Warning now the dim are in % of page,
            ## therefore there's a conversion here to make it look ok
            ratio=float(self.page.x)/float(self.page.y)
            if s.loctype!='world':
                if ratio<1.:
                    y1=str(eval(y1)/ratio)
                    y2=str(eval(y2)/ratio)
                else:
                    x1=str(eval(x1)*ratio)
                    x2=str(eval(x2)*ratio)
            else:
                ln.append('  box g'+str(s.graph)+'\n')
	    ln.append('  box '+x1+', '+y1+', '+x2+', '+y2+'\n')
	    ln.append('  box linewidth '+str(s.linewidth)+'\n')
	    ln.append('  box linestyle '+str(s.linestyle)+'\n')
	    ln.append('  box color '+self.col(s.color)+'\n')
	    ln.append('  box fill color '+self.col(s.fill.color)+'\n')
	    ln.append('  box fill pattern '+self.col(s.fill.pattern)+'\n')
	    ln.append('  box def \n')
            
        # Adds the ellipses
	for i in range(self.nellipse):
	    s=self.Ellipse[i]
	    ln.append(' with ellipse\n')
	    ln.append('  ellipse '+s.status+'\n')
	    ln.append('  ellipse loctype '+s.loctype+'\n')
            x1,y1,x2,y2=str(s.x1),str(s.y1),str(s.x2),str(s.y2)
            ## Warning now the dim are in % of page,
            ## therefore there's a conversion here to make it look ok
            ratio=float(self.page.x)/float(self.page.y)
            if s.loctype!='world':
                if ratio<1.:
                    y1=str(eval(y1)/ratio)
                    y2=str(eval(y2)/ratio)
                else:
                    x1=str(eval(x1)*ratio)
                    x2=str(eval(x2)*ratio)
            else:
                ln.append('  ellipse g'+str(s.graph)+'\n')
	    ln.append('  ellipse '+x1+', '+y1+', '+x2+', '+y2+'\n')
	    ln.append('  ellipse linewidth '+str(s.linewidth)+'\n')
	    ln.append('  ellipse linestyle '+str(s.linestyle)+'\n')
	    ln.append('  ellipse color '+self.col(s.color)+'\n')
	    ln.append('  ellipse fill color '+self.col(s.fill.color)+'\n')
	    ln.append('  ellipse fill pattern '+self.col(s.fill.pattern)+'\n')
	    ln.append('  ellipse def \n')
            
        # Adds the regions
	for i in range(self.nr):
	    n=str(i)
	    r=self.R[i]
	    ln.append(' r'+n+' type '+str(r.type)+'\n')
            ln.append(' r'+n+' linestyle '+str(r.linestyle)+'\n')
            ln.append(' r'+n+' linewidth '+str(r.linewidth)+'\n')
            ln.append(' r'+n+' color '+self.col(r.color)+'\n')
            if r.type[0:4]=='poly' :
                for l in r.xy:
                    x,y=str(l[0]),str(l[1])
                    ln.append(' r'+n+' xy '+x+', '+y+'\n')
            else:
                l=r.line
                x1,y1,x2,y2=str(l[0]),str(l[1]),str(l[2]),str(l[3])
                ln.append(' r'+n+' line '+x1+', '+y1+', '+x2+', '+y2+'\n')
            ln.append(' link r'+n+' to g'+str(r.link)+'\n')
            ln.append(' r'+n+' '+r.status+'\n')
        def dset(self,s,l,ln):
            ns=str(l)
            ln.append('s'+ns+' hidden '+s.hidden+'\n')
            ln.append('s'+ns+' type '+str(s.type)+'\n')
            ln.append('s'+ns+' symbol '+str(s.symbol.type)+'\n')
            ln.append('s'+ns+' symbol size '+str(s.symbol.size)+'\n')
            ln.append('s'+ns+' symbol color '+self.col(s.symbol.color)+'\n')
            ln.append('s'+ns+' symbol pattern '+str(s.symbol.pattern)+'\n')
            ln.append('s'+ns+' symbol fill color '+self.col(s.symbol.fcolor)+'\n')
            ln.append('s'+ns+' symbol fill pattern '+str(s.symbol.fpattern)+'\n')
            ln.append('s'+ns+' symbol linewidth '+str(s.symbol.linewidth)+'\n')
            ln.append('s'+ns+' symbol linestyle '+str(s.symbol.linestyle)+'\n')
            ln.append('s'+ns+' symbol char '+str(s.symbol.char)+'\n')
            ln.append('s'+ns+' symbol char font '+str(s.symbol.char_font)+'\n')
            ln.append('s'+ns+' symbol skip '+str(s.symbol.skip)+'\n')
            ln.append('s'+ns+' line type '+str(s.line.type)+'\n')
            ln.append('s'+ns+' line linestyle '+str(s.line.linestyle)+'\n')
            ln.append('s'+ns+' line linewidth '+str(s.line.linewidth)+'\n')
            ln.append('s'+ns+' line color '+self.col(s.line.color)+'\n')
            ln.append('s'+ns+' line pattern '+str(s.line.pattern)+'\n')
            ln.append('s'+ns+' baseline type '+str(s.baseline.type)+'\n')
            ln.append('s'+ns+' baseline '+s.baseline.status+'\n')
            ln.append('s'+ns+' dropline '+s.dropline+'\n')
            ln.append('s'+ns+' fill type '+str(s.fill.type)+'\n')
            ln.append('s'+ns+' fill rule '+str(s.fill.rule)+'\n')
            ln.append('s'+ns+' fill color '+self.col(s.fill.color)+'\n')
            ln.append('s'+ns+' fill pattern '+str(s.fill.pattern)+'\n')
            ln.append('s'+ns+' avalue '+s.avalue.status+'\n')
            ln.append('s'+ns+' avalue type '+str(s.avalue.type)+'\n')
            ln.append('s'+ns+' avalue char size '+str(s.avalue.char_size)+'\n')
            ln.append('s'+ns+' avalue font '+str(s.avalue.font)+'\n')
            ln.append('s'+ns+' avalue color '+self.col(s.avalue.color)+'\n')
            ln.append('s'+ns+' avalue rot '+str(s.avalue.rot)+'\n')
            ln.append('s'+ns+' avalue format '+s.avalue.format+'\n')
            ln.append('s'+ns+' avalue prec '+str(s.avalue.prec)+'\n')
            ln.append('s'+ns+' avalue prepend "'+s.avalue.prepend+'"\n')
            ln.append('s'+ns+' avalue append "'+s.avalue.append+'"\n')
            if s.avalue.offset==[]:
                x,y=s.avalue.xoffset,s.avalue.yoffset
            else:
                x,y=s.avalue.offset
            ln.append('s'+ns+' avalue offset '+str(x)+' ,'+str(y)+'\n')
            ln.append('s'+ns+' errorbar '+s.errorbar.status+'\n')
            ln.append('s'+ns+' errorbar color '+str(s.errorbar.color)+'\n')
            ln.append('s'+ns+' errorbar place '+str(s.errorbar.place)+'\n')
            ln.append('s'+ns+' errorbar size '+str(s.errorbar.size)+'\n')
            ln.append('s'+ns+' errorbar linewidth '+str(s.errorbar.linewidth)+'\n')
            ln.append('s'+ns+' errorbar linestyle '+str(s.errorbar.linestyle)+'\n')
            ln.append('s'+ns+' errorbar riser linewidth '+str(s.errorbar.riser.linewidth)+'\n')
            ln.append('s'+ns+' errorbar riser linestyle '+str(s.errorbar.riser.linestyle)+'\n')
            ln.append('s'+ns+' comment "'+s.comment+'"\n')
            ln.append('s'+ns+' legend  "'+s.legend+'"\n')
            return(ln)
	if self.ngraph==0 :
            raise 'Error: NO GRAPH'
	for l in range(self.ngraph):
	    g=self.Graph[l]
	    n=str(l)
	    ln.append(' g'+n+' '+g.status+'\n')
	    ln.append(' g'+n+' hidden '+g.hidden+'\n')
	    ln.append(' g'+n+' type '+g.type+'\n')
	    ln.append(' g'+n+' stacked '+g.stacked+'\n')
	    ln.append(' g'+n+' bar hgap '+str(g.bar_hgap)+'\n')
	    ln.append(' g'+n+' fixedpoint '+g.fixedpoint.status+'\n')
	    ln.append(' g'+n+' fixedpoint type '+str(g.fixedpoint.type)+'\n')
	    ln.append(' g'+n+' fixedpoint xy '+str(g.fixedpoint.xy[0])+', '+str(g.fixedpoint.xy[1])+'\n')
	    a=g.fixedpoint.format
	    ln.append(' g'+n+' fixedpoint format '+a+' '+a+'\n')
	    a=str(g.fixedpoint.prec)
	    ln.append(' g'+n+' fixedpoint prec '+a+', '+a+'\n')                
	    ln.append(' with g'+n+'\n')
            ln.append('     world xmin '+str(g.xmin)+'\n')
            ln.append('     world xmax '+str(g.xmax)+'\n')
            ln.append('     world ymin '+str(g.ymin)+'\n')
            ln.append('     world ymax '+str(g.ymax)+'\n')

            a,b,c,d=g.stack_world
            a,b,c,d=str(a),str(b),str(c),str(d)
	    ln.append('     stack world '+a+', '+b+', '+c+', '+d+'\n')
            ## Warning now the dim are in % of page,
            ## therefore there's a conversion here to make it look ok
            ratio=float(self.page.x)/float(self.page.y)
            if ratio<1.:
                vxmin=g.vxmin
                vxmax=g.vxmax
                vymin=g.vymin/ratio
                vymax=g.vymax/ratio
            else:
                vxmin=g.vxmin*ratio
                vxmax=g.vxmax*ratio
                vymin=g.vymin
                vymax=g.vymax

	    ln.append('     view xmin '+str(vxmin)+'\n')
	    ln.append('     view xmax '+str(vxmax)+'\n')
	    ln.append('     view ymin '+str(vymin)+'\n')
	    ln.append('     view ymax '+str(vymax)+'\n')
            if self.ngraph==1 and self.Graph[0].title=='':
                ln.append('     title "'+self.title+'"\n')
            else:
                ln.append('     title "'+g.title+'"\n')
	    ln.append('     title font '+str(g.tit.font)+'\n')
	    ln.append('     title size '+str(g.tit.size)+'\n')
	    ln.append('     title color '+self.col(g.tit.color)+'\n')
            if self.ngraph==1 and self.Graph[0].stitle=='':
                ln.append('     subtitle "'+self.stitle+'"\n')
            else:
                ln.append('     subtitle "'+g.stitle+'"\n')
	    ln.append('     subtitle font '+str(g.stit.font)+'\n')
	    ln.append('     subtitle size '+str(g.stit.size)+'\n')
	    ln.append('     subtitle color '+self.col(g.stit.color)+'\n')
	    def axe(self,ln,ax,nm):
                import numpy.ma
                if nm[:3]!='alt' : ln.append('     '+nm+'axes scale '+ax.scale+'\n')
                if nm[:3]!='alt' : ln.append('     '+nm+'axes invert '+ax.invert+'\n')
		ln.append('     '+nm+'axis  '+ax.status+'\n')
		ln.append('     '+nm+'axis  type zero '+ax.zero+'\n')
		if ax.offset==[]:
		    ln.append('     '+nm+'axis  offset '+str(ax.xoffset)+', '+str(ax.yoffset)+'\n')
		else:
		    ln.append('     '+nm+'axis  offset '+str(ax.offset[0])+', '+str(ax.offset[1])+'\n')   
		ln.append('     '+nm+'axis  bar '+ax.bar.status+'\n')
		ln.append('     '+nm+'axis  bar color '+self.col(ax.bar.color)+'\n')
		ln.append('     '+nm+'axis  bar linestyle '+str(ax.bar.linestyle)+'\n')
		ln.append('     '+nm+'axis  bar linewidth '+str(ax.bar.linewidth)+'\n')
		ln.append('     '+nm+'axis  label "'+ax.label+'"\n')
		ln.append('     '+nm+'axis  label layout '+ax.lbl.layout+'\n')
                if isinstance(ax.lbl.place.loc,str):
                    ln.append('     '+nm+'axis  label place %s\n' % (ax.lbl.place.loc))
                else:
                    ln.append('     '+nm+'axis  label place spec\n')
                    ln.append('     '+nm+'axis  label place %s, %s\n' % (ax.lbl.place.loc[0],ax.lbl.place.loc[1]))
		ln.append('     '+nm+'axis  label char size '+str(ax.lbl.char_size)+'\n')
		ln.append('     '+nm+'axis  label font '+str(ax.lbl.font)+'\n')
		ln.append('     '+nm+'axis  label color '+self.col(ax.lbl.color)+'\n')
		ln.append('     '+nm+'axis  label place '+str(ax.lbl.place.side)+'\n')
		t=ax.tick
		ln.append('     '+nm+'axis  tick '+t.status+'\n')
		ln.append('     '+nm+'axis  tick major '+str(t.inc)+'\n')
		ln.append('     '+nm+'axis  tick minor ticks '+str(t.minor_ticks)+'\n')
		ln.append('     '+nm+'axis  tick default '+str(t.nsub)+'\n')
		ln.append('     '+nm+'axis  tick place rounded '+t.place_rounded+'\n')
		ln.append('     '+nm+'axis  tick '+t.orientation+'\n')
		ln.append('     '+nm+'axis  tick major size '+str(t.major.size)+'\n')
		ln.append('     '+nm+'axis  tick major color '+self.col(t.major.color)+'\n')
		ln.append('     '+nm+'axis  tick major linewidth '+str(t.major.linewidth)+'\n')
		ln.append('     '+nm+'axis  tick major linestyle '+str(t.major.linestyle)+'\n')
		ln.append('     '+nm+'axis  tick major grid '+t.major.grid+'\n')
		ln.append('     '+nm+'axis  tick minor size '+str(t.minor.size)+'\n')
		ln.append('     '+nm+'axis  tick minor color '+self.col(t.minor.color)+'\n')
		ln.append('     '+nm+'axis  tick minor linewidth '+str(t.minor.linewidth)+'\n')
		ln.append('     '+nm+'axis  tick minor linestyle '+str(t.minor.linestyle)+'\n')
		ln.append('     '+nm+'axis  tick minor grid '+t.minor.grid+'\n')
		ln.append('     '+nm+'axis  ticklabel '+t.label.status+'\n')
		ln.append('     '+nm+'axis  ticklabel prec '+str(t.label.prec)+'\n')
		ln.append('     '+nm+'axis  ticklabel format '+t.label.format+'\n')
		ln.append('     '+nm+'axis  ticklabel append "'+t.label.append+'"\n')
		ln.append('     '+nm+'axis  ticklabel prepend "'+t.label.prepend+'"\n')
		ln.append('     '+nm+'axis  ticklabel angle '+str(t.label.angle)+'\n')
		ln.append('     '+nm+'axis  ticklabel skip '+str(t.label.skip)+'\n')
		ln.append('     '+nm+'axis  ticklabel stagger '+str(t.label.stagger)+'\n')
		ln.append('     '+nm+'axis  ticklabel place '+str(t.label.place)+'\n')
		if t.label.offset!='auto':
		    x,y=t.label.offset
		    ln.append('     '+nm+'axis  ticklabel offset spe\n')
		    ln.append('     '+nm+'axis  ticklabel offset '+str(x)+' ,'+str(y)+'\n')
		else:
		    ln.append('     '+nm+'axis  ticklabel offset auto\n')
		    ln.append('     '+nm+'axis  ticklabel offset 0.00 ,0.01\n')
		if t.label.start=='auto' :
		    ln.append('     '+nm+'axis  ticklabel start type auto\n')
		    ln.append('     '+nm+'axis  ticklabel start 0.000000\n')
		else:
		    ln.append('     '+nm+'axis  ticklabel start type spe\n')
		    ln.append('     '+nm+'axis  ticklabel start '+str(t.label.start)+'\n')
		if t.label.stop=='auto' :
		    ln.append('     '+nm+'axis  ticklabel stop type auto\n')
		    ln.append('     '+nm+'axis  ticklabel stop 0.000000\n')
		else:
		    ln.append('     '+nm+'axis  ticklabel stop type spe\n')
		    ln.append('     '+nm+'axis  ticklabel stop '+str(t.label.stop)+'\n')
		ln.append('     '+nm+'axis  ticklabel char size '+str(t.label.char_size)+'\n')
		ln.append('     '+nm+'axis  ticklabel font '+str(t.label.font)+'\n')
		ln.append('     '+nm+'axis  ticklabel color '+self.col(t.label.color)+'\n')
		ln.append('     '+nm+'axis  tick place '+t.place+'\n')
                if t.type=='zmean':
                    t.type='spec'
##                     t.spec.loc=[-1.,-0.866,-0.5,0.,0.5,0.866,1.]
##                     t.spec.values=['90S','60S','30S','Eq','30N','60N','90N']
                    t.spec.values={-1.:'90S',-0.866:'60S',-0.5:'30S',0.:'Eq',0.5:'30N',0.866:'60N',1.:'90N'}
                if numpy.ma.isMA(t.spec.loc): t.spec.loc=list(t.spec.loc)
		if t.type=='spec' or type(t.spec.loc)==type({}) or type(t.spec.values)==type({}) or (t.spec.loc!=[] and t.spec.values!=[]):
                    if type(t.spec.values)==type({}):
                        mydic=t.spec.values
                        t.spec.loc=[]
                        t.spec.values=[]
                        for ky in mydic.keys():
                            t.spec.loc.append(ky)
                            t.spec.values.append(mydic[ky])
                    if type(t.spec.loc)==type({}):
                        mydic=t.spec.loc
                        t.spec.loc=[]
                        t.spec.values=[]
                        for ky in mydic.keys():
                            t.spec.loc.append(ky)
                            t.spec.values.append(mydic[ky])
                    t.type='spec'
                    ln.append('     '+nm+'axis  tick type '+str(t.type)+' \n')
		    ln.append('     '+nm+'axis  tick type spec\n')
                    ln.append('     '+nm+'axis  tick spec type '+str(t.spec.type)+'\n')
                    ln.append('     '+nm+'axis  tick spec '+str(len(t.spec.loc))+'\n')
                    for i in range(len(t.spec.loc)):
                        if t.spec.values[i]!='':
                            ln.append('     '+nm+'axis  tick major '+str(i)+', '+str(t.spec.loc[i])+'\n')
                            ln.append('     '+nm+'axis  ticklabel '+str(i)+', "'+str(t.spec.values[i])+'"\n')
                        else:
                            ln.append('     '+nm+'axis  tick minor '+str(i)+', '+str(t.spec.loc[i])+'\n')
                else:
                    ln.append('     '+nm+'axis  tick type '+str(t.type)+' \n')
                #ln.append('     alt'+nm+'axis '+ax.altaxis+'\n')
                return ln
	    ax=g.xaxis
	    ln=axe(self,ln,ax,'x')
	    ax=g.yaxis
	    ln=axe(self,ln,ax,'y')
	    ax=g.altxaxis
	    ln=axe(self,ln,ax,'altx')
	    ax=g.altyaxis
	    ln=axe(self,ln,ax,'alty')
            leg=g.legend	    
            ln.append('   legend '+leg.status+'\n')
            ln.append('   legend loctype '+leg.loctype+'\n')
            if leg.loctype!='world':
                ratio=float(self.page.x)/float(self.page.y)
                if ratio<1.:
                    y=str(eval(str(leg.y))/ratio)
                    x=str(leg.x)
                else:
                    x=str(eval(str(leg.x))*ratio)
                    y=str(leg.y)
            else:
                x=str(leg.x)
                y=str(leg.y)
                    
            ln.append('   legend x1 '+x+'\n')
            ln.append('   legend y1 '+y+'\n')
            ln.append('   legend box color '+self.col(leg.box.color)+'\n')
            ln.append('   legend box pattern '+str(leg.box.color)+'\n')
            ln.append('   legend box linewidth '+str(leg.box.linewidth)+'\n')
            ln.append('   legend box linestyle '+str(leg.box.linestyle)+'\n')
            ln.append('   legend box fill color '+self.col(leg.box.fcolor)+'\n')
            ln.append('   legend box fill pattern '+str(leg.box.fpattern)+'\n')
            ln.append('   legend font '+str(leg.font)+'\n')
            ln.append('   legend char size '+str(leg.char_size)+'\n')
            ln.append('   legend color '+self.col(leg.color)+'\n')
            ln.append('   legend length '+str(leg.length)+'\n')
            ln.append('   legend vgap '+str(leg.vgap)+'\n')
            ln.append('   legend hgap '+str(leg.hgap)+'\n')
            ln.append('   legend invert '+leg.invert+'\n')
            frm=g.frame
            ln.append('   frame type '+str(frm.type)+'\n')
            ln.append('   frame linestyle '+str(frm.linestyle)+'\n')
            ln.append('   frame linewidth '+str(frm.linewidth)+'\n')
            ln.append('   frame color '+self.col(frm.color)+'\n')
            ln.append('   frame pattern '+str(frm.pattern)+'\n')
            ln.append('   frame background color '+self.col(frm.background_color)+'\n')
            ln.append('   frame background pattern '+str(frm.background_pattern)+'\n')
    
            for iset in range(g.nset):
                ln=dset(self,g.Set[iset],iset,ln)
        return ln
    
    def plot(self,dat,xs=None,G=None,S=None):
        import numpy
        if isinstance(dat,numpy.ndarray):
            dat=[dat,] # if you passed an array alone then put it in a list
            if not xs is None:
                xs=[xs]
        lister = []
        iS=-1
        iG=0 # which graph
##         if len(dat)!=self.nset:
##             # automatically adds the necessary number of sets
##             for i in range(len(dat)-self.nset):
##                 self.add_set()
        for idat in range(len(dat)):
            y=dat[idat]
            if not isinstance(y,numpy.ma.MaskedArray):
                # it is not a variable, well we'll make one
                y=numpy.ma.array(y)
            sh=list(y.shape)
            if xs is None:
                try:
                    x=y.getAxis(-1)
                except:
                    x=numpy.ma.arange(sh[-1])
            else:
                x=xs[idat]
            if len(sh)==1:
                sh.insert(0,1)
                y=numpy.ma.reshape(y,sh)
            iS=iS+1
            if not G is None:
                iG=G
                iS=S
            else:
                if self.Graph[iG].nset<iS+1:
                    iG=iG+1
                    if iG>self.ngraph:
                        iG=iG-1
                        self.add_set(iG)
                    else:
                        iS=0
            for i in xrange(len(x)):
                if ((y.mask is None) or (y.mask is MV2.nomask)) :
                    lister.append('G'+str(iG)+'.S'+str(iS)+' point '+str(x[i]) + ', ' + str(y[0,i]) + ' \n')
                    for j in range(1,sh[0]):
                        tmp='G'+str(iG)+\
                             '.S'+str(iS)+\
                             '.Y'+str(j)+\
                             '[G'+str(iG)+'.S'+str(iS)+'.LENGTH -1]'+\
                             ' = '+str(y[j,i])\
                             +'\n'
##                              '['+str(i)+']'+\
                        lister.append(tmp)
                else:
                    if y.mask[0,i]==0 :
                        lister.append(
                            'G'+str(iG)+\
                            '.S'+str(iS)+\
                            ' point '+str(x[i]) + ', ' + str(y[0,i]) +\
                            ' \n')
                        for j in range(1,sh[0]):
                            tmp='G'+str(iG)+\
                                 '.S'+str(iS)+\
                                 '.Y'+str(j)+\
                                 '[G'+str(iG)+'.S'+str(iS)+'.LENGTH -1]'+\
                                 ' = '+str(y[j,i])\
                                 +'\n'
                            lister.append(tmp)
                        
        ln=self.make_parameter()
##         f=open('myparameter.par','w')
##         f.writelines(ln)
##         f.writelines(['OK now the dataset\n\n\n\n*****************\n\n\n',])
##         f.writelines(lister)
##         f.close()
        self.pipe.writelines(ln)
        self.pipe.writelines(lister)
        self.flush()
        self.redraw()
        return
        
    def output(self,fnm,out='PostScript'):
        self('hardcopy device "'+out+'"\n')
        self('print to "'+fnm+'"\n')
        self('print\n')
        return
        
#                                                       PostScript
    def postscript(self,fnm,color='color',level='level2',
                   docdata='8bit',xoffset=0,yoffset=0,
                   mediafeed='auto',hwresolution='off',dpi=300):
        out='PostScript'
        # add the extension .ps if necessary
        if fnm[-3:]!='.ps' : fnm=fnm+'.ps'
        # Now sets the options
        self('DEVICE "'+out+'" OP "'+color+'"\n')
        self('DEVICE "'+out+'" OP "'+level+'"\n')
        self('DEVICE "'+out+'" OP "docdata:'+docdata+'"\n')
        self('DEVICE "'+out+'" OP "xoffset:'+str(xoffset)+'"\n')
        self('DEVICE "'+out+'" OP "yoffset:'+str(yoffset)+'"\n')
        self('DEVICE "'+out+'" OP "mediafeed:'+mediafeed+'"\n')
        self('DEVICE "'+out+'" OP "hwresolution:'+hwresolution+'"\n')
        self('DEVICE "'+out+'" DPI '+str(dpi)+'\n')
        self.output(fnm,out)
        return
    
#                                                       PostScript
    def ps(self,*args,**kw):
        self.postscript(*args,**kw)
        return
        
#                                                       JPEG
    def jpeg(self,fnm,color='color',optimize='off',
             quality=75,smoothing=0,baseline='off',
             progressive='on',dct='islow',dpi=72):
        out='JPEG'
        if fnm[-4:]!='.jpg' and fnm[-5:]!='.jpeg' : fnm=fnm+'.jpg'
        # Now sets the options
        self('DEVICE "'+out+'" OP "'+color+'"\n')
        self('DEVICE "'+out+'" OP "optimize:'+optimize+'"\n')
        self('DEVICE "'+out+'" OP "quality:'+str(quality)+'"\n')
        self('DEVICE "'+out+'" OP "smoothing:'+str(smoothing)+'"\n')
        self('DEVICE "'+out+'" OP "baseline:'+baseline+'"\n')
        self('DEVICE "'+out+'" OP "progressive:'+progressive+'"\n')
        self('DEVICE "'+out+'" OP "dct:'+dct+'"\n')
        self('DEVICE "'+out+'" DPI '+str(dpi)+'\n')
        self.output(fnm,out)
        return

#                                                       PDF
    def pdf(self,fnm,pdf='1.3',compression=4,dpi=72):
        if fnm[-4:]!='.pdf' : fnm=fnm+'.pdf'
        out='PDF'
        # Now sets the options
        self('DEVICE "'+out+'" OP "PDF'+str(pdf)+'"\n')
        self('DEVICE "'+out+'" OP "compression:'+str(compression)+'"\n')
        self('DEVICE "'+out+'" DPI '+str(dpi)+'\n')
        self.output(fnm,out)
        return

#                                                       EPS
    def eps(self,fnm,color='color',level='level2',
            bbox='tight',docdata='8bit',dpi=300):
        out='EPS'
        if fnm[-4:]!='.eps' : fnm=fnm+'.eps'
        # Now sets the options
        self('DEVICE "'+out+'" OP "'+color+'"\n')
        self('DEVICE "'+out+'" OP "'+level+'"\n')
        self('DEVICE "'+out+'" OP "bbox:'+bbox+'"\n')
        self('DEVICE "'+out+'" OP "docdata:'+docdata+'"\n')
        self('DEVICE "'+out+'" DPI '+str(dpi)+'\n')
        self.output(fnm,out)
        return

#                                                       MIF
    def mif(self,fnm,dpi=72):
        out='MIF'
        if fnm[-4:]!='.mif' : fnm=fnm+'.mif'
        self('DEVICE "'+out+'" DPI '+str(dpi)+'\n')
        self.output(fnm,out)
        return
        
#                                                       SVG
    def svg(self,fnm,dpi=72):
        out='SVG'
        if fnm[-4:]!='.svg' : fnm=fnm+'.svg'
        self('DEVICE "'+out+'" DPI '+str(dpi)+'\n')
        self.output(fnm,out)
        return

#                                                       PNM
    def pnm(self,fnm,format='ppm',rawbit='on',dpi=72):
        out='PNM'
        if fnm[-4:]!='.pnm' : fnm=fnm+'.pnm'
        # Now sets the options
        self('DEVICE "'+out+'" OP "format:'+format+'"\n')
        self('DEVICE "'+out+'" OP "rawbits:'+rawbit+'"\n')
        self('DEVICE "'+out+'" DPI '+str(dpi)+'\n')
        self.output(fnm,out)
        return

#                                                       PNG
    def png(self,fnm,interlaced='off',transparent='on',
            compression=4,dpi=72):
        out='PNG'
        if fnm[-4:]!='.png' : fnm=fnm+'.png'
        # Now sets the options
        self('DEVICE "'+out+'" OP "interlaced:'+interlaced+'"\n')
        self('DEVICE "'+out+'" OP "transparent:'+transparent+'"\n')
        self('DEVICE "'+out+'" OP "compression:'+str(compression)+'"\n')
        self('DEVICE "'+out+'" DPI '+str(dpi)+'\n')
        self.output(fnm,out)
        return

#                                                       Metafile
    def metafile(self,fnm,dpi=72):
        out='Metafile'
        if fnm[-4:]!='.gmf' : fnm=fnm+'.gmf'
        # Now sets the options
        self('DEVICE "'+out+'" DPI '+str(dpi)+'\n')
        self.output(fnm,out)
        return

    def close(self):
        self.pipe.write('exit\n')
        self.pipe.flush()
        return

    def redraw(self):
        self('redraw')
        return
    
    def update(self):
        ln=self.make_parameter()
        self.pipe.writelines(ln)
        self('redraw')
        return
        
    def command(self, cmd):
        """Issue a command to grace followed by a newline.

        Unless the constructor was called with bufsize=0, this
        interface is buffered, and command execution may be delayed.
        To flush the buffer, either call self.flush() or send the
        command via self(cmd)."""


        try:
            self.pipe.write(cmd + '\n')
        except IOError, err:
            if err.errno == errno.EPIPE:
                self.pipe.close()
                raise Disconnected()
            else:
                raise


    def flush(self):
        """Flush any pending commands to grace."""

        try:
            self.pipe.flush()
        except IOError, err:
            if err.errno == errno.EPIPE:
                # grace is no longer reading from the pipe:
                self.pipe.close()
                raise Disconnected()
            else:
                raise

    def __call__(self,cmd):
        self.command(cmd)
        self.flush()

        
    def __del__(self):
        """If you want to force xmgrace to stay up
        change close_cmd to 'close'
        """
        close_cmd='close'
        if self.is_open():
            try:
                # Ask grace to close its end of the pipe (this also
                # flushes pending commands):
                self.flush()
                self(close_cmd)
            #except Disconnected:
            except:
                print 'XMGRACE has probably been closed manually, pipe broken'
                # Looks like grace has already disconnected.
                #pass
            else:
                if close_cmd=='close' :
                    print 'XMGRACE pipe now closed\nPython object is deleted\nbut XMGRACE hasn''t been closed'
                else:
                    print 'XMGRACE Object deleted, application terminated'
                # Close our end of the pipe (actually, it should be closed
                # automatically when it's deleted, but...):
                self.pipe.close()

    def is_open(self):
        """Return 1 iff the pipe is not known to have been closed."""

        # we could potentially send a kind of null-command to grace
        # here to see if it is really still alive...
        return not self.pipe.closed

    def exit(self):
        """Cause xmgrace to exit.

        Ask xmgrace to exit (i.e., for the program to shut down).  If
        it isn't listening, try to kill the process with a SIGTERM."""

        # First try--ask politely for xmgrace to exit:
        if not self.pipe.closed:
            try:
                self('exit') # this also flushes the queue
            except:
                if 1:
                    print 'XMGRACE has probably been closed manually before, pipe broken'
                # Looks like grace has already disconnected.
                #pass
                # self.pipe will have been closed by whomever
                # generated the exception.
                pass
            else:
                os.waitpid(self.pid, 0)
                self.pipe.close()
                self.pid = None
                return

        # Second try--kill it via a SIGTERM
        if self.pid is not None:
            try:
                os.kill(self.pid, signal.SIGTERM)
            except OSError, err:
                if err.errno == errno.ESRCH:
                    # No such process; it must already be dead
                    self.pid = None
                    return
                else:
                    raise
            os.waitpid(self.pid, 0)
            self.pid = None
        return
    
    def portrait(self):
        if self.page.x>self.page.y:
            tmp=self.page.x
            self.page.x=self.page.y
            self.page.y=tmp
        return
    
    def landscape(self):
        if self.page.y>self.page.x:
            tmp=self.page.x
            self.page.x=self.page.y
            self.page.y=tmp
        return
    
    def orientation(self):
        tmp=self.page.x
        self.page.x=self.page.y
        self.page.y=tmp
        return
    
    def col(self,c):
        import string
        if type(c)!=type(' '):
            return str(c)
        else:
            for ic in range(self.ncolor):
                if string.lower(self.Color[ic].name)==string.lower(c):
                    return str(ic)
        return '1'


    def swap(self,S0,S1,G0=0,G1=0):
        """Swap 2 sets definitions and possibly sets if presents"""
##         print "swaping G",G0,'S',S0,'and G',G1,'.S',S1
        tmp=self.Graph[G0].Set[S0]
        self.Graph[G0].Set[S0]=self.Graph[G1].Set[S1]
        self.Graph[G1].Set[S1]=tmp
        self.__call__("SWAP G"+str(G0)+".S"+str(S0)+" AND G"+str(G1)+".S"+str(S1))
    def move(self,S0,S1,G0=0,G1=0):
        """move sets definitions and possibly set if presents, destination must exist"""
        self.Graph[G1].Set[S1]=self.Graph[G0].Set[S0]
        pop(self.Graph[G0].Set,S0)
        self.nset-=1
        self.__call__("MOVE G"+str(G0)+".S"+str(S0)+" TO G"+str(G1)+".S"+str(S1))
    def copy(self,S0,S1,G0=0,G1=0):
        """copy sets definitions and possibly set if presents, destination must exist"""
        self.Graph[G1].Set[S1]=self.Graph[G0].Set[S0]
        self.__call__("COPY G"+str(G0)+".S"+str(S0)+" TO G"+str(G1)+".S"+str(S1))
        
    
    def list(self):
        print 'XMGRACE version =',self.version
        print 'Python Module Version =',self.pyversion
        print 'member = page'
        self.page.list()
        print 'member = date'
        self.date.list()
        print 'member = timestamp'
        self.timestamp.list()
        print 'title =',self.title
        print 'stitle =',self.stitle
        for i in range(self.ngraph):
            print 'member = Graph['+str(i)+']'
            self.Graph[i].list()
        for i in range(self.nset):
            print 'member = Set['+str(i)+']'
            self.Set[i].list()
        for i in range(self.nstring):
            print 'member = String['+str(i)+']'
            self.String[i].list()
        for i in range(self.nline):
            print 'member = Line['+str(i)+']'
            self.Line[i].list()
        for i in range(self.nbox):
            print 'member = Box['+str(i)+']'
            self.Box[i].list()
        for i in range(self.nellipse):
            print 'member = Ellipse['+str(i)+']'
            self.Ellipse[i].list()
        print 'link_page =',self.link_page
        print 'linewidth =',self.linewidth
        print 'linestyle =',self.linestyle
        print 'color =',self.color
        print 'pattern =',self.pattern
        print 'font =',self.font
        print 'char_size =',self.char_size
        print 'symbol_size =',self.symbol_size
        print 'sformat =',self.sformat
        print 'background_color =',self.background_color
        for i in range(self.nr):
            print 'member = R['+str(i)+']'
            self.R[i].list()
            
    __slots__=['title','stitle','link_page','linewidth','linestyle','color',
               'pattern','font','char_size','symbol_size','sformat',
               'background_color',
               'nset','ngraph','nline','nbox','nr','nellipse',
               'pid','ininit','parent',
               'Box','String','R','Ellipse','page','date',
               'Graph','Set','Line','pyversion',
               'Font','nfont','Color','ncolor','nstring',
               'timestamp','pipe','version',
               '_title','_stitle','_link_page','_linewidth','_linestyle','_color',
               '_pattern','_font','_char_size','_symbol_size','_sformat',
               '_background_color','_version','_pyversion','_nset','pipe_file','new_pipe','fd_r',
               ]

    def _getversion(self):
        return self._version
    version=property(_getversion)
    
    def _getpyversion(self):
        return self._pyversion
    pyversion=property(_getversion)
    
    def _getlinestyle(self):
        return self._linestyle
    def _setlinestyle(self,value):
        self._linestyle=ValidationFunctions.checkLinestyle(self,'linestyle',value)
    linestyle=property(_getlinestyle,_setlinestyle)

    def _getlinewidth(self):
        return self._linewidth
    def _setlinewidth(self,value):
        self._linewidth=ValidationFunctions.checkPositiveNumber(self,'linewidth',value)
    linewidth=property(_getlinewidth,_setlinewidth)

    def _getlink_page(self):
        return self._link_page
    def _setlink_page(self,value):
        self._link_page=ValidationFunctions.checkOnOff(self,'link_page',value)
    link_page=property(_getlink_page,_setlink_page)

    def _getcolor(self):
        return self._color
    def _setcolor(self,value):
        self._color=ValidationFunctions.checkColor(self,'color',value)
    color=property(_getcolor,_setcolor)

    def _getpattern(self):
        return self._pattern
    def _setpattern(self,value):
        self._pattern=ValidationFunctions.checkPattern(self,'pattern',value)
    pattern=property(_getpattern,_setpattern)

    def _gettitle(self):
        return self._title
    def _settitle(self,value):
        self._title=ValidationFunctions.checkString(self,'title',value)
    title=property(_gettitle,_settitle)
    
    def _getstitle(self):
        return self._stitle
    def _setstitle(self,value):
        self._stitle=ValidationFunctions.checkString(self,'stitle',value)
    stitle=property(_getstitle,_setstitle)
    
    def _getfont(self):
        return self._font
    def _setfont(self,value):
        self._font=ValidationFunctions.checkFont(self,'font',value)
    font=property(_getfont,_setfont)

    def _getchar_size(self):
        return self._char_size
    def _setchar_size(self,value):
        self._char_size=ValidationFunctions.checkPositiveNumber(self,'char_size',value)
    char_size=property(_getchar_size,_setchar_size)

    def _getsymbol_size(self):
        return self._symbol_size
    def _setsymbol_size(self,value):
        self._symbol_size=ValidationFunctions.checkPositiveInt(self,'symbol_size',value)
    symbol_size=property(_getsymbol_size,_setsymbol_size)

    def _getsformat(self):
        return self._sformat
    def _setsformat(self,value):
        self._sformat=ValidationFunctions.checkString(self,'sformat',value)
    sformat=property(_getsformat,_setsformat)

    def _getbackground_color(self):
        return self._background_color
    def _setbackground_color(self,value):
        self._background_color=ValidationFunctions.checkPositiveInt(self,'background_color',value)
    background_color=property(_getbackground_color,_setbackground_color)

## PropertiedClasses.set_property(init,'Set',
##                                acts=ValidationFunctions.setSet,
##                                actg=ValidationFunctions.getSet,
##                                )

"""A python replacement for grace_np.c, a pipe-based interface to xmgrace.

Copyright (C) 1999 Michael Haggerty

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.  This program is distributed in the
hope that it will be useful, but WITHOUT ANY WARRANTY; without even
the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
PURPOSE.  See the GNU General Public License for more details; it is
available at <http://www.fsf.org/copyleft/gpl.html>, or by writing to
the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.

Written by Michael Haggerty <mhagger@blizzard.harvard.edu>.  Based on
the grace_np library distributed with grace, which was written by
Henrik Seidel and the Grace Development Team.

Grace (xmgrace) is a very nice X program for doing 2-D graphics.  It
is very flexible, produces beautiful output, and has a graphical user
interface.  It is available from
<http://plasma-gate.weizmann.ac.il/Grace/>.  Grace is the successor to
ACE/gr and xmgr.

This module implements a pipe-based interface to grace similar to the
one provided by the grace_np library included with grace.  I haven't
used it very much so it is likely that it still has bugs.  I am
releasing it in the hope that it might be of use to the community.  If
you find a problem or have a suggestion, please let me know at
<mhagger@blizzard.harvard.edu>.  Other feedback is also welcome.

For a demonstration, run this file by typing `python grace_np.py'.
See the bottom of the file to see how the demonstration is programmed.

About the module:

At first I tried just to translate grace_np from C to python, but then
I realized that it is possible to do a much nicer job using classes.
The main class here is GraceProcess, which creates a running copy of
the grace program, and creates a pipe connection to it.  Through an
instance of this class you can send commands to grace.

Note that grace interprets command streams differently depending on
their source.  The pipe represented by this class is connected in such
a way that grace expects `parameter-file'-style commands (without the
@ or & or whatever).

[Details: this class communicates to grace through a -dpipe which
specified an open file descriptor from which it is to read commands.
This is the same method used by the grace_np that comes with grace.  I
thought that the -pipe option might be more convenient--just pipe
commands to standard input.  However, grace interprets commands
differently when it receives them from these two sources: -dpipe
expects parameter-file information, whereas -pipe expects datafile
information.  Also -pipe doesn't seem to respond to the `close'
command (but maybe the same effect could be had by just closing the
pipe).]

"""

