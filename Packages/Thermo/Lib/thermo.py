# Adapted for numpy/ma/cdms2 by convertcdms.py
import vcs
import MV2,cdms2
import unidata,genutil
from vcs import VCS_validation_functions
thermo_objects=[]
import numpy


def Es(T,method=None):
    """Computes saturated pressure in Pa given T in K, using the method:
    1: Hyland-Wexler formulation, polynomial coeff (absolute norm)
    2: Wexler formulation
    3: Hyland-Wexler formulation, polynomial coeff (relative error norm)
    4: classic Goff Gratch equation
    5: 6.112*numpy.ma.exp(17.67*tempc/(tempc+243.5))

    Default is method 1
    
    Note: 1 and 2 use method 3 where T is not : 173.15 < T < 473.15
    ref for 1, 2 and 3:
    Piotr Flatau and al., Journal of Applied Met., Vol 31, Dec 1992 ( http://ams.allenpress.com/perlserv/?request=get-document&doi=10.1175%2F1520-0450(1992)031%3C1507%3APFTSVP%3E2.0.CO%3B2&ct=1 )
    """
    if method is None:
        method = 1
        
    if method == 1:
        ## Put in C
        x= T-273.15
        ## Water vapor
        c0 = 0.611220713E03
        c1 = 0.443944344E02
        c2 = 0.143195336E01  
        c3 = 0.263350515E-01
        c4 = 0.310636053E-03
        c5 = 0.185218710E-05
        c6 = 0.103440324E-07
        c7 =-0.468258100E-10
        c8 = 0.466533033E-13
        eswat=c0+x*(c1+x*(c2+x*(c3+x*(c4+x*(c5+x*(c6+x*(c7+x*c8)))))))
        ## ice
        c0 = .611153246E03
        c1 = .503261230E02
        c2 = .188595709E01
        c3 = .422115970E-01
        c4 = .620376691E-03
        c5 = .616082536E-05
        c6 = .405172828E-07
        c7 = .161492905E-09
        c8 = .297886454E-12
        esice=c0+x*(c1+x*(c2+x*(c3+x*(c4+x*(c5+x*(c6+x*(c7+x*c8)))))))
        ## Combine
        es = MV2.where(MV2.less(T,273.15),esice,eswat)
        ## Overwrite values outside valid range with method 2
        mn,mx = genutil.minmax(T)
        if mn<173.16 or mx>473.15:
            es2 = Es(T,method=2)
            es = MV2.where(MV2.less(T,173.16),es2,es)
            es = MV2.where(MV2.greater(T,473.15),es2,es)
    elif method == 2:
        # over water
        g0 = -0.29912729E4
        g1 = -0.60170128E4
        g2 =  0.1887643854E2
        g3 = -0.28354721E-1
        g4 =  0.17838301E-4
        g5 = -0.84150417E-9
        g6 =  0.44412543E-12
        g7 =  0.2858487E1
        # over ice
        k0 = -0.58653696e4
        k1 =  0.2224103300E2
        k2 =  0.13749042E-1
        k3 = -0.34031775E-4
        k4 =  0.26967687E-7
        k5 =  0.6918651
        esice = (k0+(k1+k5*MV2.log(T)+(k2+(k3+k4*T)*T)*T)*T)/T # over ice
        eswat = (g0+(g1+(g2+g7*MV2.log(T)+(g3+(g4+(g5+g6*T)*T)*T)*T)*T)*T)/T**2 # over water
        es = MV2.where(MV2.less(T,273.15),esice,eswat)
        es = MV2.exp(es)
    elif method == 3:
        ## Put in C
        x= T-273.15
        ## Water vapor
        c0 = 0.611213476E03
        c1 = 0.444007856E02
        c2 = 0.143064234E01  
        c3 = 0.264461437E-01
        c4 = 0.305930558E-03
        c5 = 0.196237241E-05
        c6 = 0.892344772E-08
        c7 =-0.373208410E-10
        c8 = 0.209339997E-13
        eswat=c0+x*(c1+x*(c2+x*(c3+x*(c4+x*(c5+x*(c6+x*(c7+x*c8)))))))
        ## ice
        c0 = .611123516E03
        c1 = .503109514E02
        c2 = .1888369801E01
        c3 = .420547422E-01
        c4 = .614396778E-03
        c5 = .602780717E-05
        c6 = .387940929E-07
        c7 = .149436277E-09
        c8 = .262655803E-12
        esice=c0+x*(c1+x*(c2+x*(c3+x*(c4+x*(c5+x*(c6+x*(c7+x*c8)))))))
        ## Combine
        es = MV2.where(MV2.less(T,273.15),esice,eswat)
        ## Overwrite values outside valid range with method 2
        mn,mx = genutil.minmax(T)
        if mn<173.16 or mx>473.15:
            es2 = Es(T,method=2)
            es = MV2.where(MV2.less(T,173.16),es2,es)
            es = MV2.where(MV2.greater(T,473.15),es2,es)
    elif method == 4:
        est=101324.6 #Pa
        Ts=373.16/T
        a=-7.90298
        b=5.02808
        c=-1.3816E-7
        d=11.344
        f=8.1328E-3
        h=-3.49149
        maxexp=int(numpy.log10(numpy.finfo(numpy.float).max))
        minexp=1-a
        es=a*(Ts-1.)
        es=es+b*numpy.ma.log10(Ts)
        A=d*(1.-Ts)
        A=numpy.ma.masked_greater(A,maxexp)
        A=numpy.ma.masked_less(A,minexp)       
        es=es+c*(numpy.ma.power(10,A)-1.)
        A=h*(Ts-1.)
        A=numpy.ma.masked_greater(A,maxexp)
        A=numpy.ma.masked_less(A,minexp)       
        es=es+f*(numpy.ma.power(10,A)-1.)
        es = est*numpy.ma.power(10,es)
    elif method == 5:
        tempc = T-273.15
        es = 611.2*numpy.ma.exp(17.67*tempc/(tempc+243.5))
    return es

def getPvalues(T):
    """computes pressure values from level axis,
    if not designated level then use last axis...
    """
    P = T.getLevel()
    if P is None:
        P=T.getAxis(-1)
    P = P.clone()
    try: # tries to convert to Pa
        for i in range(len(P)):
            u = unidata.udunits(P[i],P.units)
            P[i] = u.to("Pa").value
    except Exception,err:
        pass
    return P[:]
    

def Ws(T,P=None,method=None):
    """ Compute saturated mixing ratio at T,P, cpomputing es using the method:
    T in K
    P in Pa
    """
    es=Es(T,method)
    if P is None:
        P=getPvalues(T)
        
    return .622*es/(P-es)

def gammaw(T,rh,P=None,method=None):
    """
    Function to calculate the moist adiabatic lapse rate (K/Pa) based
    on the temperature, pressure, and rh of the environment.
    T in K
    rh in %
    P in Pa
    if P is not passed will be optaines from T level axis (or last axis)
    """
    if P is None:
        P=getPvalues(T)
    ws=Ws(T,P,method=method)
    w=rh*ws/100
    Tv=T*(1.0+0.6*w)
    latent=1000*(2502.2-2.43089*(T-273.15))
    A=1.0+latent*ws/(287*T)
    B=1.0+0.622*latent*latent*ws/(1005*287*T*T)
    Density=P/(287*Tv) # in SI
    lapse=(A/B)/(1005.*Density)
    return lapse

def  LiftWet(To,Po,Pend,deltaP=1000.,method=None):
    """
    Lift a parcel moist adiabatically from Po to Pend.
    Initial temperature is To in K.
    To in K
    Po in Pa
    detail # number to increment P slowly
    returns T at all P (every deltaP Pa), and P
    """
    if method is None:
        method=5 # MUCH faster
    temp=[To,]
    pres=[Po,]
    while (pres[-1]-deltaP >= Pend ) :
        tmp=temp[-1]-deltaP*gammaw(temp[-1],100,pres[-1]-deltaP/2,method=method)
        temp.append(tmp)
        pres.append(pres[-1]-deltaP)
    return MV2.array(temp,id='T'),MV2.array(pres,id='P')



def Dewpoint(T,rh,method=None):
    """Computes dewpoint profile from T and rh
    Usage:
      Td = Dewpoint(T,rh,method=None)
    Where:
     T is in K
     rh is in % (relative humidity)
     method used to compute Es:
         1: Hyland-Wexler formulation, polynomial coeff (absolute norm) (default)
         2: Wexler formulation
         3: Hyland-Wexler formulation, polynomial coeff (relative error norm)
         4: classic Goff Gratch equation
         5: 6.112*numpy.ma.exp(17.67*tempc/(tempc+243.5))


     
    Assumes constant latent temperature (except over ice or water)    
    """
    To = 273.15     # K
    Rv = 461.53     # J/(K*kg)
    eo = 611        # Pa
    L = MV2.ones(T.shape)*2.502E6   # J/kg
    L = MV2.where(MV2.less(T,To),2.83E6,L)   # J/kg
    es = Es(T,method=method) # in Pa
    e = rh*es/100.
    Td = 1./(1./To - Rv/L*(e/eo))
    Td.setAxisList(T.getAxisList())
    return Td

class Gth(object):
    
    def TP2XY(self,T,P):
        x,y = self._TP2XY(T,P)
        try:
            x=float(x)
            y=float(y)
        except:
            pass
        return x,y
    
    def XY2TP(self,X,Y):
        t,p = self._XY2TP(X,Y)
        try:
            t=float(t)
            p=float(p)
        except:
            pass
        return t,p

        
    def setdiagramdefs(self):
        # Figures out which kind of diagram we're drawing
        if self.type[:2]=='sk':
            self._TP2XY=self.TP2XYskewT
            self._XY2TP=self.XY2TPskewT
        elif self.type[:2]=='em':
            self._TP2XY=self.TP2XYemagram
            self._XY2TP=self.XY2TPemagram
        elif self.type[:2]=='te':
            self._TP2XY=self.TP2XYtephigram
            self._XY2TP=self.XY2TPtephigram
        elif self.type[:2]=='st':
            self._TP2XY=self.TP2XYstuve
            self._XY2TP=self.XY2TPstuve
        elif self.type[:2]=='cu':
            # user defined diagram
            pass
        else:
            raise ValueError,'Error type: '+repr(self.type)+' is not a valid diagram type'
        
        ## Gets the xs for each corner
        dwx1=self.datawc_x1+273.16
        dwx2=self.datawc_x2+273.16
        dwy1=self.datawc_y1*100.
        dwy2=self.datawc_y2*100.
        self.xmin,self.ymin=self.TP2XY(dwx1,dwy1)
        self.xmax,dum=self.TP2XY(dwx2,dwy1)
        dum,self.ymax=self.TP2XY(dwx2,dwy2)
        

    def __del__(self):
        self.x.removeobject(self._isotherms)
        self.x.removeobject(self._isothermsfilled)
        self.x.removeobject(self._isobars)
        self.x.removeobject(self._dryadiabats)
        self.x.removeobject(self._pseudoadiabats)
        self.x.removeobject(self._mixingratio)
        self.x.removeobject(self_line)
        
    def __init__(self,x=None,type='skewT',name='default'):
        self.displays = []
        if x is None:
            self.x=vcs.init()
        else:
            self.x=x

##         if name in thermo_objects:
##             raise Exception,"Error thermo object %s already exists" % name
##         thermo_objects.append(name)
        self.gname='Gth'
        self._name=name
        ## Set some constants
        self.k=0.286
        self.R=287.04
        self.cp=1004.
##         self.L=2.5E3

        ## World coordiantes in C/hPa
        self._datawc_x1=-25.
        self._datawc_x2=50.
        self._datawc_y1=1000.
        self._datawc_y2=100.
        
        ## Skewness (for skewT)
        self._skewness = -35.
        
        ## Sets the diagram type
        self._type=type
        self.setdiagramdefs()


        ## Level of detail
        self._detail=25

        ## Draw mixing ratio and pseudoadiabats up to
        self._Pmaxmixingratio=200

        ## Do we draw all the lines on the diagram ? (default, yes)
        self._drawisothermsfilled=1
        self._drawisotherms=1
        self._drawisobars=1
        self._drawdryadiabats=1
        self._drawpseudoadiabats=1
        self._drawmixingratio=1
        
        # Creates the isolines
        self._isotherms=self.x.createisoline()
        self._isobars=self.x.createisoline()
        self._dryadiabats=self.x.createisoline()
        self._pseudoadiabats=self.x.createisoline()
        self._mixingratio=self.x.createisoline()
        self._isothermsfilled=self.x.createisofill()

        # Isofill stuff for temperatures
        self._isothermsfilled.levels=[[-200,-190],[-180,-170],[-160,-150],[-140,-130],[-120,-110],[-100,-90],[-80,-70],[-60,-50],[-40,-30],[-20,-10],[0,10],[20,30],[40,50],[60,70],[80,90],[100,110],[120,130],[140,150],[160,170],[180,190],[200,210]]
##         self.isothermsfilled.levels=([-240.0, -200.0], [-160.0, -120.0], [-80.0, -40.0], [0.0, 40.0], [80.0, 120.0])
        self._isothermsfilled.fillareacolors=[253,]*50
        self._isothermsfilled.xticlabels1={}
        self._isothermsfilled.yticlabels1={}

        # Isotherms
        self._isotherms.level=list(range(-200,200,10))
        self._isotherms.linecolors=[249,]
        self._isotherms.line=['solid']
        self._isotherms.label='y'

        # Isobars default settings
        self._isobars.linecolors=[249]
        self._isobars.line=['solid']
        self._isobars.label='n'

        # Dry adiabatics
        self._dryadiabats.label='y'
        self._dryadiabats.linecolors=[249,]
        self._dryadiabats.line=['solid']
##         self._dryadiabats.level=range(-40,170,10)
        
        # Pseudoadiabatics
        self._pseudoadiabats.linecolors=[243,]
        self._pseudoadiabats.line=['solid']
        self._pseudoadiabats.label='y'
##         self._pseudoadiabats.level=vcs.mkevenlevels(-40,40,40)

        # Mixing ratio
        self._mixingratio.line=['long-dash']
        self._mixingratio.linecolors=[243,]
        self._mixingratio.level=[.1,.2,.4,.6,.8,1.,1.5,2.,2.5,
                                3,4,5,6,7,8,9,10,12,14,16,18,20,
                                24,28,32,36,40,44,48,52,56,60,68]
        self._mixingratio.label='y'

        # Windbarb
        self._windbarbsscales=[25.,10.,5.]

        # Lines for plot
        self._line=self.x.createline()
        


    def list(self):
        print '----------ThermodynamicDiagrma (Gth) member (attribute) listings----------'
        print 'graphic method = Gth'
        print 'name =',self.name
        print 'type =',self.type
        if self.type[:2]=='sk':
            print 'skewness:',self.skewness
        print 'detail =',self.detail
        print 'datawc_x1 =',self.datawc_x1
        print 'datawc_x2 =',self.datawc_x2
        print 'datawc_y1 =',self.datawc_y1
        print 'datawc_y2 =',self.datawc_y2
        print 'Pmaxmixingratio =',self.Pmaxmixingratio
        print 'isotherms =',self.isotherms.name
        print 'drawisotherms =',self.drawisotherms
        print 'isothermsfilled =',self.isothermsfilled.name
        print 'drawisothermsfilled =',self.drawisothermsfilled
        print 'isobars =',self.isobars.name
        print 'drawisobars =',self.drawisobars
        print 'dryadiabats =',self.dryadiabats.name
        print 'drawdryadiabats =',self.drawdryadiabats
        print 'pseudoadiabats =',self.pseudoadiabats.name
        print 'drawpseudoadiabats =',self.drawpseudoadiabats
        print 'mixingratio =',self.mixingratio.name
        print 'drawmixingratio =',self.drawmixingratio
##         print 'windbarbs =',self.windbarbs.name
        print 'windbarbsscales =',self.windbarbsscales
        print 'linecolor',self.line.color[0]
        print 'linewidth',self.line.width[0]
        print 'linetype',self.line.type[0]

    __slots__=[
       '_TP2XY',
       '_XY2TP',
##        'TP2XYskewT',
##        'TP2XYemagram',
##        'TP2XYtephigram',
##        'TP2XYstuve',
       'gname',
       'name',
       '_name',
       'displays',
       '_displays',
       'detail',
       '_detail',
       'R',
       'cp',
       'k',
       'datawc_x1',
       'datawc_x2',
       'datawc_y1',
       'datawc_y2',
       '_datawc_x1',
       '_datawc_x2',
       '_datawc_y1',
       '_datawc_y2',
       'type',
       '_type',
       'xmin',
       'xmax',
       'ymin',
       'ymax',
       'x',
       'Pmaxmixingratio',
       'isotherms',
       'drawisotherms',
       'isothermsfilled',
       'drawisothermsfilled',
       'isobars',
       'drawisobars',
       'dryadiabats',
       'drawdryadiabats',
       'pseudoadiabats',
       'drawpseudoadiabats',
       'mixingratio',
       'drawmixingratio',
       'windbarbs',
       'windbarbsscales',
       'skewness',
       'linecolor',
       'linewidth',
       'linetype',
       'line',
       'linecolor',
       'linewidth',
       'linetype',
       '_Pmaxmixingratio',
       '_isotherms',
       '_drawisotherms',
       '_isothermsfilled',
       '_drawisothermsfilled',
       '_isobars',
       '_drawisobars',
       '_dryadiabats',
       '_drawdryadiabats',
       '_pseudoadiabats',
       '_drawpseudoadiabats',
       '_mixingratio',
       '_drawmixingratio',
       '_windbarbs',
       '_windbarbsscales',
       '_skewness',
       '_line',
       '_linecolor',
       '_linewidth',
       '_linetype',
       ]

        
    def _getskewness(self):
        return self._skewness
    def _setskewness(self,value):
        VCS_validation_functions.checkName(self,'skewness',value)
        if self.type[:2]!='sk':
            raise ValueError,'skewness can only be set for skewT type diagrams'
        value=VCS_validation_functions.checkIntFloat(self,'skewness',value)
        self._skewness=value
        self.setdiagramdefs()
    skewness=property(_getskewness,_setskewness,None,'Skewness of skewT diagram')
        
    def _getwindbarbsscales(self):
        return self._windbarbsscales
    def _setwindbarbsscales(self,value):
        value=VCS_validation_functions.checkListOfNumbers(self,'windbarbsscales',value,minelements=3,maxelements=3)
        self._windbarbsscales=value
    windbarbsscales=property(_getwindbarbsscales,_setwindbarbsscales,None,'WindBarbs scaless: Triangles, Full Barbs, Half Barb\ndefault is [25,10,5]')

    def _getdrawpseudoadiabats(self):
        return self._drawpseudoadiabats
    def _setdrawpseudoadiabats(self,value):
        value = VCS_validation_functions.checkOnOff(self,'drawpseudoadiabats',value)
        self._drawpseudoadiabats=value
    drawpseudoadiabats=property(_getdrawpseudoadiabats,_setdrawpseudoadiabats,None,'Draw the pseudo-adiabats ? (1/0) or (y/n)\nAlso called moist-adiabats')
        
    def _getdrawmixingratio(self):
        return self._drawmixingratio
    def _setdrawmixingratio(self,value):
        value = VCS_validation_functions.checkOnOff(self,'drawmixingratio',value)
        self._drawmixingratio=value
    drawmixingratio=property(_getdrawmixingratio,_setdrawmixingratio,None,'Draw the pseudo-adiabats ? (1/0) or (y/n)\nAlso called moist-adiabats')
        
    def _getdrawdryadiabats(self):
        return self._drawdryadiabats
    def _setdrawdryadiabats(self,value):
        value = VCS_validation_functions.checkOnOff(self,'drawdryadiabats',value)
        self._drawdryadiabats=value
    drawdryadiabats=property(_getdrawdryadiabats,_setdrawdryadiabats,None,'Draw the dryadiabats ? (1/0) or (y/n)')
        
    def _getdrawisobars(self):
        return self._drawisobars
    def _setdrawisobars(self,value):
        value = VCS_validation_functions.checkOnOff(self,'drawisobars',value)
        self._drawisobars=value
    drawisobars=property(_getdrawisobars,_setdrawisobars,None,'Draw the isobars ? (1/0) or (y/n)')
        
    def _getdrawisothermsfilled(self):
        return self._drawisothermsfilled
    def _setdrawisothermsfilled(self,value):
        value = VCS_validation_functions.checkOnOff(self,'drawisothermsfilled',value)
        self._drawisothermsfilled=value
    drawisothermsfilled=property(_getdrawisothermsfilled,_setdrawisothermsfilled,None,'Draw the isothermsfilled ? (1/0) or (y/n)')
    
    def _getdrawisotherms(self):
        return self._drawisotherms
    def _setdrawisotherms(self,value):
        value = VCS_validation_functions.checkOnOff(self,'drawisotherms',value)
        self._drawisotherms=value
    drawisotherms=property(_getdrawisotherms,_setdrawisotherms,None,'Draw the isotherms ? (1/0) or (y/n)')
        
    def _getline(self):
        return self._line
    def _setline(self,value):
        value=VCS_validation_functions.checkLine(self,'line',value)
        if isinstance(value,str):
            self._line=self.x.getline(value)
        else:
            self._line=value
    line=property(_getline,_setline,None,'Line graphic method to use to draw the profile\nUse line.list() to see attributes')
    
    def _getlinecolor(self):
        return self._line.color
    def _setlinecolor(self,value):
        value=VCS_validation_functions.checkColor(self,'linecolor',value)
        self._line.color=[value]
    linecolor=property(_getlinecolor,_setlinecolor,None,'Color Line graphic method to use to draw the profile\nUse line.list() to see attributes')
    
    def _getlinewidth(self):
        return self._line.width
    def _setlinewidth(self,value):
        if not isinstance(value,(int,float,long)):
            raise 'Error width must be a number'
        if not 1<value<300:
            raise 'Error width must be between 1 and 300'
        self._line.width=[value]
    linewidth=property(_getlinewidth,_setlinewidth,None,'Width Line graphic method to use to draw the profile\nUse line.list() to see attributes')
    
    def _getlinetype(self):
        return self._line.type
    def _setlinetype(self,value):
        self._line.type=[value]
    linetype=property(_getlinetype,_setlinetype,None,'Type Line graphic method to use to draw the profile\nUse line.list() to see attributes')
    
    def _getwindbarbs(self):
        return self._windbarbs
    def _setwindbarbs(self,value):
        value=VCS_validation_functions.checkLine(self,'windbarbs',value)
        self._windbarbs=value
    windbarbs=property(_getwindbarbs,_setwindbarbs,None,'Line graphic method to use to draw the windbarbs\nUse windbarbs.list() to see attributes')
    
    def _getdryadiabats(self):
        return self._dryadiabats
    def _setdryadiabats(self,value):
        value=VCS_validation_functions.checkIsoline(self,'dryadiabats',value)
        self._dryadiabats=value
    dryadiabats=property(_getdryadiabats,_setdryadiabats,None,'Isoline graphic method to use to draw the dryadiabats\nUse dryadiabats.list() to see attributes')
    def _getpseudoadiabats(self):
        return self._pseudoadiabats
    def _setpseudoadiabats(self,value):
        value=VCS_validation_functions.checkIsoline(self,'pseudoadiabats',value)
        self._pseudoadiabats=value
    pseudoadiabats=property(_getpseudoadiabats,_setpseudoadiabats,None,'Isoline graphic method to use to draw the pseudoadiabats\nUse pseudoadiabats.list() to see attributes')
    def _getmixingratio(self):
        return self._mixingratio
    def _setmixingratio(self,value):
        value=VCS_validation_functions.checkIsoline(self,'mixingratio',value)
        self._mixingratio=value
    mixingratio=property(_getmixingratio,_setmixingratio,None,'Isoline graphic method to use to draw the mixingratio\nUse mixingratio.list() to see attributes')
    def _getisobars(self):
        return self._isobars
    def _setisobars(self,value):
        value=VCS_validation_functions.checkIsoline(self,'isobars',value)
        self._isobars=value
    isobars=property(_getisobars,_setisobars,None,'Isoline graphic method to use to draw the isobars\nUse isobars.list() to see attributes')
    def _getisothermsfilled(self):
        return self._isothermsfilled
    def _setisothermsfilled(self,value):
        value=VCS_validation_functions.checkIsofill(self,'isothermsfilled',value)
        self._isothermsfilled=value
    isothermsfilled=property(_getisothermsfilled,_setisothermsfilled,None,'Isofill graphic method to use to draw the isothermsfilled\nUse isothermsfilled.list() to see attributes')
    def _getisotherms(self):
        return self._isotherms
    def _setisotherms(self,value):
        value=VCS_validation_functions.checkIsoline(value)
        self._isotherms=value
    isotherms=property(_getisotherms,_setisotherms,None,'Isoline graphic method to use to draw the isotherms\nUse isotherms.list() to see attributes')
    def _getPmaxmixingratio(self):
        return self._Pmaxmixingratio
    def _setPmaxmixingratio(self,value):
        value=VCS_validation_functions.checkIntFloat(self,'Pmaxmixingratio',value)
        self._Pmaxmixingratio=value
    Pmaxmixingratio=property(_getPmaxmixingratio,_setPmaxmixingratio,None,'Pressure after which mixing ratio and pseudoadiats curves are not drawn anymore (hPa)')
    def _getdetail(self):
        return self._detail
    def _setdetail(self,value):
        value=VCS_validation_functions.checkIntFloat(self,'detail',value)
        self._detail=value
    detail=property(_getdetail,_setdetail,None,'Smoothness of contours/graphics\nhigher value means smoother (and slower) plots')
    def _gettype(self):
        return self._type
    def _settype(self,value):
        VCS_validation_functions.checkName(self,'type',value)
        if not isinstance(value,str):
            raise ValueError, 'Type should be a string'
        value=value.lower()
        if not value[:2] in ['sk','te','st','cu','em']:
            raise ValueError, 'Error Thermodynamic Diagram type must skewT, Tephigram, Emmagram, Stuve or Custom'
        self._type=value
        self.setdiagramdefs()
    type=property(_gettype,_settype,None,'Thermodynamic Diagram type, one of: skewT, Tephigram, Emmagram, Stuve or Custom')
    
    def _getname(self):
        return self._name
    def _setname(self,value):
        value=VCS_validation_functions.checkname(self,'name',value)
        self._name=value
    name=property(_getname,_setname,None,'Name of the Graphic method')
    
    def _getdatawc_x1(self):
        return self._datawc_x1
    def _setdatawc_x1(self,value):
        value=VCS_validation_functions.checkIntFloat(self,'datawc_x1',value)
        self._datawc_x1=value
        self.setdiagramdefs()
    datawc_x1=property(_getdatawc_x1,_setdatawc_x1,None,'Temperature of lower left corner (in C)')

    def _getdatawc_x2(self):
        return self._datawc_x2
    def _setdatawc_x2(self,value):
        value=VCS_validation_functions.checkIntFloat(self,'datawc_x2',value)
        self._datawc_x2=value
        self.setdiagramdefs()
    datawc_x2=property(_getdatawc_x2,_setdatawc_x2,None,'Temperature of lower right corner (in C)')

    def _getdatawc_y1(self):
        return self._datawc_y1
    def _setdatawc_y1(self,value):
        value=VCS_validation_functions.checkIntFloat(self,'datawc_y1',value)
        self._datawc_y1=value
        self.setdiagramdefs()
    datawc_y1=property(_getdatawc_y1,_setdatawc_y1,None,'Minimum Pressure (in hPa)')

    def _getdatawc_y2(self):
         return self._datawc_y2
    def _setdatawc_y2(self,value):
        value=VCS_validation_functions.checkIntFloat(self,'datawc_y2',value)
        self._datawc_y2=value
        self.setdiagramdefs()
    datawc_y2=property(_getdatawc_y2,_setdatawc_y2,None,'Maximum Pressure (in hPa)')

    def _getdisplays(self):
        return self._displays
    def _setdisplays(self,value):
        if not isinstance(value,list):
            raise Exception,"displays must be a list of displays"
        for v in value:
            if not isinstance(v,vcs.displayplot.Dp):
                raise Exception,"displays must be a list of displays"
        self._displays = value
    displays = property(_getdisplays,_setdisplays,None,"List of displays")

    def plot_pseudo(self,template,bg):
        """
        ## Ok this is pathetic but i can't get it to work any other way!!!
        ## We'll have to do the integration for each levels
        """
        levs=self.pseudoadiabats.level
        levels=[]
        for l in levs:
            levels.append(l[0])
        ## Ok we now have the required levels
        xs=[]
        ys=[]
        xstxt=[]
        ystxt=[]
        txt=[]
        for il in range(len(levels)):
            l=levels[il]
            tmpxs=[]
            tmpys=[]
            t,p=LiftWet(l+273.15,self.datawc_y1*100.,self.Pmaxmixingratio*100.,25000./self.detail)
            nt=len(t)
            if il%3==0:
                crit1=.05
                crit2=.95
            elif il%3==1:
                crit1=.2
                crit2=.9
            elif il%3==2:
                crit1=.32
                crit2=.85
            for i in range(nt):
                tx,ty=self.TP2XY(t[i],p[i])
                tmpxs.append(tx)
                tmpys.append(ty)
                if ((i-1.)/nt<crit1<i/float(nt)) or ((i-1.)/nt<crit2<i/float(nt)):
                    xstxt.append(tx)
                    ystxt.append(ty)
                    txt.append(str(l))
            xs.append(tmpxs)
            ys.append(tmpys)
        l=self.x.createline()
        l.color=self.pseudoadiabats.linecolors[0]
        l.viewport=[template.data.x1,template.data.x2,template.data.y1,template.data.y2]
        l.worldcoordinate=[self.xmin,self.xmax,self.ymin,self.ymax]
        l.x=xs
        l.y=ys
        self.displays.append(self.x.plot(l,bg=bg))
        t=self.x.createtext()
        t.viewport=[template.data.x1,template.data.x2,template.data.y1,template.data.y2]
        t.worldcoordinate=[self.xmin,self.xmax,self.ymin,self.ymax]
        t.color=l.color[0]
        t.x=xstxt
        t.y=ystxt
        t.string=txt
        t.height=10
        self.displays.append(self.x.plot(t,bg=bg))


    def clear(self):
        self.displays=[]
        self.x.clear()
    
    def plot_TP(self,T,P=None,template=None,bg=0):
        if template is None:
            template='thdiags_template'
        elif not isinstance(template,str):
            template=template.name
        try:
            isotemplate=self.x.createtemplate(template)
        except:
            isotemplate=self.x.gettemplate(template)
        self.plotIsos(template,bg=bg)
        if P is None:
            P=getPvalues(T)

        while T.rank()>1:
            T=T[0]
        xs=[]
        ys=[]
        for i in range(len(T)):
            t=T[i]
            p=P[i]
            tx,ty=self.TP2XY(t,p)
            try:
                tx=float(tx)
                ty=float(ty)
                if numpy.ma.is_masked(tx) or numpy.ma.is_masked(ty) or numpy.isnan(tx) or numpy.isnan(ty) or numpy.isinf(tx) or numpy.isinf(ty):
                    raise ValueError
                xs.append(tx)
                ys.append(ty)
            except:
                pass
##             if not(MV2.isMA(tx) or MV2.isMA(ty)):
##                 xs.append(tx)
##                 ys.append(ty)
##             else:
##                 print tx,ty,t,p
##                 xs.append(tx)
##                 ys.append(ty)
        if xs==[] and ys==[]:
            for d in self.displays:
                d.off=1
                self.x.remove_display_name(d.name)
            for i in range(len(self.displays)):
                self.displays.pop(-1)
                
            raise Exception,"Error: data are all out of range!"
        ierr=1
        iso=self.x.createline()
        iso.type=self.line.type
        iso.width=self.line.width
        iso.color=self.line.color
            
        iso.worldcoordinate=[self.xmin,self.xmax,self.ymin,self.ymax]
        iso.viewport=[isotemplate.data.x1,isotemplate.data.x2,
                      isotemplate.data.y1,isotemplate.data.y2]
        iso.x=xs
        iso.y=ys
        iso.priority=3
        self.displays.append(self.x.plot(iso,bg=bg))
        return

    def setIso(self,iso,values=None):
        iso.datawc_x1=self.xmin
        iso.datawc_x2=self.xmax
        iso.datawc_y1=self.ymin
        iso.datawc_y2=self.ymax
        if not values is None:
            if iso.g_name=='Gi':
                iso.level=values
                col=[iso.linecolors[0],]*len(values)
                iso.linecolors=col
                iso.line=[iso.line[0],]*len(values)
                if iso.text is None:
                    iso.text=[1,]*len(values)
                else:
                    iso.text=[iso.text[0],]*len(values)
                iso.textcolors=col
            else:
                col=[iso.fillareacolors[0],]*len(values)
                iso.fillareacolors=col
                iso.levels=values
        return
            
    def plotIsos(self,template=None,bg=0):
        if template is None:
            template='thdiags_template'
        elif type(template)!=type(''):
            template=template.name
        try:
            isotemplate=self.x.createtemplate(template)
        except:
            isotemplate=self.x.gettemplate(template)
            
        att=dir(isotemplate)
        for a in att:
            try:
                b=getattr(isotemplate,a)
                setattr(b,'priority',0)
            except:
                pass
            
        isotemplate.data.priority=1
        isotemplate.box1.priority=1
        isotemplate.box1.x1=isotemplate.data.x1
        isotemplate.box1.x2=isotemplate.data.x2
        isotemplate.box1.y1=isotemplate.data.y1
        isotemplate.box1.y2=isotemplate.data.y2
        
        dX=self.xmax-self.xmin
        dY=self.ymax-self.ymin
        X=numpy.ma.arange(self.detail,dtype=MV2.float)
        X=X*dX/(self.detail-1)+self.xmin
        Xaxis=cdms2.createAxis(X)
        X=numpy.ma.resize(X,(self.detail,self.detail))
        Y=numpy.ma.arange(self.detail,dtype=MV2.float)
        Y=Y*dY/(self.detail-1)+self.ymin
        Yaxis=cdms2.createAxis(Y)
        Y=numpy.ma.resize(Y,(self.detail,self.detail))
        Y=numpy.ma.transpose(Y)

        # Computes T,P on this grid
        T,P=self.XY2TP(X,Y)
        T=MV2.array(T)
        
        # Potential Temperature
        Tp=T/numpy.ma.power(P/100000.,self.k)
        

        ws=Ws(T,P)
        # Seems like we need not ws after 600mb
        ws=numpy.ma.masked_where(numpy.ma.less(P,self.Pmaxmixingratio*100.),ws)

        T=T-273.16
        Tmin,Tmax=vcs.minmax(T)
        
        Tvalues=self.isotherms.level
        if Tvalues==[[0.0, 1.0000000200408773e+20]]:
            Tvalues=vcs.mkscale(Tmin,Tmax)
            ## Now sets the isothermsfilled
            Tvalues2=[]
            for i in range(len(Tvalues)/2-1):
                t1,t2=Tvalues[2*i],Tvalues[2*i+1]
                if isinstance(t1,(list,tuple)):
                    t1,t2=t1[0],t2[0]
                Tvalues2.append([t1,t2])
        else:
            Tvalues2=self.isothermsfilled.levels
        self.setIso(self.isotherms,Tvalues)
##         self.setIso(self.isothermsfilled,Tvalues2)
        
        P=P/100.
        Pvalues=vcs.mkscale(self.datawc_y2,self.datawc_y1)
        self.setIso(self.isobars,Pvalues)

        Tp=Tp-273.16
        Tpvalues=self.dryadiabats.level
        if Tpvalues==[[0.0, 1.0000000200408773e+20]]:
            min,max=vcs.minmax(Tp)
            Tpvalues=vcs.mkscale(min,max)            
        self.setIso(self.dryadiabats,Tpvalues)

        ## Pseudoadiabats
        Thevalues=self.pseudoadiabats.level
        if Thevalues==[[0.0, 1.0000000200408773e+20]]:
            Thevalues=vcs.mkevenlevels(-40,40,40)
        self.setIso(self.pseudoadiabats,Thevalues)
        
        
        ## Mixing Ratio
        ws=ws*1000.
        wsvalues=self.mixingratio.level
        if wsvalues==[[0.0, 1.0000000200408773e+20]]:
            min,max=vcs.minmax(ws)
            wsvalues=vcs.mkscale(min,max)
        self.setIso(self.mixingratio,wsvalues)

        # Figures out where to plot the P labels
        dicP={}
        dicPl=vcs.mklabels(Pvalues)
        for p in Pvalues:
            X,Y=self.TP2XY(self.datawc_x1+273.15,p*100)
            if not numpy.ma.isMA(X):
                dicP[Y]=dicPl[p]
        try:
            ttp=self.x.createtexttable('Plabels')
        except:
            ttp=self.x.gettexttable('Plabels')
        ttp.color=self.isobars.linecolors[0]
        isotemplate.ylabel1.texttable=ttp
        dicT={}
        Tvalues=list(numpy.ma.array(Tvalues).filled().ravel())
        dicTl=vcs.mklabels(Tvalues)
        for t in Tvalues:
            X,Y=self.TP2XY(t+273.15,self.datawc_y1*100)
            dicT[X]=dicTl[t]
        try:
            ttp=self.x.createtexttable('Tlabels')
        except:
            ttp=self.x.gettexttable('Tlabels')
        ttp.color=self.isotherms.linecolors[0]
        isotemplate.xlabel1.texttable=ttp
        isotemplate.ylabel1.priority=1
        isotemplate.xlabel1.priority=1
        isotemplate.data.x1=isotemplate.data.x1
        isotemplate.data.x2=isotemplate.data.x2
        isotemplate.data.y1=isotemplate.data.y1
        isotemplate.data.y2=isotemplate.data.y2
        self.isotherms.yticlabels1=dicP
        self.isotherms.xticlabels1=dicT
        self.isobars.yticlabels1={}
        self.isobars.xticlabels1={}
        self.dryadiabats.yticlabels1={}
        self.dryadiabats.xticlabels1={}
        self.pseudoadiabats.yticlabels1={}
        self.pseudoadiabats.xticlabels1={}
        self.mixingratio.yticlabels1={}
        self.mixingratio.xticlabels1={}
        self.isothermsfilled.datawc_x1=self.isothermsfilled.datawc_x1
        self.isothermsfilled.datawc_x2=self.isothermsfilled.datawc_x2
        self.isothermsfilled.datawc_y1=self.isothermsfilled.datawc_y1
        self.isothermsfilled.datawc_x2=self.isothermsfilled.datawc_y2
        # Puts the dims on it
        T.id='T'
        T.setAxis(0,Yaxis)
        T.setAxis(1,Xaxis)
        P=MV2.array(P)
        P.id='P'
        P.setAxis(0,Yaxis)
        P.setAxis(1,Xaxis)
        Tp=MV2.array(Tp)
        Tp.setAxis(0,Yaxis)
        Tp.setAxis(1,Xaxis)
        ws=MV2.array(ws)
        ws.setAxis(0,Yaxis)
        ws.setAxis(1,Xaxis)

        # plot if self.drawisothermsfilled:
        if self.drawisothermsfilled:
            self.displays.append(self.x.plot(T,isotemplate,self.isothermsfilled,bg=bg))
        if self.drawisotherms:
            self.displays.append(self.x.plot(T,isotemplate,self.isotherms,bg=bg))
        if self.drawisobars:
            self.displays.append(self.x.plot(P,isotemplate,self.isobars,bg=bg))
        if self.drawdryadiabats:
            self.displays.append(self.x.plot(Tp,isotemplate,self.dryadiabats,bg=bg))
        if self.drawpseudoadiabats:
           self.plot_pseudo(isotemplate,bg=bg)
        if self.drawmixingratio:
            self.displays.append(self.x.plot(ws,isotemplate,self.mixingratio,bg=bg))
        return


    ## All take args with T in K and P in Pa
    # SkewT-LogP diagram defs
    def TP2XYskewT(self,T,P):
        X=T+self.skewness*numpy.ma.log(P)
        Y=-self.R*numpy.ma.log(P)
        return X,Y    
    def XY2TPskewT(self,X,Y):
        P=numpy.ma.exp(-Y/self.R)
        T=X-self.skewness*numpy.ma.log(P)
        return T,P

    # Emagram defs
    def TP2XYemagram(self,T,P):
        X=T
        Y=-self.R*numpy.ma.log(P)
        return X,Y
    def XY2TPemagram(self,X,Y):
        T=X
        P=numpy.ma.exp(-Y/self.R)
        return T,P
    
    #Tephigram defs
    def TP2XYtephigram(self,T,P):
        X=T
        Tp=T/numpy.ma.power(P/100000.,self.k)
        Y=self.cp*numpy.ma.log(Tp)
        return X,Y
    def XY2TPtephigram(self,X,Y):
        T=X
        Tp=numpy.ma.exp(Y/self.cp)
        P=100000.*numpy.ma.power(T/Tp,1./self.k)
        return T,P

    #Stuve diagram defs
    def TP2XYstuve(self,T,P):
        X=T
        Y=numpy.ma.power(P,self.k)
        return X,Y
    def XY2TPstuve(self,X,Y):
        T=X
        P=numpy.ma.power(Y,1./self.k)
        return T,P


    def plot_windbarb(self,u,v,P=None,template=None,type='uv',bg=0):
        if type=='uv':
            n=numpy.ma.sqrt(u*u+v*v)
            d=numpy.ma.arccos(u/n)
            d=numpy.ma.where(numpy.ma.less(v,0.),-d,d)

        if P is None:
            P=u.getLevel()
            if P is None:
                P=u.getAxis(-1)
            P = P.clone()
            try: # tries to convert to Pa
                for i in range(len(P)):
                    uni = unidata.udunits(P[i],P.units)
                    P[i] = uni.to("Pa").value
            except Exception,err:
                pass
            P=P[:]
            
        n1=n/self.windbarbsscales[0]
        n1=n1.astype('i')
        n2=(n-n1*self.windbarbsscales[0])/self.windbarbsscales[1]
        n2=n2.astype('i')
        n3=(n-n1*self.windbarbsscales[0]-n2*self.windbarbsscales[1])/self.windbarbsscales[2]
        n3=n3.astype('i')

##         print 'n1:',n1
##         print 'n2:',n2
##         print 'n3:',n3
        
        nitems=len(P)
        for i in range(nitems):
            if not(MV2.isMA(n[i])):
                p=P[i]
                ## Figure out the altitude to plot it (y coord) !
                dum,Y=self.TP2XY(273,p)
                lin=self.x.createline()
                lin.viewport=[template.data.x1,template.data.x2,template.data.y1,template.data.y2]
                lin.worldcoordinate=[-1,1,-1,1]
                lin.x=[0,0]
                lin.y=[-1,1]
                self.displays.append(self.x.plot(lin,bg=bg))
                lin=self.x.createline()
                lin.viewport=[template.data.x1,template.data.x2,template.data.y1,template.data.y2]
                lin.worldcoordinate=[-1,1,self.ymin,self.ymax]
                ## Set everything correctly y wise
                rw=2/(self.ymax-self.ymin)
                if self.x.islandscape():
                    r=1.375
                else:
                    r=.72727272
                rv=(template.data.x2-template.data.x1)/(template.data.y2-template.data.y1)
                rw=1./rw
                x,y=self.make_barb(n[i],d[i],n1[i],n2[i],n3[i],rw*r*rv,Y)
                lin.x=x
                lin.y=y
                linetype=['solid']
                self.displays.append(self.x.plot(lin,bg=bg))
            

    def make_barb(self,n,d,n1,n2,n3,ydeformation,yoff):
        """ Draws a wind barb, the head is at location 0,0, the tail length is 1


        ---------------------------------------------------------------
        \  |  /   /    /
         \ | /   /    /
          \|/   /
           V

           total length is 1
           barbs angle is 60 degrees
           barb depth is .1
        
        """
        ## Intitalize the Drawing, head is at zero
        xs=[0]
        ys=[0]
        xs.append(numpy.cos(d))
        ys.append(numpy.sin(d))

        ## Ok now does the BIG barbs
        bxs=[]
        bys=[]
        bangle=25./180.*numpy.pi # 30 degrees in radians
        bdepth=.3
        bwidth=numpy.tan(bangle)*bdepth

        hinit=1.
        ## Drawing of big barbs
        for i in range(n1):
            ## summit
            l=hinit-bwidth # legth to the middle of big barb
            h=numpy.sqrt(bdepth**2+l**2)
            mu=numpy.arctan(bdepth/l)
            ys.append(h*numpy.sin(d+mu))
            xs.append(h*numpy.cos(d+mu))

            ## End of barb
            ys.append((l-bwidth)*numpy.sin(d))
            xs.append((l-bwidth)*numpy.cos(d))
            hinit=hinit-2*bwidth
        if n1!=0: hinit=hinit+bwidth/2.

        ## Ok now the long sticks
        for i in range(n2):
            l=hinit-bwidth
            ## Move to begining of next one
            ys.append(l*numpy.sin(d))
            xs.append(l*numpy.cos(d))

            ## Go to the top one
            l=hinit
            h=numpy.sqrt(bdepth**2+l**2)
            mu=numpy.arctan(bdepth/l)
            ys.append(h*numpy.sin(d+mu))
            xs.append(h*numpy.cos(d+mu))
            
            ## Back to begining
            ys.append(ys[-2])
            xs.append(xs[-2])
            
            ## Adjust hinit
            hinit=hinit-bwidth/2.

        ## Now the half barbs
        bwidth=bwidth/2.
        bdepth=bdepth/2.
        for i in range(n3):
            l=hinit-bwidth
            ## Move to begining of next one
            ys.append(l*numpy.sin(d))
            xs.append(l*numpy.cos(d))
            
            ## Go to the top one
            l=hinit
            h=numpy.sqrt(bdepth**2+l**2)
            mu=numpy.arctan(bdepth/l)
            ys.append(h*numpy.sin(d+mu))
            xs.append(h*numpy.cos(d+mu))
            ## Back to begining
            ys.append(ys[-2])
            xs.append(xs[-2])
            
            ## Adjust hinit
            hinit=hinit-bwidth/2.

        ## Scales and offset
        for i in range(len(ys)):
##             print 'i,x,y:',i,xs[i],ys[i]
            ys[i]*=ydeformation
            ys[i]+=yoff

        return xs,ys
    
        
