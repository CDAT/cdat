import udunits_wrap,sys,string,unidata
version=sys.version.split()[0].split('.')
version=string.join(version[:2],'.')
udunits_name=unidata.__path__[0]+'/udunits.dat'

class udunits(object):
    """UNIDATA/UDUNITS Python Object

    initialization: a=unidata(value,units)

    availabe commands:
    all of ufuncs are avalailable:
    +,-,/,*, appropriate conversion to left side units is done on the fly
    example:
    a=unidata.udunits(5,'m')
    b=unidata.udunits(6,'in')
    c=a+b # udunits(5.1524,"m")
    # but
    c=b+a # udunits(202.850393701,"in")

    # wrong units conversion will generate errors:
    c=unidata.udunits(7,'K')
    Traceback (most recent call last):
    File "<stdin>", line 1, in ?
    File "/lgm/cdat/latest/lib/python2.3/site-packages/unidata/udunits.py", line 111, in __add__
    s,i=udunits_wrap.convert(other.units,self.units)
    TypeError: Error Converting.

    #a*c # works

    Note: a/b # returns a number (unitless)
    a+c #


    OTHER FUNCTION
    
    UNIT CONVERSION
    a.units='feet'
    print a # 16.4041994751 feet

    c=a.to('km') # udunits(0.005,"km")

    c=unidata.udunits(7,'K')
    factor, offset = c.how('degF') # (1.7999999999999998, -459.67000000000002)

    WHICH UNITS ?
    By available (known) units
    lst = c.available_units() #  returns list of all known units
    
    By units:
    dict = c.known_units() # returns a dictionary of units (keys) associated with type (values)
    dict['k'] # returns : 'THERMODYNAMIC TEMPERATURE'

    By type of units
    dict = c.known_units(bytype=1) # returns a dictionary of units type (keys) associated with a list of units for each type
    e.g
    dict['THERMODYNAMIC TEMPERATURE'] # returns['degree_Kelvin', 'degree_Celsius', 'degree_Rankine', 'degree_Fahrenheit', 'Celsius', 'celsius', 'degree_centigrade', 'degC', 'degreeC', 'degree_C', 'degree_c', 'deg_C', 'deg_c', 'degK', 'degreeK', 'degree_K', 'degree_k', 'deg_K', 'deg_k', 'K', 'Kelvin', 'degF', 'degreeF', 'degree_F', 'degree_f', 'deg_F', 'deg_f', 'F', 'Fahrenheit', 'fahrenheit', 'degR', 'degreeR', 'degree_R', 'degree_r', 'deg_R', 'deg_r', 'Rankine', 'rankine']
    
    """
    __slots__ = [ 'units', '_units', 'value', '_value' ]
    def __init__(self,value,units):
        if isinstance(value,str):
            if isinstance(units,(int,long,float)):
                tmp=units
                units=value
                value=tmp
            else:
                if units is None:
                    units=value
                    value=None
                else:
                    units=value
                    
        if isinstance(units,(int,long,float)):
            raise 'Error, must provide at least one args as string representing units'
        
        if unidata.udunits_init == 0:
            udunits_wrap.init()#udunits_name)
            unidata.udunits_init=1
        self._units=units
        self._value=value

    def to(self,units):
        if isinstance(units,udunits):
            u=units.units
        else:
            u=units
        s,i=udunits_wrap.convert(self.units,u)
        return udunits(self.value*s+i,u)

    def how(self,units):
        if isinstance(units,udunits):
            u=units.units
        else:
            u=units
        s,i=udunits_wrap.convert(self.units,u)
        return s,i


    def available_units(self):
        out=self.known_units().keys()
        out.sort()
        return out
    
    def known_units(self,bytype=0):
        f=open(udunits_name)
        units={}
        type='???'
        types={}
        ln=f.readlines()
        f.close()
        for i in range(len(ln)):
            if ln[i][0] == '#':
                if ln[i]=='#\n' and ln[i+1][0]=='#' and ln[i+2]=='#\n':
                    type=ln[i+1][2:-1]
                    if type[:8]=='UNITS OF' : type=type[8:].strip()
            elif not ln[i][0] in ['\n',' ']:
                unit=ln[i].split()[0]
                units[unit]=type
                tmp=types.get(type,[])
                tmp.append(unit)
                types[type]=tmp
        if bytype==1:
            return types
        else:
            return units

    def _getunits(self):
        return self._units

    def _setunits(self,units):
        if self.value is None:
            self._units=units
        elif units!=self.units:
            s,i=udunits_wrap.convert(self.units,units)
            self.value=self.value*s+i
            self._units=units
        return
    units=property(_getunits,_setunits)


    def __str__(self):
        return str(self.value)+' '+str(self.units)

    def __repr__(self):
        return "udunits("+str(self.value)+',"'+str(self.units)+'")'

    
    def _getvalue(self):
        return self._value

    def _setvalue(self,value):
        self._value=value
        return
    value=property(_getvalue,_setvalue)
    
    
    def __add__(self,other):
        if not isinstance(other, (udunits,int,long,float)):
            raise "Error must add a number or a udunit object"
        out=udunits(self.units,self.value)
        if isinstance(other, udunits):
            s,i=udunits_wrap.convert(other.units,self.units)
            out.value=self.value+other.value*s+i
        else:
            out.value+=other
        return out
    
    def __radd__(self,other):
        if not isinstance(other, (udunits,int,long,float)):
            raise "Error must add a number or a udunit object"
        out=udunits(self.units,self.value)
        if isinstance(other, udunits):
            s,i=udunits_wrap.convert(other.units,self.units)
            out.value=self.value+other.value*s+i
        else:
            out.value+=other
        return out
    
    def __sub__(self,other):
        if not isinstance(other, (udunits,int,long,float)):
            raise "Error must sub a number or a udunit object"
        out=udunits(self.units,self.value)
        if isinstance(other, udunits):
            s,i=udunits_wrap.convert(other.units,self.units)
            out.value=self.value-(other.value*s+i)
        else:
            out.value-=other
        return out
    
    def __rsub__(self,other):
        if not isinstance(other, (udunits,int,long,float)):
            raise "Error must sub a number or a udunit object"
        out=udunits(self.units,self.value)
        if isinstance(other, udunits):
            s,i=udunits_wrap.convert(other.units,self.units)
            out.value=(other.value*s+i)-self.value
        else:
            out.value=other-out.value
        return out

    def __mul__(self,other):
        if not isinstance(other, (udunits,int,long,float)):
            raise "Error must multiply a number or a udunit object"
        out=udunits(self.units+'*'+self.units,self.value)
        if isinstance(other, udunits):
            try:
                s,i=udunits_wrap.convert(other.units,self.units)
                out.value=self.value*(other.value*s+i)
            except: #Ok uncompatible units, just do the produce
                out=udunits(self.units+'*'+other.units,self.value*other.value)
        else:
            out = udunits(other*self.value,self.units)
        return out
    
    def __rmul__(self,other):
        if not isinstance(other, (udunits,int,long,float)):
            raise "Error must multiply a number or a udunit object"
        out=udunits(self.units+'*'+self.units,self.value)
        if isinstance(other, udunits):
            try:
                s,i=udunits_wrap.convert(other.units,self.units)
                out.value=self.value*(other.value*s+i)
            except: #Ok uncompatible units, just do the produce
                out=udunits(self.units+'*'+other.units,self.value*other.value)
        else:
            out = udunits(other*self.value,self.units)
        return out

    def __div__(self,other):
        if not isinstance(other, (udunits,int,long,float)):
            raise "Error must divide by a number or a udunit object"
        out=udunits(self.units,self.value)
        if isinstance(other, udunits):
            try:
                s,i=udunits_wrap.convert(other.units,self.units)
                out=self.value/(other.value*s+i)
            except: #Ok uncompatible units, just do the produce
                out=udunits(self.value/other.value,self.units+'/'+other.units)
        else:
            out.value/=other
        return out
    
    def __rdiv__(self,other):
        if not isinstance(other, (udunits,int,long,float)):
            raise "Error must divide by a number or a udunit object"
        out=udunits(self.units,self.value)
        if isinstance(other, udunits):
            try:
                s,i=udunits_wrap.convert(other.units,self.units)
                out=(other.value*s+i)/self.value
            except: #Ok uncompatible units, just do the produce
                out=udunits(other.value/self.value,other.units+'/'+self.units)
        else:
            out.value=other/self.value
            out._units='1/('+self.units+')'
        return out
    
    def __pow__(self,other):
        if not isinstance(other, (int,long,float)):
            raise "Error must power to a number"
        out=udunits(self.value**other,self.units+'**'+str(other))
        return out

    
           
