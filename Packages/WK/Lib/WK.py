# Adapted for numpy/ma/cdms2 by convertcdms.py
## Wheeler Koladis Reproduction Package
import cdutil, genutil, cdms2, numpy, MV2

class WK(object):
    
    def __init__(self,frequency=1,number_of_days=96,shift=30,dosymetric=1,tkbar=0):
        """
        frequency: data frequency in samples per day (default=1, i.e daily)
        number_of_days: length of time subdomains in days (default=96 days)
        shift: Number of days to shift subtimedomains by (default=30 days)
        dosymetric : if we need to do the symetric/antisymetric separation (default=1)
        tkbar: Do we want a tk bar or text bar when processing FFTs (default=1, i.e tk)
        """
        self._number_of_days=number_of_days
        self._shift=shift
        self.frequency=frequency # frequency in samples/day
        if tkbar: #When doing FFTs, text bar or tkbar
            self.tkbar=1
        else:
            self.tkbar=None
        self.symetric=dosymetric #Do we need to do the symetric/antisymetric treatment
        return
    
    __slots__ = [
        'frequency',
        '_frequency',
        '_NShift',
        '_NTSub',
        'shift',
        '_shift',
        'number_of_days',
        '_number_of_days',
        'tkbar',
        'symetric',
        ]
    def _getfrequency(self):
        return self._frequency
    def _setfrequency(self,value):
        if not isinstance(value,(int,float)):
            raise ValueError,"Error value must be a number"
        self._frequency = value
        self._NShift=int(self.shift*value) # Number of time points to shift by
        self._NTSub=int(self.number_of_days*value) #Number of time point per subdomain
##         print self.scm()
    frequency = property(_getfrequency,_setfrequency,None,"data frequency in samples per day (default=1, i.e daily)")

    def _getnumber_of_days(self):
        return self._number_of_days
    def _setnumber_of_days(self,value):
        if not isinstance(value,(int,float)):
            raise ValueError,"Error value must be a number"
        self._number_of_days = value
        self._NTSub=int(self.frequency*value) #Number of time point per subdomain
##         print self.scm()
    number_of_days = property(_getnumber_of_days,_setnumber_of_days,None,"length of time subdomains in days (default=96 days)")
    
    def _getshift(self):
        return self._shift
    def _setshift(self,value):
        if not isinstance(value,(int,float)):
            raise ValueError,"Error value must be a number"
        self._shift = value
        self._NShift=int(self.frequency*value) # Number of time points to shift by
    shift = property(_getshift,_setshift,None,"Number of days to shift subtimedomains by (default=30 days)")
    

    def process(self,data):
##         if self.symetric:
##             data = symetric(data)
        # Make sure we have an even number of time steps
        t=data.getTime()

        # length of time axis
        nt=len(t)
        if nt%2!=0:
            print "Warning time wasn't even, removed last time step"
            data=data[:-1]
            t=data.getTime() ## get the new time axis
            nt=len(t)

        if len(t)<self._NTSub:
            raise Exception,"Error your data must have at least %i time steps, adjust frequency (currently: %i/day) or number_of_days (currently: %i processed at once) to reach that limit, or get more data" % (self._NTSub,self.frequency,self.number_of_days)
        ## Computes PP, number of sub-domain
        PP=float(nt-self._NTSub)/self._NShift+1
        PP=int(PP)

        ## Number of longitudes
        lons=data.getLongitude()
        NL=len(lons)
        tt=cdms2.createAxis(numpy.arange(self._NTSub),id='sub_time')
        
        ## Should redo that with just an arange (eventually...)!!!
        ## Frequencies in cycles/day
        ff=numpy.arange(0,self._NTSub+1,1,numpy.float)
        for i in range(1,self._NTSub+2):
            ff[i-1]=float(i-1-self._NTSub/2.)*self.frequency/float(self._NTSub)
            
        ## Should redo that with just an arange (eventually...)!!!
        ## Wave numbers
        ss=numpy.arange(0,NL+1,1,numpy.float)
        for i in range(1,NL+2):
            ss[i-1]=float(i-1-NL/2.)
##         print 'Frequencies:',ff
##         print 'Wave numbers:',ss
        ## Ok, we now do the real stuff
        ## Creates the array of powers (Number of subtimes,fqcy,wave_numbers,latitudes)
        lats=data.getLatitude()
        Power=numpy.zeros((PP,self._NTSub+1,NL+1,len(lats)),numpy.float)
        
        ## LOOP through time sub domains
        prev=0 # initialize the scrolling bar
        for Pcount in range(PP):
            if PP>1:
                prev=genutil.statusbar(float(Pcount),PP-1,prev=prev,tk=self.tkbar)
            
            ## Get the time subdomain
            EEo=data[Pcount*self._NShift:Pcount*self._NShift+self._NTSub](order='tx...')
            ## First does the symetric/antisymetric thing if needed
            if self.symetric: EEo=symetrick(EEo)
            
            ## Now detrending
            ##  Step 1- Get the slope and intercept
            slope,intercept=genutil.statistics.linearregression(EEo,nointercept=0)
            ##  Step 2- remove the trend
            ##    Step 2a: Create an array with the time values
            a=EEo.getTime()
            A=MV2.array(a[:],typecode='d')
            A.setAxis(0,a)
            ##    Step 2b: "Grows" it so it has the same shape than data
            A,EEo=genutil.grower(A,EEo)
            ##    Step 2c: Actually remove the trend
            EE=EEo-A*slope-intercept

            ## we don't need A,EEo,slope,intercept anymore
            del(EEo)
            del(slope)
            del(intercept)
            del(A)

            ## Remove the time mean 
            mean=MV2.average(EE,0)
            EE=EE-mean
            del(mean) # could be big in memory

            ## Tapering time...
            tapertozero(EE,1,len(EE)-1,5*self.frequency)
            
            ## OK here Wheeler has some windowing on longitude, but it's commented out
            ## I'll pass it for now

            ## Ok the actuall FFT work
            EE=numpy.fft.fft2(EE,axes=(1,0))/NL/self._NTSub
            
            ## OK NOW THE LITTLE MAGIC WITH REORDERING !
            A=numpy.absolute(EE[0:self._NTSub/2+1,1:NL/2+1])**2
            B=numpy.absolute(EE[self._NTSub/2:self._NTSub,1:NL/2+1])**2
            C=numpy.absolute(EE[self._NTSub/2:self._NTSub,0:NL/2+1])**2
            D=numpy.absolute(EE[0:self._NTSub/2+1,0:NL/2+1])**2
            Power[Pcount,self._NTSub/2:,:NL/2]=A[:,::-1]
            Power[Pcount,:self._NTSub/2,:NL/2]=B[:,::-1]
            Power[Pcount,self._NTSub/2+1:,NL/2:]=C[::-1,:]
            Power[Pcount,:self._NTSub/2+1,NL/2:]=D[::-1,:]
        ## End of Pcount loop
        if self.tkbar and PP>1:
            prev[1].destroy()
            prev[0].destroy()
        ## Now generates the decorations

        ## first the time axis (subdomains)
        vals=[]
        bounds=[]
        pp=0
        for i in range(0,len(t)-self._NShift,self._NShift):
            st=t.subAxis(i,i+self._NTSub)
            if len(st[:])==self._NTSub:
                pp+=1
                vals.append((st[0]+st[-1])/2.)
                bds=st.getBounds()
                #print 'Bounds:',bds
                if bds is None:
                    raise ValueError, "Data need to have bounds on time dimension"
                else:
                    bounds.append([bds[0][0],bds[-1][1]])
        ## Convert lists to arrays
        vals=numpy.array(vals)
        bounds=numpy.array(bounds)
        ## Creates the time axis
        dumt=cdms2.createAxis(vals,bounds=bounds)
        dumt.id='time'
        dumt.units=t.units
        dumt.designateTime()
        dumt.setCalendar(t.getCalendar())

        ## Create the frequencies axis
        T=cdms2.createAxis(ff)
        T.id='frequency'
        T.units='cycles per day'

        ## Create the wave numbers axis
        S=cdms2.createAxis(ss)
        S.id='planetaryzonalwavenumber'
        S.units='-'

        ## Makes it an MV2 with axis and id (id come sfrom orignal data id)
        Power=MV2.array(Power,axes=(dumt,T,S,lats),id=data.id+'_'+'power')
        ## Adds a long name attribute
        Power.longname='Real power spectrum for the many different parts (i.e. over separate time divisions)'
        ## And return the whole thing ordered 'time', 'latitude', 'frequencies','wavenumbers'
        return Power(order='ty...')
    
    ## Define the default call function for the object
##     __call__=get

    def split(self,power,compresstime=True,smooth=True):
        """ This splits the power between the Symetric and Antisymetric component
        Will average over time if compresstime is True (default)
        Will apply 121 smoothing if smooth is True (default)
        """
        t = power.getTime()
        id = power.id
        if t is not None and compresstime:
            power = cdutil.averager(power,axis='t')
        power = 2*power(order='y...')
        S = MV2.sum(power(latitude=(-90,-1)),axis=0)
        A = MV2.sum(power(latitude=(90,1)),axis=0)
        S.id = id+'_S'
        A.id = id+'_A'
        if smooth:
            return MWsmooth121(S),MWsmooth121(A)
        else:
            return S,A

    def background(self,S,A,wavenumber_smoothing_windows=((1000,10,),),frequencies_smoothing_windows=((.1,5),(.2,10),(.3,20),(1000,40))):
        """ return the background power
        Input:
        Power, with frequencies as dimension 0 and wave numbers as dim 1
        frenquencies_smoothing_windows: list of tuples of frequency/number of 121 smoothing, frequencies must be increasing
        wavenumber_smoothing_windows: list of tuples of wavenumber/number of 121 smoothing, wavenumber must be increasing

        Output:
        background_power

        Usage:
        bg=background(power,wavenumber_smoothing=((1000,10)),frenquencies_smoothing_windows=((.1,5),(.2,10),(.3,20),(1000,40)))

        """
        id = S.id.split("_")[0]
        power=(S+A)/2.
        trans=range(power.rank())
        trans[0]=1
        trans[1]=0
        ## Puts wave number first
        power=MV2.transpose(power,trans)
        istart=0
        fqcies=power.getAxis(1)[:]

        ## Little bit to do the bar
        total = -1
        for fqcy,n in frequencies_smoothing_windows:
            total+=n
        for wn,n in wavenumber_smoothing_windows:
            total+=n
        counter = 0
        prev = 0
        
        for fqcy,n in frequencies_smoothing_windows:
            ## determine which one is the limit
            iend=0
            for i in range(len(fqcies)):
                f=fqcies[i]
                if f<=fqcy: iend=i
##         print istart,iend+1
            for i in range(n):
                tmp=MWsmooth121(power[:,istart:iend+1])
                power[:,istart:iend+1]=tmp
                prev=genutil.statusbar(float(counter),total,prev=prev,tk=self.tkbar)
                counter+=1
                
            istart=iend+1

        ## Puts fqcies first
        power=MV2.transpose(power,trans)
        istart=0
        wns=power.getAxis(1)[:]
        for wn,n in wavenumber_smoothing_windows:
            ## determine which one is the limit
            iend=0
            for i in range(len(wns)):
                w=wns[i]
                if w<=wn: iend=i
##         print istart,iend+1
            for i in range(n):
                tmp=MWsmooth121(power[:,istart:iend+1])
                power[:,istart:iend+1]=tmp
                prev=genutil.statusbar(float(counter),total,prev=prev,tk=self.tkbar)
                counter+=1
        power.id = id+'_background'
        if self.tkbar:
            prev[1].destroy()
            prev[0].destroy()
        return power
    


    def scm(self,a=None,b=None,iterations=200):
        """Return the smallest common multiple"""
        if a is None:
            a = self._NTSub
        if b is None:
            b= self._NShift
        m=min(a,b)
        M=max(a,b)
        Mlist=[]
        for i in range(1,iterations):
            Mlist.append(M*i)
            if m*i in Mlist:
                return m*i
        raise "Error Couldn't find a common multiple within the first "+str(iterations)


def symetrick(slab,axis='y'):
    """ Breaks a field into the symetric and antisymetric components
    symetric component goes into first half,
    antisymetric into the second half
    """
    if type(axis)==type(1):
        ax=slab.getAxisIds(axis)
    else:
        if axis=='x':
            ax=slab.getLongitude()
        elif axis=='y':
            ax=slab.getLatitude()
        elif axis=='z':
            ax=slab.getLevel()
        elif axis=='t':
            ax=slab.getTime()
        else:
            axs=slab.getAxisIndex(axis)
            if axs==-1:
                raise 'Error could not find axis:'+axis
            ax=slab.getAxis(axs)
    if ax is None:
        raise 'Error Axis: '+str(axis)+' does not exists (getAxis returned None)'

    n=len(ax)
    ax1=ax[0]
    ax2=ax[-1]
    ## Gets first and second half of the dataset
    tmp=MV2.array(slab*1.,copy=1)
    tmp.setAxisList(slab.getAxisList())
    tmp=tmp(order=str(axis)+'...')
    if n%2==0:
        H1=tmp[:n/2]
    else:
        H1=tmp[:n/2+1]
    H2=tmp[n/2:]
    H1=H1[::-1]
    sym=(H1+H2)/2.
    anti=(H2-H1)/2.
    if n%2==0:
        tmp[:n/2]=sym[::-1]
        tmp[n/2:]=anti
    else:
        tmp[:n/2+1]=sym[::-1]
        tmp[n/2+1:]=anti[1:]
    sh=tmp.shape
    for i in range(1,len(sh)):
        tmp.setAxis(i,H1.getAxis(i))
    tmp.setAxis(0,ax)
    tmp.id=slab.id
    for a in slab.attributes.keys():
        setattr(tmp,a,getattr(slab,a))
    return tmp(order=slab.getOrder(ids=1))

def MWsmooth121(slab):
    """
    Apply 121 smoothing, 1st and last are 13 and 31
    """
    out=MV2.zeros(slab.shape,typecode='f')
    out[0]=(slab[0]*3.+slab[1])/4.
    out[-1]=(slab[-2]+3.*slab[-1])/4.
    out[1:-1]=genutil.filters.smooth121(slab)
    out.id=slab.id
    out.setAxisList(slab.getAxisList())
    return out

def mask_wave_number_frequencies_range(power, wv_range, fqcies_range):
    for s in wv_range:
        for fq in fqcies_range:
            power = graphics.mask_power(power,wv_range,fqcies_range)
    return power

def mask_data(power,
              wn_fq_couples = [[[13,14],[0.09375,0.10416667,0.114583333333,0.125,0.21875,0.22916667,]],
                               [[-14.00, -13.00, -12.00, -11.00, -10.00, -9.00, -8.00,],[0.791666666667,0.78125,0.77083333]],
                               ]
              ):
              
    for wv,fqcies in wn_fq_couples:
        power = mask_wave_number_frequencies_range(power,wv,fqcies)
    return power

    for s in [13,14]:
        for fq in [0.09375,0.10416667,0.114583333333,0.125,0.21875,0.22916667,]:
            power=graphics.mask_power(power,s,fq)

    for s in [-14.00, -13.00, -12.00, -11.00, -10.00, -9.00, -8.00,]:
        for fq in [0.791666666667,0.78125,0.77083333]:
            power=graphics.mask_power(power,s,fq)
    return
            
def tapertozero(slab,nmi,nn,tp):
    """
    'taper' the first and last 'tp' members of 'ts' by multiplication by
    a segment of the cosine curve so that the ends of the series
    taper toward zero. This satisfies the
    periodic requirement of the FFT.
    Only the data from nmi to nmi+nn-1 is deemed useful, and
    the rest is set to zero. (There are nn points of useful data)
    """
    N=len(slab)
    if N<25:
        raise "Error can not taper if less than 25 values"
    if (nmi+nn)>N:
        raise "length of valid interval is bigger than slab"

    for i in range(1,N+1):
        j=i-nmi+1
        if j<=0 or j>nn:
            slab[i-1]=0.
        elif j<=tp:
            slab[i-1]=slab[i-1]*.5*(1.-numpy.cos((j -1)*numpy.pi/float(tp)))
        elif j>(nn-tp) and j<=nn:
            slab[i-1]=slab[i-1]*.5*(1.-numpy.cos((nn-j)*numpy.pi/float(tp)))
    return
