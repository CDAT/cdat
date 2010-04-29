# Adapted for numpy/ma/cdms2 by convertcdms.py
import cdms2
import MV2, numpy, numpy.ma,cdtime
import compall,time
import cdutil
import exceptions
import os

class ComparisonStatisticsError(exceptions.Exception):
    def __init__(self,args):
        self.args=args

class StatisticError(ComparisonStatisticsError):
    pass

class Statisitic:
    def __init__(self,stat,components,components_names,time_domain,time_domain_names,id=None):
        # Set the id
        if id is None:
            self.id=""
        else:
            self.id=id

        # Check components
        if not isinstance(components,(list,tuple)):
            raise StatisticError,"components argument must be a list"
        else:
            if  isinstance(components_names[0],str):
                self.components={}
                for i in range(len(components)):
                    self.components[components[i]]=components_names[i]
            elif isinstance(components_names,dict):
                self.components=components
            else:
               raise StatisticError,"components_names argument must be a list"

        # Check time_domain
        if not isinstance(time_domain,(list,tuple)):
            raise StatisticError,"time_domain argument must be a list"
        else:
            if isinstance(time_domain_names[0],str):
                self.time_domain={}
                for i in range(len(time_domain)):
                    self.time_domain[time_domain[i]]=time_domain_names[i]
            elif isinstance(time_domain_names,dict):
                self.time_domain=time_domain
            else:
               raise StatisticError,"time_domain_names argument must be a list" 

        # check stat array
        if isinstance (stat,numpy.ndarray ) or numpy.ma.isMA(stat):
            s=stat.shape
            if len(s)!=2:
                raise StatisticError,"stat argument must be 2D"
            nc=len(components)
            nt=len(time_domain)
            if nc!=s[0]:
                raise StatisticError,"You claim "+str(nc)+" components but your stat shows:"+str(s[0])
            if nt!=s[1]:
                raise StatisticError,"You claim "+str(nt)+" time_domain but your stat shows:"+str(s[1])
            self.stat=cdms2.createVariable(stat,copy=0,id=str(self.id))
            autobounds=cdms2.getAutoBounds()
            cdms2.setAutoBounds('off')
            compaxis=cdms2.createAxis(components)
            compaxis.id='component'
            timaxis=cdms2.createAxis(time_domain)
            timaxis.id='time_domain'
            self.stat.setAxis(0,compaxis)
            self.stat.setAxis(1,timaxis)
            self.stat.components=repr(self.components)
            self.stat.time_domain=repr(self.time_domain)
            cdms2.setAutoBounds(autobounds)
        else:
            raise StatisticError,"stat argument must be A numpy array a MA or a MV2"

    def get(self,components=None,time_domain=None):

        # Check the components
        if not components is None:
            if isinstance(components,(int,float,numpy.floating)):
                components=[components]
            if isinstance(components,(list,tuple)):
                k=self.components.keys()
                for c in components:
                    if not c in k:
                        raise StatisticError,"component:"+str(c)+"is not defined"
            else:
                raise StatisticError,"components must be a number or list (or None)"

        # Check the time components
        if not time_domain is None:
            if  isinstance(time_domain,(int,float,numpy.floating)):
                time_domain=[time_domain]
            if isinstance(time_domain,(list,tuple)):
                k=self.time_domain.keys()
                for c in time_domain:
                    if not c in k:
                        raise StatisticError,"component:"+str(c)+"is not defined"
            else:
                raise StatisticError,"time_domain must be a number or list (or None)"
            
        # Now gets only the wanted components
        if components is None and time_domain is None:
            return self.stat()

        # Now prepare the output array
        s=self.stat.shape
        if components is None:
            nc=s[0]
        else:
            nc=len(components)
        if time_domain is None:
            nt=s[1]
        else:
            nt=len(time_domain)

        out=MV2.zeros((nc,nt),typecode=numpy.float32)
        out.id=self.stat.id

        autobounds=cdms2.getAutoBounds()
        cdms2.setAutoBounds('on')
        if components is None:# Retrieve all the comp
            components_dic=self.components
            time_domain_dic={}
            for it in range(nt):
                time_domain_dic[time_domain[it]]=self.time_domain[time_domain[it]]
                out[:,it]=self.stat(time_domain=(time_domain[it],time_domain[it],'cc'),squeeze=1)
        elif time_domain is None:# Retrieve all the time
            time_domain_dic=self.time_domain
            for ic in range(nc):
                components_dic[components[ic]]=self.components[components[ic]]
                out[ic]=self.stat(component=components[ic],squeeze=1)
        else: # Retrieve specific comp and time_domain
            for ic in range(nc):
                components_dic[components[ic]]=self.components[components[ic]]
                for it in range(nt):
                    if ic==0 : time_domain_dic[time_domain[it]]=self.time_domain[time_domain[it]]
                    out[ic,it]=self.stat(component=components[ic],time_domain=time_domain[it],squeeze=1)

        cdms2.setAutoBounds('off')
        # Create the Axis
        if components is None:
            compaxis=self.stat.getAxis(0)
        else:
            compaxis=cdms2.createAxis(components)
            compaxis.id='component'
        if time_domain is None:
            timaxis=self.stat.getAxis(1)
        else:
            timaxis=cdms2.createAxis(time_domain)
            timaxis.id='time_domain'
            
        # Set the Axes
        out.setAxis(0,compaxis)
        out.setAxis(1,timaxis)
        out.components=repr(components_dic)
        out.time_domain=repr(time_domain_dic)
        cdms2.setAutoBounds(autobounds)
        return out(squeeze=1)
    __call__=get
    
    def __str__(self):
        s="Statistic:"+str(self.id)+'\n'
        s+='Shape :'+str(self.stat.shape)
        s+="Components:\n"
        for k in self.components.keys():
            s+='\t '+str(k)+' : '+str(self.components[k])+'\n'
        s+="\nTime Domain:\n"
        for k in self.time_domain.keys():
            s+='\t '+str(k)+' : '+str(self.time_domain[k])+'\n'
        s+="Data:\n"
        s+=str(self.stat)+'\n'
        return s

    def info(self):
        print self
        return

    def write(self,file,mode='w'):
        autobounds=cdms2.getAutoBounds()
        cdms2.setAutoBounds('off')
        if isinstance(file,str):
            f=cdms2.open(file,mode)
        elif isinstance(file,cdms2.dataset.CdmsFile):
            f=file
        else:
            raise StatisticError,"write arguments expects string or cdms2 file"
        f.write(self.stat)
        if not isinstance(file,cdms2.dataset.CdmsFile): f.close()
        cdms2.setAutoBounds(autobounds)
   
        
class ComparisonStatistics(cdutil.VariablesMatcher):
    def __init__(self,*args,**kw):
        cdutil.VariablesMatcher.__init__(self,*args,**kw)
        # aliases to make my life MUCH easier !
        self.V1=self.variableConditioner1
        self.V2=self.variableConditioner2
        self.EV=self.externalVariableConditioner
        self.idoclim=0
        self.fracmin=0.5
        self.minyr=2
        self.inptfreq=12
        self.NA='Unvailable'
        self.NAN=-999
        self.components={
            1 : 'spatial and temporal mean',
            2 : 'climatological, spatial-mean, seasonal cycle',
            3 : 'climatological, time-mean, zonal-average',
            4 : 'climatological, seasonal cycle of zonal mean (with spatial-mean seasonal cycle removed)',
            5 : 'climatological, time-mean deviations from the zonal mean',
            6 : 'climatological, seasonal cycle of deviations from the zonal mean',
            7 : 'interannual variations',
            8 : 'interannual variations of spatial and annual mean',
            9 : 'interannual variations of spatial-mean, seasonal cycle',
            10 : 'interannual variations of annual-mean, zonal-average (with spatial mean removed)',
            11 : 'interannual variations of seasonal cycle of zonal mean (with spatial-mean seasonal cycle removed)',
            12 : 'interannual variations of annual-mean deviations from the zonal mean',
            13 : 'interannual variations of seasonal cycle of deviations from the zonal mean',
            14 : '3 + 5  (climatological time-mean component)',
            15 : '2 + 4  (climatological zonal mean seasonal cycle component)',
            16 : '2 + 4 + 6 (climatological seasonal cycle component',
            17 : '2 + 3 + 4  (climatological zonal-mean component',
            18 : '5 + 6  (climatological, deviations from the zonal mean)',
            19 : '2 + 3 + 4 + 5 + 6  (climatological component)',
            20 : '2 + 3 + 4 + 5 + 6 + 7 (total space-time field)',
            21 : '9 + 11 (interannual variations of seasonal cycle of zonal mean)',
            22 : '9 + 11 + 13  (interannual variations of seasonal cycle',
            23 : '8 + 10  (interannual variations of annual mean, zonal average)',
            24 : '8 + 10 + 12 (interannual variations of annual mean)',
            25 : '8 + 9 + 10 + 11 (interannual variations of zonal mean)',
            26 : '12 + 13 (interannual variations of deviations from the zonal mean)',
            27 : 'interannual variations',
            28 : '2 + 3 + 4 + 5 + 6 + 7 (total space-time field)',
            }

        self.time_domain={
            1 : 'January',
            2 : 'February',
            3 : 'March',
            4 : 'April',
            5 : 'May',
            6 : 'June',
            7 : 'July',
            8 : 'August',
            9 : 'September',
            10: 'October',
            11: 'November',
            12: 'December',
            13: 'DJF',
            14: 'MAM',
            15: 'JJA',
            16: 'SON',
            17: 'Annual',
            18: 'Seasonal Space-Time',
            19: 'Monthly Space-Time',
            }
    def _compute(self,icall,ref=None,test=None,output=None,returnTuple=1):
        idoclim=self.idoclim

        testin=test
        refin=ref
        # Test passed ?
        if test is None or ref is None:
            ref,test=self.get(returnTuple=returnTuple)
            testin=test
            refin=ref

        if isinstance(test,(list,tuple)):
            testfrac=test[1]
            test=test[0]
        else:
            testfrac=test.mask
            if not testfrac is None:
                testfrac=MV2.array(1.-testfrac)
            else:
                testfrac=MV2.ones(test.shape,numpy.float32)
            testfrac.setAxisList(test.getAxisList())

        if isinstance(ref,(list,tuple)):
            reffrac=ref[1]
            ref=ref[0]
        else:
            reffrac=ref.mask
            if not reffrac is None:
                reffrac=MV2.array(1.-reffrac)
            else:
                reffrac=MV2.ones(ref.shape,numpy.float32)
            reffrac.setAxisList(ref.getAxisList())

        if test.shape != ref.shape:
            raise ValueError, "Input arrays have different shape:"+str(test.shape)+" "+str(ref.shape)

##         # Calendar 360 ?
##         try:
##             icalndr=test.getTime().getCalendar()
##             if icalndr == cdtime.Calendar360:
##                 icalndr = 2
##             else:
##                 icalndr = 1
##         except Exception,err:
##             icalndr = 1



##         sh=ref.shape
##         tmpw=cdutil.area_weights(reffrac(squeeze=1)[0])
##         lenreg=sh[-1]*sh[-2]
##         tmpw=MV2.reshape(tmpw,(lenreg,))
##         tmpw=MV2.resize(tmpw,(sh[0],lenreg))
##         tmpw=MV2.reshape(tmpw,sh)

##         a=reffrac*tmpw
##         print 'Right inside::',sh[0],MA.sum(a.ravel())/sh[0],MA.sum(MA.ravel(reffrac))/sh[0]



        icalndr = 1

        # lon lat time only ?
        if test.rank()!=3:
            test=test(squeeze=1)
            testfrac=testfrac(squeeze=1)
            if test.rank()!=3:
                raise ComparisonStatisticsError," Rank of test data is not 3 !"
        if ref.rank()!=3:
            ref=ref(squeeze=1)
            reffrac=reffrac(squeeze=1)
            if ref.rank()!=3:
                raise ComparisonStatisticsError," Rank of ref data is not 3 !"
        # Time
        tim=test.getTime()

        # Dimensions lengths
        nlat = len(test.getLatitude())
        nlon = len(test.getLongitude())
        lenreg =  nlat*nlon
        lentime = len(tim)

        # Input Frequency
        inptfreq=self.inptfreq
        if inptfreq!=12:
            raise ComparisonStatisticsError,'Frequency can only be 12 at this time\n(mosea1 is not treated right if not 1)'


        # First month
        mosea1 = tim.asComponentTime()[0].month
        ## Makes sure the reference starts the same month
        testrefvalid=ref.getTime().asComponentTime()[0].month
        if testrefvalid!=mosea1:
            raise ComparisonStatisticsError," Reference and Test do not start at the same month: ref is "+str(testrefvalid)+", Test is: "+str(mosea1)
##         Commented out on 7/10 by Karl, he thinks we don't need it
##         if 12 < icall < 17:
##             if mosea1 < 3:
##                 mosea1 = 1
##             elif mosea1 < 6:
##                 mosea1 = 2
##             elif mosea1 < 9:
##                 mosea1 = 3
##             elif mosea1 < 12:
##                 mosea1 = 4
##             else:
##                 mosea1 = 1
##   Commented by C Doutriaux on 7/8/2003,
##   seemed to create offset in ref starting at 2
## So why there was an else ? mystere et boule de gommes !
##         else:
##             mosea1 = 1

        # Reshape test (time,space)
        tmptest=MV2.reshape(test,(lentime,lenreg))
        # TestMask stuff
        # First makes weights 1 for time dim
        sh=test.shape
        a=MV2.ones(tuple(sh[1:]))
        a.setAxisList(test.getAxisList()[1:])
        tmpw=cdutil.area_weights(a)
        tmpw=MV2.reshape(tmpw,(lenreg,))
        tmpw=MV2.resize(tmpw,(lentime,lenreg))
        tmpw=MV2.reshape(tmpw,test.shape)
        testmask=tmpw*testfrac
        testmask=MV2.reshape(testmask,(lentime,lenreg))
        # Reshape reference (time,space)
        tmpref=MV2.reshape(ref,(lentime,lenreg))
        # RefMask stuff
        refmask=tmpw*reffrac
        refmask=MV2.reshape(refmask,(lentime,lenreg))
        # Fill the MV2s
        tmptest=tmptest.filled(0.)
        tmpref=tmpref.filled(0.)
        testmask=testmask.filled(0.).astype(numpy.float32)
        refmask=refmask.filled(0.).astype(numpy.float32)
        # Max year
        maxyr= (lentime -1) / inptfreq + 1

        # Number of times per year
        if icall <= 17:
            itimpyr=1
##         elif icall<=16:
##             itimpyr=1
##             # Now make sure it's a multiple of 4 !
##             if lentime % 4 != 0:
##                 nadd = 4 - (lentime % 4) # number of time steps to add
##         elif icall==17:
##             itimpyr=1
##             # Now make sure it's a multiple of 12 !
##             if lentime % 12 != 0:
##                 nadd = 12 - (lentime % 12) # number of time steps to add
        elif icall == 18:
            itimpyr = 4
        elif icall == 19:
            itimpyr = inptfreq

        # Now adds the missing months at the end if necessary
##         if nadd != 0:
##             sh=tmptest.shape
##             zadd=Numeric.zeros((nadd,sh[1]),Numeric.Float32)
##             tmptest=Numeric.concatenate((tmptest,zadd))
##             tmpref=Numeric.concatenate((tmpref,zadd))
##             testmask=Numeric.concatenate((testmask,zadd))
##             refmask=Numeric.concatenate((refmask,zadd))
##             lentime += nadd # New length of time dimension

        # determine nmx
        nmx = itimpyr * maxyr
        fracmin=self.fracmin
        minyr=self.minyr

        # Additional masking where ref is masked
        testmask=numpy.where(numpy.equal(refmask,0.),0.,testmask).astype(numpy.float32)




        if icall == 19:
##             mosea1=numpy.array(mosea1,copy=1) # make sure it's contiguous
            testmask = testmask.transpose()
            compall.mkmask(icalndr, itimpyr, mosea1, minyr, fracmin, testmask, refmask.transpose(),lenreg, lentime)
            testmask = testmask.transpose()
            tmptest=test.filled(1.E20)
            testmask=numpy.reshape(testmask,(lentime,nlat,nlon))
            tmpref=ref.filled(1.E20)

            
            
            # temporary arrays
            wt6=numpy.zeros((nlon,nlat,itimpyr))
            a2=numpy.zeros((itimpyr))
            a3=numpy.zeros((nlat))
            a4=numpy.zeros((nlat,itimpyr))
            a5=numpy.zeros((nlon,nlat))
            a6=numpy.zeros((nlon,nlat,itimpyr))
            b2=numpy.zeros((itimpyr))
            b3=numpy.zeros((nlat))
            b4=numpy.zeros((nlat,itimpyr))
            b5=numpy.zeros((nlon,nlat))
            b6=numpy.zeros((nlon,nlat,itimpyr))
            wt2=numpy.zeros((itimpyr))
            wt3=numpy.zeros((nlat))
            wt4=numpy.zeros((nlat,itimpyr))
            wt5=numpy.zeros((nlon,nlat))
            ai1=numpy.zeros((maxyr))
            ai2=numpy.zeros((lentime))
            ai3=numpy.zeros((nlat,maxyr))
            ai4=numpy.zeros((nlat,lentime))
            ai5=numpy.zeros((nlon,nlat,maxyr))
            bi1=numpy.zeros((maxyr))
            bi2=numpy.zeros((lentime))
            bi3=numpy.zeros((nlat,maxyr))
            bi4=numpy.zeros((nlat,lentime))
            bi5=numpy.zeros((nlon,nlat,maxyr))
            wi1=numpy.zeros((maxyr))
            wi2=numpy.zeros((lentime))
            wi3=numpy.zeros((nlat,maxyr))
            wi4=numpy.zeros((nlat,lentime))
            wi5=numpy.zeros((nlon,nlat,maxyr))
            siwyr=numpy.zeros((maxyr*2))
            siayr=numpy.zeros((maxyr*2))
            sibyr=numpy.zeros((maxyr*2))

            testmask=testmask.transpose()
            wts, avga, avgb, vara, varb, correl, rms = compall.resolve(idoclim, tmptest.transpose(), tmpref.transpose(), testmask,
                                                                       a2,a3,a4,a5,a6,
                                                                       b2,b3,b4,b5,b6,
                                                                       wt2,wt3,wt4,wt5,wt6,
                                                                       ai1,ai2,ai3,ai4,ai5,
                                                                       bi1,bi2,bi3,bi4,bi5,
                                                                       wi1,wi2,wi3,wi4,wi5,
                                                                       siwyr,siayr,sibyr,
                                                                       nlon, nlat, itimpyr, maxyr, lentime,
                                                                       )
            testmask=testmask.transpose()
        else:
##             mosea1=numpy.array(mosea1,copy=1) # make sure it's contiguous
##             testmask = numpy.array(testmask,copy=1)
##             tmptest = numpy.array(tmptest,copy=1)
##             refmask = numpy.array(refmask,copy=1)
##             tmpref = numpy.array(tmpref,copy=1)
            testmask = numpy.transpose(testmask)
            tmptest = numpy.transpose(tmptest)
            refmask = numpy.transpose(refmask)
            tmpref = numpy.transpose(tmpref)
##             testmask = numpy.array(testmask,copy=1)
##             tmptest = numpy.array(tmptest,copy=1)
##             refmask = numpy.array(refmask,copy=1)
##             tmpref = numpy.array(tmpref,copy=1)

            a1,awt1,a2,awt2,lentime=compall.mksubset(icall,icalndr,idoclim,
                                                     inptfreq,mosea1,testmask, tmptest,refmask,tmpref,nmx,lenreg,lentime)
            compall.mkmask(icalndr, itimpyr, mosea1, minyr, fracmin, awt1[:,:lentime], awt2[:,:lentime],lenreg, lentime)
            a1=numpy.reshape(numpy.transpose(a1),(nmx,nlat,nlon))
            awt1=numpy.reshape(numpy.transpose(awt1),(nmx,nlat,nlon))
            a2=numpy.reshape(numpy.transpose(a2),(nmx,nlat,nlon))
            # temporary arrays
            wt6=numpy.zeros((nlon,nlat,itimpyr))
            A2=numpy.zeros((itimpyr))
            a3=numpy.zeros((nlat))
            a4=numpy.zeros((nlat,itimpyr))
            a5=numpy.zeros((nlon,nlat))
            a6=numpy.zeros((nlon,nlat,itimpyr))
            b2=numpy.zeros((itimpyr))
            b3=numpy.zeros((nlat))
            b4=numpy.zeros((nlat,itimpyr))
            b5=numpy.zeros((nlon,nlat))
            b6=numpy.zeros((nlon,nlat,itimpyr))
            wt2=numpy.zeros((itimpyr))
            wt3=numpy.zeros((nlat))
            wt4=numpy.zeros((nlat,itimpyr))
            wt5=numpy.zeros((nlon,nlat))
            ai1=numpy.zeros((maxyr))
            ai2=numpy.zeros((lentime))
            ai3=numpy.zeros((nlat,maxyr))
            ai4=numpy.zeros((nlat,lentime))
            ai5=numpy.zeros((nlon,nlat,maxyr))
            bi1=numpy.zeros((maxyr))
            bi2=numpy.zeros((lentime))
            bi3=numpy.zeros((nlat,maxyr))
            bi4=numpy.zeros((nlat,lentime))
            bi5=numpy.zeros((nlon,nlat,maxyr))
            wi1=numpy.zeros((maxyr))
            wi2=numpy.zeros((lentime))
            wi3=numpy.zeros((nlat,maxyr))
            wi4=numpy.zeros((nlat,lentime))
            wi5=numpy.zeros((nlon,nlat,maxyr))
            siwyr=numpy.zeros((maxyr*2))
            siayr=numpy.zeros((maxyr*2))
            sibyr=numpy.zeros((maxyr*2))

            awt1=awt1[:lentime].transpose()
            wts, avga, avgb, vara, varb, correl, rms = compall.resolve(idoclim,
                                                                       numpy.transpose(a1[:lentime]),
                                                                       numpy.transpose(a2[:lentime]),
                                                                       awt1,
                                                                       A2,a3,a4,a5,a6,
                                                                       b2,b3,b4,b5,b6,
                                                                       wt2,wt3,wt4,wt5,wt6,
                                                                       ai1,ai2,ai3,ai4,ai5,
                                                                       bi1,bi2,bi3,bi4,bi5,
                                                                       wi1,wi2,wi3,wi4,wi5,
                                                                       siwyr,siayr,sibyr,
                                                                       nlon, nlat, itimpyr, maxyr, lentime,
                                                                       )
            awt1=awt1.transpose()
        wts    = wts.transpose()
        avga   = avga.transpose()
        avgb   = avgb.transpose()
        vara   = vara.transpose()
        varb   = varb.transpose()
        correl = correl.transpose()
        rms = rms.transpose()
        
        if not refin is None:
            ref=refin
            if isinstance(ref,(list,tuple)):
                ref,reffrac=refin
        if not testin is None:
            test=testin
            if isinstance(test,(list,tuple)):
                test,testfrac=testin
        if returnTuple:
            return (ref,reffrac),(test,testfrac), wts, avga, avgb, vara, varb, correl, rms
        else:
            return ref, test, wts, avga, avgb, vara, varb, correl, rms


    def __cleanup(self, input):
        """
        Cleans up a fortran returned array from NaN and other "bad" values
        Returns an MA, maksed where input is "bad"
        Thx to Jean Marie Epitalon for the idea for masking
        """
        # Mask out NAN values from the input
        # In order to discriminate out NAN values,
        # the only way I found is to compare against a valid float value
        # By the same token discriminate out null values
        a=numpy.greater(input,0.)
        b=numpy.less_equal(input,0.)
        mask = numpy.logical_not(numpy.logical_or(a,b))

        if not numpy.ma.allequal(mask,0.):
            ## Did we actually had bad values
            output = numpy.ma.array (input, mask= mask)
        else:
            output=numpy.ma.array(input,copy=0)

        return numpy.ma.transpose(output)


    def compute(self,time_domain=range(1,20),returnTuple=1):
        if  isinstance(time_domain,(int,float,numpy.floating)):
            time_domain=[time_domain]
        wts    = numpy.zeros((len(time_domain),28),numpy.float32)
        avga   = numpy.zeros((len(time_domain),28),numpy.float32)
        avgb   = numpy.zeros((len(time_domain),28),numpy.float32)
        vara   = numpy.zeros((len(time_domain),28),numpy.float32)
        varb   = numpy.zeros((len(time_domain),28),numpy.float32)
        correl = numpy.zeros((len(time_domain),28),numpy.float32)
        rms    = numpy.zeros((len(time_domain),28),numpy.float32)
        time_domain_names=[]
        a=None
        b=None
        for i in range(len(time_domain)):
            icall=time_domain[i]
            if not icall in self.time_domain.keys():
                raise ComparisonStatisticsError, "Time Domain:"+str(icall)+" Invalid"
            time_domain_names.append(self.time_domain[icall])
            a, b, wts[i], avga[i], avgb[i], vara[i], varb[i], correl[i], rms[i] = self._compute(icall,a,b,returnTuple=1)
        wts=self.__cleanup(wts)
        avga=self.__cleanup(avga)
        avgb=self.__cleanup(avgb)
        vara=self.__cleanup(vara)
        varb=self.__cleanup(varb)
        correl=self.__cleanup(correl)
        rms=self.__cleanup(rms)
##         print wts[0]
##         print wts2[0]
        components_names=[]
        for i in range(1,29):
            components_names.append(self.components[i])

        # Now creates the stat objects
        self.area_wts=self.AreaWeights=Statisitic(wts,range(1,29),components_names,time_domain,time_domain_names,id='Weights')
        self.avg1=self.TestAverage=Statisitic(avga,range(1,29),components_names,time_domain,time_domain_names,id='TestAverage')
        self.avg2=self.ReferenceAverage=Statisitic(avgb,range(1,29),components_names,time_domain,time_domain_names,id='ReferenceAverage')
        self.var1=self.TestVariance=Statisitic(vara,range(1,29),components_names,time_domain,time_domain_names,id='TestVariance')
        self.var2=self.ReferenceVariance=Statisitic(varb,range(1,29),components_names,time_domain,time_domain_names,id='ReferenceVariance')
        self.correl=self.Correlation=Statisitic(correl,range(1,29),components_names,time_domain,time_domain_names,id='Correlation')
        self.rms=self.RMS=self.RootMeanSquare=Statisitic(rms,range(1,29),components_names,time_domain,time_domain_names,id='RootMeanSquare')

        tmp = vara/vara[27]
        tmp = 100. * tmp
        self.pvar1=self.TestPercentOfTotalVariance=Statisitic(tmp,range(1,29),components_names,time_domain,time_domain_names,id='TestPercentofTotalVariance')

        tmp = varb/varb[27]
        tmp = 100. * tmp
        self.pvar2=self.ReferencePercentOfTotalVariance=Statisitic(tmp,range(1,29),components_names,time_domain,time_domain_names,id='ReferencePercentofTotalVariance')

        tmp = vara/varb[27]
        tmp = 100. * tmp
        self.pvar1a=self.TestPercentOfReferenceTotalVariance=Statisitic(tmp,range(1,29),components_names,time_domain,time_domain_names,id='TestPercentofReferenceTotalVariance')

        rms2 = rms * rms
        tmp = rms2/rms2[27]
        tmp = 100. * tmp
        self.pmsdiff=self.PercentOfTotalMeanSquareDifference=Statisitic(tmp,range(1,29),components_names,time_domain,time_domain_names,id='PercentTotalMeanSquareDifference')

        tmp = rms/numpy.ma.sqrt (numpy.ma.sqrt (vara) * numpy.ma.sqrt (varb))
        tmp = 100. * tmp
        self.rank=self.Rank=Statisitic(tmp,range(1,29),components_names,time_domain,time_domain_names,id='Rank')
        self.icalls=time_domain
        if returnTuple:
            return a,b
        else:
            return a[0],b[0]


    def write(self,file,comments='None',ref=None,test=None,mode='w'):
        # Fisrt check if it is an ascii output or not
        sp=file.split('.')
        if not mode in ['r+','w','a']:
            raise ComparisonStatisticsError, "mode must be one of: 'w', 'r+', 'a'"
        #print 'SPLIT IS:',sp
        if sp[-1].lower() in ['txt','asc','doc','out','text','asci','ascii']:
            # OK user wants ascii output (Karl's stuff)
            if test is None or ref is None:
                raise ComparisonStatisticsError, "ASCII output requires that you pass the result from compute (2 slabs)"
            asciiargs=None
            if mode=='w' :
                a,b=os.popen4('rm \''+file+'\'')
                b.readlines()
            for icall in self.icalls:
                wts=self.area_wts(time_domain=icall)
                avg1=self.avg1(time_domain=icall)
                avg2=self.avg2(time_domain=icall)
                var1=self.var1(time_domain=icall)
                var2=self.var2(time_domain=icall)
                correl=self.correl(time_domain=icall)
                rms=self.rms(time_domain=icall)
                reset_n=0
                if icall == self.icalls[0] and mode == 'w' : reset_n=1
                if isinstance(test,(list,tuple)):
                    test=test[0]
                if isinstance(ref,(list,tuple)):
                    ref=ref[0]
                    
                asciiargs=self.write_asc(file, icall, ref, test, wts, avg1, avg2, var1, var2, correl, rms,asciiargs=[asciiargs,reset_n])
        else:
            f=cdms2.open(file,mode)
            f.comments=comments
            f.dataset1=str(self.V1)
            f.dataset2=str(self.V2)
            f.externaldataset=str(self.EV)
            f.final_grid=str(self.weightedGridMaker)
            tmp=self.area_wts()
            f.time_domain=tmp.time_domain
            f.components=tmp.components
            del(tmp.time_domain)
            del(tmp.components)
            f.write(tmp)
            tmp=self.avg1()
            del(tmp.time_domain)
            del(tmp.components)
            f.write(tmp)
            tmp=self.avg2()
            del(tmp.time_domain)
            del(tmp.components)
            f.write(tmp)
            tmp=self.var1()
            del(tmp.time_domain)
            del(tmp.components)
            f.write(tmp)
            tmp=self.var2()
            del(tmp.time_domain)
            del(tmp.components)
            f.write(tmp)
            tmp=self.correl()
            del(tmp.time_domain)
            del(tmp.components)
            f.write(tmp)
            tmp=self.rms()
            del(tmp.time_domain)
            del(tmp.components)
            f.write(tmp)
            tmp=self.pvar1()
            del(tmp.time_domain)
            del(tmp.components)
            f.write(tmp)
            tmp=self.pvar2()
            del(tmp.time_domain)
            del(tmp.components)
            f.write(tmp)
            tmp=self.pvar1a()
            del(tmp.time_domain)
            del(tmp.components)
            f.write(tmp)
            tmp=self.pmsdiff()
            del(tmp.time_domain)
            del(tmp.components)
            f.write(tmp)
            tmp=self.rank()
            del(tmp.time_domain)
            del(tmp.components)
            f.write(tmp)
            f.close()
            
    def write_asc(self, output, icall, b, a, wts, avga, avgb, vara, varb, correl, rms,asciiargs=[None,0]):
##         if returnTuple:
##             afrac=a[1]
##             a=a[0]
##             bfrac=b[1]
##             b=b[0]
        asciiargs,reset_n=asciiargs
        itarg= ms1a = icomp = alon = alat = self.NAN
        ifulla=11
        imona=14
        iseasa=6
        ianna=1
        ncomps=1

        if asciiargs is None:
            # pre
            months = numpy.zeros((3,2),dtype='i')
            years  = numpy.zeros((3,2),dtype='i')
            # test test
            vars1    = str(self.V2.var)
            modl1    = str(self.V2.id)
            pthd1    = str(self.V2.file)
            pthmask1 = str(self.V2.weightsMaker.file)
            mskvar1  = str(self.V2.weightsMaker.var)
            grd1     = self.V2.weightedGridMaker.latitude.type
            if grd1 == 0:
                grd1='uniform'
            elif grd1 == 1:
                grd1='gaussian'
            elif grd1 == 2:
                grd1='equal'
            source1  = getattr(a,'source',self.NA)
            if not self.V2.cdmsKeywords.has_key('level'):
                alev10 = 0
                retrlev1 = 0
                levunits1 = ''
            else:
                alev10 = self.V2.cdmsKeywords['level']
                try:
                    retrlev1 = a.getLevel()[0]
                except:
                    retrlev1 = self.NAN
                levunits1 = getattr(a.getLevel(),'units','')
            t=a.getTime()
            mons10=0
            mons11=0
            if not self.V2.file is None:
                f=cdms2.open(self.V2.file)
                V=f[self.V2.var]
                tim=V.getTime()
                for i in range(len(tim)):
                    tv=tim[i]
                    if t[0]  ==tv : mons10 = i+1
                    if t[-1] ==tv : mons11 = i+1
                f.close()
            tc=t.asComponentTime()
            months[0,0] = tc[0].month
            months[0,1] = tc[-1].month
            years[0,0]  = tc[0].year
            years[0,1]  = tc[-1].year
            units1 = getattr(a,'units',self.NA)
            sloptst = self.V2.slope
            offsett = self.V2.offset

            # Ref test
            vars2    = str(self.V1.var)
            modl2    = str(self.V1.id)
            pthd2    = str(self.V1.file)
            pthmask2 = str(self.V1.weightsMaker.file)
            mskvar2  = str(self.V1.weightsMaker.var)
            grd2     = self.V1.weightedGridMaker.latitude.type
            if grd2 == 0:
                grd2='uniform'
            elif grd2 == 1:
                grd2='gaussian'
            elif grd2 == 2:
                grd2='equal'
            source2  = getattr(b,'source',self.NA)
            if not self.V1.cdmsKeywords.has_key('level'):
                alev20 = 0
                retrlev2 = 0
                levunits2 = ''
            else:
                alev20 = self.V1.cdmsKeywords['level']
                try:
                    retrlev2 = b.getLevel()[0]
                except:
                    retrlev2 = self.NAN
                levunits2 = getattr(b.getLevel(),'units',self.NA)
            t=b.getTime()
            mons20=0
            mons21=0
            if not self.V1.file is None:
                f=cdms2.open(self.V1.file)
                V=f[self.V1.var]
                tim=V.getTime()
                for i in range(len(tim)):
                    tv=tim[i]
                    if t[0]  ==tv : mons20 = i+1
                    if t[-1] ==tv : mons21 = i+1
                f.close()
            tc=t.asComponentTime()
            months[1,0] = tc[0].month
            months[1,1] = tc[-1].month
            years[1,0]  = tc[0].year
            years[1,1]  = tc[-1].year
            units2 = getattr(a,'units',self.NA)
            slopref = self.V1.slope
            offsetr = self.V1.offset

            # Ext data
            if not self.EV is None:
                itarg=1
                mvar    = str(self.EV.var)
                mmodl   = str(self.EV.id)
                mpth   = str(self.EV.file)
                mpthmask = str(self.EV.weightsMaker.file)
                mmskvar  = str(self.EV.weightsMaker.var)
                mgrd     = self.EV.weightedGridMaker.latitude.type
                if mgrd == 0:
                    mgrd='uniform'
                elif mgrd == 1:
                    mgrd='gaussian'
                elif mgrd == 2:
                    mgrd='equal'
            else:
                itarg=-1
                mvar=mmodl=mpth=mpthmask=mmskvar=mgrd=sourcem=self.NA
            sourcem=self.NA
            alev0 = 0
            retrlevm = 0
            levunits3 = ' '
            mmons0=0
            mmons1=0
            months[2,0] = months[1,0]
            months[2,1] = months[1,1]
            years[2,0]  = years[1,0]
            years[2,1]  = years[1,1]

            # Target Grid
            tmodl=self.NA
            if not self.weightedGridMaker() is None:
                itarg=itarg*2
                tvar=str(self.weightedGridMaker.var)
                tpth=str(self.weightedGridMaker.file)
                tpthmask=str(self.weightedGridMaker.weightsMaker.file)
                tmskvar=str(self.weightedGridMaker.weightsMaker.var)
                tgrd=self.weightedGridMaker.latitude.type
                if tgrd == 0:
                    tgrd='uniform'
                elif tgrd == 1:
                    tgrd='gaussian'
                elif tgrd == 2:
                    tgrd='equal'
            else:
                tvar=tpth=tpthmask=tmskvar=tgrd=self.NA
            sourcet  = self.NA
            try:
                region=self.karls_dictionary['region']
            except:
                region=self.NA
            minyr=self.minyr
            fracmin=self.fracmin
            lon=a.getLongitude()
            blon=lon.getBounds()
            lat=a.getLatitude()
            blat=lat.getBounds()
            alon0 = numpy.ma.minimum(blon)
            alon1 = numpy.ma.maximum(blon)
            alat0 = numpy.ma.minimum(blat)
            alat1 = numpy.ma.maximum(blat)

            l1f=numpy.zeros(5)
            l2f=numpy.zeros(5)
            l1o=numpy.zeros(5)
            l2o=numpy.zeros(5)
            mi=numpy.zeros(5)
            mo=numpy.zeros(5)
            l1i=numpy.zeros(5)
            l2i=numpy.zeros(5)
            if not self.V2.file is None:
                f=cdms2.open(self.V2.file)
                V=f[self.V2.var]
                lon=V.getLongitude()
                lat=V.getLatitude()
                tim=V.getTime()
                l=V.getLevel()
                l1i[2]=len(tim)
                l1i[1]=len(lat)
                l1i[0]=len(lon)
                if not l is None: l1i[3]=len(l)
                f.close()

            if not self.V1.file is None:
                f=cdms2.open(self.V1.file)
                V=f[self.V1.var]
                lon=V.getLongitude()
                lat=V.getLatitude()
                tim=V.getTime()
                l=V.getLevel()
                l2i[2]=len(tim)
                l2i[1]=len(lat)
                l2i[0]=len(lon)
                if not l is None: l2i[3]=len(l)
                f.close()

            if not self.EV is None:
                if not self.EV.file is None:
                    f=cdms2.open(self.EV.file)
                    V=f[self.EV.var]
                    lon=V.getLongitude()
                    lat=V.getLatitude()
                    tim=V.getTime()
                    l=V.getLevel()
                    mi[2]=len(tim)
                    mi[1]=len(lat)
                    mi[0]=len(lon)
                    if not l is None: mi[3]=len(l)
                    f.close()
                tmp=self.EV(returnTuple=0)
                lon=tmp.getLongitude()
                lat=tmp.getLatitude()
                tim=tmp.getTime()
                l=tmp.getLevel()
                mo[2]=len(tim)
                mo[1]=len(lat)
                mo[0]=len(lon)
                if not l is None: mo[3]=len(l)


            tmp=self.V2(returnTuple=0)
            lon=tmp.getLongitude()
            lat=tmp.getLatitude()
            tim=tmp.getTime()
            l=tmp.getLevel()
            l1o[2]=len(tim)
            l1o[1]=len(lat)
            l1o[0]=len(lon)
            if not l is None: l1o[3]=len(l)

            tmp=self.V1(returnTuple=0)
            lon=tmp.getLongitude()
            lat=tmp.getLatitude()
            tim=tmp.getTime()
            l=tmp.getLevel()
            l2o[2]=len(tim)
            l2o[1]=len(lat)
            l2o[0]=len(lon)
            if not l is None: l2o[3]=len(l)

            tmp=a
            lon=tmp.getLongitude()
            lat=tmp.getLatitude()
            tim=tmp.getTime()
            l=tmp.getLevel()
            l1f[2]=len(tim)
            l1f[1]=len(lat)
            l1f[0]=len(lon)
            if not l is None: l1f[3]=len(l)

            tmp=b
            lon=tmp.getLongitude()
            lat=tmp.getLatitude()
            tim=tmp.getTime()
            l=tmp.getLevel()
            l2f[2]=len(tim)
            l2f[1]=len(lat)
            l2f[0]=len(lon)
            if not l is None: l2f[3]=len(l)

            if not self.weightedGridMaker() is None:
                mlon = self.weightedGridMaker.longitude.n
                mlat = self.weightedGridMaker.latitude.n
                dellon = self.weightedGridMaker.longitude.delta
                dellat = self.weightedGridMaker.latitude.delta
                alon = self.weightedGridMaker.longitude.first
                alat = self.weightedGridMaker.latitude.first
                if mlon is None: mlon=mlat=dellon=dellat=alon=alat=self.NAN
            else:
                mlon=mlat=dellon=dellat=self.NAN
            template = output
            date=time.asctime()
            sp = date.split()
            rundate = '.'.join([sp[0],sp[1],sp[2]+',',sp[4]])
            runtime = sp[3]
            idoclim=self.idoclim
##             print 'No else this time'
        else:
##             print 'Else now'
            nn,icall1, vars1, modl1, pthd1, pthmask1, vars2, modl2, pthd2, pthmask2,  region, minyr, fracmin, months, years,  alon0, alon1, alat0, alat1, date, icomp,  ncomps, mons10, mons11, mons20, mons21,  l1i, l2i, l1f, l2f, itarg,  tmodl, tpth, tvar, tpthmask, alon, mlon, dellon, alat, mlat, dellat,   mvar, mmodl, mpth, mpthmask,  mmons0, mmons1,   wts1, avga1, avgb1, vara1, varb1, correl1, rms1,  template, retrlev1, retrlev2, retrlevm, source1, units1,  sloptst, offsett,  mskvar1, grd1, l1o, source2, units2,  slopref, offsetr,  mskvar2, grd2, l2o, sourcem, mmskvar, mgrd, mi, mo, sourcet, tmskvar, tgrd,  levunits1, levunits2, levunits3, ms1a,    idoclim, ifulla, imona, iseasa, ianna,  rundate, runtime, alev0, alev10, alev20 = asciiargs

        #print 'before going in:',type(pthmask2),type(modl1),units1
        tmpm= months.transpose()
        tmpyr = years.transpose()
        compall.wrtstats(1, icall,
                         vars1+' ', modl1+' ', pthd1+' ', pthmask1+' ', #6
                         vars2+' ', modl2+' ', pthd2+' ', pthmask2+' ', #10
                         region+' ', minyr, fracmin, tmpm,tmpyr, #15
                         alon0, alon1, alat0, alat1, #21
                         ncomps, mons10, mons11, mons20, mons21, #26
                         l1i, l2i, l1f, l2f, itarg, #31
                         tmodl+' ', tpth+' ', tvar+' ', tpthmask+' ', #35
                         alon, mlon, dellon, alat, mlat, dellat, #39
                         mvar+' ', mmodl+' ', mpth+' ', mpthmask+' ', #43
                         mmons0, mmons1, #45
                         wts, avga, avgb, vara, varb, correl, rms, #52
                         template, retrlev1, retrlev2, retrlevm, units1+' ', #58
                         sloptst, offsett, #60
                         mskvar1+' ', l1o, units2+' ', #65
                         slopref, offsetr, #67
                         mskvar2+' ', l2o, mmskvar+' ', #73
                         mi, mo, tmskvar+' ', #78
                         levunits1+' ', levunits2+' ', levunits3+' ', #82
                         idoclim, ifulla, imona, iseasa, ianna, #87
                         rundate+' ', runtime+' ', alev0, alev10, alev20, reset_n) #92

        return (1, icall,
                vars1, modl1, pthd1, pthmask1, #6
                vars2, modl2, pthd2, pthmask2, #10
                region, minyr, fracmin, months, years, #15
                alon0, alon1, alat0, alat1, date, icomp, #21
                ncomps, mons10, mons11, mons20, mons21, #26
                l1i, l2i, l1f, l2f, itarg, #31
                tmodl, tpth, tvar, tpthmask, #35
                alon, mlon, dellon, alat, mlat, dellat, #39
                mvar, mmodl, mpth, mpthmask, #43
                mmons0, mmons1, #45
                wts, avga, avgb, vara, varb, correl, rms, #52
                template, retrlev1, retrlev2, retrlevm, source1, units1, #58
                sloptst, offsett, #60
                mskvar1, grd1, l1o, source2, units2, #65
                slopref, offsetr, #67
                mskvar2, grd2, l2o, sourcem, mmskvar, mgrd, #73
                mi, mo, sourcet, tmskvar, tgrd, #78
                levunits1, levunits2, levunits3, ms1a, #82
                idoclim, ifulla, imona, iseasa, ianna, #87
                rundate, runtime, alev0, alev10, alev20)
