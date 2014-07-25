# Adapted for numpy/ma/cdms2 by convertcdms.py
import types
import string
import numpy
import numpy.ma
import MV2
import cdms2
import regrid2
import cdtime
import ValidationFunctions
import exceptions
import genutil

class WeightsMakerError(exceptions.Exception):
    def __init__(self,args):
        self.args=args

class WeightsMaker(object):
    def __init__(self,source=None,var=None,values=None,actions=[MV2.equal,],combiningActions=[MV2.multiply,]):
        if type(source)==types.StringType:
            self.file=source
            self.mask=None
        else:
            self.mask=source
            self.file=None
        self.var=var
        self.values=values
        self.actions=actions
        self.combiningActions=combiningActions

    def get(self,input=None):
        v=self.mask
        if type(self.file)==types.StringType:
            f=cdms2.open(self.file)
            v=f(self.var,squeeze=1)
            f.close()
##         if v is None: return v
        if self.values is None:
            return v
        else:
##             if not v is None:
##                 m=MV2.zeros(v.shape)
##                 m.setAxisList(v.getAxisList())
##             else:
##                 # if it is still None that means we didn't define a mask slab
##                 # probably all coming from input
##                 m=MV2.zeros(input.shape)
##                 m.setAxisList(input.getAxisList())                
            for iv in range(len(self.values)):
                val=self.values[iv]
                comp=v
                if type(val) in [types.ListType,types.TupleType]:
                    if val[0]=='input':
                        val=val[1]
                        comp=input
                m2=self.actions[iv](comp,val)
                if iv==0 :
                    m=m2
                else:
                    m=self.combiningActions[iv-1](m,m2)
            return m
            
    __call__=get

    # Representation of Mask object
    def __str__(self):
        s='WeightsMaker Object: '
        if self.file is None and ((self.mask is None) or (self.mask is MV2.nomask)):
            s+='None'
        else:
            if not self.file is None: s+='\nfile:'+str(self.file)+'\nVar:'+str(self.var)
##             if not self.data is None: s+='\ndata:'+str(self.data)
            if not ((self.mask is None) or (self.mask is MV2.nomask)): s+='\nmask:'+str(self.mask)
            if not self.values is None:
                s+='\nvalues: ['
                for v in self.values:
                    s+=str(v)+', '
                s+=']\nactions: ['
                for a in self.actions:
                    s+=str(a)+', '
                s+=']\ncombiningActions: ['
                for c in self.combiningActions:
                    s+=str(c)
                s+=']\n'
        return s
    
    __slots__=['mask','var','values','actions','file','combiningActions','data','_var']

    def _set_var(self,value):
        self._var=ValidationFunctions.checkStringOrNone(self,'var',value)

    def _get_var(self):
        return self._var

    def _del_var(self):
        del self._var
    var=property(_get_var,_set_var)
        
        
class WeightedGridMakerError(WeightsMakerError):
    pass

class GridAxis(object):
    def __init__(self):
        self.first=None
        self.n=None
        self.delta=None
        self.type='Uniform'
    def __str__(self):
        s ='first:'+str(self.first)+'\n'
        s+='n:'+str(self.n)+'\n'
        s+='delta:'+str(self.delta)+'\n'
        s+='type:'+str(self.type)
        return s

    __slots__=['first','n','delta','type','_first','_n','_delta','_type',]

    def _set_first(self,value):
        self._first=ValidationFunctions.checkNumberOrNone(self,'first',value)
    def _get_first(self):
        return self._first
    first=property(_get_first,_set_first)
    
    def _set_delta(self,value):
        self._delta=ValidationFunctions.checkNumberOrNone(self,'delta',value)
    def _get_delta(self):
        return self._delta
    delta=property(_get_delta,_set_delta)

    def _set_n(self,value):
        self._n=ValidationFunctions.checkIntOrNone(self,'n',value)
    def _get_n(self):
        return self._n
    n=property(_get_n,_set_n)
    
    def _set_type(self,value):
        self._type=ValidationFunctions.checkAxisType(self,'type',value)
    def _get_type(self):
        return self._type
    type=property(_get_type,_set_type)
    

class WeightedGridMaker(object):
    def __init__(self,
                 source=None,
                 var=None,
                 nlat=None,flat=None,dellat=None,
                 grid_type='uniform',
                 nlon=None,flon=None,dellon=None,
                 weightsMaker=None
                 ):
        self.longitude=GridAxis()
        self.latitude=GridAxis()
        self.longitude.n=nlon
        self.longitude.first=flon
        self.longitude.delta=dellon
        self.latitude.n=nlat
        self.latitude.first=flat
        self.latitude.delta=dellat
        self.latitude.type=grid_type
        if isinstance(source,cdms2.grid.AbstractGrid):
            self.grid=source
        elif isinstance(source,(str,unicode)):
            self.file=source
            f=cdms2.open(source)
            V=f[var]
            self.grid=V.getGrid()
        else:
            self.grid=None
            self.file=None
        if isinstance(weightsMaker,WeightsMaker):
            self.weightsMaker=weightsMaker
        elif weightsMaker is None:
            self.weightsMaker=WeightsMaker()
        else:
            raise WeightedGridMakerError, "WeightedGridMaker: weightsMaker must be None or WeightsMaker instance, you passed:"+repr(weightsMaker)
        self.var=var
        
    def __str__(self):
        s='GridMaker Object:'
        if not self.grid is None:
            s+='\n'+str(self.grid)
        elif not self.latitude.n is None:
            s+='\nLongitude:\n'+str(self.longitude)
            s+='\nLatitude:\n'+str(self.latitude)
        elif not self.file is None:
            s+='\nFile:'+str(self.file)+'\n'
            s+='Variable:'+str(self.var)
        else:
            s+='None'
        s+='\n'+str(self.weightsMaker)
        return s
    
    def get(self):
        value=self.grid
        if value is None:
            if not self.latitude.n is None: # Ok we seem to have grid definition
                if self.latitude.type == 1: # Gaussian
                    if self.longitude.first is None:
                        value=cdms2.createGaussianGrid(self.latitude.n)
                    else:
                        value=cdms2.createGaussianGrid(self.latitude.n,self.longitude.first)
                elif self.latitude.type == 0: # Uniform
                    value=cdms2.createUniformGrid(self.latitude.first,
                                                 self.latitude.n,
                                                 self.latitude.delta,
                                                 self.longitude.first,
                                                 self.longitude.n,
                                                 self.longitude.delta)
                elif self.latitude.type== 2: # Equalarea
                    lat=cdms2.createEqualAreaAxis(self.latitude.n)
                    lon=cdms2.createUniformLongitude(self.longitude.first,
                                                    self.longitude.n,
                                                    self.longitude.delta)
                    value=cdms2.createGenericGrid(lat[:],lon[:],lat.getBounds(),lon.getBounds())
            elif not self.file is None:
                f=cdms2.open(self.file)
                value=f[self.var].getGrid()
                ## Ok temporary stuff to try to be able to close the file
                lat=value.getLatitude()
                lon=value.getLongitude()
                value=cdms2.createRectGrid(lat,lon)
                f.close()
        return value
    
    __call__=get
    

    __slots__=['grid','var','file','longitude','latitude','weightsMaker','_var','_file','_grid']

    def _get_var(self):
        return self._var
    def _set_var(self,value):
        self._var=ValidationFunctions.checkStringOrNone(self,'var',value)
    var=property(_get_var,_set_var)
    
    def _get_file(self):
        return self._file
    def _set_file(self,value):
        name,value=ValidationFunctions.setSlab(self,'file',value)
        if name=='file':
            self._file=value
        else:
            setattr(self,name,value)
    file=property(_get_file,_set_file)
    
    def _get_grid(self):
        return self._grid
    def _set_grid(self,value):
        self._grid=ValidationFunctions.setGrid(self,'grid',value)
    grid=property(_get_grid,_set_grid)
    
class VariableConditionerError(WeightedGridMakerError):
    pass

class VariableConditioner(object):
    def __init__(self,source,var=None,weightsMaker=None,weightedGridMaker=None,offset=0.,slope=1.,cdmsArguments=None,cdmsKeywords=None,id=None,preprocess=None,preprocessKeywords={},comments=''):
        self.id=id
        self.var=var
        self.offset=offset
        self.slope=slope
        if cdmsArguments is None:
            self.cdmsArguments=[]
        else:
            self.cdmsArguments=cdmsArguments
        if cdmsKeywords is None:
            self.cdmsKeywords={}
        else:
            self.cdmsKeywords=cdmsKeywords
        if  weightsMaker is None:
            self.weightsMaker=WeightsMaker()
        elif isinstance(weightsMaker,WeightsMaker) or weightsMaker is None:
            self.weightsMaker=weightsMaker
        else:
            raise VariableConditionerError,"WeightsMaker is wrong"
        if weightedGridMaker is None:
            self.weightedGridMaker=WeightedGridMaker()
        elif isinstance(weightedGridMaker,WeightedGridMaker):
            self.weightedGridMaker=weightedGridMaker
        elif isinstance(weightedGridMaker,cdms2.grid.AbstractGrid):
            self.weightedGridMaker=WeightedGridMaker()
            self.weightedGridMaker.grid=weightedGridMaker
            
        if type(source)==types.StringType :
            self.file=source
            self.data=None
        else:
            self.data=source
            self.file=None
        self.preprocess=preprocess
        self.preprocessKeywords=preprocessKeywords
        self.comments=comments
        
    def __str__(self):
        s='VariableConditioner Object\n'
        s+='id:'+str(self.id)
        if not self.file is None:
            s+='\nfile:'+str(self.file)
            s+='\nvar:'+str(self.var)
        else:
            s+='\ndata:'+str(self.data)
        if not self.cdmsArguments is None:
            s+='\ncdmsArguments:'
            for a in self.cdmsArguments:
                s+=str(a)+', '
        if not self.cdmsKeywords is None:
            s+='\ncdmsKeywords:'
            for k in self.cdmsKeywords.keys():
                s+='\n\t'+k+':'+str(self.cdmsKeywords[k])
        s+='\n'+str(self.weightsMaker)
        s+='\n'+str(self.weightedGridMaker)
        if self.slope!=1. : s+='\nslope:'+str(self.slope)
        if self.offset!=0. : s+='\noffset:'+str(self.offset)
        if self.comments!='':
            s+='\ncomments:'+str(self.comments)
        return s
    
    def get(self,returnTuple=1):
        value=self.data
        frc=None
        if type(value) in [types.TupleType, types.ListType]:
            value,frc=value
        if isinstance (value,numpy.ndarray ) or numpy.ma.isMA(value): # Variable defined from array
            if frc is None: frc=numpy.ma.ones(value.shape,dtype=numpy.float32)
            kw={}
            args=[]
            # Add user defined cdmsArguments
            for a in self.cdmsArguments:
                args.append(a)
            # Add user defined cdmsKeywords
            for k in self.cdmsKeywords.keys():
                kw[k]=self.cdmsKeywords[k]
            # try to apply, if not forget about it
            try:
                v=value(*args,**kw)
                frc=frc(*args,**kw)
                # Now removes the slice types
                # because they can't be used twice
                for k in kw.keys():
                    if type(kw[k])==types.SliceType:
                        del(kw[k])
                for i in range(len(args)):
                    if type(args[i])==types.SliceType:
                        pop(args,i)
                        i=i-1                
            except:
                v=value
        else: # Variable comes from a file, need to be retrieved
            f=cdms2.open(self.file)
            kw={}
            args=[]
            # Add user defined cdmsArguments
            for a in self.cdmsArguments:
                args.append(a)
            # Add user defined cdmsKeywords
            for k in self.cdmsKeywords.keys():
                kw[k]=self.cdmsKeywords[k]
            v=f(self.var,*args,**kw)
            f.close()
            # Now removes the slice types
            # because they can't be used twice
            for k in kw.keys():
                if type(kw[k])==types.SliceType:
                    del(kw[k])
            for i in range(len(args)):
                if type(args[i])==types.SliceType:
                    pop(args,i)
                    i=i-1

        ## At that stage applied the preprocess function
        if self.preprocess is not None:
            v=apply(self.preprocess,(v,),self.preprocessKeywords)

        # Create the fractions
        if frc is None:
            frc=v.mask
            if frc is numpy.ma.nomask: #no mask
                # Create a bunch of ones (100%)
                frc=numpy.ones(v.shape,numpy.float32)
            else:
                # Fraction are actually just the opposite of the mask at that stage !
                frc=frc.astype(MV2.float32) # Sometimes if it is bytes it doesn't work
                frc=1.-frc
                frc=frc.astype(MV2.float32) # no need for double precision here !
        else:
            m=v.mask
            if not m is numpy.ma.nomask:
                frc=MV2.where(m,0.,frc).filled(0.)
        # Now get the associted weights object
        # Note that we pass v in case some of the values are defined as "input"
        # in which case it would use v instead of the weights for weightsing
        m=self.weightsMaker(v)
        if not m is None:
            # grows the variable and the weights for possible Xtra dimensions
            m=m(*args,**kw)
            v,m=genutil.grower(v,m)
            # make sure variable and weights are compatible
            if m.shape != v.shape:
                raise VariableConditionerError, 'weights and variable have different shapes: weights is '+str(m.shape)+' and grid is '+str(v.shape)
            # make sure they're on the same grid (in case one starts at 0 and one at -180 for example

            if not m.getGrid() is v.getGrid() :
                m = m.astype("i").regrid(v.getGrid())
            
            # Mask the dataset where the fraction are 0.
            v   = MV2.masked_where(MV2.equal(m.filled(0),0.),v)
            # Update the fractions
            frc=m.filled(0.)
            m=v.mask
            if not m is numpy.ma.nomask:
                frc=numpy.where(m,0.,frc)
##             # Filll the mask with ones, i.e. set fraction to 0 when the mask is masked hahah
##             frc = numpy.where(m.filled(1),0.,frc)
        # Now get the target grid
        g=self.weightedGridMaker()
        if not g is None: # we do have a target grid to go to !
            # Create the regridder object
            rf=regrid2.Horizontal(v.getGrid(),g)
            # and regrid passing the weights to use to each grid cell
            # at this point it should be only 0/1
            v,frc=rf(v,mask=1.-frc,returnTuple=1)
            frc=MV2.array(frc)
            frc.setAxisList(v.getAxisList())
            v=v(*args,**kw)
            frc=frc(*args,**kw).filled(0.)
            # Note that now frc is not necessarily 0. and 1. but actuall fraction
            # of the grid cell that has real data in it.
            # do we weights after this regridding ?
            # once again pass v in case the weightsing wants
            # to work on the variable
            m=self.weightedGridMaker.weightsMaker(v)
            if not m is None: # we have a weights
                m=m(*args,**kw) # apply the extra cdmsKeywords to it
                v,m=genutil.grower(v,m)
                # make sure variable and weights are compatible
                if m.shape != v.shape:
                    raise VariableConditionerError, 'weights and variable have different shapes: weights is '+str(m.shape)+' and grid is '+str(v.shape)
                # make sure they're on the same grid (in case one starts at 0 and one at -180 for example

                if not m.getGrid() is v.getGrid() :
                    m = m.astype("i").regrid(v.getGrid())

                v=MV2.masked_where(MV2.equal(m.filled(0.),0.),v)
                # weights the fraction where needed
                frc=m.filled(0.)
                m=v.mask
                if not m is numpy.ma.nomask:
                    frc=numpy.where(m,0.,frc)
##                 frc=numpy.where(m.filled(1),0.,frc)
        # Now make the fraction an MV2 and puts the dim from v on it
        frc=MV2.array(frc)
        frc.setAxisList(v.getAxisList())
        # just in case applies the cdmsKeywords again
        # usefull in case your final grid is global
        # and you specified Nino3 region for example.
        v   = v  (*args,**kw)
        frc = frc(*args,**kw) .filled(0.)
        if v.missing_value is None:
            v.missing_value=1.e20
        v=MV2.masked_where(MV2.equal(frc,0.),v)
        # Now applies the slope and offset if necessary
        if self.slope!=1.:
            v=v*self.slope
        if self.offset!=0.:
            v=v+self.offset

        if not ((v.mask is None) or (v.mask is MV2.nomask)):
            if numpy.ma.allclose(v.mask,0.):
                v._mask=numpy.ma.nomask
        # Returns the variable and the fractions or just the variable
        if returnTuple:
##             if not ((frc.mask is None) or (frc.mask is MV2.nomask)):
##                 if numpy.ma.allclose(frc.mask,0.):
##                     frc._mask=None
            return v,frc
        else:
            return v

    # makes get the default function
    __call__=get
    

    __slots__=['source','var','data','file','weightsMaker','weightedGridMaker','slope','offset','cdmsArguments','cdmsKeywords','id','preprocessKeywords','preprocess','comments','_var','_id','_slope','_offset','_file','_data','_weightedGridMaker']
    
    def _get_var(self):
        return self._var
    def _set_var(self,value):
        self._var=ValidationFunctions.checkStringOrNone(self,'var',value)
    var=property(_get_var,_set_var)

    def _get_id(self):
        return self._id
    def _set_id(self,value):
        self._id=ValidationFunctions.checkStringOrNone(self,'id',value)
    id=property(_get_id,_set_id)    

    def _get_slope(self):
        return self._slope
    def _set_slope(self,value):
        self._slope=ValidationFunctions.checkNumber(self,'slope',value)
    slope=property(_get_slope,_set_slope)

    def _get_offset(self):
        return self._offset
    def _set_offset(self,value):
        self._offset=ValidationFunctions.checkNumber(self,'offset',value)
    offset=property(_get_offset,_set_offset)

    def _get_file(self):
        return self._file
    def _set_file(self,value):
        name,value=ValidationFunctions.setSlab(self,'file',value)
        if name=='file':
            self._file=value
        else:
            setattr(self,name,value)
    file=property(_get_file,_set_file)

    def _get_data(self):
        return self._data
    def _set_data(self,value):
        self._data=ValidationFunctions.setSlabOnly(self,'data',value)
    data=property(_get_data,_set_data)
    
    def _get_weightedGridMaker(self):
        return self._weightedGridMaker
    def _set_weightedGridMaker(self,value):
        self._weightedGridMaker=ValidationFunctions.setDataSetGrid(self,'weightedGridMaker',value)
    weightedGridMaker=property(_get_weightedGridMaker,_set_weightedGridMaker)
    

class VariablesMatcherError(VariableConditionerError):
    pass

class VariablesMatcher(object):
    def __init__(self,variableConditioner1=None,variableConditioner2=None,externalVariableConditioner=None,weightedGridMaker=None,cdmsArguments=[],cdmsKeywords={}):
        # First  VariableConditioner
        if isinstance(variableConditioner1,VariableConditioner):
            self.variableConditioner1=variableConditioner1
        else:
            self.variableConditioner1=VariableConditioner(variableConditioner1)
        # Second VariableConditioner
        if isinstance(variableConditioner2,VariableConditioner):
            self.variableConditioner2=variableConditioner2
        else:
            self.variableConditioner2=VariableConditioner(variableConditioner2)
        # External VariableConditioner
        if isinstance(externalVariableConditioner,VariableConditioner):
            self.externalVariableConditioner=externalVariableConditioner
        elif externalVariableConditioner is None:
            self.externalVariableConditioner=None
        else:
            self.externalVariableConditioner=VariableConditioner(externalVariableConditioner)
        # Final Grid
        if weightedGridMaker is None:
            self.weightedGridMaker=WeightedGridMaker()
        elif isinstance(weightedGridMaker,WeightedGridMaker):
            self.weightedGridMaker=weightedGridMaker
        elif isinstance(weightedGridMaker,cdms2.grid.AbstractGrid):
            self.weightedGridMaker=weightedGridMaker()
            self.weightedGridMaker.grid=weightedGridMaker
        
        self.cdmsArguments=cdmsArguments
        self.cdmsKeywords=cdmsKeywords

        # aliases to make my life MUCH easier !
        self.V1=self.variableConditioner1
        self.V2=self.variableConditioner2
        self.EV=self.externalVariableConditioner

    def __str__(self):
        s='VariablesMatcher Object:\n'
        s+='---------\nVariableConditioner 1\n---------\n'+str(self.V1)
        s+='\n---------\nVariableConditioner 2\n---------\n'+str(self.V2)
        s+='\n----------------\nExternal VariableConditioner\n----------------\n'+str(self.EV)
        s+='\n----------\nFinal Grid\n----------\n'+str(self.weightedGridMaker)
        return s
    
    def get(self,returnTuple=1):
        # Ok now the tough part try to figure out everything for the user...
        
        # overwrite the defintion for the variableConditioners cdmsArguments
        if self.cdmsArguments!=[] :
            setattr(self.V1,'cdmsArguments',self.cdmsArguments)
            setattr(self.V2,'cdmsArguments',self.cdmsArguments)
            if not self.EV is None :
                setattr(self.EV,'cdmsArguments',self.cdmsArguments)
            
        # overwrite the defintion for the variableConditioners cdmsKeyowrds
        for k in self.cdmsKeywords.keys():
            self.V1.cdmsKeywords[k]=self.cdmsKeywords[k]
            self.V2.cdmsKeywords[k]=self.cdmsKeywords[k]
            if not self.EV is None:
                self.EV.cdmsKeywords[k]=self.cdmsKeywords[k]

        # Checks the time:
        # 2003-9-15: Added options if both var don't have time then still works
        d1  = None
        d2  = None
        frc1 = None
        frc2 = None
        autotime = None
        
        if not self.V1.cdmsKeywords.has_key('time'):
            if self.V2.cdmsKeywords.has_key('time'):
                d2=self.V2(returnTuple=returnTuple)
                if returnTuple:
                    t=d2[0].getTime().asComponentTime()
                else:
                    t=d2.getTime().asComponentTime()
                self.V1.cdmsKeywords['time']=(t[0],t[-1])
                d1=self.V1(returnTuple=returnTuple)
                del(self.V1.cdmsKeywords['time'])
            else: # Automatically gets the maximum common time
                d2=self.V2(returnTuple=returnTuple)
                if returnTuple:
                    t=d2[0].getTime()
                    if not t  is None:
                        t=t.asComponentTime()
                else:
                    t=d2.getTime()
                    if not t is None:
                        t=t.asComponentTime()
                if not t is None:
                    self.V1.cdmsKeywords['time']=(t[0],t[-1])
                d1=self.V1(returnTuple=returnTuple)
                if returnTuple:
                    t1=d1[0].getTime()
                    if not t1 is None:
                        t1=t1.asComponentTime()
                else:
                    t1=d1.getTime()
                    if not t1 is None:
                        t1=t1.asComponentTime()
                if not t1 is None:
                    autotime=[t1[0],t1[-1],'ccb']
                    if cdtime.compare(t1[0],t[0])==-1:
                        autotime[0]=t[0]
                    if cdtime.compare(t1[-1],t[-1])==1:
                        autotime[1]=t[-1]
                    self.V1.cdmsKeywords['time']=autotime
                d1=self.V1(returnTuple=returnTuple)
                if not t1 is None:
                    del(self.V1.cdmsKeywords['time'])
                    self.V2.cdmsKeywords['time']=autotime
                    d2=self.V2(returnTuple=returnTuple)
                    del(self.V2.cdmsKeywords['time'])
        elif not self.V2.cdmsKeywords.has_key('time'):
            d1=self.V1(returnTuple=returnTuple)
            if returnTuple:
                t=d1[0].getTime().asComponentTime()
            else:
                t=d1.getTime().asComponentTime()
            if not t is None:
                self.V2.cdmsKeywords['time']=(t[0],t[-1])
            d2=self.V2(returnTuple=returnTuple)
            if not t is None: del(self.V2.cdmsKeywords['time'])

        
        # Now get the variableConditioners 1 and 2 if necessary
        if d1 is None:
            d1=self.V1(returnTuple=returnTuple)
        if d2 is None:
            d2=self.V2(returnTuple=returnTuple)
        
        if returnTuple:
            # break the output if necessary
            frc2=d2[1]
            d2=d2[0]
            frc1=d1[1]
            d1=d1[0]
            frc1=MV2.array(frc1)
            frc2=MV2.array(frc2)
        else:
            frc1=MV2.ones(d1.shape,typecode=MV2.float32)
            frc2=MV2.ones(d2.shape,typecode=MV2.float32)
        
        frc1.setAxisList(d1.getAxisList())
        frc2.setAxisList(d2.getAxisList())

##         # Gets the common time period, only if time keyword isn't defined
##         if not(d1.getTime() is None) and not (d2.getTime() is None):
##             if len(d1.getTime())!=len(d2.getTime()) and not self.V1.cdmsKeywords.has_key('time') and not self.V2.cdmsKeywords.has_key('time'):
##                 t1=d1.getTime().asComponentTime()
##                 t2=d2.getTime().asComponentTime()
##                 t=[t1[0],t1[-1]]
##                 if cdtime.compare(t1[0],t2[0])<0:
##                     t[0]=t2[0]
##                 if cdtime.compare(t1[-1],t2[-1])>0:
##                     t[1]=t2[-1]
##                 d1   = d1  (time=(t[0],t[1]))
##                 frc1 = frc1(time=(t[0],t[1]))
##                 d2   = d2  (time=(t[0],t[1]))
##                 frc2 = frc2(time=(t[0],t[1]))

##         # remember the number of element in d1 to see if we add non dummy dimensions
##         nd1=MV2.count(d1)
##         nd2=MV2.count(d2)

##         # Now tries to grow extra dims (like dummy levels, etc...)
##         o1=d1.getOrder(ids=1)
##         o2=d2.getOrder(ids=1)

        if d1.shape!=d2.shape:
            if d1.ndim>d2.ndim:
                d1,d2=genutil.grower(d1,d2,singleton=1)
                frc1,frc2=genutil.grower(frc1,frc2,singleton=1)
            else:
                d2,d1=genutil.grower(d2,d1,singleton=1)
                frc2,frc1=genutil.grower(frc2,frc1,singleton=1)
        
        # External variableConditioner ?
        if not self.EV is None:
            ed=None
            if not self.EV.cdmsKeywords.has_key('time'):
                t=d1.getTime().asComponentTime()
                if not t is None: self.EV.cdmsKeywords['time']=(t[0],t[-1])
                ed=self.EV(returnTuple=1)
                frced=ed[1]
                ed=ed[0]
                frced=MV2.array(frced)
                frced.setAxisList(ed.getAxisList())

##                 # Gets the common time between d1 and ed
##                 if not t is None: del(self.EV.cdmsKeywords['time'])
##                 if (not ed.getTime() is None) and (not d1.getTime() is None):
##                     if (len(ed.getTime())!=len(d1.getTime())):
##                         t1=d1.getTime().asComponentTime()
##                         t2=ed.getTime().asComponentTime()
##                         t=[t1[0],t1[-1]]
##                         if cdtime.compare(t1[0],t2[0])<0:
##                             t[0]=t2[0]
##                         if cdtime.compare(t1[-1],t2[-1])>0:
##                             t[1]=t2[-1]
##                         d1    = d1  (time=(t[0],t[1]))
##                         d2    = d2  (time=(t[0],t[1]))
##                         ed    = ed  (time=(t[0],t[1]))

##                         frc1  = frc1(time=(t[0],t[1]))
##                         frc2  = frc2(time=(t[0],t[1]))
##                         frced = wed(time=(t[0],t[1]))

                    
            if ed is None:
                ed=self.EV(returnTuple=1)
                frced=ed[1]
                ed=ed[0]
                frced=MV2.array(frced)
                frced.setAxisList(ed.getAxisList())
            g=ed.getGrid()
            g1=d1.getGrid()
            rf=regrid2.Horizontal(g1,g)
            d1,frc1=rf(d1,mask=1.-frc1.filled(0.),returnTuple=1)
            g2=d2.getGrid()
            rf=regrid2.Horizontal(g2,g)
            d2,frc2=rf(d2,mask=1.-frc2.filled(0.),returnTuple=1)
            frc1=MV2.array(frc1)
            frc1.setAxisList(d1.getAxisList())
            frc2=MV2.array(frc2)
            frc2.setAxisList(d2.getAxisList())
            d1,ed=genutil.grower(d1,ed,singleton=1)
            d2,ed=genutil.grower(d2,ed,singleton=1)
            ed,frced=genutil.grower(ed,frced,singleton=1)
            
            frc1=numpy.ma.where(numpy.ma.equal(frc1.filled(0.),0.),0.,frced.filled(0.))           
            frc2=numpy.ma.where(numpy.ma.equal(frc2.filled(0.),0.),0.,frced.filled(0.))

            d1=MV2.masked_where(MV2.equal(frc1.filled(0.),0.),d1)
            d2=MV2.masked_where(MV2.equal(frc2.filled(0.),0.),d2)

        # Final grid ?
        g=self.weightedGridMaker()
        if not g is None:
            g1=d1.getGrid()
            g2=d2.getGrid()
            rf1=regrid2.Horizontal(g1,g)
            rf2=regrid2.Horizontal(g2,g)
            d1,frc1=rf1(d1,mask=1.-frc1.filled(0.),returnTuple=1)
##             m=1.-frc2.filled(0.)
            d2,frc2=rf2(d2,mask=1.-frc2.filled(0.),returnTuple=1)
            frc1=MV2.array(frc1)
            frc1.setAxisList(d1.getAxisList())
            frc2=MV2.array(frc2)
            frc2.setAxisList(d2.getAxisList())
            m=self.weightedGridMaker.weightsMaker(d1)
            if not m is None:
                d1,m=genutil.grower(d1,m)
                frc1,m=genutil.grower(frc1,m)
                frc1=m.filled(0.)
                d1=MV2.masked_where(MV2.equal(frc1,0.),d1)
                m=d1.mask
                if not m is None:
                    frc1=numpy.where(m,0.,frc1)
            m=self.weightedGridMaker.weightsMaker(d2)
            if not m is None:
                d2,m=genutil.grower(d2,m)
                frc2,m=genutil.grower(frc2,m)
                frc2=m.filled(0.)
                d2=MV2.masked_where(MV2.equal(frc2,0.),d2)
                m=d2.mask
                if not m is numpy.ma.nomask:
                    frc2=numpy.where(m,0.,frc2)
        elif d1.getGrid()!=d2.getGrid():
            g1=d1.getGrid()
            g2=d2.getGrid()
            rf=regrid2.Horizontal(g2,g1)
            d2,frc2=rf(d2,mask=1.-frc2.filled(0.),returnTuple=1)
        frc1=MV2.array(frc1)
        frc1.setAxisList(d1.getAxisList())
        frc2=MV2.array(frc2)
        frc2.setAxisList(d2.getAxisList())
            
        # CdmsArguments or CdmsKeywords
        if not self.cdmsArguments is None:
            d1=d1(*self.cdmsArguments)
            d2=d2(*self.cdmsArguments)
            frc1=frc1(*self.cdmsArguments)
            frc2=frc2(*self.cdmsArguments)
            
        if not self.cdmsKeywords is None:
            d1=d1(**self.cdmsKeywords)
            d2=d2(**self.cdmsKeywords)
            frc1=frc1(**self.cdmsKeywords)
            frc2=frc2(**self.cdmsKeywords)

        d1=MV2.masked_where(MV2.equal(frc1,0.),d1)
        d2=MV2.masked_where(MV2.equal(frc2,0.),d2)
        if not ((d1.mask is None) or (d1.mask is MV2.nomask)):
            if numpy.ma.allclose(d1.mask,0.):
                d1._mask=numpy.ma.nomask
        if not ((d2.mask is None) or (d2.mask is MV2.nomask)):
            if numpy.ma.allclose(d2.mask,0.):
                d2._mask=numpy.ma.nomask
        if returnTuple:
            if not ((frc1.mask is None) or (frc1.mask is MV2.nomask)):
                if numpy.ma.allclose(frc1.mask,0.):
                    frc1._mask=numpy.ma.nomask
            if not ((frc2.mask is None) or (frc2.mask is MV2.nomask)):
                if numpy.ma.allclose(frc2.mask,0.):
                    frc2._mask=numpy.ma.nomask
            return (d1,frc1),(d2,frc2)
        else:
            return d1,d2

    __call__=get
