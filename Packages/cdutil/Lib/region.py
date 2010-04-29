# Adapted for numpy/ma/cdms2 by convertcdms.py
from cdms2.selectors import SelectorComponent
class DomainComponent(SelectorComponent):
    '''gets a domain, and by default adjusts the bounds to the domain
    or if exact is set to 0 or None gets all the domain that has
    parts of the domain requested, also post processing allows you to apply a mask
    dimension names can be passed as keywords,
    but if no name is passed arguments are taken in order and applied to the corresponding axis
    Overwritting an axis (2 keywords or keyword + argument) is not allowed
    Example of use:
    NH=cdms.selectors.Selector(domain(latitude=(0.,90.)))
    '''
    
    def __init__(self,*args,**kargs):
        ''' initialise some value such as tolerances for equality'''
        self.args=args
        self.kargs=kargs
        self.atol=kargs.get('atol',1.E-8)
        self.rtol=kargs.get('rtol',1.E-5)
        self.exact=kargs.get('exact',1)
        if not (self.exact is None or type(self.exact)==type(0)):
            raise 'Error keyword: ''exact'' value: '+str(exact)+' not legal'

    def __str__(self):
        s='Exact Region Selector\n'
        if len(self.args)>0:
            s+='Arguments: ('
            for a in len(self.args):
                s+=str(a)+', '
            s+=')\n'
        if self.kargs!={}:
            s+='Keywords:\n'
            for k in self.kargs.keys():
                s+='\t'+str(k)+':'+str(self.kargs[k])+'\n'
        return s
    
    def specify(self,slab,axes,specification,confined_by,aux):
        ''' First part: confine the slab within a Domain wide enough to do the exact in post'''
        import string,copy
        from numpy.ma import minimum,maximum
        # myconfined is for later, we can't confine a dimension twice with an argument plus a keyword or 2 keywords
        myconfined=[None]*len(axes)
        self.aux=copy.copy(specification)
        # First look at the arguments (i.e not keywords) and confine the dimensions
        # in the order of the arguments
        for i in range(len(self.args)):
            if confined_by[i] is None :  # Check it hasn't been confined by somebody else
                myconfined[i]=1  # dim confined by argument list
                confined_by[i]=self # for cdms I want to confine this dimension
                self.aux[i]=specs=list(self.args[i]) # How do we want to confine this dim ?
                if type(specs)==type(slice(0)):
                    specification[i]=specs  # If it's a slicing nothing to do
                else: # But if it's not...
                    if specs[0] is None:
                        tmp=axes[i].getBounds()
                        if tmp is None:
                            raise ValueError, 'Region error, axis:'+axes[i].id+' has no bounds'
                        specs[0]=minimum(minimum(tmp[0],tmp[-1]))
                    if specs[1] is None:
                        tmp=axes[i].getBounds()
                        if tmp is None:
                            raise ValueError, 'Region error, axis:'+axes[i].id+' has no bounds'
                        specs[1]=maximum(maximum(tmp[0],tmp[-1]))
                    if axes[i].isTime(): # Time is as always "Special"
                        import cdtime
                        tc=type(cdtime.comptime(0))  # component time type
                        tr=type(cdtime.reltime(0,'months since 0'))  # relative time type
                        t=type(specs[0]) # my first spec type
                        if t==type(''): #if my first spec is passed as a string
                            specs[0]=cdtime.s2r(specs[0],axes[i].units)
                        elif t==tc or t==tr: #if my first spec is passed as a cdtime object
                            specs[0]=cdtime.torel(specs[0],axes[i].units)
                        else: # If not it has to be that the users knows the time values in the axis
                            pass
                        t=type(specs[1]) # my second spec type
                        if t==type(''): #if my second spec is passed as a string
                            specs[1]=cdtime.s2r(specs[1],axes[i].units)
                        elif t==tc or t==tr: #if my second spec is passed as a cdtime object
                            specs[1]=cdtime.torel(specs[1],axes[i].units)
                    sp=[specs[0],specs[1],'oob']  # Now retrieve the values wide enough for the exact                    specification[i]=sp  # sets the specifications
            else:
                return 1
        for kw in self.kargs.keys():
            axis=None
            for i in range(len(axes)):
                if axes[i].id==kw : axis=i
            if axis is None:
                if kw=='time' :
                    for i in range(len(axes)):
                        if axes[i].isTime() : axis=i
                elif kw=='level' :
                    for i in range(len(axes)):
                        if axes[i].isLevel() : axis=i
                elif kw=='longitude' :
                    for i in range(len(axes)):
                        if axes[i].isLongitude() : axis=i
                elif kw=='latitude' :
                    for i in range(len(axes)):
                        if axes[i].isLatitude() : axis=i
                elif not kw in ['exact','atol','rtol']: # keyword not a recognised keyword or dimension name
                    raise 'Error, keyword: '+kw+' not recognized'
            # At this point, if axis is None:
            # we are dealing with a keyword for the selector
            # so we'll skip it
            if not axis is None : 
                if confined_by[axis] is None:
                    confined_by[axis]=self
                    myconfined[axis]=1
                    self.aux[axis]=specs=list(self.kargs[kw])
                    if type(specs)!=type(slice(0)):
                        if specs[0] is None:
                            tmp=axes[axis].getBounds()
                            if tmp is None:
                                raise ValueError, 'Region error, axis:'+axes[axis].id+' has no bounds'
                            specs[0]=minimum(minimum(tmp[0],tmp[-1]))
                        if specs[1] is None:
                            tmp=axes[axis].getBounds()
                            if tmp is None:
                                raise ValueError, 'Region error, axis:'+axes[axis].id+' has no bounds'
                            specs[1]=maximum(maximum(tmp[0],tmp[-1]))
                        if axes[axis].isTime():
                            import cdtime
                            tc=type(cdtime.comptime(0))
                            tr=type(cdtime.reltime(0,'months since 0'))
                            t=type(specs[0])
                            if t==type(''):
                                specs[0]=cdtime.s2r(specs[0],axes[i].units)
                            elif t==tc or t==tr:
                                specs[0]=cdtime.torel(specs[0],axes[i].units)
                            t=type(specs[1])
                            if t==type(''):
                                specs[1]=cdtime.s2r(specs[1],axes[i].units)
                            elif t==tc or t==tr:
                                specs[1]=cdtime.torel(specs[1],axes[i].units)
                        sp=[specs[0],specs[1],'oob']
                        specification[axis]=sp
                    else:
                        specification[axis]=specs

                else:
                    if myconfined[axis]==1:
                        raise 'Error you are attempting to set the axis: '+str(axes[axis].id)+' more than once'
                    else:
                        return 1
        return 0
    
    def same(self,data,value):
        ''' Check if data is basically the same than value'''
        return abs(data-value)<self.atol+self.rtol*abs(value)
    
    def post(self,fetched,slab,axes,specifications,confined_by,aux,axismap):
        ''' Post processing retouches the bounds and later will deal with the mask'''
        import cdms2 as cdms
        fetched=cdms.createVariable(fetched,copy=1)
        faxes=fetched.getAxisList()
        if self.exact:
            for i in range(len(faxes)):
                if confined_by[i] is self and type(self.aux[i])!=type(slice(0)):
                    # ok we touched that axis, and didn't just sliced it let's adjust the bounds
                    ax=faxes[axismap[i]].clone()  # retrieve the axis and clones it
                    bounds=ax.getBounds()
                    if bounds is None:
                        raise ValueError, 'Region error, axis:'+ax.id+' has no bounds'
                    ax0=ax[0]
                    ax1=ax[-1]
                    '''sets xb with the bounds
                    smaller value of axis first
                    switches ax0 and ax1 if necessary'''
                    if ax[0]<ax[-1]:
                        xb=[bounds[0],bounds[-1]] # Extreme bounds 
                    else:
                        xb=[bounds[-1],bounds[0]] # Extreme bounds
                        tmp=ax0*1.  # *1. is just to make sure I'm copying
                        ax0=ax1*1.
                        ax1=tmp
                    specs=self.aux[i]  # the specifications
                    fbound=specs[0]  # specs for the first bound
                    lbound=specs[1]  # specs for the last bound
                    if fbound>lbound : # if the first bound is smaller then flip it
                        tmp=fbound*1.
                        fbound=lbound*1.
                        lbound=tmp
                    b0=xb[0] # bounds of lower   value of the axis
                    b1=xb[1] # bounds of greater value of the axis
                    ''' The folowing reset the values of the axis
                    sets it to the middle of the new cell
                    also reset the bounds accordingly'''
                    if not(b0[0]>fbound and b0[0]<lbound):
                        if not self.same(b0[0],fbound):  # make sure they are actually different not just very close
                            b0[0]=fbound
                            ax0=(b0[1]+b0[0])/2.
                    if not(b0[1]>fbound and b0[1]<lbound):
                        if not self.same(b0[1],fbound):
                            b0[1]=fbound
                            ax0=(b0[1]+b0[0])/2.
                    if not(b1[0]>fbound and b1[0]<lbound):
                        if not self.same(b1[0],lbound):
                            b1[0]=lbound
                            ax1=(b1[1]+b1[0])/2.
                    if not(b1[1]>fbound and b1[1]<lbound):
                        if not self.same(b1[1],lbound):
                            b1[1]=lbound
                            ax1=(b1[1]+b1[0])/2.
                    xb[0]=b0
                    xb[1]=b1
                    if ax[-1]>ax[0]:
                        bounds[0],bounds[-1]=xb # Extreme bounds
                        ax[-1]=ax1
                        ax[0]=ax0
                    else:
                        bounds[-1],bounds[0]=xb # Extreme bounds
                        ax[-1]=ax0
                        ax[0]=ax1
                    ax.setBounds(bounds)
                    if faxes[axismap[i]].isLatitude():
                        ax.designateLatitude()
                    faxes[axismap[i]]=ax

        a=cdms.createVariable(fetched.filled(),mask=fetched.mask,axes=faxes)
        return a                        
                        
def domain(*args, **kargs):
    '''construct the selector'''
    import cdms2 as cdms
    a=cdms.selectors.Selector(DomainComponent(*args,**kargs))
    return a

NH=NorthernHemisphere=domain(latitude=(0.,None))
SH=SouthernHemisphere=domain(latitude=(None,0.))
Tropics=domain(latitude=(-23.4,23.4))
NPZ=AZ=ArcticZone=domain(latitude=(66.6,None))
SPZ=AAZ=AntarcticZone=domain(latitude=(None,-66.6))



