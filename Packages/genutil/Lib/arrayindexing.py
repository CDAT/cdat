# Adapted for numpy/ma/cdms2 by convertcdms.py
import numpy
#from statistics import __checker
import statistics
import numpy.ma,cdms2,genutil
def get(Array,Indices,axis=0):
    """
    Arrayrrayindexing returns Array[Indices], indices are taken along dimension given with axis
    
    Usage:
    C=get(Array,Indices,axis=0) # i.e. C=Array[Indices]
    Indices accepts negative value ,e.g: -1 is last element
    """
    ## First some checks

    isma=numpy.ma.isMA(Array)
    if isinstance(Indices,int):
        return Array[Indices]
    if Indices.dtype not in [numpy.int,numpy.int32,numpy.int16]:
        raise "Error indices array must be made of integers (try: Indices=Indices.astype('l') first)"
    
    if cdms2.isVariable(Array) :
        xatt=Array.attributes
        id=Array.id
        
    if len(Array.shape)!=len(Indices.shape):
        Array,Indices,weights,axis,ax=statistics.__checker(Array,Indices,None,axis,smally=1)
        if isinstance(Indices,int):
            return Array[Indices]
        if Indices.shape!=Array.shape[1:]:
            raise "Error uncompatible shapes: "+str(Array.shape)+" and "+str(Indices.shape)
    else:
        Array,Indices,weights,axis,ax=statistics.__checker(Array,Indices,None,axis)
        if Indices.shape!=Array.shape:
            raise "Error uncompatible shapes: "+str(Array.shape)+" and "+str(Indices.shape)

    m=Array.mask
    if not isinstance(Indices,int): Indices=Indices.data.astype('i') # Sometihng happened with masking of y by x mask
    print Array.data.dtype.char,Indices.dtype.char
    C=genutil.array_indexing.extract(Array.data,Indices)
    if m is not numpy.ma.nomask:
        M=genutil.array_indexing.extract(m.astype('i'),Indices)
        C=numpy.ma.masked_where(M,C,copy=0)
    elif isma:
        C=numpy.ma.array(C,copy=0,mask=None)
    if not ax is None:
        C=cdms2.createVariable(C,axes=ax,id=id,copy=0)
        for at in xatt.keys():
            setattr(C,at,xatt[at])
    return C

def set(Array,Indices,Values,axis=0):
    """
    Arrayrrayindexing set Array[Indices] with Values, indices are taken along dimension given with axis
    
    Usage:
    Array=set(Array,Indices,Values,axis=0) # i.e. Array[Indices]=Values

    Indices accepts negative value ,e.g: -1 is last element
    """
##     if numpy.rank(Indices)==0:
##         Array[Indices]=Values
    ## First some checks
    #isma=numpy.ma.isMA(Array)
    if Indices.dtype not in [numpy.int,numpy.int32,numpy.int16]:
        raise "Error indices array must be made of integers (try: Indices=Indices.astype('l') first)"
    
    if cdms2.isVariable(Array) :
        xatt=Array.attributes
        id=Array.id
    if len(Array.shape)!=len(Indices.shape):
        crap,Indices,crap,axis,ax=statistics.__checker(Array,Indices,None,axis,smally=1)
        Array,Values,crap,axis,ax=statistics.__checker(Array,Values,None,axis,smally=1)
        if Indices.shape!=Array.shape[1:]:
            raise "Error uncompatible shapes: "+str(Array.shape)+" and "+str(Indices.shape)
    else:
        Array,Indices,Values,axis,ax=statistics.__checker(Array,Indices,Values,axis)
        if Indices.shape!=Array.shape:
            raise "Error uncompatible shapes: "+str(Array.shape)+" and "+str(Indices.shape)

    m=numpy.ma.getmask(Array)
    mv=numpy.ma.getmask(Values)
    if numpy.rank(Indices)>0:
        Indices=Indices.raw_data() # Something happened with masking of y by x mask
        Values=Values.raw_data()
    genutil.array_indexing_emulate.set(Array.raw_data(),Indices.astype('i'),Values)
    if m is not numpy.ma.nomask:
        if mv is not numpy.ma.nomask:
            genutil.array_indexing_emulate.set(m,Indices,mv)
    elif mv is not numpy.ma.nomask:
        m=numpy.zeros(mv.shape,mv.typcode())
        genutil.array_indexing_emulate.set(m,Indices,mv)
        if not numpy.ma.allequal(m,0):
            Array=numpy.ma.masked_where(m,Array,copy=0)
    if not ax is None:
        Array=cdms2.createVariable(C,axes=ax,id=id,copy=0)
        for at in xatt.keys():
            setattr(C,at,xatt[at])
    return Array

 
