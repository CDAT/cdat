
"Utilities for manipulating slices"

# Intersect a slice with a half-open interval [i,j).
# slice.start and slice.stop must be integers (not None).
# Returns a slice, or None if the intersection is empty.


def sliceIntersect(aSlice,interval):
    p0,p1 = interval
    i = aSlice.start
    j = aSlice.stop
    k = aSlice.step
    if k is None:
        k=1

    # If the slice has a negative step, generate the
    # equivalent slice with positive step
    irev=0
    if k<0:
        k = -k
        pk = ((j-i+k)/k)*k+i
        j = i+1
        i = pk
        irev = 1

    # Calculate the intersection for an increasing slice
    px = ((p0-i+k-1)/k)*k+i
    a = max(px,i)
    b = min(j,p1)
    if a<b:
        if k==1:
            newSlice = slice(a,b)
        else:
            newSlice = slice(a,b,k)
    else:
        newSlice = None

    # Reverse the slice if necessary
    if irev==1 and newSlice is not None:
        px = -((-b+a+k)/k*k-a)
        newSlice = slice(px,a-1,-k)
    
    return newSlice

# Intersect a slice with a partition. The partition is a list of
# intervals, with shape (n,2). The result is a list of pairs
# [(interval,slice), (interval,slice) ...]  where the intervals are
# those intervals in the partition which have non-empty intersection,
# in the same order as in the partition. If the intersection is empty,
# the result is an empty list.

def slicePartition(aSlice,partition):
    result = []
    for interval in partition:
        intslice = sliceIntersect(aSlice,interval)
        if intslice is not None:
            result.append((interval,intslice))
    return result

def lenSlice(aSlice):
    "Return the number of values associated with a slice"

    step = aSlice.step
    if step is None:
        step = 1
    if step>0:
        start = aSlice.start
        stop = aSlice.stop
    else:
        start = aSlice.stop
        stop = aSlice.start
        step = -step

    return ((stop-start-1)/step + 1)

def reverseSlice(s,size):
    """For 'reversed' slices (slices with negative stride),
    return an equivalent slice with positive step. For positive
    strides, just return the slice unchanged.
    """
    if s.step>0 or s.step is None:
        return s

    i = s.start
    j = s.stop
    k = s.step
    if i is None:
        i=size-1
    elif i<0:
        i = i%size
    if j is None:
        j=-1
    elif -size-1<j<0:
        j = j%size
    
    if i<-size or j<-size-1:
        raise 'Invalid slice',`s`

    k = -k
    pk = ((j-i+k)/k)*k+i
    j = i+1
    i = pk%size

##     if j==size:
##         j = None

    return slice(i,j,k)


def splitSlice(s,size):
    """For a 'wraparound' slice, return two equivalent slices
    within the range 0..size-1."""
    i,j,k = s.start,s.stop,s.step
    if k>0:
        wrap1 = slice(i,size,k)
        wrap2 = slice((i-size)%k, j-size, k)
    else:
        wrap1 = slice(i-size, None, k)
        wrap2 = slice(size+(i-size)%k, j, k)
    return (wrap1,wrap2)


def splitSliceExt(s,size):
    """
    mf 20010330 --
    For a 'wraparound' slice, return N equivalent slices
    within the range 0...(N*size) N = anything"""
    i,j,k = s.start,s.stop,s.step

    # slice of form [i:] sets j to large int
    if j>2000000000L:
        j = size

    _debug=0
    if(_debug): print "SSSS0: ",i,j,k

    wrap=[]

    if k>0:

        iter=0
        if(_debug): print "SSSS1: iter ",iter,j,size,k
        while(j>0):
            if(_debug): print " "
            if(_debug): print "SSSS2: iter",iter,j,size,k
            jo=size
            if(iter>0): jo=size+1
            if(_debug): print "SSSS3: iter",iter,j,jo
            if(j<size): jo=j
            if(_debug): print "SSSS4: iter",iter,j,jo
            wrap.append(slice(i,jo,k))
            j=j-size
            i=0
            iter=iter+1
            
    else:

        wraprev=[]
        iter=0
        if(_debug): print "SSSS1 neg: iter ",iter,i,j,size,k
        while(i>=0):
            if(_debug): print " "
            if(_debug): print "SSSS2 neg: iter",iter,i,j,size,k
            io=size-1
            if(_debug): print "SSSS3 neg: iter",iter,i,j,io
            if(i<size): io=i
            if(_debug): print "SSSS4 neg: iter",iter,i,j,io
            
            # mf 20010405 python does not return nothing for slice(size-1,size-1,-1); force it
            if( not ( io==size-1 and j==size-1 ) ):
                wraprev.append(slice(io,j,k))
            
            i=i-size
            j=None
            iter=iter+1
        #
        # reverse
        #
        for k in range(0,len(wraprev)):
            kk=len(wraprev)-k-1
            wrap.append(wraprev[kk])
            if(_debug): print "SSSS5 neg: ",kk,wraprev[kk]

    return (wrap)







