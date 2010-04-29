# Adapted for numpy/ma/cdms2 by convertcdms.py
# Adapted for numpy/ma/cdms2 by convertcdms.py
import numpy
import genutil
import cdms2
import numpy.ma
import os
import sys

### EXTRACT TESTS

### 1D

A=numpy.array([6,7,8,9,2],'f')
B=numpy.argsort(A).astype('i')
print A.dtype.char
print B.dtype.char
C=genutil.arrayindexing.get(A,B)
print C
D=numpy.array([6.5,7.5,8.5,9.5,2.5],'f')
print D
E=genutil.arrayindexing.set(A,B,D)
print E

## 2D
A=numpy.array([[1,2],[3,4,],[5,6],[7,8]],numpy.float)

print A.shape,A.dtype.char
print A[0]
print A[1]
print A[2]
print A[3]

B=numpy.array([3,2],numpy.int32)

C=genutil.array_indexing.extract(A,B)
print C,C.dtype.char
C=genutil.arrayindexing.get(A,B)
print C,C.dtype.char

f=cdms2.open(os.path.join(cdms2.__path__[0],'..','..','..','..','sample_data','clt.nc'))
clt=f('clt')
## clt=cdms2.MV2.average(clt,2)
print clt.shape

M=numpy.ma.maximum.reduce(clt,axis=0)
marg=numpy.ma.argmax(clt,axis=0)
M2=genutil.arrayindexing.get(clt,marg)

print M2.shape,M2.mask,genutil.minmax(M2-M)

M=numpy.ma.maximum.reduce(clt,axis=1)
marg=numpy.ma.argmax(clt,axis=1)
marg=cdms2.MV2.array(marg)
marg.setAxis(0,clt.getAxis(0))
marg.setAxis(1,clt.getAxis(2))
print clt.dtype.char,M.shape
M2=genutil.arrayindexing.get(clt,marg,axis=1)

print M2.shape,M2.mask,genutil.minmax(M2-M)

clt=cdms2.MV2.masked_greater(clt,80)
M=numpy.ma.maximum.reduce(clt,axis=1)
print M.mask,'is the mask'
marg=numpy.ma.argmax(clt,axis=1)
marg=cdms2.MV2.array(marg)
marg.setAxis(0,clt.getAxis(0))
marg.setAxis(1,clt.getAxis(2))
print clt.dtype.char,M.shape,marg.dtype.char
M2=genutil.arrayindexing.get(clt,marg,axis=1)
print M2.shape,M2.mask,genutil.minmax(M2-M)

## 3D
I=numpy.random.random(clt.shape)*clt.shape[0]
I=I.astype('i') # integers required
M2=genutil.arrayindexing.get(clt,I)

#### Set tests
V=numpy.array([1345,34],A.dtype.char)
B=numpy.array([-3,2],numpy.int)
A=genutil.arrayindexing.set(A,B,V)
print A

A=numpy.array([[1,2],[3,4,],[5,6],[7,8]],numpy.float)
B=numpy.array([[1,2],[3,0,],[1,2],[0,3]],numpy.int)
V=numpy.array([[10.,21.],[13,.4,],[1.5,6.4],[77.7,9.8]],numpy.float)
C=genutil.arrayindexing.set(A,B,V)
print A
print C

## ## Test with mask
## I=numpy.random.random(clt.shape)*clt.shape[0]
## I=I.astype('i') # integers required
## clt2=genutil.arrayindexing.set(clt,I,clt)

## import vcs
## x=vcs.init()
## x.plot(clt2)
## raw_input("HJ")
