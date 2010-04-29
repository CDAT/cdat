## Automatically adapted for numpy.oldnumeric Aug 01, 2007 by 

import numpy
import cdms2,os,sys
from cdms2.tvariable import TransientVariable as TV
import MV2

print 'Test 11: MV module (transient variable arithmetic) ... ',
from markError import NTIME,NLAT,NLON,x,clearError,markError,reportError
clearError()

d = cdms2.open(os.path.join(sys.prefix,'sample_data','test.xml'))
ud = d['u']
vd = d['v']
udat = ud[:]
vdat = vd[:]
ulat = ud.getLatitude()
if not isinstance(udat, TV): markError('Slice does not return TV')
f = cdms2.open(os.path.join(sys.prefix,'sample_data','u_2000.nc'))
uf = f['u']

vel = MV2.sqrt(ud*ud + vd*vd)
vel.id = 'velocity'
vel2 = MV2.sqrt(udat*udat + vdat*vdat)
vel2.id = 'velocity'
vel2.units = ud.units
if not MV2.allequal(vel,vel2): markError('Slice operators do not compare')

x1 = uf+1.0
x2 = 1.0-ud
x11 = -uf
x12 = MV2.absolute(ud)
x3 = uf+x2
x4 = 1.0+ud
x5 = uf-1
x6 = ud*uf
x7 = ud/x2
x8=1/uf
x9 = 3*ud
x10=uf**3
x13 = MV2.add.reduce(uf)
x14 = MV2.add.reduce(ud)
x15 = x9.astype(numpy.float32)
if not x15.dtype.char==numpy.sctype2char(numpy.float32): markError('astype error')

## arrayrange(start, stop=None, step=1, typecode=None, axis=None, attributes=None, id=None) 
##   Just like range() except it returns a variable whose type can be specfied
##   by the keyword argument typecode. The axis of the result variable may be specified.
xarange = MV2.arange(16., axis=ulat)

## masked_array(a, mask=None, fill_value=None, axes=None, attributes=None, id=None) 
##   masked_array(a, mask=None) = 
##   array(a, mask=mask, copy=0, fill_value=fill_value)
##   Use fill_value(a) if None.
xmarray = MV2.masked_array(ud)

## masked_object(data, value, copy=1, savespace=0) 
##   Create array masked where exactly data equal to value

## masked_values(data, value, rtol=1.0000000000000001e-05, atol=1e-08, copy=1, savespace=0, axes=None,
## attributes=None, id=None) 
##   masked_values(data, value, rtol=1.e-5, atol=1.e-8)
##   Create a masked array; mask is None if possible.
##   May share data values with original array, but not recommended.
##   Masked where abs(data-value)<= atol + rtol * abs(value)

## ones(shape, typecode='l', savespace=0, axes=None, attributes=None, id=None) 
##   ones(n, typecode=Int, savespace=0, axes=None, attributes=None, id=None) = 
##   an array of all ones of the given length or shape.
xones = MV2.ones(uf.shape, numpy.float32, axes=uf.getAxisList(), attributes=uf.attributes, id=uf.id)
if not xones[0,0,0]==1.0: markError('xones')

## zeros(shape, typecode='l', savespace=0, axes=None, attributes=None, id=None) 
##   zeros(n, typecode=Int, savespace=0, axes=None, attributes=None, id=None) = 
##   an array of all zeros of the given length or shape.
xzeros = MV2.zeros(ud.shape, dtype=numpy.float, axes=ud.getAxisList(), attributes=ud.attributes, id=ud.id)
xmasked = MV2.as_masked(xzeros)

## argsort(x, axis=-1, fill_value=None) 
##   Treating masked values as if they have the value fill_value,
##   return sort indices for sorting along given axis.
##   if fill_value is None, use fill_value(x)

## asarray(data, typecode=None) 
##   asarray(data, typecode=None) = array(data, typecode=None, copy=0)
##   Returns data if typecode if data is a MaskedArray and typecode None
##   or the same.

## average(a, axis=0, weights=None, returned=0) 
##   average(a, axis=0, weights=None, returned=0)
##   Computes average along indicated axis. Masked elements are ignored.
##   Result may equal masked if average cannot be computed.
##   If weights are given, result is sum(a*weights)/sum(weights), with
##   all elements masked in a or in weights ignored.
##   weights if given must have a's shape. 
##   Denominator is multiplied by 1.0 to prevent truncation for integers.
##   returned governs return of second quantity, the weights.
xav = MV2.average(xones, axis=1)
xav2 = MV2.average(ud)
xav3 = MV2.average(udat)
xav4, wav4 = MV2.average(udat, weights=MV2.ones(udat.shape, numpy.float), returned=1)

## choose(indices, t) 
##   Shaped like indices, values t[i] where at indices[i]
##   If t[j] is masked, special treatment to preserve type.
ct1 = MV2.TransientVariable([1,1,2,0,1])
ctr = MV2.choose(ct1, [numpy.ma.masked, 10,20,30,40])
if not MV2.allclose(ctr, [10, 10, 20, 100, 10]): markError('choose error 1')
ctx = MV2.TransientVariable([1,2,3,150,4])
cty = -MV2.TransientVariable([1,2,3,150,4])
ctr = MV2.choose(MV2.greater(ctx,100), (ctx, 100))
if not MV2.allclose(ctr, [1,2,3,100,4]): markError('choose error 2')
ctr = MV2.choose(MV2.greater(ctx,100), (ctx, cty))
if not MV2.allclose(ctr, [1,2,3,-150,4]): markError('choose error 3')

## concatenate(arrays, axis=0, axisid=None, axisattributes=None) 
##   Concatenate the arrays along the given axis. Give the extended axis the id and
##   attributes provided - by default, those of the first array.

try:
    xcon = MV2.concatenate((ud,vd))
except:
    markError('Concatenate error')

## isMaskedVariable(x) 
##   Is x a masked variable, that is, an instance of AbstractVariable?
im1 = MV2.isMaskedVariable(xones)
im2 = MV2.isMaskedVariable(xmasked)

## outerproduct(a, b) 
##   outerproduct(a,b) = {a[i]*b[j]}, has shape (len(a),len(b))
xouter = MV2.outerproduct(MV2.arange(16.),MV2.arange(32.))
lat = uf.getLatitude()
lon = uf.getLongitude()
xouter.setAxis(0,lat)
xouter.setAxis(1,lon)
xouter.setAxisList([lat,lon])           # Equivalent

## masked_equal(x, value) 
##   masked_equal(x, value) = x masked where x == value
##   For floating point consider masked_values(x, value) instead.

## masked_greater(x, value) 
##   masked_greater(x, value) = x masked where x > value

## masked_greater_equal(x, value) 
##   masked_greater_equal(x, value) = x masked where x >= value
xge = MV2.masked_greater_equal(xouter, 120)

## masked_less(x, value) 
##   masked_less(x, value) = x masked where x < value
xl = MV2.masked_less(xouter, 160)

## masked_less_equal(x, value) 
##   masked_less_equal(x, value) = x masked where x <= value

## masked_not_equal(x, value) 
##   masked_not_equal(x, value) = x masked where x != value

## masked_outside(x, v1, v2) 
##   x with mask of all values of x that are outside [v1,v2]
xmo = MV2.masked_outside(xouter, 120, 160)

## count(a, axis=None) 
##   Count of the non-masked elements in a, or along a certain axis.
xcount = MV2.count(xmo,0)
xcount2 = MV2.count(xmo,1)

## masked_where(condition, x, copy=1) 
##   Return x as an array masked where condition is true. 
##   Also masked where x or condition masked.
xmwhere = MV2.masked_where(MV2.logical_and(MV2.greater(xouter,120),MV2.less(xouter,160)),xouter)

## maximum(a, b=None) 
##   returns maximum element of a single array, or elementwise
maxval = MV2.maximum(xouter)
xmax = MV2.maximum(ud,vd)
xmax = MV2.maximum.reduce(ud)
xmax = MV2.maximum.reduce(vd)
xmax2 = numpy.ma.maximum.reduce(vd.subSlice(),axis=0)
if not MV2.allclose(xmax, xmax2): markError('maximum.reduce')

## minimum(a, b=None) 
##   returns minimum element of a single array, or elementwise
minval = MV2.minimum(xouter)
xmin = MV2.minimum(ud,vd)
xmin = MV2.minimum.reduce(ud)
xmin = MV2.minimum.reduce(vd)
xmin2 = numpy.ma.minimum.reduce(vd.subSlice(),axis=0)
if not MV2.allclose(xmin, xmin2): markError('minimum.reduce')
t1 = MV2.TransientVariable([1.,2.,3.])
t2 = MV2.TransientVariable([1.,10.])
t3 = MV2.add.outer(t1,t2)
t3 = MV2.minimum.outer(t1,t2)
t3 = MV2.maximum.outer(t1,t2)
## product(a, axis=0, fill_value=1) 
##   Product of elements along axis using fill_value for missing elements.
xprod = MV2.product(ud)

## power(a, b, third=None) 
##   a**b
xpower = xprod**(1./3.)

## repeat(a, repeats, axis=0) 
##   repeat elements of a repeats times along axis
##   repeats is a sequence of length a.shape[axis]
##   telling how many times to repeat each element.
xreshape = xarange.reshape(8,2)
xrepeat = MV2.repeat(xreshape, repeats=2)
xrepeat2 = MV2.repeat(xreshape, repeats=2, axis=1)

## reshape(a, newshape, axes=None, attributes=None, id=None) 
##   Copy of a with a new shape.
xreshape = MV2.reshape(xarange, (8,2))

## resize(a, new_shape, axes=None, attributes=None, id=None) 
##   resize(a, new_shape) returns a new array with the specified shape.
##   The original array's total size can be any size.
xresize = MV2.resize(xarange, (8,2))

## set_default_fill_value(value_type, value) 
##   Set the default fill value for value_type to value.
##   value_type is a string: 'real','complex','character','integer',or 'object'.
##   value should be a scalar or single-element array.

## sometrue(a, axis=None)
##   True iff some element is true
xsome = MV2.zeros((3,4))
xsome[1,2] = 1
res = MV2.sometrue(xsome)
res2 = MV2.sometrue(xsome, axis=1)

## sum(a, axis=0, fill_value=0) 
##   Sum of elements along a certain axis using fill_value for missing.
xsum = MV2.sum(uf, axis=1)
xsum2 = MV2.sum(xones, axis=1)
xsum3 = MV2.sum(xones)

## take(a, indices, axis=0) 
##   take(a, indices, axis=0) returns selection of items from a.
xtake = MV2.take(xmasked, [0,2,4,6,8], 1)
xtake2 = MV2.take(xmasked, [0,2,4,6,8])

## transpose(a, axes=None) 
##   transpose(a, axes=None) reorder dimensions per tuple axes
xtr = MV2.transpose(ud)
xtr = numpy.arange(24)
xtr.shape = (4,6)
xtr1 = MV2.transpose(xtr)
xtr2 = MV2.transpose(MV2.TransientVariable(xtr))
if not xtr2.shape == (6,4): markError('transpose shape')
xtr3 = numpy.transpose(xtr)
if not MV2.allclose(xtr1, xtr3): markError('transpose 2')
if not MV2.allclose(xtr2, xtr3): markError('transpose 1')

## where(condition, x, y) 
##   where(condition, x, y) is x where condition is true, y otherwise
xwhere = MV2.where(MV2.greater(xouter,200),xouter,MV2.masked)
xwhere2 = MV2.where(MV2.greater(ud,200),ud,MV2.masked)
xwhere3 = MV2.where(MV2.greater(uf,200),uf,MV2.masked)
xwhere2 = MV2.choose(MV2.greater(xouter,200), (MV2.masked, xouter))
if not MV2.allclose(xwhere,xwhere2): markError('where test 1')

## diagonal(x, k)
xdiag = MV2.TransientVariable([[1,2,3],[4,5,6]])
if not MV2.allclose(MV2.diagonal(xdiag, 1), [2,6]): markError('diagonal')
# Broadcast
vdat2 = vdat[0]
try:
    vsum = udat - vdat2
except:
    markError('Broadcast')

d.close()
f.close()

reportError()
