import cdms,os,sys
from cdms.tvariable import TransientVariable as TV
from cdms.MV import *

print 'Test 11: MV module (transient variable arithmetic) ... ',
from markError import NTIME,NLAT,NLON,x,clearError,markError,reportError
clearError()

d = cdms.open(os.path.join(sys.prefix,'sample_data','test.xml'))
ud = d['u']
vd = d['v']
udat = ud[:]
vdat = vd[:]
ulat = ud.getLatitude()
if not isinstance(udat, TV): markError('Slice does not return TV')
f = cdms.open(os.path.join(sys.prefix,'sample_data','u_2000.nc'))
uf = f['u']

vel = sqrt(ud*ud + vd*vd)
vel.id = 'velocity'
vel2 = sqrt(udat*udat + vdat*vdat)
vel2.id = 'velocity'
vel2.units = ud.units
if not allequal(vel,vel2): markError('Slice operators do not compare')

x1 = uf+1.0
x2 = 1.0-ud
x11 = -uf
x12 = abs(ud)
x3 = uf+x2
x4 = 1.0+ud
x5 = uf-1
x6 = ud*uf
x7 = ud/x2
x8=1/uf
x9 = 3*ud
x10=uf**3
x13 = add.reduce(uf)
x14 = add.reduce(ud)
x15 = x9.astype(Float32)
if not x15.typecode()==Float32: markError('astype error')

## arrayrange(start, stop=None, step=1, typecode=None, axis=None, attributes=None, id=None) 
##   Just like range() except it returns a variable whose type can be specfied
##   by the keyword argument typecode. The axis of the result variable may be specified.
xarange = arange(16., axis=ulat)

## masked_array(a, mask=None, fill_value=None, axes=None, attributes=None, id=None) 
##   masked_array(a, mask=None) = 
##   array(a, mask=mask, copy=0, fill_value=fill_value)
##   Use fill_value(a) if None.
xmarray = masked_array(ud)

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
xones = ones(uf.shape, Float32, axes=uf.getAxisList(), attributes=uf.attributes, id=uf.id)
if not xones[0,0,0]==1.0: markError('xones')

## zeros(shape, typecode='l', savespace=0, axes=None, attributes=None, id=None) 
##   zeros(n, typecode=Int, savespace=0, axes=None, attributes=None, id=None) = 
##   an array of all zeros of the given length or shape.
xzeros = zeros(ud.shape, Float, axes=ud.getAxisList(), attributes=ud.attributes, id=ud.id)
xmasked = as_masked(xzeros)

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
xav = average(xones, axis=1)
xav2 = average(ud)
xav3 = average(udat)
xav4, wav4 = average(udat, weights=ones(udat.shape, Float), returned=1)

## choose(indices, t) 
##   Shaped like indices, values t[i] where at indices[i]
##   If t[j] is masked, special treatment to preserve type.
ct1 = TransientVariable([1,1,2,0,1])
ctr = choose(ct1, [MA.masked, 10,20,30,40])
if not allclose(ctr, [10, 10, 20, 100, 10]): markError('choose error 1')
ctx = TransientVariable([1,2,3,150,4])
cty = -TransientVariable([1,2,3,150,4])
ctr = choose(greater(ctx,100), (ctx, 100))
if not allclose(ctr, [1,2,3,100,4]): markError('choose error 2')
ctr = choose(greater(ctx,100), (ctx, cty))
if not allclose(ctr, [1,2,3,-150,4]): markError('choose error 3')

## concatenate(arrays, axis=0, axisid=None, axisattributes=None) 
##   Concatenate the arrays along the given axis. Give the extended axis the id and
##   attributes provided - by default, those of the first array.
try:
    xcon = concatenate((ud,vd))
except:
    markError('Concatenate error')

## isMaskedVariable(x) 
##   Is x a masked variable, that is, an instance of AbstractVariable?
im1 = isMaskedVariable(xones)
im2 = isMaskedVariable(xmasked)

## outerproduct(a, b) 
##   outerproduct(a,b) = {a[i]*b[j]}, has shape (len(a),len(b))
xouter = outerproduct(arange(16.),arange(32.))
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
xge = masked_greater_equal(xouter, 120)

## masked_less(x, value) 
##   masked_less(x, value) = x masked where x < value
xl = masked_less(xouter, 160)

## masked_less_equal(x, value) 
##   masked_less_equal(x, value) = x masked where x <= value

## masked_not_equal(x, value) 
##   masked_not_equal(x, value) = x masked where x != value

## masked_outside(x, v1, v2) 
##   x with mask of all values of x that are outside [v1,v2]
xmo = masked_outside(xouter, 120, 160)

## count(a, axis=None) 
##   Count of the non-masked elements in a, or along a certain axis.
xcount = count(xmo)
xcount2 = count(xmo,1)

## masked_where(condition, x, copy=1) 
##   Return x as an array masked where condition is true. 
##   Also masked where x or condition masked.
xmwhere = masked_where(logical_and(greater(xouter,120),less(xouter,160)),xouter)

## maximum(a, b=None) 
##   returns maximum element of a single array, or elementwise
maxval = maximum(xouter)
xmax = maximum(ud,vd)
xmax = maximum.reduce(ud)
xmax = maximum.reduce(vd)
xmax2 = MA.maximum.reduce(vd.subSlice())
if not allclose(xmax, xmax2): markError('maximum.reduce')

## minimum(a, b=None) 
##   returns minimum element of a single array, or elementwise
minval = minimum(xouter)
xmin = minimum(ud,vd)
xmin = minimum.reduce(ud)
xmin = minimum.reduce(vd)
xmin2 = MA.minimum.reduce(vd.subSlice())
if not allclose(xmin, xmin2): markError('minimum.reduce')
t1 = TransientVariable([1.,2.,3.])
t2 = TransientVariable([1.,10.])
t3 = add.outer(t1,t2)
t3 = minimum.outer(t1,t2)
t3 = maximum.outer(t1,t2)
## product(a, axis=0, fill_value=1) 
##   Product of elements along axis using fill_value for missing elements.
xprod = product(ud)

## power(a, b, third=None) 
##   a**b
xpower = xprod**(1./3.)

## repeat(a, repeats, axis=0) 
##   repeat elements of a repeats times along axis
##   repeats is a sequence of length a.shape[axis]
##   telling how many times to repeat each element.

## reshape(a, newshape, axes=None, attributes=None, id=None) 
##   Copy of a with a new shape.
xreshape = reshape(xarange, (8,2))

## resize(a, new_shape, axes=None, attributes=None, id=None) 
##   resize(a, new_shape) returns a new array with the specified shape.
##   The original array's total size can be any size.
xresize = resize(xarange, (8,2))

## set_default_fill_value(value_type, value) 
##   Set the default fill value for value_type to value.
##   value_type is a string: 'real','complex','character','integer',or 'object'.
##   value should be a scalar or single-element array.

## sum(a, axis=0, fill_value=0) 
##   Sum of elements along a certain axis using fill_value for missing.
xsum = sum(uf, axis=1)
xsum2 = sum(xones, axis=1)

## take(a, indices, axis=0) 
##   take(a, indices, axis=0) returns selection of items from a.
xtake = take(xmasked, [0,2,4,6,8], 1)

## transpose(a, axes=None) 
##   transpose(a, axes=None) reorder dimensions per tuple axes
xtr = transpose(ud)
xtr = MA.Numeric.arange(24)
xtr.shape = (4,6)
xtr1 = transpose(xtr)
xtr2 = transpose(TransientVariable(xtr))
if not xtr2.shape == (6,4): markError('transpose shape')
xtr3 = MA.Numeric.transpose(xtr)
if not allclose(xtr1, xtr3): markError('transpose 2')
if not allclose(xtr2, xtr3): markError('transpose 1')

## where(condition, x, y) 
##   where(condition, x, y) is x where condition is true, y otherwise
xwhere = where(greater(xouter,200),xouter,masked)
xwhere2 = where(greater(ud,200),ud,masked)
xwhere3 = where(greater(uf,200),uf,masked)
xwhere2 = choose(greater(xouter,200), (masked, xouter))
if not allclose(xwhere,xwhere2): markError('where test 1')

## diagonal(x, k)
xdiag = TransientVariable([[1,2,3],[4,5,6]])
if not allclose(diagonal(xdiag, 1), [2,6]): markError('diagonal')
# Broadcast
vdat2 = vdat[0]
try:
    vsum = udat - vdat2
except:
    markError('Broadcast')

d.close()
f.close()

reportError()
