import cdms
from MA import allclose
f = cdms.open('clt.nc')
v = f('clt')
s = cdms.selectors.timeslice(0,120,12)
x = s.select(v)
print x.shape
y = v(s)
print y.shape
assert allclose(x,y)
assert allclose(x, f('clt', time=slice(0,120,12)))
x = v(s, longitude=(100,150))
print x.shape
y = x(latitude=(-40,40))
print y.shape
w = f('clt', cdms.selectors.longitude(100,140))
print w.shape
print f('clt', slice(0,1), squeeze=1).shape
