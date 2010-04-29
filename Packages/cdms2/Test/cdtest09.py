#!/usr/bin/env python
print 'Test 9: cu emulation...',
try:
    import sys, cu, cdms2, numpy, os
except:
    sys.exit()
from markError import clearError,markError,reportError
clearError()
f = cu.open(os.path.join(sys.prefix,'sample_data','test.xml'))
c = f.getslab('u')
c1 = f.getslab('u', 2.0, 390.0)
c.createattribute('flavor', 'chocolate')
assert hasattr(c, 'flavor')
assert c.flavor == 'chocolate'
c.setattribute('flavor', 'vanilla')
assert c.flavor == 'vanilla'
assert c.attributes['flavor'] == 'vanilla'
assert c.getattribute('flavor') == 'vanilla'
assert 'flavor' in c.listattributes()
c.setdimattribute(0, 'nice', 1)
assert 'nice' in c.listdimattributes(0)
assert c.getAxis(0).nice == 1
g = cdms2.open(os.path.join(sys.prefix,'sample_data','test.xml'))
gc = g.variables['u']
assert numpy.ma.allclose(c[...], gc[...])
assert numpy.ma.allclose(c1[...], gc.getRegion(time=cu.dimensionrange(2.0,390.0)))

#Some of these tests assume axis 0 is time.
list1 = c.getAxisList()
def axisChooser(a):
    return a.units == list1[1].units
list2 = []
for x in list1:
    if not x.isLatitude():
        list2.append(x)
list3 = c.getAxisList(omit='latitude')
list4 = c.getAxisList(omit=1)
list5 = c.getAxisList(omit=[0,1])
list6 = c.getAxisList(omit=-1)
list7 = c.getAxisList(omit=[-1,-2])
list8 = c.getAxisList([0,2], omit='time')
list9 = c.getAxisList(omit=list1[1])
list10 = c.getAxisList(axisChooser)
list11 = c.getAxisList(['time', 'latitude'], order='yt')

assert len(list2) == len(list3)
assert len(list5) == len(list1)-2
assert len(list6) == len(list1)-1
assert len(list7) == len(list1)-2
assert len(list8) == 1 

assert list2[1] is list3[1]
assert list4[1] is list3[1]
assert list5[0] is list1[2]
assert list6[1] is list1[1]
assert list7[0] is list1[0]
assert list8[0] is list1[2]
assert list9[0] is list1[0]
assert list9[1] is list1[2]
assert list10[0] is list1[1]
assert list11[1] is list1[0]
assert list11[0].isLatitude()
reportError()
