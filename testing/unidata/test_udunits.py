import unidata,numpy

m=unidata.udunits(5,'m')

cm=unidata.udunits(7,'cm')

i=unidata.udunits(3,'in')

f=unidata.udunits(5,'feet')

print m
print cm
print i
print f

dC=unidata.udunits(33,'degC')
dF=unidata.udunits(83,'degF')
dK=unidata.udunits(300,'K')

o = m+cm
print m,'+',cm,'=',o
assert(o.units==m.units)
assert(numpy.allclose(o.value,5.07))


o=cm+i
print cm,'+',i,'=',cm+i
assert(o.units==cm.units)
assert(numpy.allclose(o.value,14.62))
o=i+cm
print i,'+',cm,'=',i+cm
assert(o.units==i.units)
assert(numpy.allclose(o.value,5.75590551181))

m.units='km'
print m
assert(numpy.allclose(m.value,5E-3))
try:
    m2=m+dK
except:
    print 'Ok could not add',m,'and',dK

print dK,'-',dC,'=',dK-dC

print dF,'/',dC,'=',dF/dC
print dC,'/',dF,'=',dC/dF
print dC,'/',m,'=',dC/m


print dF,'*',dC,'=',dF*dC
print dC,'*',dF,'=',dC*dF
print dC,'*',m,'=',dC*m

print dC,'to K',dC.to('K')



print 'how to go from',dF.units,'to K',dF.how('K')

## dF.show()

## print dF.available()

kn=dF.known_units()
"""for k in kn.keys():
    print k,kn[k]
"""
kn=dF.known_units(bytype=1)

"""for k in kn.keys():
    print 'UNITS OF',k
    ln=kn[k]
    for l in ln:
        print '\t',l
"""

print m
print m**2
print m*m
print 5+m
print 5-m
print 5*m
print m*5
print 5/m
print m/5
