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

# Make sure incompatible units can't be added
try:
    m2=m+dK
    failed = False
except:
    print 'Ok could not add',m,'and',dK
    failed = True
assert(failed)

#Just to be sure test against another system
o=dK-dC
print dK,'-',dC,'=',dK-dC
assert(o.units==dK.units)
assert(numpy.allclose(o.value,-6.15))

o = dF/dC
print dF,'/',dC,'=',dF/dC
#division by compatible units result in pur float (unitless)
assert(isinstance(o,float))
assert(numpy.allclose(o,0.908096280088))
o=dC/dF
print dC,'/',dF,'=',dC/dF
assert(isinstance(o,float))
assert(numpy.allclose(o,1.16470588235))
o=dC/m
print dC,'/',m,'=',dC/m
assert(o.units=="degC/km")
assert(numpy.allclose(o.value,6600.0))


o=dF*dC
print dF,'*',dC,'=',dF*dC
assert(o.units=="degF*degF")
assert(numpy.allclose(o.value,7586.2))
o=dC*dF
print dC,'*',dF,'=',dC*dF
assert(o.units=="degC*degC")
assert(numpy.allclose(o.value,935.0))
o=dC*m
print dC,'*',m,'=',dC*m
assert(o.units=="degC*km")
assert(numpy.allclose(o.value,0.165))

#Test simple conversion
o=dC.to("K")
print dC,'to K',dC.to('K')
assert(o.units=="K")
assert(numpy.allclose(o.value,306.15))

print 'how to go from',dF.units,'to K',dF.how('K')
## dF.show()
## print dF.available()

"""
kn=dF.known_units()
for k in kn.keys():
    print k,kn[k]
kn=dF.known_units(bytype=1)

for k in kn.keys():
    print 'UNITS OF',k
    ln=kn[k]
    for l in ln:
        print '\t',l
"""

# Test remaining ufuncs
o=m**2
print m**2
assert(o.units=="km**2")
assert(numpy.allclose(o.value,2.5e-05))
o=5+m
print 5+m
assert(o.units=="km")
assert(numpy.allclose(o.value,5.005))
o=5-m
print 5-m
assert(o.units=="km")
assert(numpy.allclose(o.value,4.995))
o=5*m
print 5*m
assert(o.units=="km")
assert(numpy.allclose(o.value,0.025))
o=m*5
print m*5
assert(o.units=="km")
assert(numpy.allclose(o.value,0.025))
o=5/m
print 5/m
assert(o.units=="1/(km)")
assert(numpy.allclose(o.value,1000.))
o=m/5
print m/5
assert(o.units=="km")
assert(numpy.allclose(o.value,.001))
