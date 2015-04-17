import unidata
import numpy

## Test for udunits2 feature wrapped
## Author: Charles Doutriaux
## The units created bellow
## have absolutely no meaning
## They are just used to test that the features exposed
## work properly

unidata.udunits_wrap.init()

## Create a new baseunits called eq
unidata.addBaseUnit("eq")

## Create a dimensionless units named dimless
unidata.addDimensionlessUnit("dimless")

## Created a scaled units for dimless
unidata.addScaledUnit("pourcent",.01,"dimless")

## Create an offsetted units for dimless
unidata.addOffsettedUnit("fakeCelsius",273.15,"dimless")

## Create mutliplied units
unidata.addMultipliedUnits("efC","eq","fakeCelsius")
unidata.addMultipliedUnits("efP","eq","pourcent")

## Create divded units
unidata.addDividedUnits("defC","eq","fakeCelsius")
unidata.addDividedUnits("defP","eq","pourcent")

## Test scaled
p = unidata.udunits(1,"pourcent")
## Test new base unit
eq = unidata.udunits(1,"eq")

o=p.to("dimless")
print o
assert(o.units=="dimless")
assert(o.value == 0.01)

fC = unidata.udunits(2,"fakeCelsius")
o=fC.to("dimless")
print o
assert(o.units=="dimless")
assert(numpy.allclose(o.value,275.15))
o=fC.to("pourcent")
print o
assert(o.units=="pourcent")
assert(numpy.allclose(o.value,27515))

#Trying multiplied
o=eq*fC
print o
assert(o.units=="eq*fakeCelsius")
assert(o.value == 2)
o=o.to("efC")
print o
assert(o.units=="efC")
assert(o.value == 2)
o=o.to("efP")
print o
assert(o.units=="efP")
assert(o.value == 200)

#Trying divided
o=eq/fC
print o
assert(o.units=="eq/fakeCelsius")
assert(o.value == .5)
o=o.to("defC")
print o
assert(o.units=="defC")
assert(o.value == .5)
o=o.to("defP")
print o
assert(o.units=="defP")
assert(o.value == 5E-3)
