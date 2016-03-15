import cdms2
import numpy
import sys

f = cdms2.open(sys.argv[1])

v = f("tas")

fok = cdms2.open(sys.argv[2])

vok = f("tas")
print f,fok
assert(numpy.ma.allclose(v,vok))
