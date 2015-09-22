import vcs
import numpy

x=vcs.init()
print x.size
x.open()
i = x.canvasinfo()
r = float(i["width"])/i["height"]
print "r is:",r,i
assert(numpy.allclose(r,1.29438202247,.001))

x=vcs.init(size=4)
x.open()
i = x.canvasinfo()
r = float(i["width"])/i["height"]
print "r is:",r,i
assert(numpy.allclose(r,4.,atol=0.01))

x=vcs.init(size="A4")
x.open()
i = x.canvasinfo()
r = float(i["width"])/i["height"]
print "r is:",r,i
assert(numpy.allclose(r,1.41523341523,atol=0.01))
