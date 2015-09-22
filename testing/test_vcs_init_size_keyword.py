import vcs
import numpy

x=vcs.init()
x.open()
i = x.canvasinfo()
r = float(i["width"])/i["height"]
assert(numpy.allclose(r,1.29438202247))

x=vcs.init(size=4)
x.open()
i = x.canvasinfo()
r = float(i["width"])/i["height"]
assert(numpy.allclose(r,4.))

x=vcs.init(size="A4")
x.open()
i = x.canvasinfo()
r = float(i["width"])/i["height"]
assert(numpy.allclose(r,1.41523341523))
