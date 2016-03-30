import vcs
import numpy

data = numpy.sin(numpy.arange(100))
data = numpy.reshape(data, (10, 10))

x = vcs.init()
x.plot(data, bg=1)
assert x.orientation() == "landscape", "Default canvas orientation failed"
c = x.canvasinfo()
assert c['width'] == 814, "Default canvas width failed"
assert c['height'] == 606, "Default canvas height failed"

x.clear()
x.portrait()
x.plot(data, bg=1)
assert x.orientation() == "portrait", "Portrait canvas orientation failed"
c = x.canvasinfo()
assert c['width'] == 606, "Portrait canvas width failed"
assert c['height'] == 814, "Portrait canvas height failed"

x.clear()
x.landscape()
x.plot(data, bg=1)
assert x.orientation() == "landscape", "Landscape canvas orientation failed"
c = x.canvasinfo()
assert c['width'] == 814, "Landscape canvas width failed"
assert c['height'] == 606, "Landscape canvas height failed"

x.clear()
x.landscape()
x.plot(data, bg=1)
assert x.orientation() == "landscape", "Landscape canvas orientation failed"
c = x.canvasinfo()
assert c['width'] == 814, "Landscape canvas width failed"
assert c['height'] == 606, "Landscape canvas height failed"

x.clear()
x.portrait()
x.plot(data, bg=1)
assert x.orientation() == "portrait", "Portrait canvas orientation failed"
c = x.canvasinfo()
assert c['width'] == 606, "Portrait canvas width failed"
assert c['height'] == 814, "Portrait canvas height failed"
