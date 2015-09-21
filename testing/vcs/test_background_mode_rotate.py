import vcs
import numpy

original_state = dict(width=814, height=606, mapstate=True, depth=24, y=0, x=0)
rotated_state = dict(width=606, height=814, mapstate=True, depth=24, y=0, x=0)

data = numpy.sin(numpy.arange(100))
data = numpy.reshape(data, (10, 10))

x = vcs.init()
x.plot(data, bg=1)
assert(x.orientation() == "landscape")
assert(cmp(x.canvasinfo(), original_state) == 0)

x.portrait()
assert(x.orientation() == "portrait")
assert(cmp(x.canvasinfo(), rotated_state) == 0)

x.landscape()
assert(x.orientation() == "landscape")
assert(cmp(x.canvasinfo(), original_state) == 0)
