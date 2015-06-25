import numpy
import MV2
import cdms2
import os

f = cdms2.open("test_simple_write.nc", "w")

data = numpy.random.random((20, 64, 128))
data = MV2.array(data)
data.getAxis(0).designateTime()
f.write(data, dtype=numpy.float32, id="test_simple")
f.close()
os.remove("test_simple_write.nc")
