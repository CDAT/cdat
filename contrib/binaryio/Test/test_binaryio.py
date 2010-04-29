# Adapted for numpy/ma/cdms2 by convertcdms.py
import numpy, os, binaryio
if os.path.isfile('test.bin'):
    os.unlink('test.bin')
x = numpy.arange(2*3*4*5)*1.
x2 = numpy.array(x); x2.shape = (2*3*4, 5)
x3 = numpy.array(x); x3.shape = (2*3, 4, 5)
x4 = numpy.array(x); x4.shape = (2, 3, 4, 5)
j = binaryio.bincreate('test.bin')
binaryio.binwrite(j, 3)
binaryio.binwrite(j, x)
binaryio.binwrite(j, x2)
binaryio.binwrite(j, x3)
binaryio.binwrite(j, x4)
binaryio.binclose(j)
j = binaryio.binopen('test.bin')
y0 = binaryio.binread(j)
y = binaryio.binread(j, 2*3*4*5)
y2 = binaryio.binread(j, 2*3*4, 5)
y3 = binaryio.binread(j, 2*3, 4, 5)
y4 = binaryio.binread(j, 2, 3, 4, 5)
binaryio.binclose(j)
if not numpy.allclose(binaryio.row_major(x4), y4):
    raise FuioError, "x4 and y4 fail to compare."
print "Test completed successfully."

