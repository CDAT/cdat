# Adapted for numpy/ma/cdms2 by convertcdms.py
"""binaryio: Fortran unformatted io
Uses Fortran wrapper module "binout"
Usage:
    from binaryio import *
    iunit = bincreate('filename')
    binwrite(iunit, some_array)  (up to 4 dimensions, or scalars)
    binclose(iunit)
    iunit = binopen('filename')
    y = binread(iunit, n, ...)   (1-4 dimensions)
    binclose(iunit)

Note that reads and writes must be paired exactly. Errors will cause a Fortran
stop that cannot be recovered from. You must know (or have written earlier
in the file) the sizes of each array. 

All data is stored as 32-bit floats.
"""

__version__ = "1.0"
__all__ = ['__init__', 'binout']
import numpy
import binout
FuioError = "Fuio error."

def row_major(x):
    "Same shape but row-major order for data."
#    return numpy.transpose(x)
    return numpy.reshape(numpy.transpose(x), x.shape)

def bincreate(filename):
    "iunit = bincreate(filename) to create output file for Fortran binary writes."
    return binout.bincreate(filename)

def binopen(filename):
    "iunit = binopen(filename) to open existing Fortran binary file for read."
    return binout.binopen(filename)

def binclose(iunit):
    "binclose(iunit) -- close unit after binary read or write."
    binout.binclose(iunit)

def binwrite (unit, x):
    "binwrite(iunit, x): write binary row-major form of x to unit iunit."
    d = numpy.array(x)
    s = d.shape
    n = len(s)
    if n == 0:
       binout.binout1(unit, d, 1)
    elif n == 1:
       binout.binout1 (unit, d, s[0])
    elif n == 2:
       binout.binout2 (unit, row_major(d), s[0], s[1])
    elif n == 3:
       binout.binout3 (unit, row_major(d), s[0], s[1], s[2])
    elif n == 4:
       binout.binout4 (unit, row_major(d), s[0], s[1], s[2], s[3])
    else:
	raise FuioError, "binwrite(unit, x): x must be 1-4 dimension"

def binread(unit, *sizes):
    "binread(iunit, *sizes): read binary row-major array of given dimensions."
    n = len(sizes)
    if n == 0:
        y = binout.binin1(unit, 1)
    elif n == 1:
        y = binout.binin1(unit, sizes[0])
    elif n == 2:
        y = binout.binin2(unit, sizes[0], sizes[1])
    elif n == 3:
        y = binout.binin3(unit, sizes[0], sizes[1], sizes[2])
    elif n == 4:
        y = binout.binin4(unit, sizes[0], sizes[1], sizes[2], sizes[3])
    else:
        raise FuioError, "binread(unit, *sizes): must be 0-4 dimensions given."
    return y

def test():
    x = numpy.arange(2*3*4*5)*1.
    x2 = numpy.array(x); x2.shape = (2*3*4, 5)
    x3 = numpy.array(x); x3.shape = (2*3, 4, 5)
    x4 = numpy.array(x); x4.shape = (2, 3, 4, 5)
    j = bincreate('test.bin')
    binwrite(j, 3)
    binwrite(j, x)
    binwrite(j, x2)
    binwrite(j, x3)
    binwrite(j, x4)
    binclose(j)
    j = binopen('test.bin')
    y0 = binread(j)
    y = binread(j, 2*3*4*5)
    y2 = binread(j, 2*3*4, 5)
    y3 = binread(j, 2*3, 4, 5)
    y4 = binread(j, 2, 3, 4, 5)
    binclose(j)
    if not numpy.allclose(row_major(x4), y4):
        print x4
        print y4
        raise FuioError, "x4 and y4 fail to compare."

if __name__ == "__main__":
    test()
    print "Test completed successfully."
