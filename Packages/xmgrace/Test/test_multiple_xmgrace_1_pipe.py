#!/usr/bin/env python
# Adapted for numpy/ma/cdms2 by convertcdms.py

import xmgrace
import sys
import time

print 'Test disabled'
sys.exit()

# x=xmgrace.xmgrace.init()
# x=xmgrace.xmgrace.init(pipe_file='tmp.txt',new_pipe=True,clean_on_exit=False)
# y=xmgrace.xmgrace.init(pipe_file='tmp.txt',new_pipe=False,clean_on_exit=False)
x = xmgrace.xmgrace.init(pipe_file='tmp.txt', new_pipe=True)
y = xmgrace.xmgrace.init(pipe_file='tmp.txt', new_pipe=False)

import numpy

end = 2 * numpy.pi
inc = .1
a = numpy.arange(0, end, inc)
x.Graph[0].xmax = end / inc
x.Graph[0].ymin = -1.
x.plot(a, G=0, S=0)
time.sleep(2)
b = numpy.sin(a)
print b
y.Graph[0].xmax = end / inc
y.Graph[0].ymin = -1.
y.plot(b, G=0, S=1)
time.sleep(2)
x.postscript("test")
time.sleep(2)
x.close()

y.close()
