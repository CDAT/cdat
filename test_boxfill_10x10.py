import vcs,numpy

s= numpy.sin(numpy.arange(100))
s=numpy.reshape(s,(10,10))
x=vcs.init()
x.plot(s)

raw_input()

