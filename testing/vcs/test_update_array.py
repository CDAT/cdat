import vcs,numpy

a=numpy.arange(100)
a.shape=(10,10)
b=numpy.ma.cos(a)/2.
a=numpy.ma.sin(a)

x=vcs.init()
d = x.plot(a)
raw_input("Press enter")
print d
x.backend.update_input(d.backend,b)
raw_input("ok done")
