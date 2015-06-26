import vcs
import numpy
import MV2
import resource

data = numpy.sin(numpy.arange(360*180))
data.shape=(180,360)

x=vcs.init()

mem_previous = 0
for i in range(10):
    x.plot(data+i,"default","isofill",bg=True)
    x.png(__file__+".png")
    x.clear()
    mem = float(resource.getrusage(resource.RUSAGE_SELF).ru_maxrss)/1024.
    memStr = 'Iter: %i: Max mem: %.3g MB' % (i,mem)
    print memStr,
    if i>0:
        memStr = ' Mem increase: %.3g MB' % (mem-mem_previous)
    else:
        memStr = ''
    print memStr
    mem_previous = mem
