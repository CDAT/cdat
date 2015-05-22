import vcs
import cdms2
import os
import sys
import MV2

def runTest():
    pth = os.path.join(os.path.dirname(__file__),"..")
    sys.path.append(pth)
    import checkimage

    f=cdms2.open(os.path.join(vcs.prefix,"sample_data","sampleCurveGrid4.nc"))
    s=f("sample")
    print s.shape

    s2=MV2.resize(s,(4,32,48))
    t=cdms2.createAxis(range(4))
    t.units="months since 2015"
    t.id="time"
    t.designateTime()
    s2.setAxis(0,t)
    s2.setAxis(1,s.getAxis(0))
    s2.setAxis(2,s.getAxis(1))
    s2.setGrid(s.getGrid())
    for i in range(4):
        s2[i]=s2[i]*(1+float(i)/10.)
    x=vcs.init()
    x.drawlogooff()
    x.setbgoutputdimensions(1200,1091,units="pixels")

    gm=x.createmeshfill()
    x.plot(s2,gm,bg=1)
    print "plotted!"
    x.animate.create()
    print "created!"
    prefix= os.path.split(__file__)[1][:-3]
    #import pdb;pdb.set_trace()
    x.animate.save("%s.mp4"%prefix)
    print "saved!"
    pngs = x.animate.close(preserve_pngs = True) # so we can look at them again
    print "closed!"
    src_pth = sys.argv[1]
    pth = os.path.join(src_pth,prefix)
    ret = 0
    for p in pngs:
      print "Checking:",p
      ret += checkimage.check_result_image(p,os.path.join(pth,os.path.split(p)[1]),checkimage.defaultThreshold)
    if ret == 0:
        os.removedirs(os.path.split(p)[0])
        os.remove("%s.mp4" % prefix)
    return ret

import gc
gc.enable()
gc.set_debug(gc.DEBUG_LEAK)

result = runTest()

print "Collecting:"
gc.collect()

for g in gc.garbage:
    print "%s @0x%x:\n   %s"%(type(g), id(g), str(g)[:70])

sys.exit(result)
