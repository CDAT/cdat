import vcs
import cdms2
import os
import sys
import time
pth = os.path.join(os.path.dirname(__file__),"..")
sys.path.append(pth)
import checkimage

f=cdms2.open(os.path.join(vcs.sample_data,"clt.nc"))
s=f("clt",slice(0,12)) # read only 12 times steps to speed up things

x=vcs.init()
x.setantialiasing(0)
x.drawlogooff()
x.setbgoutputdimensions(1200,1091,units="pixels")

iso=x.createisoline()
iso.label='y'
x.plot(s,iso,bg=1)
x.animate.create()
print "Saving now"
prefix= os.path.split(__file__)[1][:-3]
x.animate.save("%s.mp4"%prefix)
pngs = x.animate.close(preserve_pngs = True) # so we can look at them again
src_pth = sys.argv[1]
pth = os.path.join(src_pth,prefix)
ret = 0
for p in pngs:
  print "Checking:",p
  ret += checkimage.check_result_image(p,os.path.join(pth,os.path.split(p)[1]),checkimage.defaultThreshold)
if ret == 0:
    os.removedirs(os.path.split(p)[0])
    os.remove("%s.mp4" % prefix)
x.close()
sys.exit(ret)
