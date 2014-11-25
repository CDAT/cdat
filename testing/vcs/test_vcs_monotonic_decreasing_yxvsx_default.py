import vcs,numpy,cdms2,MV2,os,sys
src=sys.argv[1]
pth = os.path.join(os.path.dirname(__file__),"..")
sys.path.append(pth)
import checkimage
x=vcs.init()

x.setbgoutputdimensions(1200,1091,units="pixels")

t=cdms2.createAxis(numpy.arange(120))
t.designateTime()
t.id="time"
t.units="months since 2014"
data = MV2.arange(120,0,-1)
data.id="data"
data.setAxis(0,t)
x=vcs.init()
x.plot(data,bg=1)
fnm = 'test_vcs_monotonic_decreasing_yxvsx_default.png'

x.png(fnm)
print "fnm:",fnm
print "src:",src
ret = checkimage.check_result_image(fnm,src,checkimage.defaultThreshold)
sys.exit(ret)
