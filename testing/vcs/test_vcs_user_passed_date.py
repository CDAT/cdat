import vcs,cdms2,os,sys,cdtime
src=sys.argv[1]
pth = os.path.join(os.path.dirname(__file__),"..")
sys.path.append(pth)
import checkimage
f=cdms2.open(os.path.join(vcs.sample_data,"clt.nc"))
s=f("clt",squeeze=1)
x=vcs.init()
x.setantialiasing(0)
x.drawlogooff()
x.setbgoutputdimensions(1200,1091,units="pixels")
x.plot(s,bg=1,time=cdtime.comptime(2015))
fnm = os.path.split(__file__)[1][:-3]+".png"
x.png(fnm)
print "fnm:",fnm
print "src:",src
ret = checkimage.check_result_image(fnm,src,checkimage.defaultThreshold)
sys.exit(ret)
