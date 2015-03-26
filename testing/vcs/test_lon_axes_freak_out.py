import os,sys,vcs,cdms2
src=sys.argv[1]
pth = os.path.join(os.path.dirname(__file__),"..")
sys.path.append(pth)
import checkimage


f=cdms2.open(os.path.join(vcs.prefix,"sample_data","clt.nc"))
s=f("clt")
s3=f("clt",longitude=(0,360))

print s.shape,s3.shape


x=vcs.init()
x.drawlogooff()
x.setbgoutputdimensions(1200,1091,units="pixels")

x.plot(s,bg=1)
x.clear()
x.plot(s3,bg=1)

fnm = "test_lon_axes_freak_out.png"

x.png(fnm)
print "fnm:",fnm
print "src:",src
ret = checkimage.check_result_image(fnm,src,checkimage.defaultThreshold)
sys.exit(ret)
