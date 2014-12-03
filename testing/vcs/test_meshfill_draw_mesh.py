import vcs,cdms2,sys,os
src=sys.argv[1]
pth = os.path.join(os.path.dirname(__file__),"..")
sys.path.append(pth)
import checkimage
x=vcs.init()
x.setbgoutputdimensions(1200,1091,units="pixels")
x.drawlogooff()
fnmcurv = os.path.join(sys.prefix,"sample_data",'sampleCurveGrid4.nc')
f=cdms2.open(fnmcurv)

s=f("sample")
m=x.createmeshfill()
m.mesh=True

x.plot(s,m,bg=1)
fnm = "test_meshfill_draw_mesh.png"
x.png(fnm)
print "fnm:",fnm
print "src:",src
ret = checkimage.check_result_image(fnm,src,checkimage.defaultThreshold)
sys.exit(ret)
