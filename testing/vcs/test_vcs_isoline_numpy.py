import vcs,cdms2,sys,os
src=sys.argv[1]
pth = os.path.join(os.path.dirname(__file__),"..")
sys.path.append(pth)
import checkimage
x=vcs.init()
x.setbgoutputdimensions(1200,1091,units="pixels")
x.drawlogooff()
fnm = os.path.join(sys.prefix,"sample_data",'clt.nc')
f=cdms2.open(fnm)

s=f("clt")
gm=x.createisofill()
x.plot(s.filled(),gm,bg=1)
fnm = "test_vcs_isoline_numpy.png"
x.png(fnm)
print "fnm:",fnm
print "src:",src
ret = checkimage.check_result_image(fnm,src,checkimage.defaultThreshold)
sys.exit(ret)
