import vcs,numpy,cdms2,MV2,os,sys
src=sys.argv[1]
pth = os.path.join(os.path.dirname(__file__),"..")
sys.path.append(pth)
import checkimage

x=vcs.init()
x.setbgoutputdimensions(1200,1091,units="pixels")
a=numpy.arange(100)
a.shape=(10,10)
x.plot(a,bg=1)
fnm = "test_vcs_draw_logo_on.png"
x.png(fnm)

print "fnm:",fnm
print "src:",src
ret = checkimage.check_result_image(fnm,src,checkimage.defaultThreshold)
sys.exit(ret)
