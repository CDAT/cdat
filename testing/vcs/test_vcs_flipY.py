import vcs,numpy,cdms2,MV2,os,sys
src=sys.argv[1]
pth = os.path.join(os.path.dirname(__file__),"..")
sys.path.append(pth)
import checkimage
x=vcs.init()

x.setbgoutputdimensions(1200,1091,units="pixels")

f=cdms2.open(os.path.join(sys.prefix,"sample_data","ta_ncep_87-6-88-4.nc"))


vr = "ta"
s=f(vr,slice(0,1),longitude=slice(90,91),squeeze=1)
x.plot(s,bg=1)
fnm = "test_vcs_flipY.png"
x.png(fnm)
print "fnm:",fnm
print "src:",src
ret = checkimage.check_result_image(fnm,src,checkimage.defaultThreshold)
sys.exit(ret)


