import vcs,numpy,cdms2,MV2,os,sys
src=sys.argv[1]
pth = os.path.join(os.path.dirname(src),"..")
sys.path.append(pth)
import checkimage
f=cdms2.open(os.path.join(sys.prefix,"sample_data","clt.nc"))
T=f('clt')
v = vcs.init()
v.setbgoutputdimensions(1200,1091,units="pixels")
v.plot(T,bg=1)
# This will write a blank plot to a file:
fnm = "first_png_blank.png"
v.png(fnm)

print "fnm:",fnm
print "src:",src
ret = checkimage.check_result_image(fnm,src,checkimage.defaultThreshold)
sys.exit(ret)
