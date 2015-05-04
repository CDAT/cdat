import vcs, cdms2, os, sys
src=sys.argv[1]
pth = os.path.join(os.path.dirname(__file__),"..")
sys.path.append(pth)
import checkimage
x=vcs.init()
x.drawlogooff()

x.setbgoutputdimensions(1200,1091,units="pixels")
x = vcs.init()
f = cdms2.open(os.path.join(vcs.prefix, "sample_data", "clt.nc"))
s = f("clt")
x.meshfill(s,bg=1)
fnm = "test_meshfill_regular_grid.png"
x.png(fnm)

print "fnm:",fnm
print "src:",src
ret = checkimage.check_result_image(fnm,src,checkimage.defaultThreshold)
sys.exit(ret)
