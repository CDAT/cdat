import vcs,numpy,cdms2,MV2,os,sys
src=sys.argv[1]
pth = os.path.join(os.path.dirname(__file__),"..")
sys.path.append(pth)
import checkimage

x=vcs.init()
x.drawlogooff()
x.setbgoutputdimensions(1200,1091,units="pixels")
f = cdms2.open(os.path.join(vcs.sample_data, "clt.nc"))
s = f("clt")
box = x.createboxfill()
# Should ignore the string here
box.xmtics1 = {i:"Test" for i in range(-180, 180, 15) if i % 30 != 0}
box.ymtics1 = {i:"Test" for i in range(-90, 90, 5) if i % 10 != 0}
template = x.createtemplate()
template.xmintic1.priority = 1
template.ymintic1.priority = 1
x.plot(s, template, box, bg=1)
fnm = "test_vcs_mintics.png"
x.png(fnm)

print "fnm:",fnm
print "src:",src
ret = checkimage.check_result_image(fnm,src,checkimage.defaultThreshold)
sys.exit(ret)
