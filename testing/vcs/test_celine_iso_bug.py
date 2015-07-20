
import vcs,numpy,cdms2,MV2,os,sys
src=sys.argv[1]
pth0 = os.path.dirname(__file__)
pth = os.path.join(pth0,"..")
sys.path.append(pth)
import checkimage
f=cdms2.open(os.path.join(pth0,"celine.nc"))
s=f("data")
x=vcs.init()
x.setantialiasing(0)
x.drawlogooff()
x.scriptrun(os.path.join(pth0,"celine.json"))
i=x.getisofill("celine")
x.plot(s,i,bg=1)
fnm = "test_celine_iso.png"
x.png(fnm)

print "fnm:",fnm
print "src:",src
ret = checkimage.check_result_image(fnm,src,checkimage.defaultThreshold)
x.close()
sys.exit(ret)
