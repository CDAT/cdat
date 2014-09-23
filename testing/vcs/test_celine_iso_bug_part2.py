import vcs,numpy,cdms2,MV2,os,sys
src=sys.argv[1]
pth0 = os.path.dirname(src)
pth = os.path.join(pth0,"..")
sys.path.append(pth)
import checkimage
f=cdms2.open(os.path.join(pth0,"celine.nc"))
s=f("data")
x=vcs.init()
x.scriptrun(os.path.join(pth0,"celine.json"))
i=x.getisofill("celine")
b=vcs.createboxfill()
b.levels=i.levels
b.fillareacolors=i.fillareacolors
b.boxfill_type="custom"
x.plot(s,b,bg=1)
fnm = "test_celine_iso_2.png"
x.png(fnm)

print "fnm:",fnm
print "src:",src
ret = checkimage.check_result_image(fnm,src,checkimage.defaultThreshold)
sys.exit(ret)
