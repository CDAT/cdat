import sys,os,cdms2,vcs
import vcs
src1=sys.argv[1]
src2=sys.argv[2]
pth = os.path.join(os.path.dirname(__file__),"..")
sys.path.append(pth)
import checkimage
x=vcs.init()
x.drawlogooff()
x.setbgoutputdimensions(1200,1091,units="pixels")
f=cdms2.open(os.path.join(vcs.prefix,"sample_data","clt.nc"))
s=f("clt",time=slice(0,1),latitude=(-7,5),squeeze=1)
x.plot(s,bg=1)
fnm = "test_vcs_issue_960_labels_1.png"
x.png(fnm)
print "fnm:",fnm
print "src:",src1
ret = checkimage.check_result_image(fnm,src1,checkimage.defaultThreshold)
b=x.createboxfill()
b.datawc_y1=-7
b.datawc_y2=5
x.plot(s,b,bg=1)
fnm = "test_vcs_issue_960_labels_2.png"
x.png(fnm)
print "fnm:",fnm
print "src:",src2
ret += checkimage.check_result_image(fnm,src2,checkimage.defaultThreshold)
sys.exit(ret)
