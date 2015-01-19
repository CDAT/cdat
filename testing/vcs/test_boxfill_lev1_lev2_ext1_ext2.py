
import cdms2,sys,vcs,sys,os
src=sys.argv[1]
pth = os.path.join(os.path.dirname(__file__),"..")
sys.path.append(pth)
import checkimage
x=vcs.init()
x.drawlogooff()
f=cdms2.open(sys.prefix+"/sample_data/clt.nc")
s=f("clt",slice(0,1),squeeze=1)
b=x.createboxfill()
b.level_1=20
b.level_2=80
b.ext_1="y"
b.ext_2="y"
x.plot(s,b,bg=1)

fnm= "test_boxfill_lev1_lev2_ext1_ext2.png"

x.png(fnm)

print "fnm:",fnm
print "src:",src
ret = checkimage.check_result_image(fnm,src,checkimage.defaultThreshold)
sys.exit(ret)

