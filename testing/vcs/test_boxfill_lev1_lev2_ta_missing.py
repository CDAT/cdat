
import cdms2,sys,vcs,sys,os
src=sys.argv[1]
pth = os.path.join(os.path.dirname(__file__),"..")
sys.path.append(pth)
import checkimage
x=vcs.init()
x.drawlogooff()
x.setbgoutputdimensions(1200,1091,units="pixels")
f=cdms2.open(sys.prefix+"/sample_data/ta_ncep_87-6-88-4.nc")
s=f("ta",slice(0,1),longitude=slice(34,35),squeeze=1)-273.15
s=cdms2.MV2.masked_less(s,-45.)
b=x.createboxfill()
b.level_1=-40
b.level_2=40
x.plot(s,b,bg=1)

fnm= "test_boxfill_lev1_lev2_ta_missing.png"

x.png(fnm)

print "fnm:",fnm
print "src:",src
ret = checkimage.check_result_image(fnm,src,checkimage.defaultThreshold)
sys.exit(ret)
raw_input()

