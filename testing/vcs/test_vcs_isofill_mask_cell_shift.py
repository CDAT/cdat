import os,sys
import MV2
import vcs
import cdms2
src=sys.argv[1]
pth = os.path.join(os.path.dirname(__file__),"..")
sys.path.append(pth)
import checkimage
x=vcs.init()
x.drawlogooff()

x.setbgoutputdimensions(1200,1091,units="pixels")

f=cdms2.open(os.path.join(sys.prefix,"sample_data","clt.nc"))
s=f("clt",slice(0,1),latitude=(30,70),longitude=(-130,-60))
s2=MV2.masked_greater(s,65.)

x.plot(s2,"default","isofill",bg=1)
fnm = "test_vcs_isofill_mask_cell_shift.png"
x.png(fnm)
print "fnm:",fnm
print "src:",src
ret = checkimage.check_result_image(fnm,src,checkimage.defaultThreshold)
sys.exit(ret)
