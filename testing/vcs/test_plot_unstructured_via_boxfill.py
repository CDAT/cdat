import vcs
import os,sys
import cdms2

pth = os.path.join(os.path.dirname(__file__),"..")
sys.path.append(pth)
import checkimage

f=cdms2.open(os.path.join(vcs.prefix,"sample_data","sampleCurveGrid4.nc"))

s=f("sample")
x=vcs.init()
x.drawlogooff()
x.setbgoutputdimensions(1200,1091,units="pixels")

import vcs
import os,sys
import cdms2

pth = os.path.join(os.path.dirname(__file__),"..")
sys.path.append(pth)
import checkimage

f=cdms2.open(os.path.join(vcs.prefix,"sample_data","sampleCurveGrid4.nc"))

s=f("sample")
x=vcs.init()
x.drawlogooff()
x.setbgoutputdimensions(1200,1091,units="pixels")

x.plot(s,bg=1)

fnm = "test_plot_unstructured_via_boxfill.png"
src=sys.argv[1]

x.png(fnm)

print "fnm:",fnm
print "src:",src
ret = checkimage.check_result_image(fnm,src,checkimage.defaultThreshold)
sys.exit(ret)

fnm = "test_plot_unstructured_via_boxfill.png"
src=sys.argv[1]

x.png(fnm)

print "fnm:",fnm
print "src:",src
ret = checkimage.check_result_image(fnm,src,checkimage.defaultThreshold)
sys.exit(ret)
