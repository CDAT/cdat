import vcs
import os,sys
import cdms2

pth = os.path.join(os.path.dirname(__file__),"..")
sys.path.append(pth)

import checkimage

f = cdms2.open(os.path.join(vcs.sample_data,"sampleCurveGrid4.nc"))
s = f("sample")
x = vcs.init()
x.setantialiasing(0)
x.drawlogooff()
x.setbgoutputdimensions(1200,1091,units="pixels")
x.plot(s,bg=1)
fnm = "test_plot_unstructured_via_boxfill.png"
src = sys.argv[1]
x.png(fnm)
ret = checkimage.check_result_image(fnm,src,checkimage.defaultThreshold)
x.close()
sys.exit(ret)
