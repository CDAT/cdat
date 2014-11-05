import os, sys, vcs, cdms2
src=sys.argv[1]
pth = os.path.join(os.path.dirname(__file__),"..")
sys.path.append(pth)
import checkimage
cdmsfile = cdms2.open(vcs.prefix+"/sample_data/clt.nc")
data = cdmsfile('clt')
x = vcs.init()
x.plot(data, bg=1)
x.close()
x.plot(data[4][1:89], bg=1)
fnm = "test_vcs_close.png"
x.png(fnm)
ret = checkimage.check_result_image(fnm, src, checkimage.defaultThreshold)
sys.exit(ret)
