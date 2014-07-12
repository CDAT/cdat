import vcs_legacy
x=vcs_legacy.init()
import sys,os
src = sys.argv[1]
pth = os.path.join(os.path.dirname(src),"..")
sys.path.append(pth)
import checkimage

m=x.createmarker()
m.x=[.5]
m.y=[.5]
m.type=18
m.size = 24
m.color=242
x.plot(m,bg=1)
x.setbgoutputdimensions(1200,1091,units="pixels")
x.png("test_vcs_legacy_hurricane_marker")

ret = checkimage.check_result_image("test_vcs_legacy_hurricane_marker.png",src,0.05)
sys.exit(ret)
