import vcs
x=vcs.init()
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
x.plot(m)
x.png("test_vcs_hurricane_marker")

ret = checkimage.check_result_image("test_vcs_hurricane_marker.png",src,0.05)
sys.exit(ret)
