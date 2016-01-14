import cdms2
import sys
import vcs
import os

src = sys.argv[1]
pth = os.path.join(os.path.dirname(__file__), "..")
sys.path.append(pth)
import checkimage
x = vcs.init()
x.setantialiasing(0)
x.drawlogooff()
# Needs to set the size of window so it is consistent accross
# test platforms
x.open(814, 606)


f = cdms2.open(vcs.sample_data + "/clt.nc")
s = f("clt")

# Has to plot in foreground to simulate a click
x.plot(s)

# Simulate a click -- VTK Specific
i = x.backend.renWin.GetInteractor()
i.SetEventInformation(200, 200)
i.LeftButtonPressEvent()

fnm = "test_vcs_click_info.png"
x.png(fnm, width=814, height= 606)

print "fnm:", fnm
print "src:", src
ret = checkimage.check_result_image(fnm, src, checkimage.defaultThreshold)
sys.exit(ret)
