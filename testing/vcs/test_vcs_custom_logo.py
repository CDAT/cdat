import vcs
import os
import sys
x=vcs.init()
x.open()

logo1 = vcs.utils.Logo("My Test Logo")
logo1.x = .2
logo1.y = .2
logo1.plot(x)

png_pth = os.path.join(sys.prefix,"share","vcs","uvcdat.png")
print "PNG:",png_pth
logo2 = vcs.utils.Logo(png_pth)
logo2.x=.7
logo2.y=.8
logo2.plot(x)

fnm = "test_vcs_custom_logo.png"
print "dumping to:",fnm
x.png(fnm)

ret = vcs.testing.regression.check_result_image(fnm,sys.argv[1])

sys.exit(ret)
