# Test if the marker can be removed (marker=None) from a 1D plot
# Works in 1.5.1, but fails in 2.1
#
# J-Y Peterschmitt - LSCE - 03/2015

import vcs,numpy,cdms2,MV2,os,sys
src=sys.argv[1]
pth = os.path.join(os.path.dirname(__file__),"..")
sys.path.append(pth)
import checkimage

dummy_data = numpy.arange(50, dtype=numpy.float32)

x = vcs.init()
x.drawlogooff()
x.setbgoutputdimensions(1200,1091,units="pixels")

gm = x.createyxvsx('test_yxvsx')


# Remove the marker
gm.marker = None

x.plot(gm, dummy_data,bg=1)

fnm = "test_remove_marker_none_1d.png"
x.png(fnm)
print "fnm:",fnm
print "src:",src
ret = checkimage.check_result_image(fnm,src,checkimage.defaultThreshold)
sys.exit(ret)
# The end
