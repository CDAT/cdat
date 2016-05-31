import sys,os
src = sys.argv[1]
pth = os.path.join(os.path.dirname(__file__),"..")
sys.path.append(pth)
import checkimage
import vcs
import vcsaddons, numpy

x=vcs.init()
x.setantialiasing(0)
x.drawlogooff()
x.setbgoutputdimensions(1200,1091,units="pixels")

numpy.random.seed(seed=12345)
vals = numpy.random.random_sample(2000) * 100
histo = vcsaddons.histograms.Ghg()
histo.plot(vals, bg=True, x=x)

fnm = "vcs_addons_test_histogram_defaults.png"
x.png(fnm)
ret = checkimage.check_result_image(fnm, src, checkimage.defaultThreshold)
sys.exit(ret)
