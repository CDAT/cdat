import sys,os
src = sys.argv[1]
import vcs.testing.regression as regression
import vcs
import vcsaddons, numpy

x = regression.init()

numpy.random.seed(seed=12345)
vals = numpy.random.random_sample(2000) * 100
histo = vcsaddons.histograms.Ghg()
histo.plot(vals, bg=True, x=x)

fnm = "test_vcs_addons_histogram_defaults.png"
x.png(fnm)
ret = regression.check_result_image(fnm, src)
sys.exit(ret)
