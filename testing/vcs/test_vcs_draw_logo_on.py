import vcs, numpy, cdms2, MV2, os, sys, vcs.testing.regression as regression

x = vcs.init()
a=numpy.arange(100)
a.shape=(10,10)
x.plot(a,bg=1)
fnm = "test_vcs_draw_logo_on.png"
x.png(fnm)
regression.check_result_image(fnm, sys.argv[1])
