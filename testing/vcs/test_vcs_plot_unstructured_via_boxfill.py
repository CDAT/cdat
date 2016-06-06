import vcs, os, sys, cdms2, testing.regression as regression

f = cdms2.open(os.path.join(vcs.sample_data,"sampleCurveGrid4.nc"))
s = f("sample")
x = regression.init()
x.plot(s,bg=1)
regression.run(x, "test_plot_unstructured_via_boxfill.png")
