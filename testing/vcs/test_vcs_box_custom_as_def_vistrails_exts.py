import os, sys, cdms2, vcs, vcs.vcs.vcs.testing.regression as regression

f = cdms2.open(os.path.join(vcs.sample_data,"clt.nc"))
s = f("clt",slice(0,1),squeeze=1)
x = regression.init()
gm = x.createboxfill()
gm.boxfill_type = "custom"
gm.levels = [1.e20,1.e20]
gm.ext_1 = "y"
gm.ext_2 = "y"
x.plot(s, gm, bg=1)
regression.run(x, "test_box_custom_as_def_vistrails_exts.png", sys.argv[1])