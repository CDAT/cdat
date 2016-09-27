import os, sys, MV2, cdms2, vcs, vcs.testing.regression as regression

x = regression.init()
f = cdms2.open(os.path.join(vcs.sample_data,"clt.nc"))
s = f("clt",slice(0,1),latitude=(30, 70),longitude=(-130, -60))
s2 = MV2.masked_greater(s, 65.)
x.plot(s2,"default","isofill",bg=1)
regression.run(x, "test_vcs_isofill_mask_cell_shift.png")