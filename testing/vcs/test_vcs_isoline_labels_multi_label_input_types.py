import os, sys, cdms2, vcs, vcs.testing.regression as regression

f = cdms2.open(os.path.join(vcs.sample_data,"clt.nc"))
s = f("clt")
x = regression.init()
iso = x.createisoline()
t = x.createtext()
t.color = 243
t.height = 25
to = x.createtextorientation()
to.height = 55
tt = x.createtexttable()
tt.color = 245
iso.textcolors = [None,None,None,242,244]
iso.text = [t,tt,to]
iso.label = "y"
x.plot(s, iso, bg=1)
regression.run(x, "test_vcs_isoline_labels_multi_label_input_types.png")
