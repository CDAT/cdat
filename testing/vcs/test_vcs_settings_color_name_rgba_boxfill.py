import vcs, numpy, os, sys, cdms2, vcs.testing.regression as regression

x = regression.init()
f=cdms2.open(os.path.join(vcs.sample_data,"clt.nc"))
data=f("clt",slice(0,1,))
gm = x.createboxfill()
gm.boxfill_type = "custom"
gm.levels = range(0,110,10)
gm.fillareacolors = ["green","red","blue","bisque","yellow","grey",
        [100,0,0,50], [0,100,0],"salmon",[0,0,100,75]]
x.plot(data,gm,bg=True)
regression.run(x, 'test_vcs_settings_color_name_rgba_boxfill.png')