import vcs, numpy, os, sys, cdms2, vcs.testing.regression as regression

x = regression.init()
f=cdms2.open(os.path.join(vcs.sample_data,"sampleCurveGrid4.nc"))
data=f("sample")
gm = x.createmeshfill()
gm.levels = range(0,1501,150)
gm.fillareacolors = ["green","red","blue","bisque","yellow","grey",
        [100,0,0,50], [0,100,0],"salmon",[0,0,100,75]]
x.plot(data,gm,bg=True)
ret = regression.run(x, 'test_vcs_settings_color_name_rgba_meshfill.png')