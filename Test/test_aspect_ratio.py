import vcs
import cdms2
import sys
import os
import MV2

f=cdms2.open(os.path.join(sys.prefix,"sample_data","clt.nc"))
s=f("clt",time=slice(0,1),squeeze=1)
gm=vcs.createisofill()

def plot_a_ratio(s,gm,ratio):
    x=vcs.init()
    x.open()
    x.geometry(400,800)
    y=vcs.init()
    y.open()
    y.geometry(800,400)
    for X in [x,y]:
        X.plot(s,gm,ratio=ratio)
        if X.islandscape():
            orient = "ldscp"
        else:
            orient = "port"
        X.png("aspect_ratio_%s_%s.png" % (orient,ratio))
    return x

for ratio in ["1t","2t",".5t","autot"]:
    x = plot_a_ratio(s,gm,ratio)
x.interact()



