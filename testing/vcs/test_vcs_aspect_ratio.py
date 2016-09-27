import vcs, numpy, cdms2, MV2, os, sys, vcs.testing.regression as regression
src = sys.argv[1]
pth0 = os.path.dirname(_file__)
pth = os.path.join(pth0,"..")
sys.path.append(pth)

f = cdms2.open(os.path.join(vcs.sample_data,"clt.nc"))
s = f("clt",time=slice(0,1),squeeze=1)
gm = vcs.createisofill()

def plot_a_ratio(s,gm,ratio):
    ret = 0
    x = regression.init()
    x.drawlogooff()
    x.open()
    x.geometry(400,800)
    y = regression.init()
    y.open()
    y.geometry(800,400)
    for X in [x,y]:
        X.plot(s,gm,ratio=ratio)
        if X.islandscape():
            orient = "ldscp"
        else:
            orient = "port"
        fnm = "aspect_ratio_%s_%s.png" % (orient,ratio)
        X.png(fnm)
        src = os.path.join(pth0,fnm)
        ret += regression.check_result_image(fnm, src)
    return ret

ret = 0
for ratio in ["1t","2t",".5t","autot"]:
    ret  += plot_a_ratio(s,gm,ratio)


sys.exit(ret)