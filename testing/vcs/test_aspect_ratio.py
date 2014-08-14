import vcs
import cdms2
import sys
import os
import MV2
import vcs,numpy,cdms2,MV2,os,sys
src=sys.argv[1]
pth0 = os.path.dirname(src)
pth = os.path.join(pth0,"..")
sys.path.append(pth)
import checkimage

f=cdms2.open(os.path.join(sys.prefix,"sample_data","clt.nc"))
s=f("clt",time=slice(0,1),squeeze=1)
gm=vcs.createisofill()

def plot_a_ratio(s,gm,ratio):
    ret = 0
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
        fnm = "aspect_ratio_%s_%s.png" % (orient,ratio)
        X.png(fnm)
        print "fnm:",fnm
        src = os.path.join(pth0,fnm)
        print "src:",src
        ret += checkimage.check_result_image(fnm,src,0.05)
    return ret

ret = 0 
for ratio in ["1t","2t",".5t","autot"]:
    ret  += plot_a_ratio(s,gm,ratio)


sys.exit(ret)




