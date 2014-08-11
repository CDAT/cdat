import sys,os
import argparse

p = argparse.ArgumentParser(description="Basic gm testing code for vcs")
p.add_argument("--source", dest="src", help="source image file")
p.add_argument("--gm_type", dest="gm", help="gm to test")

args = p.parse_args(sys.argv[1:])

gm_type= args.gm
src = args.src
pth = os.path.join(os.path.dirname(src),"..")
sys.path.append(pth)
import checkimage

import vcs
import sys
import cdms2
import vtk
import os
import MV2

x=vcs.init()
x.setbgoutputdimensions(1200,1091,units="pixels")
x.setcolormap("rainbow")
exec("gm=vcs.create%s()" % gm_type)
if gm_type=="meshfill":
    f=cdms2.open(os.path.join(sys.prefix,'sample_data','sampleCurveGrid4.nc'))
else:
    f=cdms2.open(os.path.join(sys.prefix,'sample_data','clt.nc'))
if gm_type=="vector":
    u=f("u")
    v=f("v")
elif gm_type=="meshfill":
    s=f("sample")
    gm.mesh=True
else:
    s=f("clt")
bg = True
if gm_type=="vector":
    x.plot(u,v,gm,bg=bg)
else:
    x.plot(s,gm,bg=bg)
fnm = "test_vcs_basic_%s" % gm_type
x.png(fnm)
print "fnm:",fnm
print "src:",src
ret = checkimage.check_result_image(fnm+".png",src,0.05)
sys.exit(ret)
