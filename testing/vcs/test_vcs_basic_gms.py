import sys,os
import argparse

p = argparse.ArgumentParser(description="Basic gm testing code for vcs")
p.add_argument("--source", dest="src", help="source image file")
p.add_argument("--gm_type", dest="gm", help="gm to test")
p.add_argument("--mask", dest="mask", action="store_true",help="mask out part of data")
p.add_argument("--show", dest="show", action="store_true",help="show plots on screen (no bg)")

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

bg = not args.show

x=vcs.init()
x.setbgoutputdimensions(1200,1091,units="pixels")
x.setcolormap("rainbow")
if gm_type=="oned":
    gm_type="oneD"
exec("gm=vcs.create%s()" % gm_type)
if gm_type=="meshfill":
    f=cdms2.open(os.path.join(sys.prefix,'sample_data','sampleCurveGrid4.nc'))
else:
    f=cdms2.open(os.path.join(sys.prefix,'sample_data','clt.nc'))
if gm_type=="vector":
    u=f("u")
    v=f("v")
    if args.mask:
        u=MV2.masked_greater(u,58.)
elif gm_type=="meshfill":
    s=f("sample")
    gm.mesh=True
    print "ARGS MASK:",args.mask
    if args.mask:
        s=MV2.masked_greater(s,1450.)
else:
    s=f("clt")
    if args.mask:
        s=MV2.masked_greater(s,78.)
    if gm_type in ["oneD","yxvsx","xyvsy","xvsy","scatter"]:
        s = s(latitude=(20,20,"cob"),longitude=(112,112,"cob"),squeeze=1)
        s2=MV2.sin(s)
if gm_type=="vector":
    x.plot(u,v,gm,bg=bg)
elif gm_type in ["scatter","xvsy"]:
    gm.list()
    x.plot(s,s2,gm,bg=bg)
else:
    x.plot(s,gm,bg=bg)
fnm = "test_vcs_basic_%s" % gm_type.lower()
if args.mask:
    fnm+="_masked"
x.png(fnm)
print "fnm:",fnm
print "src:",src
ret = checkimage.check_result_image(fnm+".png",src,0.05)
if args.show:
    raw_input("Press Enter")
sys.exit(ret)
