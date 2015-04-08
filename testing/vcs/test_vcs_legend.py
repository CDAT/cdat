
import sys,os
import argparse

p = argparse.ArgumentParser(description="Basic gm testing code for vcs")
p.add_argument("--source", dest="src", help="source image file")
p.add_argument("--gm_type", dest="gm", help="gm to test",choices=["boxfill","meshfill","isofill"])
p.add_argument("--show", dest="show", action="store_true",help="show plots on screen (no bg)")
p.add_argument("--keep", dest="keep", action="store_true",help="Save image, even if baseline matches.")
p.add_argument("--orientation",dest="orientation",choices=["horizontal","vertical"],default="horizontal")
p.add_argument("--ext1", dest="ext1", choices=["y","n"], default="n", help="turn on extension 1")
p.add_argument("--ext2", dest="ext2", choices=["y","n"], default="n", help="turn on extension 2")

args = p.parse_args(sys.argv[1:])

gm_type= args.gm
src = args.src
pth = os.path.join(os.path.dirname(__file__),"..")
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
x.drawlogooff()
if bg:
  x.setbgoutputdimensions(1200,1091,units="pixels")
x.setcolormap("rainbow")
exec("gm=vcs.create%s()" % gm_type)
nm_xtra=""
xtra = {'time':slice(0,1),'squeeze':1}
if gm_type=="meshfill":
    f=cdms2.open(os.path.join(vcs.prefix,'sample_data','sampleCurveGrid4.nc'))
else:
    f=cdms2.open(os.path.join(vcs.prefix,'sample_data','clt.nc'))
if gm_type=="meshfill":
    s=f("sample")
else:
    s=f("clt",**xtra)

if gm_type=="boxfill":
    gm.level_1=20
    gm.level_2=80
else:
    if gm_type=="isofill":
        levels = [20, 30, 40, 50, 60, 70, 80]
    else:
        levels = [300,500,800,1000,1200]
    gm.levels=levels
    gm.colors = vcs.getcolors(levels)
tmpl = x.createtemplate()
if args.orientation=="vertical":
    tmpl.data.x2=.8
    tmpl.box1.x2=.8
    tmpl.ytic2.x1=.8
    tmpl.ytic2.x2=.815
    tmpl.legend.x1=.86
    tmpl.legend.x2=.9
    tmpl.legend.y1=.3
    tmpl.legend.y2=.8

if args.ext1=="y":
    gm.ext_1="y"
if args.ext2=="y":
    gm.ext_2="y"
x.plot(s,gm,tmpl,bg=bg)

fnm = "test_vcs_legend_%s_%s_ext1_%s_ext2_%s" % (gm_type.lower(),args.orientation,args.ext1,args.ext2)
x.png(fnm)
print "fnm:",fnm
print "src:",src
ret = checkimage.check_result_image(fnm+'.png',src,checkimage.defaultThreshold, cleanup=not args.keep)
if args.show:
    raw_input("Press Enter")
sys.exit(ret)
