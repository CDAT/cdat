import sys,os
import argparse

p = argparse.ArgumentParser(description="Basic gm testing code for vcs")
p.add_argument("--source", dest="src", help="source image file")
p.add_argument("--mask", dest="mask", action="store_true",help="mask out part of data")
p.add_argument("--show", dest="show", action="store_true",help="show plots on screen (no bg)")
p.add_argument("--projection-type", dest="projtype", default="default", help="use a specific projection type")
p.add_argument("--keep", dest="keep", action="store_true",help="Save image, even if baseline matches.")
p.add_argument("--scale", dest="scale", type=float, help="scale arrows", default=1.)
p.add_argument("--angle", dest="angle", type=int, help="vectors angle",default=45)

args = p.parse_args(sys.argv[1:])

if not args.show:
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
import numpy


bg = not args.show

x=vcs.init()
if bg:
  x.setbgoutputdimensions(1200,1091,units="pixels")
x.setcolormap("rainbow")
gm=vcs.createvector()
if args.projtype != "default":
    p = vcs.createprojection()
    try:
        ptype = int(args.projtype)
    except:
        ptype = args.projtype
    p.type = ptype
    gm.projection = p
nm_xtra=""
xtra = {}
#Creates 4x5 grid
lats = cdms2.createAxis(numpy.arange(-88,89,4))
lats.id="latitude"
lats.units="degrees_north"
lons = cdms2.createAxis(numpy.arange(0,360,5))
lons.id="longitude"
lons.units="degrees_east"
if args.angle in [-45,0,45]:
    u=MV2.ones((45,72))
elif args.angle in [-135,-180,135]:
    u=-MV2.ones((45,72))
else:
    u=MV2.zeros((45,72))
if args.angle in [45,90,135]:
    v=MV2.ones((45,72))
elif args.angle in [-45,-90,-135]:
    v=-MV2.ones((45,72))
else:
    v=MV2.zeros((45,72))
u.setAxis(0,lats)
u.setAxis(1,lons)
v.setAxis(0,lats)
v.setAxis(1,lons)
x.plot(u,v,gm,bg=bg)
ret=0
if args.show:
  x.interact()
else:
  fnm = "test_vcs_basic_vectors_%i" % args.angle
  if args.mask:
      fnm+="_masked"
  if args.scale!=1.:
    fnm+="_%.1g" % args.scale
  fnm+=nm_xtra
  x.png(fnm)
  print "fnm:",fnm
  print "src:",src
  ret = checkimage.check_result_image(fnm+'.png',src,checkimage.defaultThreshold, cleanup=not args.keep)
if args.show:
    raw_input("Press Enter")
sys.exit(ret)
