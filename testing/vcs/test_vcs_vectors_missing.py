import sys,os
import argparse

p = argparse.ArgumentParser(description="Basic gm testing code for vcs")
p.add_argument("--source", dest="src", help="source image file")
p.add_argument("--show", dest="show", action="store_true",help="show plots on screen (no bg)")
p.add_argument("--keep", dest="keep", action="store_true",help="Save image, even if baseline matches.")
p.add_argument("--scale", dest="scale", type=float, help="scale arrows", default=1.)

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
gm.scale = args.scale
nm_xtra=""
xtra = {}
import cdms2
import os
f=cdms2.open(os.path.join(sys.prefix,"sample_data","clt.nc"))
u=f("u")
v=f("v")
u=MV2.masked_greater(u,58.)
v=MV2.masked_greater(v,888.)
print u.max(),v.max(),u.min(),v.min()
#x.plot(U)
x.plot(u,v,gm,bg=bg)
ret=0
if args.show:
  pass
  #x.interact()
else:
  fnm = "test_vcs_vectors_missing" 
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
