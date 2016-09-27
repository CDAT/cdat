import argparse, os, sys, cdms2, MV2, vcs.testing.regression as regression, vcs, vtk

p = argparse.ArgumentParser(description="Basic gm testing code for vcs")
p.add_argument("--source", dest="src", help="source image file")
p.add_argument("--gm_type", dest="gm", help="gm to test")
p.add_argument("--show", dest="show", action="store_true",help="show plots on screen (no bg)")
p.add_argument("--projection-type", dest="projtype", default="default", help="use a specific projection type")
p.add_argument("--lat1", dest="lat1", default=0, type=float, help="First latitude")
p.add_argument("--lat2", dest="lat2", default=0, type=float, help="Last latitude")
p.add_argument("--lon1", dest="lon1", default=0, type=float, help="First Longitude")
p.add_argument("--lon2", dest="lon2", default=0, type=float, help="Last Longitude")
p.add_argument("--range_via_gm", dest="rg", action="store_true", help="Set the range via graphic method ")
p.add_argument("--gm_flips_lat_range", dest="flip", action="store_true", help="Set the range via graphic method to flip of data")
p.add_argument("--zero", dest="zero", action="store_true", help="Set the data to zero everywhere")
p.add_argument("--keep", dest="keep", action="store_true",help="Save image, even if baseline matches.")
p.add_argument("--transparent", dest="transparent", action="store_true",help="Add transparency to colors")

dataMods = p.add_mutually_exclusive_group()
dataMods.add_argument("--mask", dest="mask", action="store_true",help="mask out part of data")
dataMods.add_argument("--bigvalues", dest="bigvalues", action="store_true",help="replace some of the data with 1e40")

args = p.parse_args(sys.argv[1:])

gm_type= args.gm
src = args.src
bg = not args.show

x = vcs.init()
x.setantialiasing(0)
x.drawlogooff()
if bg:
  x.setbgoutputdimensions(1200,1091,units="pixels")
exec("gm=vcs.create%s()" % gm_type)
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
if args.lat1!=args.lat2:
    if args.rg:
        if args.flip:
            gm.datawc_y1=args.lat2
            gm.datawc_y2=args.lat1
            nm_xtra+="_gmflip"
        else:
            gm.datawc_y1=args.lat1
            gm.datawc_y2=args.lat2
    xtra["latitude"] = (args.lat1,args.lat2)
    if args.lat1<0:
        nm_xtra+="_SH"
    else:
        nm_xtra+="_NH"
if args.lon1!=args.lon2:
    if args.rg:
        gm.datawc_x1=args.lon1
        gm.datawc_x2=args.lon2
    xtra["longitude"] = (args.lon1,args.lon2)
    nm_xtra+="_%i_%i" % (args.lon1,args.lon2)
if args.rg:
    nm_xtra+="_via_gm"
if gm_type=="meshfill":
    f=cdms2.open(os.path.join(vcs.sample_data,'sampleCurveGrid4.nc'))
else:
    f=cdms2.open(os.path.join(vcs.sample_data,'clt.nc'))
if gm_type=="vector":
    u=f("u",**xtra)
    v=f("v",**xtra)
    if args.mask:
        u=MV2.masked_greater(u,58.)
    if args.zero:
      u-=u
      v-=v
elif gm_type=="meshfill":
    s=f("sample",**xtra)
    if args.mask:
        s=MV2.masked_less(s,1150.)
    elif args.bigvalues:
        s[s < 1150] = 1e40
    if args.zero:
       s-=s
else:
    s=f("clt",**xtra)
    if args.mask:
        s=MV2.masked_greater(s,78.)
    elif args.bigvalues:
        s[s > 78] = 1e40
    if gm_type in ["1d","yxvsx","xyvsy","xvsy","scatter"]:
        s = s(latitude=(20,20,"cob"),longitude=(112,112,"cob"),squeeze=1)
        s2=MV2.sin(s)
        if args.zero:
           s2-=s2
    if args.zero:
       s-=s

if args.bigvalues:
    gm.levels = [0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 1.e36]

if args.transparent:
    cmap = x.createcolormap()
    for i in range(256):  # tweaks all colors
        cmap.setcolorcell(i,100.,0,0,i/2.55)
    x.setcolormap(cmap)
    if gm_type == "vector":
        gm.linecolor = [100, 0, 0, 50.]
    elif gm_type in ["yxvsx","xyvsy","yvsx","scatter","1d"]:
        gm.linecolor = [100, 0, 0, 50.]
        gm.markercolor = [100, 0, 0, 50.]

if gm_type=="vector":
    gm.scale = 4.
    x.plot(u,v,gm,bg=bg)
elif gm_type in ["scatter","xvsy"]:
    x.plot(s,s2,gm,bg=bg)
else:
    x.plot(s,gm,bg=bg)
fnm = "test_vcs_basic_%s" % gm_type.lower()
if args.mask:
    fnm+="_masked"
elif args.bigvalues:
    fnm+="_bigvalues"
if args.projtype!="default":
    fnm+="_%s_proj" % args.projtype
if args.zero:
   fnm+="_zero"
if args.transparent:
    fnm+="_transparent"
fnm+=nm_xtra
x.png(fnm)
print "fnm:",fnm
print "src:",src
if args.show:
    raw_input("Press Enter")
ret = regression.check_result_image(fnm+'.png',src,20., cleanup=not args.keep)
if args.show:
    raw_input("Press Enter")
sys.exit(ret)
