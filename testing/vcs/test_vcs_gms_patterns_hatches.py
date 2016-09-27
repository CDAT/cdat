import argparse, os, sys, cdms2, vcs, vcs.testing.regression as regression

p = argparse.ArgumentParser(description="Patterns/hatches testing code for vcs gms")
p.add_argument("--source", dest="src", help="source image file")
p.add_argument("--gm_type", dest="gm", help="gm to test", default="isofill")
p.add_argument("--fill_style", dest="fill_style", help="Patterns/hatches fill style",
               default="pattern", type=str)
p.add_argument("--show", dest="show", action="store_true", help="show plots on screen (no bg)", default=False)
p.add_argument("--projection-type", dest="projtype", default="default", help="use a specific projection type")
p.add_argument("--lat1", dest="lat1", default=-90, type=float, help="First latitude")
p.add_argument("--lat2", dest="lat2", default=90, type=float, help="Last latitude")
p.add_argument("--lon1", dest="lon1", default=-180, type=float, help="First Longitude")
p.add_argument("--lon2", dest="lon2", default=180, type=float, help="Last Longitude")
p.add_argument("--keep", dest="keep", action="store_true", help="Save image, even if baseline matches.")
p.add_argument("--threshold", dest="threshold", type=int, default=regression.defaultThreshold,
        help="Default threshold")
p.add_argument("--non-contiguous", dest="contig", default=True, action="store_false", help="use non contiguous levels")

args = p.parse_args(sys.argv[1:])

gm_type = args.gm
src = args.src

bg = not args.show

x = vcs.init()
x.setantialiasing(0)
x.setcolormap("classic")

if bg:
    x.setbgoutputdimensions(1200, 1091, units="pixels")
gm = vcs.creategraphicsmethod(gm_type, "default")
if args.projtype != "default":
    p = vcs.createprojection()
    try:
        ptype = int(args.projtype)
    except:
        ptype = args.projtype
    p.type = ptype
    gm.projection = p

if args.contig:
    gm.levels = [220, 230, 240, 250, 260, 270, 280, 290, 300, 310, 320]
else:
    gm.levels = [[230,235],[240,245],[250,255],[260,265],[270,275],
                 [280,285],[290,295],[300,305],[310,315],[320,325]]
gm.fillareastyle = args.fill_style
gm.fillareacolors = [242, 244, 237, 248, 250, 252, 44, 243, 139, 247]
if args.fill_style == "hatch":
    gm.fillareaindices = [2, 4, 6, 8, 10, 12, 14, 16, 18, 20]
    gm.fillareaopacity = [50, 75, 20, 100, 25, 30, 40, 80, 60, 100]
else:
    gm.fillareaindices = [1, 3, 5, 7, 9, 11, 18, 15, 17, 19]
    gm.fillareaopacity = [50, 75, 20, 0, 25, 30, 100, 0, 60, 0]

if args.gm == "boxfill":
    gm.boxfill_type = "custom"

if args.gm == "meshfill":
    gm.mesh = True

nm_xtra = ""
xtra = {}
if args.lat1 != args.lat2:
    gm.datawc_y1 = args.lat1
    gm.datawc_y2 = args.lat2
    xtra["latitude"] = (args.lat1, args.lat2)
    if args.lat1 < 0:
        nm_xtra += "_SH"
    else:
        nm_xtra += "_NH"
if args.lon1 != args.lon2:
    xtra["longitude"] = (args.lon1, args.lon2)
    nm_xtra += "_%i_%i" % (args.lon1, args.lon2)
if not args.contig:
    nm_xtra += "_non-contig"

xtra["time"] = slice(0, 1)
xtra["squeeze"] = 1

f = cdms2.open(os.path.join(vcs.sample_data, 'tas_ccsr-95a_1979.01-1979.12.nc'))
s = f("tas", **xtra)
f.close()

x.plot(s, gm, bg=bg)
fnm = "test_vcs_%s_%s" % (args.gm.lower(), args.fill_style.lower())
if args.projtype != "default":
    fnm += "_%s_proj" % args.projtype
fnm += nm_xtra
x.png(fnm)
print "fnm:", fnm
print "src:", src
ret = regression.check_result_image(fnm+'.png', src,
                                    args.threshold,
                                    cleanup=not args.keep)
if args.show:
    raw_input("Press Enter")
sys.exit(ret)
