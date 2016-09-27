# Test animation of projected plots
import argparse, os, sys, cdms2, MV2, vcs, vcs.testing.regression as regression

p = argparse.ArgumentParser(description="Testing animation of projected plots")
p.add_argument("--gm_type", dest="gm", help="gm to test")
p.add_argument("--projection_type", dest="projtype", default="default",
               help="use a specific projection type")
p.add_argument("--source", dest="src", help="path to baseline image")
p.add_argument("--keep", dest="keep", action="store_true", default=False,
               help="Save images, even if baseline matches.")
p.add_argument("--threshold", dest="threshold", type=int,
               default=regression.defaultThreshold,
               help="Threshold value for image differnces")

args = p.parse_args(sys.argv[1:])

gm_type = args.gm
x = regression.init()
s = None

if gm_type == "meshfill":
    f = cdms2.open(os.path.join(vcs.sample_data, "sampleCurveGrid4.nc"))
    s2 = f("sample")

    s = MV2.resize(s2, (4, 32, 48))
    t = cdms2.createAxis(range(4))
    t.units = "months since 2015"
    t.id = "time"
    t.designateTime()
    s.setAxis(0, t)
    s.setAxis(1, s2.getAxis(0))
    s.setAxis(2, s2.getAxis(1))
    s.setGrid(s2.getGrid())
    for i in range(4):
        s[i] = s[i] * (1 + float(i)/10.)
else:
    f = cdms2.open(os.path.join(vcs.sample_data, "clt.nc"))
    s = f("clt", slice(0, 12))  # read only 12 times steps to speed up things

gm = vcs.creategraphicsmethod(gm_type, "default")
if args.projtype != "default":
    p = vcs.createprojection()
    try:
        ptype = int(args.projtype)
    except:
        ptype = args.projtype
    p.type = ptype
    gm.projection = p

x.plot(s, gm, bg=1)
x.animate.create()

prefix = "test_vcs_animate_%s_%s" % (gm_type.lower(), args.projtype.lower())
x.animate.save("%s.mp4" % prefix)
pngs = x.animate.close(preserve_pngs=True)  # so we can look at them again

ret = 0
pdir = os.path.split(pngs[0])[0]
p = pdir + os.sep + "anim_0.png"
ret = regression.check_result_image(p, args.src, args.threshold)
if ret == 0 and not args.keep:
    for f in pngs:
        if os.path.isfile(f):
            os.remove(f)
    os.removedirs(pdir)
    os.remove("%s.mp4" % prefix)
sys.exit(ret)
