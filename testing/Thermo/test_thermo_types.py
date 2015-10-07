import argparse
import cdms2
import os
import sys
import thermo
import vcs

p = argparse.ArgumentParser(description="Thermo types testing code")
p.add_argument("--data", dest="data", help="Data file to use")
p.add_argument("--baseline", dest="baselineImage", help="Baseline image")
p.add_argument("--show", dest="show", action="store_true",
               help="Show plots on screen (no bg)", default=False)
p.add_argument("--type", dest="type", default="stuve",
               help="Type of Thermo plot")
p.add_argument("--keep", dest="keep", action="store_true",
               help="Save image, even if baseline matches.", default=False)

args = p.parse_args(sys.argv[1:])

pth = os.path.join(os.path.dirname(__file__), "..")
sys.path.append(pth)
import checkimage  # noqa

bg = not args.show
f = cdms2.open(args.data)
s = f('ta', time=slice(0, 1), squeeze=1)
f.close()


x = vcs.init()
x.setantialiasing(0)
x.setbgoutputdimensions(1200, 1091, units="pixels")

th = thermo.Gth(x=x, name='my')
th.type = args.type
th.plot_TP(s, bg=bg)

fnm = "test_thermo_%s.png" % (args.type.lower())
th.x.png(fnm)
ret = checkimage.check_result_image(fnm, args.baselineImage,
                                    checkimage.defaultThreshold,
                                    cleanup=not args.keep)
if args.show:
    raw_input("Press Enter")
sys.exit(ret)
