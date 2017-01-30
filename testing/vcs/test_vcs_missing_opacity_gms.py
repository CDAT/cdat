import argparse
import cdms2
import os
import sys
import vcs
import vcs.testing.regression as regression

p = argparse.ArgumentParser(description="Missing opacity test for vcs gms")
p.add_argument("--gm_type", dest="gm", help="gm to test", default="isofill")
p.add_argument("--source", dest="src", help="source image file")
p.add_argument("--threshold", dest="threshold", type=int, default=regression.defaultThreshold,
               help="Default threshold")
p.add_argument("--show", dest="show", action="store_true", help="show plots on screen (no bg)", default=False)
p.add_argument("--keep", dest="keep", action="store_true", help="Save image, even if baseline matches.")

args = p.parse_args(sys.argv[1:])

gm_type = args.gm

bg = not args.show

# Load the clt data
f = cdms2.open(os.path.join(vcs.sample_data, "clt.nc"))
clt = f("clt")

# Initialize the canvas
canvas = regression.init()

# Plot the dataset
canvas.plot(clt, bg=bg)

# Mask values
masked = cdms2.MV2.masked_greater(clt, 50.)
gm = vcs.creategraphicsmethod(gm_type, "default")

# Set the missing color and opacity
gm.missing = [50., 50., 50., 50.]

# Plot the masked values
canvas.plot(masked, gm, bg=bg)

# Write to png file
fnm = "test_vcs_missing_opacity_%s" % args.gm.lower()
canvas.png(fnm)

# Test the output
ret = regression.check_result_image(fnm + '.png', args.src,
                                    args.threshold,
                                    cleanup=not args.keep)

if args.show:
    raw_input("Press Enter")

sys.exit(ret)
