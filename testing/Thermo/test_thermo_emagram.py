import cdms2
import os
import sys
import thermo
import vcs

pth = os.path.join(os.path.dirname(__file__), "..")
sys.path.append(pth)
import checkimage  # noqa

test_data_dir = sys.argv[1]
baselineImage = os.path.join(test_data_dir, "baselines", "Thermo",
                             "test_thermo_emagram.png")
data_dir = os.path.join(test_data_dir, "data", "Thermo")

f = cdms2.open(
        os.path.join(data_dir,
                     "dar.meteo.mod.cam3.era.v31.h0.l3.nrstpt.cp.2000070100.2000080100.tau.12.36.nc"))
s = f('ta', time=slice(0, 1), squeeze=1)
f.close()


x = vcs.init()
x.setantialiasing(0)
x.setbgoutputdimensions(1200, 1091, units="pixels")

th = thermo.Gth(x=x, name='my')
th.type = "emagram"
th.plot_TP(s, bg=True)

fnm = "test_thermo_emagram.png"
th.x.png(fnm)
ret = checkimage.check_result_image(fnm, baselineImage,
                                    checkimage.defaultThreshold)
sys.exit(ret)
