import cdms2
import WK
import MV2
import sys
import os
test_data_dir = sys.argv[1]
src1 = os.path.join(test_data_dir, "baselines", "WK", "test_wk_01_a.png")
src2 = os.path.join(test_data_dir, "baselines", "WK", "test_wk_01_b.png")
src3 = os.path.join(test_data_dir, "baselines", "WK", "test_wk_01_c.png")
pth = os.path.join(os.path.dirname(__file__), "..")
sys.path.append(pth)
import checkimage
bg = False
nmonths = 5
# Getting about nmonths months worth of data

W = WK.WK()
data_dir = os.path.join(test_data_dir, "data", "WK")

f = cdms2.open(os.path.join(data_dir, "data.nc"))
s = f("rlut", time=slice(0, nmonths * 30), latitude=(-15, 15))
f.close()
f = cdms2.open(os.path.join(data_dir, "results.nc"))

# Process the data, i.e compute spectral wave number and frequencies
power = W.process(s)
ok = f('power')
if not MV2.allclose(power, ok):
    raise Exception('Error computing power, wrong values returned')

# Split between Sym and ASym components
# Averages over time if compresstime is True (default)
S, A = W.split(power)  # ,compresstime=False,smooth=False)
ok = f('S')
if not MV2.allclose(S, ok):
    raise Exception('Error computing symetric, wrong values returned')
ok = f('A')
if not MV2.allclose(A, ok):
    raise Exception('Error computing antisymetric, wrong values returned')

# Now tries to do plot this...
WP = WK.WKPlot()
WP.x.scriptrun(os.path.join(data_dir, "colormap.scr"))
WP.x.setcolormap("cmap")
print 'Plotting 1'
WP.plot_figure1(S, A, bg=bg)
WP.x.setantialiasing(0)
fnm = "test_wk_01_a.png"
WP.x.png(fnm)
ret = checkimage.check_result_image(fnm, src1, checkimage.defaultThreshold)
if ret != 0:
    sys.exit(ret)

background = W.background(S, A)
ok = f('background')
if not MV2.allclose(background, ok):
    raise Exception('Error computing background, wrong values returned')

WP.x.clear()
print 'Plotting 2'
WP.plot_figure2(background, min=-1, max=2, bg=bg)
fnm = "test_wk_01_b.png"
WP.x.png(fnm)
ret = checkimage.check_result_image(fnm, src2, checkimage.defaultThreshold)
if ret != 0:
    sys.exit(ret)


sid = S.id
aid = A.id
S /= background
A /= background
S.id = sid
A.id = aid

WP.x.clear()
print 'Plotting 3'
WP.plot_figure3(S, A, delta_isofill=.2, delta_isoline=.1, bg=bg)
fnm = "test_wk_01_c.png"
WP.x.png(fnm)
ret = checkimage.check_result_image(fnm, src3, checkimage.defaultThreshold)
if ret != 0:
    sys.exit(ret)
