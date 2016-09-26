import sys,os
src = sys.argv[1]
import vcs.testing.regression as regression
import vcs, cdms2
import vcsaddons, numpy

x = regression.init()

cdmsfile = cdms2.open(vcs.sample_data + "/clt.nc")
clt = cdmsfile("clt")

levels = [10, 20, 30, 40, 60, 70, 80, 90, 100]
histo = vcsaddons.histograms.Ghg()
histo.bins = levels
histo.line = ["solid", "dash", "dash-dot"]
histo.linewidth = [1, 2, 3]
histo.linecolors = ["red", "green", "blue"]
histo.fillareastyles = ["solid", "hatch", "pattern", "solid"]
histo.fillareaindices = [1, 2, 3, 4]
histo.fillareacolors = ["blue", "green", "red", "orange"]

histo2 = vcsaddons.createhistogram(source=histo)

print "Checking all inherited attributes..."
assert histo2.bins == histo.bins
assert histo2.line == histo.line
assert histo2.linewidth == histo.linewidth
assert histo2.linecolors == histo.linecolors
assert histo2.fillareastyles == histo.fillareastyles
assert histo2.fillareacolors == histo.fillareacolors
assert histo2.fillareaindices == histo.fillareaindices
print "Inherited all values."

histo2.levels = [10, 20, 10, 100, 110, 50, 20]
histo3 = vcsaddons.createhistogram(source=histo2.name, x=x)

print "Checking name-based inheritance"
assert histo3.bins == histo2.bins
assert histo3.line == histo2.line
assert histo3.linewidth == histo2.linewidth
assert histo3.linecolors == histo2.linecolors
assert histo3.fillareastyles == histo2.fillareastyles
assert histo3.fillareacolors == histo2.fillareacolors
assert histo3.fillareaindices == histo2.fillareaindices
print "Inherited all values."

histo3.datawc_y1 = -1
histo3.datawc_y2 = 200000
histo3.datawc_x1 = 0
histo3.datawc_x2 = 100

histo3.bins = None
histo3.plot(clt, template="default", bg=True)

fnm = "test_vcs_addons_histogram_inherit.png"
x.png(fnm)
ret = regression.check_result_image(fnm, src)
sys.exit(ret)
