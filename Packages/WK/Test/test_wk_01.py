# Adapted for numpy/ma/cdms2 by convertcdms.py
import cdms2 as cdms,WK,MV2 as MV
import vcs.test.support
bg = vcs.test.support.bg

nmonths = 5
## Getting about nmonths months worth of data

W = WK.WK()
f=cdms.open("data.nc")
s=f("rlut",time=slice(0,nmonths*30),latitude=(-15,15))
f.close()
f=cdms.open("results.nc")

## Process the data, i.e compute spectral wave number and frequencies
power = W.process(s)
ok = f('power')
if not MV.allclose(power,ok):
    raise Exception,'Error computing power, wrong values returned'

## Split between Sym and ASym components
## Averages over time if compresstime is True (default)
S,A = W.split(power) #,compresstime=False,smooth=False)
ok = f('S')
if not MV.allclose(S,ok):
    raise Exception,'Error computing symetric, wrong values returned'
ok=f('A')
if not MV.allclose(A,ok):
    raise Exception,'Error computing antisymetric, wrong values returned'

## Now tries to do plot this...
WP = WK.WKPlot()
WP.x.scriptrun("colormap.scr")
WP.x.setcolormap("cmap")
print 'Plotting 1'
WP.plot_figure1(S,A,bg=bg)
vcs.test.support.check_plot(WP.x)

background = W.background(S,A)
ok = f('background')
if not MV.allclose(background,ok):
    raise Exception,'Error computing background, wrong values returned'

WP.x.clear()
print 'Plotting 2'
WP.plot_figure2(background,min=-1,max=2,bg=bg)
vcs.test.support.check_plot(WP.x)


sid=S.id
aid=A.id
S/=background
A/=background
S.id=sid
A.id=aid

WP.x.clear()
print 'Plotting 3'
WP.plot_figure3(S,A,delta_isofill=.2,delta_isoline=.1,bg=bg)
vcs.test.support.check_plot(WP.x)
