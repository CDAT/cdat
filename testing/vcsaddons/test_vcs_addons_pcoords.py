
import sys,os
data = sys.argv[1]
src = sys.argv[2]
import vcs.testing.regression as regression
import vcs
import vcsaddons, numpy
import cdms2

f = cdms2.open(data)
rms_xyt = f("rms_xyt")

ax1 = cdms2.createAxis(['0071-0100' ,'ACCESS1-0' ,'ACCESS1-3' ,'CCSM4' ,'CESM1-BGC' ,'CESM1-CAM5',
     'CESM1-FASTCHEM' ,'CESM1-WACCM' ,'CSIRO-Mk3-6-0' ,'FGOALS-g2' ,'GFDL-CM3',
      'GFDL-ESM2G' ,'GFDL-ESM2M' ,'HadGEM2-AO' ,'MIROC4h' ,'bcc-csm1-1',
       'bcc-csm1-1-m'],id="models")
ax2 = cdms2.createAxis(['pr', 'prw', 'psl', 'rltcre', 'rlut', 'rstcre', 'ta-200', 'ta-850', 'tas', 'ua-200',
     'ua-850', 'va-200', 'va-850', 'zg-500'],id="statistic")

rms_xyt.setAxisList([ax2,ax1])

x = regression.init(geometry=(1200,600))

import vcsaddons
bg = False
gm = vcsaddons.createparallelcoordinates(x=x)
t = vcs.createtemplate()
to=x.createtextorientation()
to.angle=-45
to.halign="right"
t.xlabel1.textorientation = to.name
t.reset('x',0.05,0.9,t.data.x1,t.data.x2)
#t.reset('y',0.5,0.9,t.data.y1,t.data.y2)
ln = vcs.createline()
ln.color = [[0,0,0,0]]
t.legend.line = ln
t.box1.priority=0
t.legend.x1 = .91
t.legend.x2 = .99
t.legend.y1 = t.data.y1
t.legend.y2 = t.data.y2

# Set variable name
rms_xyt.id = "RMS"

# Set units of each variables on axis
rms_xyt.getAxis(-2).units = ["mm/day","mm/day","hPa","W/m2","W/m2","W/m2", "K","K","K","m/s","m/s","m/s","m/s","m"]
# Sets title
rms_xyt.title = "Annual Mean Error"

gm.plot(rms_xyt,template=t,bg=bg)

print src
fnm = os.path.join(os.getcwd(), "testParallelCoordinates.png")
x.png(fnm)
ret = vcs.testing.regression.check_result_image(
    fnm,
    src)
sys.exit(ret)
