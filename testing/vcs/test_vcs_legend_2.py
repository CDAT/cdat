import vcs
import argparse
import sys
import vcs.testing.regression as regression

parser = argparse.ArgumentParser()
parser.add_argument("-e","--extensions",default=0,type=int,choices=[0,1,2,3])
parser.add_argument("-o","--orientation",default="v",choices=["v","h"])
parser.add_argument("-a","--arrow",type=float,default=None)
parser.add_argument("-O","--offset",type=float,default=None)
parser.add_argument("-b","--baseline")

args=parser.parse_args(sys.argv[1:])

x=regression.init()

import cdms2, os
f=cdms2.open(os.path.join(vcs.sample_data,"clt.nc"))
s=f("clt",slice(0,1))+210.
iso = x.createisofill()
iso.levels=[210,220,230,240,250,260,270,275,280,285,290,295,300,305,310]
if args.extensions in [1,3]:
    iso.ext_1 = True
if args.extensions in [2,3]:
    iso.ext_2 = True
t=x.createtemplate()
if args.orientation == "v":
    t.scale(.7,'x')
    t.legend.x1=t.data.x2+.03
    t.legend.x2=t.legend.x1+.1
    t.legend.y1=t.data.y1
    t.legend.y2=t.data.y2
else:
    t.legend.y1=.05
    t.legend.y2=.15
    t.legend.x1=t.data.x1
    t.legend.x2=t.data.x2
if args.offset is not None:
    t.legend.offset= args.offset
if args.arrow is not None:
    t.legend.arrow = args.arrow
line = x.createline()
line.color=["red"]
line.type=["dash"]
line.width=[4]
t.legend.line=line
x.plot(s,t,iso,bg=1)
fnm = "test_vcs_legend_%s_%s_%s_%s.png" % (args.orientation,args.offset,args.extensions,args.arrow)
x.png(fnm)
if args.baseline is None:
    src = "./"+fnm
else:
    src = args.baseline
regression.run(x, fnm,src)

