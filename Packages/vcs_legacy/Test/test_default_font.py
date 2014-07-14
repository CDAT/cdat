# Adapted for numpy/ma/cdms2 by convertcdms.py
import vcs_legacy,cdms2 as cdms,sys,support,os
bg=support.bg
f=cdms.open(os.path.join(cdms.__path__[0],'..','..','..','..','sample_data','clt.nc'))
s=f('clt')
x=vcs_legacy.init()
if len(sys.argv)>1 and not '--extended' in sys.argv:
    font = sys.argv[1]
    try:
        ifont = int(font)
        x.setdefaultfont(ifont)
    except:
        x.addfont(font,"new")
        x.setdefaultfont("new")
else:
    x.setdefaultfont(7)
x.plot(s,bg=bg)
support.check_plot(x)
