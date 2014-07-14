# Adapted for numpy/ma/cdms2 by convertcdms.py
import cdms2 as cdms,vcs_legacy,sys,support
bg=support.bg
x=vcs_legacy.init()

f=x.createfillarea('test')
f.x=[.2,.2,.8,.8]
f.y=[.2,.8,.8,.2]
f.color=243
f.style='hatch'

x.plot(f,bg=bg)
support.check_plot(x)
