# Adapted for numpy/ma/cdms2 by convertcdms.py
import vcs_legacy,cdms2 as cdms,os,sys,support
tp = vcs_legacy.init()
bg=support.bg
tp.portrait()

fa = tp.createfillarea('fa')
fa.x = [0.1,0.9,0.9,0.1]
fa.y=[.9,.9,0.1,.1]
fa.color=242
n=6
i=0
tp.plot(fa,bg=bg)
support.check_plot(tp)
tp.mode=0
# Testing update off, should always plot same thing
for i in range(1,3):
  fa.y = [.9,.9,0.1,.1*i]
  fa.x = [0.1,0.9,0.9,0.1*i]
  support.check_plot(tp)
## Now update should have a different plot
tp.update()
support.check_plot(tp)
## Now testing auto update
tp.mode=1
for i in range(n):
  fa.y = [.9,.9,0.1,.1*i]
  fa.x = [0.1,0.9,0.9,0.1*i]
  support.check_plot(tp)


