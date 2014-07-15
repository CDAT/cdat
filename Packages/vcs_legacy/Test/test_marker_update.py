import vcs_legacy,sys,os,support
bg=support.bg

tp = vcs_legacy.init()
tp.portrait()

mark = tp.createmarker('mark')
mark.x = [0.5]
mark.y=[.9]
mark.color=242
mark.type='cross'
mark.size=12
n=4
i=0
tp.plot(mark,bg=bg)
support.check_plot(tp)
tp.mode=0 # no aauto update
for i in range(1,3):
  mark.y = [1.-(i/float(n))]
  tp.plot(mark,bg=bg)
  support.check_plot(tp)
tp.update()
support.check_plot(tp)
tp.mode=1
for i in range(1,n):
  mark.y = [1.-(i/float(n))]
  tp.plot(mark,bg=bg)
  support.check_plot(tp)

