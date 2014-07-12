import vcs_legacy,os,support
bg=support.bg

tp = vcs_legacy.init()
tp.portrait()

lin = tp.createline('lin')
lin.x = [0.1,0.9,0.9,0.1]
lin.y=[.9,.9,0.1,.1]
lin.color=242
n=6
i=0
tp.plot(lin,bg=bg)
tp.mode=0
#no update plot should be the same until update
for i in range(1,3):
  lin.y = [.9,.9,0.1,.1*i]
  lin.x = [0.1,0.9,0.9,0.1*i]
  support.check_plot(tp)
tp.update() # should change the plot!
support.check_plot(tp)
tp.mode=1
for i in range(1,n):
  lin.y = [.9,.9,0.1,.1*i]
  lin.x = [0.1,0.9,0.9,0.1*i]
  support.check_plot(tp)


