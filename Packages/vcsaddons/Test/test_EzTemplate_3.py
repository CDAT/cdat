import EzTemplate
## 12 plot one legend per row
import vcs.test.support
bg= vcs.test.support.bg


M=EzTemplate.Multi(rows=4,columns=3)
M.legend.stretch=2.5 # 250% of width (for middle one)
for i in range(12):
    t=M.get(legend='local')
    if i%3 !=1:
        t.legend.priority=0 # Turn off legend

M.preview('test3',bg=bg)
vcs.test.support.check_plot(M.x)
