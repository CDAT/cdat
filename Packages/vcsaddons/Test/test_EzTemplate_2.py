import EzTemplate
## 12 plot one legend per plot
import vcs.test.support
bg= vcs.test.support.bg


M=EzTemplate.Multi(rows=4,columns=3)
M.legend.direction='vertical'
for i in range(12):
    t=M.get(legend='local')

M.preview('test2',bg=bg)
vcs.test.support.check_plot(M.x)

