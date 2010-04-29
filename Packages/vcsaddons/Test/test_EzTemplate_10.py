import EzTemplate,vcs
## 12 plots 1 legend per row on the right
import vcs.test.support
bg= vcs.test.support.bg

## Initialize VCS
x=vcs.init()

M=EzTemplate.Multi(rows=4,columns=3)
M.legend.direction='vertical'
for i in range(12):
    t=M.get(legend='local')
    if i%3 !=2:
        t.legend.priority=0 # Turn off legend
##     x.plot(s[i],t,iso)
M.preview('test3b',bg=bg)
vcs.test.support.check_plot(M.x)
