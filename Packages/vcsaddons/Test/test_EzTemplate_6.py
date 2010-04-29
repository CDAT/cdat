import EzTemplate
import vcs.test.support
bg= vcs.test.support.bg
## 12 plot one legend every other plot various orientation for legend

M=EzTemplate.Multi(rows=4,columns=3)
for i in range(12):
    if i%2==1:
        if i%4 == 3:
            M.legend.direction='vertical'
        t=M.get(legend='local')
        M.legend.direction='horizontal'
    else:
        t=M.get()
M.preview('test6',bg=bg)
vcs.test.support.check_plot(M.x)
