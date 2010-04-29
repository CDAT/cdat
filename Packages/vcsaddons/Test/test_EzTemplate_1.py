import EzTemplate
import vcs.test.support
bg= vcs.test.support.bg

M=EzTemplate.Multi(rows=4,columns=3)

for i in range(12):
    t=M.get()
##     x.plot(s[i],t,iso)
M.preview('test',bg=bg)
vcs.test.support.check_plot(M.x)
