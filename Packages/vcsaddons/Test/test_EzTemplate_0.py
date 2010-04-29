import EzTemplate
## 12 plots plotted in reverse order
import vcs.test.support
bg= vcs.test.support.bg

M=EzTemplate.Multi(rows=4,columns=3)

M.preview('test0',bg=bg)
vcs.test.support.check_plot(M.x)
