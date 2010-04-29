import EzTemplate
## 12 plots plotted with different spacing param
import vcs.test.support
bg= vcs.test.support.bg

M=EzTemplate.Multi(rows=4,columns=3)
M.spacing.horizontal=.25
M.spacing.vertical=.1
M.preview('test8')
vcs.test.support.check_plot(M.x)

