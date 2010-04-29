import vcs,EzTemplate,MV2,cdms2
import vcs.test.support
bg= vcs.test.support.bg
n = 4

X=vcs.init()
M = EzTemplate.Multi(rows=1,columns=1,legend_direction='vertical',right_margin=.15)
t=M.get()
leg = t.legend
tt=X.gettexttable(leg.texttable)
to=X.gettextorientation(leg.textorientation)
to.list()
tt.list()

OD = EzTemplate.oneD(n=n,template=t)

x = MV2.arange(0,360)
x = x/180.*MV2.pi
ax = cdms2.createAxis(x)
X.portrait()

for i in range(n):
    y = MV2.sin((i+1)*x)
    y.setAxis(0,ax)
    yx = X.createyxvsx()
    yx.linecolor=241+i
    yx.datawc_y1=-1.
    yx.datawc_y2=1.
    t = OD.get()
    X.plot(y,t,yx,bg=bg)

vcs.test.support.check_plot(X)
