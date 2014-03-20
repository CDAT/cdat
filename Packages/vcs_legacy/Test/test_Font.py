import vcs_legacy,vcs_legacy.test.support
bg=vcs_legacy.test.support.bg
#bg=0
x=vcs_legacy.init()
#raw_input('x ready press enter')
t=x.createtext()
l1=x.createline()
l2=x.createline()
#t.spacing=50
l2.x=[.5,.5]
l2.y=[0,1.]
l1.x=[0,1.]
l1.y=[.5,.5]
l3=x.createline()
l3.color=242

## t.worldcoordinate=[-2,2,-2,2]
## t.viewport=[.5,1.,.5,1.]
t.x=[.5]
t.y=[.5]
t.color=244
t.height=20
#t.angle=30
t.string=['Hello! E=mc!U2!U3!D2!Dno!D(maybe?)!Dhum...!U!U?']
#t.string=['Hello! E=mc!U2']
#t.string=['_greater!U2!D_GREATER!U2!D_']
t.font=3
paths= ['right','left','down','up']
h = ['left','right','center']
v = ['top','cap','half','base','bottom']
v = ['top','half','base','bottom']
v=['bottom']
h=['center']
angles = range(0,361,45)
#angles=[0,]
for path in paths:
    for halign in h:
        for valign in v:
            for angle in angles:
                print path,halign,valign,angle
                t.angle=angle
                t.valign=valign
                t.halign=halign
                t.path=path
                x.plot(l1,bg=bg)
                x.plot(l2,bg=bg)
                x.plot(t,bg=bg)
##                 ex = x.gettextextent(t)[0]
##                 l3.x=[ex[0][0],ex[1][0],ex[2][0],ex[3][0],(ex[0][0]+ex[1][0])/2.]
##                 l3.y=[ex[0][1],ex[1][1],ex[2][1],ex[3][1],(ex[0][1]+ex[3][1])/2.]
##                 x.plot(l3,bg=bg)
                vcs_legacy.test.support.check_plot(x)
##                 else:
##                     x.png("crap")
##                     import os
## ##                     os.popen("eog crap.png").read()
##                     raw_input()
                x.clear()
                
