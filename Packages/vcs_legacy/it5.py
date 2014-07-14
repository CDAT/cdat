#/usr/bin/env python
import sys,vcs,time
x=vcs.init()
#x.open()
l=x.createline()
l.x=[.2,.2,.5,.8,.8]
l.y=[.2,.8,.6,.8,.2]
l.width=5
l.color=242
x.plot(l)
raw_input()
x.geometry(640,480,10,30)
raw_input()
y=vcs.init()
y.open()
#y.geometry(640,480,100,30)
l2=x.createline()
l2.x=[.2,.2,.5,.8,.8]
l2.y=[.2,.8,.9,.8,.2]
l2.width=5
l2.color=243

y.plot(l2)

raw_input()
y.close()
raw_input()
