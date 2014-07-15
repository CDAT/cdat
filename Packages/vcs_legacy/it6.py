import vcs,cdms2,sys
x=vcs.init()
y=vcs.init()
y.open()
l=x.createline()
l.x=[.2,.2,.5,.8,.8]
l.y=[.2,.8,.7,.8,.2]
l.width=6

f=cdms2.open(sys.prefix+'/sample_data/clt.nc')

s=f("clt")
s2=s*.8

x.plot(l)
#x.plot(s)

raw_input()
y.open()

raw_input()

