import vcs
x=vcs.init()
b=x.createboxfill()
#b.list()
print x.listelements("boxfill")
b=x.getboxfill("mercator")
#b.list()
print x.listelements("projection")
f=x.getfillarea("GEN_seaice_7")
print f.list()

t=x.createtemplate()
t.list()
print x.listelements("template")

#import sys,cdms2
#f=cdms2.open(sys.prefix+"/sample_data/clt.nc")
#s=f("clt")
#x.plot(s,b)

