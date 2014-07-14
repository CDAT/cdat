import vcs
x=vcs.init()
b=x.createboxfill()
#b.list()
e = x.listelements("boxfill")
print e
e = x.getboxfill(e[-1])
print e
#b.list()

#e = x.listelements("projection")
f=x.getfillarea("GEN_seaice_7")
#f.list()

t=x.createtemplate()
#t.list()
e = x.listelements("template")
print e
e = x.gettemplate(e[-1])
print e

tt = x.createtexttable()
#tt.list()
e = x.listelements("texttable")
print e
e = x.gettexttable(e[-1])
print e

to = x.createtextorientation()
#to.list()
e = x.listelements("textorientation")
print e
e = x.gettextorientation(e[-1])
print e


l = x.createline()
#l.list()
e = x.listelements("line")
print e
e = x.getline(e[-1])
print e

isof = x.createisofill()
#isof.list()
e = x.listelements("isofill")
print e
e = x.getisofill(e[-1])
print e

iso = x.createisoline()
#iso.list()
e = x.listelements("isoline")
print e
e = x.getisoline(e[-1])
print e

yx=x.createyxvsx()
#yx.list()
e = x.listelements("yxvsx")
print e
e = x.getyxvsx(e[-1])
print e
yx=x.createxyvsy()
#yx.list()
e = x.listelements("xyvsy")
print e
e = x.getxyvsy(e[-1])
print e
yx=x.createscatter()
#yx.list()
e = x.listelements("scatter")
print e
e = x.getscatter(e[-1])
print e
yx=x.createxvsy()
#yx.list()
e = x.listelements("xvsy")
print e
e = x.getxvsy(e[-1])
print e

e = x.listelements("oned")
print e
e = x.getoneD(e[-1])
print e

v=x.createvector()
#v.list()
e = x.listelements("vector")
print e
e = x.getvector(e[-1])
print e

m=x.createmarker()
m.list()
e=x.listelements("marker")
print e
e=x.getmarker(e[-1])
x.scriptrun("Test/marker.scr")
print x.listelements("marker")
m=x.getmarker('__mark_57568')
m.list()

m=x.createmeshfill()
m.list()
e=x.listelements("meshfill")
print e
e=x.getmeshfill(e[-1])
e.list()


import sys,cdms2
f=cdms2.open(sys.prefix+"/sample_data/clt.nc")
s=f("clt")
iso=x.createisofill()
x.plot(s,iso)

